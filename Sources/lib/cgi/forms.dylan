module:		forms
author:         Peter Hinely <phinely@hawaii.edu>
copyright:      (C) 1996-1998 Peter Hinely
version:        0.12, 2/1/98
synopsis:       HTTP cgi utilities

// Caution:  Ugly code which needs to be cleaned up
// This is some code for parsing cgi input from html forms.
// The parsing method is decent, but everything
// needs to be cleaned up.  
// There isn't much functionality here yet, but it did what I needed.
// If you use this, please email me at the address above.
// Also email me changes for me to incorporate into this distribution.

//////////////////////////////////////////////////////////////////////////
define constant $debug = #t;
define constant $debug-stream = make(<byte-string-stream>, direction: #"output");
define constant $multiple-value-suffix = "-list";

//////////////////////////////////////////////////////////////////////////
// define method main (argv0 :: <string>, #rest args)
define method main ()
  let args = application-arguments();
  let handler <serious-condition> = method (condition, next-handler)
    format-out("cgi error: ");
    apply(format-out, condition.condition-format-string, condition.condition-format-arguments);
    format-out("\n");
    if($debug)
      write(*standard-output*, stream-contents($debug-stream));
    end if;
    exit-application(1);
  end;

  let handler <simple-warning> = method (condition, next-handler)
    format(*standard-output*, "cgi warning: ");
    apply(format, *standard-output*, condition.condition-format-string, condition.condition-format-arguments);
    format(*standard-output*, "\n");
  end;

  cgi-main(); // the library user needs to implement the cgi-main method which this calls;
  if($debug)
    write(*standard-output*, stream-contents($debug-stream));
  end if;
end method;


define open generic cgi-main (); // exported. This is implemented by the cgi programmer

define variable $function-name :: false-or(<symbol>) = #f;

define variable *arguments* :: <list> = #();

//////////////////////////////////////////////////////////////////////////
define method parse-values () => ()
  // a test string
  //let the-string = "method=hello&test-list=1&test-list=2&test-list=3&name-1=The%20Origin+%20of%20Species.html&name-2=5&name-4=67&name-3=The%20Origin%20of%20Pete.htm";
  let non-unary-arguments = make(<table>);
  let content-length = getenv("CONTENT_LENGTH");
  let the-string = "";

  if(content-length)
    if($debug)
      write($debug-stream, "<HR>");
      format-debug("<B>content-length:</B> %s\n<P>", content-length);
    end if; 
    the-string := read(*standard-input*, string-to-integer(content-length));
  end if;

  //the-string := "method=eat-in&name=%25";
  let string-size = size(the-string);

  let index = 0;
  let name = "";
  let accumulator = make(<stretchy-vector>);
  let the-state = #"reading-name";

  while(index < string-size)
    let the-character = the-string[index];
    select(the-character)
      '+' => add!(accumulator, ' ');
      '%' => // we could explicity check to make sure we are not at the last or next-to-last position
             // if(index < (string-size - 2)) 
             add!(accumulator, as(<character>, string-to-integer(subsequence(the-string, start: index + 1, end: index + 3), base: 16)));
             index := index + 2;
      '=' => // we are entering the value part of the name-value pair
             // signal an error if we are not in the reading-name state 
             // because we know the input is malformed
             if(the-state == #"reading-name")
               name := as(<byte-string>, accumulator); 
 	           accumulator := make(<stretchy-vector>); // reset the accumulator
               the-state := #"reading-value"; // switch to reading-value state
             else
               error("Parsing error:  The \"=\" came too soon.");
             end if;
      '&' => // the name-value pair has ended; add to the table, and reset
             // signal an error if we are not in the reading-value state 
             // because we know the input is malformed
             if(the-state == #"reading-value")
               let name-symbol = as(<symbol>, name);
               unless(empty?(accumulator))              
                 let value = as(<byte-string>, accumulator);
                 if(suffix?(name, $multiple-value-suffix))
                   let the-element = element(non-unary-arguments, name-symbol, default: #f);
                   if(the-element)
                     add!(the-element, value);
                   else
                      non-unary-arguments[name-symbol] := make(<stretchy-vector>, fill: value, size: 1);
                   end if;
                 else
                   if(name-symbol == #"function")
                     $function-name := as(<symbol>, value);
                   else
                     *arguments* := add!(*arguments*, name-symbol);
                     *arguments* := add!(*arguments*, value);
                     //non-unary-arguments[name-symbol] := value; 
                   end if;
                 end if;
               end unless;
 	           accumulator := make(<stretchy-vector>); // reset the accumulator
               the-state := #"reading-name"; // switch to reading-name state
             else
               error("Parsing error:  The \"&\" came too soon.");
             end if;
      otherwise => add!(accumulator, the-character);
    end select;
    index := index + 1;
  end;  
  if(the-state == #"reading-value") // add the final name-value pair to the table
    let name-symbol = as(<symbol>, name);
	let value = as(<byte-string>, accumulator);
	//non-unary-arguments[name] := as(<byte-string>, accumulator);
    if(name-symbol == #"function")
      $function-name := as(<symbol>, value);
    else
      *arguments* := add!(*arguments*, name-symbol);
      *arguments* := add!(*arguments*, value);
    end if;
  else
    if(string-size > 0)
      error("Parsing error:  The last name-value pair was incomplete.");
    end if;
  end if;

  for(value keyed-by name-symbol in non-unary-arguments)
    *arguments* := add!(*arguments*, name-symbol);
    *arguments* := add!(*arguments*, value);
  end for;

  *arguments* := reverse!(*arguments*);
  if($debug)
    write($debug-stream, "<B>standard-input:</B> "); 
    write($debug-stream, the-string); 
    write($debug-stream, "<P>"); 
  end if;
end method;

define method list-to-string (collection :: <collection>, #key separator :: <string> = ", ", default = "")
  if(empty?(collection))
    default;
  else
    reduce1(method(a, b) concatenate(a, separator, b) end, collection);
  end if;
end;

define constant $function-table = make(<table>);

define function export-cgi-methods (functions :: <sequence>) => ();
  for(i :: <integer> from 0 below size(functions) by 2)
    let name = functions[i];
    let function = functions[i + 1];
    $function-table[name] := function;
  end for;
end;

define macro dispatch-cgi-function
  { dispatch-cgi-function(?names:*) }
    => { do-dispatch-cgi-function(?names) }
names:
  { } 
    => { }
  { ?:name, ... }
    => { ?#"name", ?name, ... }
end macro;

define method do-dispatch-cgi-function (#rest functions)
//#key default: default-function
//  let default-function = #f;

  export-cgi-methods(functions);
  parse-values();
  
  unless($function-name)
    error("No function name was specified.");
  end;
  
  let function = element($function-table, $function-name, default: #f);
  unless(function)
//    if(default-function)
      //apply(default, #());
//      default-function();
//    else
      error("Could not find requested function \"%s\".", $function-name);
//    end if;
  end;
  

  if($debug)
    format-debug("<B>function:</B> %s<P>", $function-name);
    format-debug("<B>arguments:</B> ");
    print-cgi-function-arguments();
    format-debug("<HR>");
  end if;
  apply(function, *arguments*);
end method;

define method format-debug (control-string :: <string>, #rest arguments)
  apply(format, $debug-stream, control-string, arguments);
end method;

define method write-debug (control-string :: <string>, #rest arguments)
  apply(write, $debug-stream, control-string, arguments);
end method;

//////////////////////////////////////////////////////////////////////////
define method print-cgi-function-arguments ()
  if(empty?(*arguments*))
    write-debug("none<P>"); 
  else
    write-debug("<pre><code>");
  
    write-debug("\n                Name   Value");
    write-debug("\n                ----   -----");
  
    let leader = *arguments*;
    let follower = leader.tail;
    until(empty?(leader))
      let name = leader.head;
      let value = follower.head;
      format-debug("\n%20s   ", name);
      //format-out("        %=   ", value);
      if(instance?(value, <string>))
        format-debug("%s", value);
      else
        format-debug("#[%s]", list-to-string(value));
      end if;
      leader := follower.tail;
      follower := leader.tail;
    end until;

    write-debug("\n\n");
    write-debug("</code></pre>\n");
  end if;
end method;

define method suffix? (big-string, suffix)
  let big-string-size = size(big-string);
  let suffix-size = size(suffix);
  // is there a backwards iteration protocol?
  for(index1 from size(big-string) - 1 to 0 by -1, 
      index2 from size(suffix) - 1 to 0 by -1,
      while: big-string[index1] = suffix[index2])
    //format(*standard-output*, "\n%c   %c", form-variable[index], char2);
    finally index2 == -1;
  end for;
end;


