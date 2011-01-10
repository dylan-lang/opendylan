module: ppml
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// PPML
//

// Program notes need to be stored and later displayed in browsers.  This 
// presents us with two problems.  At the point of creation we have no way of
// knowing  the column width that will be used when the note is displayed.  
// There may even be more than one width if we want to be smart when a browser
// window is resized.  A second problem arises if we store a program note in
// the form of a condition string + arguments and the arguments are context
// sensitive.  We could just save everything as a string, but then the logical
// structure of the message is lost.  An alternative is to store the text in
// a more structured form.  This is the purpose of the <ppml> class and its 
// derivatives.   The interface is based on Oppen's 1980 TOPLAS paper.  


define constant <nat> = limited(<integer>, min: 0);

define abstract class <ppml> (<object>)
end class <ppml>;


define constant <ppml-sequence> = <sequence>; // TODO: LIMITED SEQUENCES
  // = limited(<sequence>, of: <ppml>);

// The token-size method computes the maximum size of the ppml token, 
// i.e. assuming we aren't force to break it.

define generic token-size (t :: <ppml>) => (token-size :: <nat>);


// <PPML-STRING>

//   The simplest ppml token is just a string.  

define class <ppml-string> (<ppml>)
  constant slot the-string :: <byte-string>,
    required-init-keyword: string:;
end class <ppml-string>;

define function ppml-string (str :: <byte-string>) => (ppml :: <ppml-string>)
  make(<ppml-string>, string: str)
end;

define method token-size (t :: <ppml-string>) => (s :: <nat>); 
  t.the-string.size 
end;

define class <ppml-quote-string> (<ppml-string>)
end class <ppml-quote-string>;

define function ppml-quote-string 
    (str :: <byte-string>) => (ppml :: <ppml-quote-string>)
  make(<ppml-quote-string>, string: str)
end;

define method token-size (t :: <ppml-quote-string>) => (s :: <nat>); 
  next-method() + 2
end;


// <PPML-BREAK>

//   A <ppml-break> indicates a position in the output where it is permissible
//   to break the output if it won't fit on a single line.  If we don't
//   need to break the line then we output blank-space spaces.  If we do need
//   to break then we indent offset spaces relative to the current line indent.

define class <ppml-break> (<ppml>)
  constant slot blank-space :: <nat>, 
    init-value: 1,
    init-keyword: blank-space:; // number of spaces per blank
  constant slot offset :: <nat>,
    init-value: 0,
    init-keyword: offset:; // indent for overflow lines
end class <ppml-break>;

// Breaks occur frequently, so we cache the more common ones to save storage.

define constant $break-cache-max-space = 3;
define constant $break-cache-max-offset = 3;

define constant $break-cache = 
  make(<array>, dimensions: vector($break-cache-max-space  + 1,
                                   $break-cache-max-offset + 1));

for (i from 0 to $break-cache-max-space)
  for (j from 0 to $break-cache-max-offset)
    $break-cache[i,j] := make(<ppml-break>, blank-space: i, offset: j)
  end
end;

define function ppml-break 
    (#key space :: <nat> = 1, offset :: <nat> = 0) => (ppml :: <ppml-break>)
  if ( space  >= 0 & space  <= $break-cache-max-space
     & offset >= 0 & offset <= $break-cache-max-offset )
    $break-cache[space, offset]
  else
    make(<ppml-break>, blank-space: space, offset: offset)
  end if
end;

define method token-size (t :: <ppml-break>) => (s :: <nat>); 
  t.blank-space 
end;
 

// We can force a line break by making a break with a space larger than the
// column width.

define constant $line-break = ppml-break(space: 999);



// <PPML-BLOCK>

//   To add structure to the output, we can package up a sequence of tokens
//   into a block.  There are a couple of attributes associated with a block.
//   The offset indicates how much to indent subsequent lines of the block
//   if a break is necessary.  When a block is longer than a line then we have
//   a number of options.  We can display as much on each line as possible, 
//   only breaking when really necessary.  Alternatively, we can break the 
//   block at each break point, e.g.
//
//               aaa             aaa bbb
//               bbb             ccc ddd
//               ccc
//               ddd
//
//   The choice of layout depends on whethe the break-type attribute of the
//   block is #"consistent" or #"inconsistent".  The third alternative is
//   #"fit".  This suppresses all breaks and truncates the output if it 
//   won't fit on a line.
//
//   The size of the block is cached in the block for efficiency.

define constant <ppml-break-type> = 
  one-of(#"consistent", #"inconsistent", #"fit");

define class <ppml-block> (<ppml>)
  constant slot constituent-tokens :: <ppml-sequence>,
    required-init-keyword: constituents:;
  constant slot offset :: <nat>,
    init-value: 2,
    init-keyword: offset:; // indent for this group
  constant slot break-type :: <ppml-break-type>, 
    init-value: #"inconsistent",
    init-keyword: break-type:; 
  slot cached-total-size :: false-or(<nat>), init-value: #f;
end class <ppml-block>;

define generic total-size (bt :: <ppml-block>) => (sz :: <nat>);

define method token-size (t :: <ppml-block>) => (s :: <nat>); t.total-size end;

define method total-size (bt :: <ppml-block>) => (sz :: <nat>)
  bt.cached-total-size 
  | (bt.cached-total-size := compute-block-size(bt.constituent-tokens))
end method total-size;

define method compute-block-size (cs :: <ppml-sequence>)
    => (sz :: <nat>)
  let prev-max = 0;
  let bs = 0;
  for (c in cs)
    if (c == $line-break)
      prev-max := max(prev-max, bs);
      bs := c.offset;
    else 
      bs := bs + c.token-size
    end if
  end for;
  max(bs, prev-max)
end method compute-block-size;

define function ppml-block 
    (constituents :: <ppml-sequence>,
     #key offset :: <nat> = 2, type :: <ppml-break-type> = #"inconsistent") 
        => (ppml :: <ppml-block>)
  make(<ppml-block>, constituents: constituents, 
                     offset: offset, break-type: type)
end;
  


// <PPML-SEPARATOR-BLOCK>

//   When constructing blocks representing collections it is wasteful to
//   explicitly store the separators between elements.  The 
//   <ppml-separator-block> class captures this common case.

define constant $comma-ppml-separator =
  vector(ppml-string(","), ppml-break(space: 1));

define class <ppml-separator-block> (<ppml-block>)
  constant slot separator :: <ppml-sequence>,
    init-value: $comma-ppml-separator,
    init-keyword: separator:;
end class <ppml-separator-block>;

define method token-size (t :: <ppml-separator-block>) => (s :: <nat>); 
  let csize = t.constituent-tokens.size;
  if (csize = 0) 
    0
  else
    // TODO: This produces the wrong answer if the block contains $line-break
    let block-size = t.total-size;
    let separator-size = compute-block-size(t.separator);
    block-size + ((csize - 1) * separator-size);
  end;
end;

define function ppml-separator-block 
  (constituents   :: <ppml-sequence>,
   #key separator :: <ppml-sequence> = $comma-ppml-separator,
        offset :: <nat> = 0, 
        type :: <ppml-break-type> = #"inconsistent",
        left-bracket  :: false-or(<ppml>) = #f,
        right-bracket :: false-or(<ppml>) = #f) => (ppml :: <ppml>)

  let body = make(<ppml-separator-block>, constituents: constituents, 
                  separator: separator,
                  offset: offset, break-type: type);
  if (left-bracket | right-bracket)
    let v = vector(left-bracket, body, right-bracket);
    let left-bracket-size = 0;
    if (left-bracket) 
      left-bracket-size := left-bracket.token-size;
    else
      v[0] := ppml-string("") 
    end;
    unless (right-bracket) v[2] := ppml-string("") end;
    ppml-block(v, offset: left-bracket-size);
  else
    body
  end;
end;
  

// <PPML-SUSPENSION>

//   Sometimes it is more space efficient to delay the construction of the 
//   <ppml> equivalent of an object until we need to print it.  The
//   <ppml-suspension> class supports this.  It contains either a <ppml>
//   token, or a pair of a function and its arguments.  When we need a token
//   and encounter the pair we apply the function to its arguments.  This
//   should return an instance of <ppml>.  Optionally we can overwrite the
//   pair by the result.

define class <ppml-suspension> (<ppml>)
  slot the-token, // :: one-of(<ppml>, <pair>),
    required-init-keyword: pair:;
  constant slot cache-token? :: <boolean>,
    init-keyword: cache-token?:,
    init-value: #t;
end class <ppml-suspension>;

define function ppml-suspension (fun :: <function>, #rest args) 
    => (ppml :: <ppml-suspension>)
  make(<ppml-suspension>, pair: pair(fun,args))
end;

define method suspension-token (t :: <ppml-suspension>) => (st :: <ppml>); 
  let tok = t.the-token;
  if (tok.object-class == <pair>)
    let real-token = apply(tok.head, tok.tail);
    if (t.cache-token?) t.the-token := real-token else real-token end
  else
    tok
  end
end;

define method token-size (t :: <ppml-suspension>) => (s :: <nat>); 
  suspension-token(t).token-size
end;


// <PPML-BROWSER-AWARE-OBJECT>

//   The browser "knows" about some of the objects manipulated by the compiler,
//   e.g. the various kinds of definition, and so we store these directly.
//   Furthermore we recompute the ppml representation of the object
//   every time token-size is called as the representation may depend on
//   browser settings.

define class <ppml-browser-aware-object> (<ppml>)
  constant slot the-object :: <object>, 
    required-init-keyword: object:;
  slot ppml-equivalent :: <ppml>,
    init-value: ppml-string("");
end class <ppml-browser-aware-object>;

define function ppml-browser-aware-object (o :: <object>) 
    => (ppml :: <ppml-browser-aware-object>)
  make(<ppml-browser-aware-object>, object: o)
end;

define method token-size (t :: <ppml-browser-aware-object>) => (s :: <nat>); 
  token-size(t.ppml-equivalent := 
               ppml-string(format-to-string("%=", t.the-object)))
end;


//
// AS 
//

//   To make it easier to construct ppml terms, we add methods to AS.  
//   The default is to just convert the object into a string, and then
//   apply ppml-string.  

// <OBJECT>

define method as (class == <ppml>, o :: <object>) => (instance :: <ppml>);
  ppml-string(format-to-string("%=", o))
end;


// <STRING> & <SYMBOL>

define method as (class == <ppml>, string :: <byte-string>) 
    => (instance :: <ppml>);
  ppml-quote-string(string)
end;

define method as (class == <ppml>, symbol :: <symbol>) 
    => (instance :: <ppml>);
  ppml-string(as(<string>, symbol))
end;


// <PPML>

define method as (class == <ppml>, ppml :: <ppml>) => (instance :: <ppml>);
  ppml
end;


// <COLLECTION>

//   In the collection cases we try to share as many tokens as possible to
//   reduce the space overhead of ppml.

define function as-ppml (o) => (po :: <ppml>)  as(<ppml>, o) end;

define constant ppml-sb-template =
  vector(ppml-string("#["), ppml-string("..."), ppml-string("]"));

define constant ppml-rb-template =
  vector(ppml-string("#("), ppml-string("..."), ppml-string(")"));

define method as (class == <ppml>, collection :: <collection>)
    => (instance :: <ppml>);
  let v = ppml-sb-template.shallow-copy;
  v[1] := ppml-separator-block(map-as(<vector>, as-ppml, collection), offset: 0);
  ppml-block(v, offset: 2);
end method as;


// <EXPLICIT-KEY-COLLECTION>

define constant ppml-table-element-template =
  vector(ppml-string("("),
         ppml-string("key"),
         ppml-string(" ->"),
         ppml-break(space: 1, offset: 1),
         ppml-string("value"),
         ppml-string(")"));

define method as (class == <ppml>, collection :: <explicit-key-collection>) 
    => (instance :: <ppml>);
  let v = ppml-sb-template.shallow-copy;
  let cv = make(<vector>, size: collection.size);
  let (initial-state, limit, next-state, finished-state?, 
       current-key, current-element) = collection.forward-iteration-protocol;
  for (state = initial-state then next-state(collection, state),
       i from 0,
       until: finished-state?(collection, state, limit))
    let te = ppml-table-element-template.shallow-copy;
    te[1] := as(<ppml>, current-key(collection, state));
    te[4] := as(<ppml>, current-element(collection, state));
    cv[i] := ppml-block(te, offset: 1);
  end for;
  v[1] := ppml-separator-block(cv, offset: 0);
  ppml-block(v, offset: 2);
end method as;


// <VECTOR>

define method as (class == <ppml>, vec :: <vector>) 
    => (instance :: <ppml>);
  let v = ppml-sb-template.shallow-copy;
  v[1] := ppml-separator-block(map-as(<vector>, as-ppml, vec), offset: 0);
  ppml-block(v, offset: 2);
end method as;


// <LIST>

define method as (class == <ppml>, list :: <list>) 
    => (instance :: <ppml>);
  let v = ppml-rb-template.shallow-copy;
  let constituents = make(<deque>); // TODO: Use a <vector> here. 
  let remainder = #();

  for (ptr = list then ptr.tail,
       until: ~instance?(ptr, <list>) | ptr.empty?)
    push-last(constituents, as(<ppml>, ptr.head));
  finally
    remainder := ptr
  end for;

  if (remainder == #())
    v[1] := ppml-separator-block(as(<vector>, constituents), offset: 0)
  else
    v[1] := 
      ppml-block(vector(
                   ppml-separator-block(as(<vector>, constituents), offset: 0),
                   ppml-break(space: 1, offset: 1),
                   ppml-string(". "),
                   as(<ppml>, remainder)), offset: 0)
  end;

  ppml-block(v, offset: 2);
end method as;


//
// FORMAT-TO-PPML : A PPML equivalent of format-to-string
//   

//   We insert breaks at the places where arguments are inserted in the 
//   format string.  This will hopefully give us reasonable output, but not
//   always as good as we could do by hand.  We separate out the processing 
//   of the format string so that we can share the constant components of the
//   resulting ppml-block if the same format expression is used multiple 
//   times.

define thread variable *verbose-condition-output* = #f;

define method ppml-format-string (string :: <byte-string>) => (f :: <function>)
  let arguments = make(<deque>);
  let constituents = make(<deque>);
  
  let limit = string.size;
  let i = 0;

  while (i < limit)
    let character = ' ';
    let start-index = i;

    while ( i < limit 
          & (character := string[i]) ~= '\n' 
          & character ~= '%') 
      i := i + 1 
    end;
  
    if (i > start-index)
      for (j = i - 1 then j - 1, 
           until: j = start-index | string[j] ~= ' ')
      finally 
        unless (string[j] = ' ')
          push-last(constituents, 
            ppml-string(copy-sequence(string, start: start-index, 
                                              end: j + 1)));
        end;
        push-last(constituents, ppml-break(space: i - j - 1));
      end;
    end;

    if (i < limit)
      if (character == '%')
        if (i + 1 = limit)
          error("Invalid format string: '%s'", string)
        end;
        let format-char = as-uppercase(string[i + 1]);
        select (format-char)
          'X', 'D', 'S', 'C', '=' =>
            push-last(arguments, pair(constituents.size, format-char));
            push-last(constituents, #f);
          '%' =>
            push-last(constituents, ppml-string("%"));
          '\n', '&' =>
            push-last(constituents, ppml-string("\n"));
          otherwise =>
            error("Invalid '%s' command '%s'", '%', string[i + 1])
        end select;

        i := i + 2;

        for (j = i then j + 1, while: j < limit & string[j] = ' ')
        finally 
          unless (i = j)
            push-last(constituents, ppml-break(space: j - i));
            i := j
          end
        end

      else // '\n' case
        push-last(constituents, $line-break);
        i := i + 1;
      end
    end if
  end while;

  let constituents = as(<vector>, constituents);
  let arguments = as(<vector>, arguments);

  method (#rest args)
    let b = shallow-copy(constituents);
    for (arg-descriptor in arguments, arg in args) 
      let index = arg-descriptor.head;
      case
        *verbose-condition-output* => 
          b[index] := ppml-string(format-to-string("%=", arg));
        arg-descriptor.tail == 'S' =>
          case
            instance?(arg, <string>) =>
              b[index] := ppml-string(arg);
            instance?(arg, <ppml-quote-string>) =>
              b[index] := ppml-string(arg.the-string);
            otherwise => b[index] := as(<ppml>, arg)
          end;
        otherwise => b[index] := as(<ppml>, arg) 
      end;
    end;
    ppml-block(b, offset: 0)
  end;
end method ppml-format-string;

define method format-to-ppml
    (string :: <byte-string>, #rest args) => (ppml :: <ppml>)
  apply(ppml-format-string(string), args)
end method format-to-ppml;


//
// PPML Printer
//

//   When outputing ppml we need to keep track of the space left on the
//   current line and the current margin.  We store these values in a 
//   <ppml-printer> object, along with the functions used to display text
//   and line breaks.  

define class <ppml-printer> (<object>)
  constant slot margin :: <nat>, required-init-keyword: margin:;
  slot space :: <nat>, init-value: 0;
  slot terse-depth :: <integer>, init-value: 100, init-keyword: terse-depth:;
  constant slot ppml-output-string = 
      method (s :: <string>) write(*standard-output*, s) end,
    init-keyword: output-function:;
  constant slot ppml-newline = method () write(*standard-output*, "\n") end,
    init-keyword: newline-function:;
end class <ppml-printer>;

define method initialize (pp :: <ppml-printer>, #next next-method, #key)
  next-method();
  pp.space := pp.margin;
end;

define method blanks (pp :: <ppml-printer>, n :: <nat>) 
  for (i from 0 below n) pp.ppml-output-string(" ") end; 
  pp.space := pp.space - n;
end method;


//
// PRINT-PPML
//

define generic print-ppml 
    (t :: <ppml>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);

define generic print-ppml-block
    (t :: <ppml-block>, pp :: <ppml-printer>, 
     btype :: <ppml-break-type>, block-space :: <nat>, after :: <nat>);


define method print-ppml 
    (t :: <ppml-string>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  if (t.the-string.size <= pp.space)
    pp.ppml-output-string(t.the-string);  
    pp.space := pp.space - t.the-string.size;
    #t
  else // The string is too long...
    let str = t.the-string;
    let break-point = pp.space;
    for (i from pp.space - 1 to 0 by -1, until: str[i] = ' ')
      finally if (i > 0) break-point := i + 1 end
    end;
    pp.ppml-output-string(copy-sequence(str, start: 0, end: break-point));
    pp.ppml-newline(); pp.space := pp.margin;
    blanks(pp, pp.margin - block-space);     
    print-ppml(ppml-string(copy-sequence(str, start: break-point)),
      pp, distance-to-break, btype, block-space)
  end;
end method print-ppml;


define method print-ppml 
    (t :: <ppml-quote-string>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  let sz = t.the-string.size + 2;
  if (sz <= pp.space)
    pp.ppml-output-string("\"");  
    pp.ppml-output-string(t.the-string);  
    pp.ppml-output-string("\"");  
    pp.space := pp.space - sz;
    #t
  else // The string is too long...
    let str = t.the-string;
    let break-point = pp.space;
    for (i from pp.space - 3 to 0 by -1, until: str[i] = ' ')
      finally if (i > 0) break-point := i + 1 end
    end;
    pp.ppml-output-string("\"");  
    pp.ppml-output-string(copy-sequence(str, start: 0, end: break-point));
    pp.ppml-output-string("\"");  
    pp.ppml-newline(); pp.space := pp.margin;
    blanks(pp, pp.margin - block-space);     
    print-ppml(ppml-quote-string(copy-sequence(str, start: break-point)),
      pp, distance-to-break, btype, block-space)
  end;
end method print-ppml;


define method print-ppml 
    (t :: <ppml-break>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  if (btype ~== #"consistent" & t.blank-space + distance-to-break <= pp.space)
    blanks(pp, t.blank-space); #t
  elseif (btype == #"fit")
    pp.ppml-output-string(" ..."); pp.space := max(0, pp.space - 4); #f
  else
    pp.ppml-newline(); pp.space := pp.margin;
    blanks(pp, pp.margin - block-space + t.offset); #t
  end if;
end method print-ppml; 


define method print-ppml 
    (t :: <ppml-block>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  pp.terse-depth := pp.terse-depth - 1;
  let btype = 
    if (t.token-size + distance-to-break <= pp.space) 
      #"inconsistent" 
    elseif (pp.terse-depth = 0 | btype == #"fit")
      #"fit"
    else
      t.break-type 
    end;
  print-ppml-block(t, pp, btype, pp.space - t.offset, distance-to-break);
  pp.terse-depth := pp.terse-depth + 1;
  #t
end method print-ppml;


define method print-ppml 
    (t :: <ppml-suspension>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  print-ppml(t.suspension-token, pp, distance-to-break, btype, block-space)
end method print-ppml;


define method print-ppml 
    (t :: <ppml-browser-aware-object>, pp :: <ppml-printer>, 
     distance-to-break :: <nat>, btype :: <ppml-break-type>, 
     block-space :: <nat>) => (truncated? :: <boolean>);
  print-ppml(t.ppml-equivalent, pp, distance-to-break, btype, block-space)
end method print-ppml;


define method print-ppml-block
    (t :: <ppml-block>, pp :: <ppml-printer>, 
     btype :: <ppml-break-type>, block-space :: <nat>, after :: <nat>)
  let ts = t.constituent-tokens;

  unless (ts.size = 0)
    // Compute break distance vector
    let sz = ts.size;
    let bdv = make(<vector>, size: sz);  
    bdv[sz - 1] := after;
    for (i :: <integer> from sz - 1 above 0 by -1)
      let t = ts[i];
      bdv[i - 1] := 
        if (instance?(t, <ppml-break>)) 0 else t.token-size + bdv[i] end;
    end for;

    for (i from 0 below ts.size,
         while: print-ppml(ts[i], pp, bdv[i], btype, block-space))
    end for;
  end
end;

define method print-ppml-block
    (t :: <ppml-separator-block>, pp :: <ppml-printer>, 
     btype :: <ppml-break-type>, block-space :: <nat>, after :: <nat>)
  let ts = t.constituent-tokens;
  unless (ts.size = 0)
    let constituents = make(<vector>,
                            size: ts.size + (ts.size - 1) * t.separator.size);
    let i = 0;
    for (first? = #t then #f, ct in t.constituent-tokens)
      unless (first?)
        for (st in t.separator) constituents[i] := st; i := i + 1 end;
      end unless;
      constituents[i] := ct; i := i + 1
    end for;

    print-ppml-block(ppml-block(constituents),
                     pp, btype, block-space, after);
  end
end;


// 
// PPML-PRINT
//

//---*** andrewa+jonathan: put this in to stop crashes in the candidates.
//---*** We could do with working out why...
define method ppml-print (t :: <ppml>, pp :: <ppml-printer>) => ()
  block ()
    print-ppml(ppml-block(vector(t)), pp, 0, #"inconsistent", pp.margin); 
    if (pp.space < pp.margin) pp.ppml-newline() end; 
    pp.space := pp.margin;
  exception (error :: <error>)
    //---*** This hack is just to get around a PPML crash for now!
    block ()
      format-out("PPML crashed: %s\n", error)
    exception (error :: <error>)
      format-out("PPML crashed, and so did printing the condition!\n")
    end
  end;
  values()
end;


define method ppml-print-one-line (t :: <ppml>, pp :: <ppml-printer>) => ()
  ppml-print(ppml-block(vector(t), type: #"fit"), pp);
  values()
end;



//
// PRINT-OBJECT methods
//

//   These methods can be used to output a ppml token as one long string when
//   this is necessary.

define method print-object (t :: <ppml-string>, s :: <stream>) => ();
  write(s, t.the-string);  
end method print-object;

define method print-object (t :: <ppml-quote-string>, s :: <stream>) => ();
  write(s, "\"");  write(s, t.the-string);  write(s, "\"");  
end method print-object;

define method print-object (t :: <ppml-break>, s :: <stream>) => ();
  if (t == $line-break)
    write(s, "\n"); 
  else
    for (i from 0 below t.blank-space) write(s, " ") end
  end if;
  values ()
end method print-object;

define method print-object (t :: <ppml-block>, s :: <stream>) => ();
  for (ct in t.constituent-tokens) print(ct, s) end; values()
end method print-object;

define method print-object (t :: <ppml-separator-block>, s :: <stream>) => ();
  for (first? = #t then #f, ct in t.constituent-tokens) 
    unless (first?)
      for (st in t.separator) print(st, s) end;
    end;
    print(ct, s)
  end;
  values()
end method print-object;

define method print-object (t :: <ppml-suspension>, s :: <stream>) => ();
  print(t.suspension-token, s) 
end method print-object;

define method print-object (t :: <ppml-browser-aware-object>, s :: <stream>) => ();
  print(t.the-object, s) 
end method print-object;

