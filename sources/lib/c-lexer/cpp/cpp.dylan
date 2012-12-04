Module: cpp-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(<predefined-macro>, definition-setter, expanded-tokens-setter,
          formal-parameters, formal-parameters-setter,
          macro-definitions-setter,
          name, name-setter, number-of-arguments-setter,
          skip-stack-setter, source-name-setter, test-cpp,
          make-test-cpp-file-stream,
          inner-stream-stack-setter);

// A list of strings which define the system include path.
define variable *cpp-include-path* :: <deque> = make(<deque>);

define function find-file-in-path(path :: <sequence>, filename :: <string>)
                              => (full-path :: false-or(<file-locator>));
  block (return)
    for (dir in path)
      let fullname = merge-locators(as(<file-locator>, filename),
                                    as(<directory-locator>, dir));
      if (file-exists?(fullname) & file-type(fullname) = #"file")
        return(fullname);
      end if;
    end for;
    #f
  end block;
end function find-file-in-path;

define function add-cpp-include-path!(dir)
  ignore(dir);
  // This is a NOP so Jason's code still runs.  DELETE ME WHEN POSSIBLE!!!
end;

// <cpp-stream>s are wrapper streams with a set-able inner-stream.
// resetting only happens as the result of a #include, not from outside the
// stream. the inner stream is a <c-lexer>.  <cpp-stream>s support both
// read-element and unread-element.

// We  optimize hashing of identifiers by using our own table class.  The
// hash function will fail catastrophically for null strings but C doesn't
// have null identifiers.


define class <C-identifier-table> (<table>) end class;

define method C-identifier-hash (string :: <string>,
                                   old-state :: <hash-state>)
                                => (id :: <integer>,
                                    new-state :: <hash-state>);
  values((string.size * $identifier-char-range-squared) +
           ($identifier-char-range * as(<integer>, string.first)) +
           ($identifier-char-range * as(<integer>, string.last)),
         old-state);
end method C-identifier-hash;

define method table-protocol (table :: <C-identifier-table>)
 => (test-function :: <function>, hash-function :: <function>)
  values(\=, C-identifier-hash);
end method;


// cpp macro-expansion

/* Moved init-function stuff to initialize to hack around compiler bug

define open abstract primary class <cpp-stream> (<wrapper-stream>)
  slot source-name :: <string>, init-keyword: source-name:,
    init-value: "stream over unknown source";
  slot inner-stream-stack :: <t-list>, init-function: curry(make, <t-list>);
  slot skip-stack :: <t-list>, init-function: curry(make, <t-list>);
  slot unexpanded-tokens :: <token-list>,
    init-function: curry(make, <token-list>);
  slot expanded-tokens :: <token-list>,
    init-function: curry(make, <token-list>);
  slot current-token :: <token>;
  slot last-token :: <token>,
    init-keyword: last-token:, init-function: curry(make, <new-line>);
  slot macro-definitions :: <C-identifier-table>,
    init-function: curry(make, <C-identifier-table>, size: 16000);
end class;
*/

define open abstract primary class <cpp-stream> (<wrapper-stream>)
  slot source-name :: <string>, init-keyword: source-name:,
    init-value: "stream over unknown source";
  slot inner-stream-stack :: <t-list>;
  slot skip-stack :: <t-list>;
  slot unexpanded-tokens :: <token-list>;
  slot expanded-tokens :: <token-list>;
  slot current-token :: <token>;
  slot last-token :: <token>, init-keyword: last-token:;
  slot macro-definitions :: <C-identifier-table>;
end class;

// Various dialects of <cpp-stream>s

define open abstract class <Microsoft-cpp-stream> (<cpp-stream>) end;

define sealed concrete class
    <actual-Microsoft-cpp-stream> (<Microsoft-cpp-stream>)
end;

define method make
    (class == <Microsoft-cpp-stream>,
     #rest initialization-arguments,
     #key) => (result :: <actual-Microsoft-cpp-stream>);
  apply(make, <actual-Microsoft-cpp-stream>, initialization-arguments)
end method;

define open abstract class <ansi-cpp-stream> (<cpp-stream>) end;

define sealed concrete class
    <actual-ansi-cpp-stream> (<ansi-cpp-stream>)
end;

define method make
    (class == <ansi-cpp-stream>,
     #rest initialization-arguments,
     #key) => (result :: <actual-ansi-cpp-stream>);
  apply(make, <actual-ansi-cpp-stream>, initialization-arguments)
end method;

define open abstract class <C++-cpp-stream> (<cpp-stream>) end;

define sealed concrete class
    <actual-C++-cpp-stream> (<C++-cpp-stream>)
end;

define method make
    (class == <C++-cpp-stream>,
     #rest initialization-arguments,
     #key) => (result :: <actual-C++-cpp-stream>);
  apply(make, <actual-C++-cpp-stream>, initialization-arguments)
end method;


define abstract class <cpp-macro-definition> (<object>)
end class;

define class <simple-macro> (<cpp-macro-definition>)
  slot name :: <identifier>, init-keyword: name:;
  slot definition :: <token-list>, init-keyword: definition:;
end class;

define class <function-macro> (<cpp-macro-definition>)
  slot name :: <identifier>, init-keyword: name:;
  slot number-of-arguments :: <integer>, init-keyword: number-of-arguments:;
  slot formal-parameters :: <sequence>, init-keyword: formal-parameters:;
  slot definition :: <token-list>, init-keyword: definition:;
end class;


define method initialize
    (the-stream :: <cpp-stream>,
     #key define: the-definitions :: false-or(<sequence>) = #f,
     last-token = #f)
 => (the-stream :: <cpp-stream>)
  next-method();
  // hack around compiler bug with init-function:
  the-stream.inner-stream-stack := make(<t-list>);
  the-stream.skip-stack := make(<t-list>);
  the-stream.unexpanded-tokens := make(<token-list>);
  the-stream.expanded-tokens := make(<token-list>);
  unless (last-token)
    the-stream.last-token := make(<new-line>);
  end unless;
  the-stream.macro-definitions := make(<C-identifier-table>, size: 16000);
  //  end hack

  if (the-definitions)
    for (the-definition in the-definitions)
      let (macro-name, macro-definition) =
        select (the-definition by instance?)
          <string> =>
            // use lex-from-string
            values(make(<identifier>, lexer-string: the-definition),
            make(<token-list>));
          <pair> =>
            values(make(<identifier>, lexer-string: the-definition.head),
            lex-from-string
              (the-definition.tail,
               type-for-copy(the-stream.inner-stream),
               source-name: "-D definition list"));
          otherwise =>
            cpp-error(the-stream,
                      "Error  processing -D definition list: definition"
                        "must be a string or pair. Definition: %s",
                      print-to-string(the-definition));
        end select;
      the-stream.macro-definitions[macro-name.lexer-string]
        := make(<simple-macro>, name: macro-name,
                definition: macro-definition);
    end for;
  end if;
  the-stream
end method;

define method close (the-stream :: <cpp-stream>, #key) => ()
  close(the-stream.inner-stream);
  for (include-stream in the-stream.inner-stream-stack)
    close(include-stream);
  end for;
end method;

define method print-object
    (the-described-stream :: <cpp-stream>, the-stream :: <stream>) => ();
  format(the-stream, "(<cpp-stream> over %s, inner-stream-source: %s)",
         the-described-stream.source-name,
         the-described-stream.inner-stream.source-name);
end method;

// This is a buffer for actual function macro parameters.  the size should
// be large enough for most purposes but it is stretchy to allow for the
// possibility of *really* large cpp function macros.

define variable *actual-parameters* = make(<stretchy-vector>, size: 256,
                                           fill: #f);

// These are macros whose values are functions of the cpp stream or of a
// identifier token which names the macro being expanded.  They cannot be
// redefined by the user in iso c.  they are initialized into the
// macro-definition table when when the cpp stream is made and the function
// in the slot is closed over the stream.

define class <predefined-macro> (<cpp-macro-definition>)
  slot definition :: <function>, required-init-keyword: definition:;
end class;

// functions which implement predefined macros

// continue here test whether function slot works as expected.  write
// initialization code to create closures.

define method get-next-token (stream :: <cpp-stream>)
  stream.current-token :=
    if (empty?(stream.unexpanded-tokens))
      read-element(stream.inner-stream, on-end-of-stream: $eoi-token)
    else
      pop(stream.unexpanded-tokens)
    end if;
end method;

// The while loop in this method assumes that each sub-function which
// consumes part of the stream leaves the stream at the last read token.
// this is a general convention for <cpp-stream>s.  Beware off by one
// errors.
// The keyword return-new-line? is used when read-element is
// recursively called during expansion to expand tokens in a #include or
// #if line.  A <new-line> token is returned at the end of line or end of
// input so that the calling code knows where the line ended.

// *return-white-space?* passes white space through for debugging purposes.
define variable *return-white-space?* :: <boolean> = #f;

define method read-element
    (stream :: <cpp-stream>, #key return-new-line? = #f,
     evaluate-defined-expression? = #f, on-end-of-stream = #f,
     expand-macros? = #t)
 => (result);
  let result = #f;
  let new-line-not-returned-on-eoi? = #t;
  while (~result)
    if (~empty?(stream.expanded-tokens))
      result := pop(stream.expanded-tokens);
    else
      get-next-token(stream);
      select (stream.current-token by instance?)
        <identifier> =>
          // This is a candidate for macro expansion.  If it is a macro, do
          // one level of expansion and call read-element on the resulting
          // expansion.  Otherwise figure out the right parser-tag category
          // for the identifier and return it. If it is the identifier
          // "defined" and we are macro expanding an #if constant
          // expression for evaluation by cpp, evaluate the defined before
          // macro expansion (duh).
          if (evaluate-defined-expression? &
                (stream.current-token.lexer-string = "defined"))
            result := evaluate-defined-expression(stream);
          elseif (expand-macros?)
            let macro-definition =
              element(stream.macro-definitions,
                      stream.current-token.lexer-string,
                      default: #f);
            if (macro-definition)
              // An apparent function macro call isn't really a function
              // macro unless it is followed by an opening parenthesis.
              // Expand-macro returns false if an apparent function macro
              // isn't really followed by an open parenthesis.
              let identifier-which-is-apparently-a-macro =
                stream.current-token;
              // Squirrel away the current token because expand-macro for
              // function macros reads ahead on the current stream and
              // changes the stream.current-token underneath our feet.
              let expanded-macro =
                expand-macro(stream, macro-definition,
                             stream.current-token.source-line);
              if (expanded-macro)
                stream.unexpanded-tokens :=
                  concatenate!(expanded-macro, stream.unexpanded-tokens);
              else
                result := identifier-which-is-apparently-a-macro;
              end if;
            else
              result := stream.current-token;
            end if;
          else // don't expand macros
            result := stream.current-token;
          end if;
        <pound> =>
          // If the last-token was a <new-line> and there is a define word
          // following, then this is a cpp directive.  Be careful here that
          // the use of <pound> as convert-macro-parameter-to-string only
          // occurs during expansion of function-macros.  Similarly the use
          // of <pound-pound> to merge tokens occurs during expansion and
          // before re-scanning for further macro calls. Also a definition
          // cannot occur as a result of macro expansion so the unexpanded
          // token stack should be empty.
          if (instance?(stream.last-token, <new-line>)
                & empty?(stream.unexpanded-tokens))
            read-cpp-directive(stream);
          else
            error("unexpected # token");
          end if;
          // current-token should now be <new-line>
        <white-space> =>
          // Update last-token but don't return the white-space token as
          // the result.  Normally we should not expect two white-space
          // tokens in a row unless the second white-space is the result of
          // a macro expansion.
          if (*return-white-space?*)
            result := stream.current-token;
          elseif (instance?(stream.current-token, <new-line>))
            if (return-new-line?)
              // return <new-line> so that the code calling knows that this
              // is the end of the line.
              result := stream.current-token;
            end if;
            // Otherwise go around the loop again having set last-token to
            // the <new-line> so that we correctly expand lines beginning
            // with #.
            stream.last-token := stream.current-token
          end if;
        <string-literal> =>
          // Here we need to beware of concatenating adjacent string
          // literals.  Reserve the literal and call read-element
          // recursively.  If the result is a string literal concatenate
          // the strings into the first literal token and return the
          // result.  If not push the result onto the expanded token stack
          // and return the original string literal.  If this ends up being
          // the only use of the expanded token stack then I think the
          // expanded token stack can be just a single token.  Recursion is
          // tricky here.
          result := stream.current-token;
          let next-expanded-token
            = read-element(stream,
                           return-new-line?: return-new-line?,
                           evaluate-defined-expression?:
                             evaluate-defined-expression?,
                           on-end-of-stream: on-end-of-stream);
          if (instance?(next-expanded-token, <string-literal>))
            // combine adjacent string literals into a sequence so
            // that dylan-value can return a concatenated string.
            result.internal-lexer-string-value :=
              if (instance?(next-expanded-token.internal-lexer-string-value,
                            <t-list>))
                // n.b. it doesn't really matter that we are
                // destructively modifying the value in
                // next-expanded-token since that token is being
                // orphaned anyway.
                let combined-result =
                  next-expanded-token.internal-lexer-string-value;
                push(combined-result,
                     result.lexer-string);
                result.lexer-string := combined-result;
              else
                let combined-result = make(<t-list>);
                push(combined-result, result.lexer-string);
                push-last(combined-result,
                          next-expanded-token.lexer-string);
                result.lexer-string := combined-result;
              end if;
          else
            push(stream.expanded-tokens, next-expanded-token);
          end;
          stream.last-token := result;
        <eoi> =>
          if (return-new-line? & new-line-not-returned-on-eoi?)
            // return a new line so that the code calling knows that this
            // is the end of the line.  rely on the inner-stream
            // returning repeated <eoi>s so that we can just go around
            // the loop again.
            result := make(<new-line>);
            new-line-not-returned-on-eoi? := #f;
          else
            // if the current inner stream is empty this might be an included
            // file.  if so pop out to the enclosing include files stream
            // otherwise it's really the end...
            if (empty?(stream.inner-stream-stack))
              if (on-end-of-stream)
                result := on-end-of-stream;
              else
                result := stream.current-token;
              end if;
            else
              close(stream.inner-stream);
              stream.inner-stream := pop(stream.inner-stream-stack);
              stream.last-token := make(<new-line>);
              cpp-handle-include-exit(stream);
            end if;
          end if;
        otherwise =>
          // return the token, updating the last-token.
          stream.last-token := stream.current-token;
          result := stream.current-token;
      end select;
    end if;
  end while;
  result
end method;

define method cpp-error (the-stream :: <cpp-stream>,
                         error-string :: <string>, #rest format-arguments)
  apply(error,
        concatenate("cpp macro expansion error on line: %= of file: %s"
                      "\ncurrent parser token: %s\n",
                    error-string),
        the-stream.current-token.source-line,
        the-stream.inner-stream.source-name,
        print-to-string(the-stream.current-token),
        format-arguments);
end method cpp-error;


define method cpp-warning (stream :: <cpp-stream>,
                         error-string :: <string>, #rest format-arguments)
  /* DEBUG
  format-out("\n\nwarning on line: %= of file: %s\ncurrent token: ",
             stream.current-token.source-line,
             stream.inner-stream.source-name);
  print(stream.current-token, *standard-output*);
  */
  apply(signal,
        concatenate("warning during cpp expansion: ", error-string),
        format-arguments);
end method cpp-warning;

define method skip-to-end-of-line (stream :: <cpp-stream>) => ()
  until (instance?(stream.current-token, <macro-terminator>))
    get-next-token(stream);
  end until;
end method;

define method skip-to-end-of-line-and-warn
    (stream :: <cpp-stream>, warning-string :: <string>) => ()
  until (instance?(stream.current-token, <macro-terminator>))
    unless (instance?(stream.current-token, <white-space>))
      cpp-warning(stream,
                  concatenate("unexpected token >>> %s <<< following ",
                              warning-string),
                  stream.current-token.lexer-string);
    end unless;
    get-next-token(stream);
  end until;
end method;

define method get-next-non-space-token
    (stream :: <cpp-stream>)
  get-next-token(stream);
  if (instance?(stream.current-token, <space>))
    get-next-token(stream);
  end if;
end method;

define method expand-macro
    (stream :: <cpp-stream>, macro-definition :: <simple-macro>,
     macro-call-source-line :: <integer>)
 => (result :: <token-list>);
  // This does a deep copy of the definition. This may be a bit to
  // conservative but better that than crashing.  We have to deal with
  // token merging in simple macros even though it is sort of bogus. We do
  // expansion by  creating a new <token-list> and pushing tokens onto the
  // end, checking for  merges as we go.  Then we concatenate the expansion
  // onto the unexpanded tokens stack.
  let result = make(<token-list>);
  let copied-definition =
    copy-token-list(macro-definition.definition,
                    source-line: macro-call-source-line);
  let merge-tokens? = #f;
  for (token in copied-definition)
    if (merge-tokens?)
      // merging tokens can fail in which case two tokens are returned.
      // shouldn't ever be more than two!  ansi rules let this be an error
      // but gcc just returns two tokens so we will go with that behavior
      // for now.  maybe should be a warning...
      let (merged-token, failed-merge-other-token) =
        merge-tokens(stream, last(result), token);
      last(result) := merged-token;
      if (failed-merge-other-token)
        push-last(result, failed-merge-other-token);
      end if;
      merge-tokens? := #f;
    elseif (instance?(token, <pound-pound>) & ~ empty?(result))
      merge-tokens? := #t;
    else
      push-last(result, token);
    end if;
  end for;
  result
end method;

// Merging two tokens can either produce one merged token or, if the
// merge fails, two tokens.  Shouldn't ever be more than two.
// merging empty tokens should just work since their lexer-strings are the
// empty string.

define method merge-tokens
    (stream :: <cpp-stream>, first-token :: <empty-token>,
     second-token :: <token>)
 => (second-token :: <token>, failed-merge-token :: false-or(<token>));
  values(second-token, #f)
end method;

define method merge-tokens
    (stream :: <cpp-stream>, first-token :: <token>,
     second-token :: <empty-token>)
 => (first-token :: <token>, failed-merge-token :: false-or(<token>));
  values(first-token, #f)
end method;

define method merge-tokens
    (stream :: <cpp-stream>, first-token :: <token>, second-token :: <token>)
 => (result-token :: <token>, failed-merge-token :: false-or(<token>));
  let failed-merge-token = #f;
  let new-lexer :: <c-lexer> =
// this needs line information and file information propagated
    make(type-for-copy(stream.inner-stream), // same lexer class as stream
         source-name: "internal stream for merging tokens",
         inner-stream: make(<numbered-string-stream>,
                            direction: #"input",
                            contents:
                              concatenate(first-token.lexer-string,
                                          second-token.lexer-string),
                            current-line: first-token.source-line));
  let result-token = read-element(new-lexer);
  unless (stream-at-end?(new-lexer))
    failed-merge-token := read-element(new-lexer);
  end unless;
  values(result-token, failed-merge-token)
end method;

// The current-token is the macro-name, we know it is a function macro
// because it was defined as one.  The next non-space token should be the
// open parenthesis that marks the beginning of the actual parameter list.
// The actual parameters are stashed in the module variable
// *actual-parameters*.  If the expected actual parameter list, beginning
// with an open parenthesis, isn't there this method returns #f, which tells
// expand-macro to return false, which tells read-element that the apparent
// function macro call wasn't really a macro after all...

define method get-macro-actual-parameters
    (stream :: <cpp-stream>, number-of-formals :: <integer>)
 => (macro-or-not-macro? :: <boolean>);
  let maybe-open-parenthesis = read-element(stream, expand-macros?: #f);
  let macro-or-not-macro? =
    instance?(maybe-open-parenthesis, <open-parenthesis>);
  if (macro-or-not-macro?)
    for (parameter-number = 0 then parameter-number + 1,
         until: instance?(stream.current-token, <close-parenthesis>))
      get-an-actual-parameter(stream, parameter-number);
    finally
      // Although the for loop actually ends with a parameter number one
      // greater than the actual number of parameters this is fine in the
      // test below.  It just compensates for the zero origin indexing.
      unless (parameter-number = number-of-formals)
        cpp-error(stream,
                  "Wrong number of macro actual parameters, expecting %=,"
                    "got %= ",
                  number-of-formals,
                  parameter-number);
      end unless;
    end for;
  else
    // Not really a macro.  Push back the token that might have been an
    // open parenthesis and let read-element return the token that might
    // have been a function macro.
    push(stream.unexpanded-tokens, maybe-open-parenthesis);
  end if;
  macro-or-not-macro?
end method;

// Spaces are significant in actual parameters when the actuals are
// converted to strings by the # operator.
define method get-an-actual-parameter
    (stream :: <cpp-stream>, parameter-number :: <integer>) => ();
  stream.last-token := stream.current-token;
  get-next-token(stream);
  let nested-parentheses? = 0;
  let result = make(<token-list>);
  until ((nested-parentheses? = 0)
           & (instance?(stream.current-token, <close-parenthesis>)
                | instance?(stream.current-token, <comma>)))
    select (stream.current-token by instance?)
      <space> =>
        // skip leading spaces
        unless (empty?(result))
          push-last(result, stream.current-token);
        end unless;
      <new-line> =>
        // skip leading new-lines, convert new-line to space
        unless (empty?(result))
          push-last(result,
                    make(<space>,
                         source-line: stream.current-token.source-line));
        end unless;
        stream.last-token := stream.current-token;
      <open-parenthesis> =>
        nested-parentheses? := nested-parentheses? + 1;
        push-last(result, stream.current-token);
        stream.last-token := stream.current-token;
      <close-parenthesis> =>
        // nested-parentheses? should can't be less than 1
        nested-parentheses? := nested-parentheses? - 1;
        push-last(result, stream.current-token);
        stream.last-token := stream.current-token;
      <pound> =>
        // Cpp directives can occur in macro actual parameter lists!
        if (instance?(stream.last-token, <new-line>)
              & empty?(stream.unexpanded-tokens))
          read-cpp-directive(stream);
        else
          cpp-error(stream,
                    "unexpected # token in macro actual parameter list");
          // current-token should now be <new-line>
        end if;
      <eoi> =>
//??? An interesting question is whether return-new-line? needs to be
// propagated to here.  Need to look at the places where read-element is
// called recursively...
        // If the current inner stream is empty this might be an included
        // file.  If so pop out to the enclosing include files stream
        // otherwise it's really the end...
        if (empty?(stream.inner-stream-stack))
          cpp-error(stream,
                    "End of input while collecting actual macro parameters");
        else
          close(stream.inner-stream);
          stream.inner-stream := pop(stream.inner-stream-stack);
          cpp-handle-include-exit(stream);
        end if;
        stream.last-token := make(<new-line>);
      otherwise =>
        push-last(result, stream.current-token);
        stream.last-token := stream.current-token;
    end select;
    get-next-token(stream);
  end until;
  // killoff trailing spaces
  if (~empty?(result) & instance?(result.last, <space>))
     pop-last(result);
  end if;
  *actual-parameters*[parameter-number] := result;
end method;

define class <parameter-offset-marker> (<token>)
  constant slot parser-tag :: <symbol> = #"parameter-offset-marker";
  constant slot lexer-string :: <string> = "";
  slot offset :: <integer>, init-keyword: offset:;
  // The parameter name is only needed as a debugging convenience.  Get rid
  // of it to save space.
  slot formal-parameter-name :: <identifier>,
    init-keyword: formal-parameter-name:;
end class;

define method copy-token
    (source-token :: <parameter-offset-marker>,
     #key source-line: the-source-line :: false-or(<integer>))
 => (result-token :: <parameter-offset-marker>)
  let result-token = next-method();
  result-token.offset := source-token.offset;
  result-token.formal-parameter-name := source-token.formal-parameter-name;
  result-token
end method;

// An interesting question is should converting an empty parameter to a
// string produce an empty string or an empty parameter?
// Problem: Don't know how to get proper line number information into the
// new token.  Should it be the line for the actual parameter tokens if any
// or something that identifies where the expansion is happening lexer
define method convert-argument-to-string
    (current-token :: <parameter-offset-marker>)
 => (result :: <string-literal>);
  let result = make(<string-literal>, lexer-string: "");
  for (token in *actual-parameters*[current-token.offset])
    result.lexer-string :=
      concatenate(result.lexer-string, token.quoted-string);
  end for;
  result.lexer-string := concatenate("\"", result.lexer-string, "\"");
  result
end method;

define method expand-macro
    (stream :: <cpp-stream>, macro-definition :: <function-macro>,
     macro-call-source-line :: <integer>)
 => (result ::  false-or(<token-list>));
  // If we want to put location information into the tokens themselves we
  // will have to do a deeper copy using the information from the
  // current-token which instigated the expansion.  This is aNasty mix of #
  // and ## operators and parameter expansions (and #@).  This method
  // returns false if get-macro-actual-parameters doesn't find an open
  // parenthesis as the next non-white-space token.
  let result = #f;
  if (get-macro-actual-parameters(stream,
                                  macro-definition.number-of-arguments))
    result := make(<token-list>);
    let merge-tokens? = #f;
    let copied-definition =
      copy-token-list(macro-definition.definition,
                      source-line: macro-call-source-line);
    let convert-argument-to-string? = #f;
    for (token in copied-definition)
      // This let is here because current-token can be assigned to in various
      // of the if clauses below.  I'm not sure that assignment to token is ok.
      let current-token = token;
      if (convert-argument-to-string?)
        if (instance?(current-token, <parameter-offset-marker>))
          current-token := convert-argument-to-string(current-token);
        else
          cpp-error(stream, "# not followed by a macro argument");
        end if;
        convert-argument-to-string? := #f;
      end if;
      if (instance?(current-token, <pound-pound>))
        unless (empty?(result))
          merge-tokens? := #t;
        end unless;
      elseif (instance?(current-token, <pound>))
        convert-argument-to-string? := #t;
      elseif (merge-tokens?)
        let copied-argument = #f;
        if (instance?(current-token, <parameter-offset-marker>))
          copied-argument :=
            copy-token-list(*actual-parameters*[current-token.offset]);
          if (empty?(copied-argument))
            current-token := make(<empty-token>);
          else
            current-token := pop(copied-argument);
          end if;
        end if;
        let (merged-token, failed-merge-other-token) =
          merge-tokens(stream, last(result), current-token);
        last(result) := merged-token;
        if (failed-merge-other-token)
          push-last(result, failed-merge-other-token);
        end if;
        if (copied-argument)
          result := concatenate!(result, copied-argument);
        end if;
        merge-tokens? := #f;
      elseif (instance?(current-token, <parameter-offset-marker>))
        if (empty?(*actual-parameters*[current-token.offset]))
          push-last(result, make(<empty-token>));
        else
          result :=
            concatenate!
              (result,
               copy-token-list(*actual-parameters*[current-token.offset]));
        end if;
      else
        push-last(result, current-token);
      end if;
    end for;
  end if;
  result
end method;

define method read-cpp-directive(stream :: <cpp-stream>)
  get-next-non-space-token(stream);
  if (instance?(stream.current-token, <identifier>))
    select (stream.current-token.lexer-string by \=)
      "define" => read-cpp-define(stream);
      "undef" => read-cpp-undef(stream);
      "include" => read-cpp-include(stream);
      "if" => read-cpp-if(stream, #"if");
      "ifdef" => read-cpp-if(stream, #"ifdef");
      "ifndef" => read-cpp-if(stream, #"ifndef");
      "elif" => read-cpp-elif(stream);
      "else" => read-cpp-else(stream);
      "endif" => read-cpp-endif(stream);
      "line" => ;
      "pragma" =>
        // Different classes of cpp stream are free to recognize particular
        // pragmas by implementing methods on the open generic
        // cpp-handle-pragma which is specialized on the subclass of cpp
        // stream.
        read-cpp-pragma(stream);
      "error" =>
        cpp-error(stream, "Encountered #error directive");
      otherwise =>
        cpp-warning(stream,
                    "unexpected <identifier>: %s following #",
                    stream.current-token.lexer-string);
        skip-to-end-of-line(stream);
    end select;
  else
    skip-to-end-of-line(stream);
  end if;
  stream.last-token := stream.current-token;
end method read-cpp-directive;

define method read-cpp-define (stream :: <cpp-stream>)
  get-next-non-space-token(stream);
  select (stream.current-token by instance?)
    <identifier> =>
      let macro-name = stream.current-token;
      get-next-token(stream);
      select (stream.current-token by instance?)
        <space>, <eoi>, <new-line> =>
          // simple macro, if eoi or new-line the macro definition will be
          // the empty list which is fine
          define-simple-macro(stream, macro-name);
        <open-parenthesis> =>
          // function macro
          define-function-macro(stream, macro-name);
        otherwise =>
          cpp-warning(stream,
                      "warning: missing white space after #define %=",
                      macro-name.lexer-string);
          // also simple macro
          define-simple-macro(stream, macro-name);
      end select;
    otherwise =>
      cpp-error(stream,
                "Unexpected token following #define, <identifier> expected");
  end select;
  stream.last-token := make(<new-line>);
end method;

define method read-cpp-undef (stream :: <cpp-stream>) => ()
  get-next-non-space-token(stream);
  let macro-name = stream.current-token;
  select (macro-name by instance?)
    <identifier> =>
      remove-key!(stream.macro-definitions, macro-name.lexer-string);
    otherwise =>
      cpp-error(stream,
                "Unexpected token following #undef, <identifier> expected");
  end select;
  get-next-token(stream);
  until(instance?(stream.current-token, <macro-terminator>))
    unless (instance?(stream.current-token, <white-space>))
      cpp-warning(stream,
                  "Unexpected token >>> %= <<< following #undef %=",
                  stream.current-token.lexer-string,
                  macro-name.lexer-string);

    end unless;
    get-next-token(stream);
  end until;
  stream.last-token := stream.current-token;
end method;

// This isn't efficient but the expand case is rare enough that we probably
// don't care for now.
define method read-cpp-include (stream :: <cpp-stream>)
  let filename = read-include-filename(stream.inner-stream);
  unless (filename)
    let expanded-file-string = "";
      for (expanded-token = read-element(stream, return-new-line?: #t)
             then read-element(stream, return-new-line?: #t),
           until: instance?(expanded-token, <new-line>))
        expanded-file-string :=
          concatenate(expanded-file-string, expanded-token.lexer-string);
      finally
        // Having expanded the tokens and converted them to a string build
        // a lexer stream over a string stream to try to reduce the
        // expanded stream into either a <ordinary-filename> or a
        // <standard-filename>.  The lexer stream should be the same
        // subclass of <C-lexer> as the inner-stream of the stream input to
        // this function.
        let expanded-token-stream =
          make(type-for-copy(stream.inner-stream), // lexer stream
               source-name: "internal stream for re-lexing macro in #include",
               inner-stream: make(<numbered-string-stream>,
                                  direction: #"input",
                                  contents: expanded-file-string,
                                  current-line: expanded-token.source-line));
        // This should error if the expanded string doesn't reduce to an
        // ordinary or standard filename.
        filename := read-include-filename(expanded-token-stream,
                                          relexing-expanded-tokens: #t);
      end for;
  end unless;
  // Ordinary files are searched for in the current directory and
  // standard headers are searched for in *cpp-include-path*.
  select (filename by instance?)
    <ordinary-filename> =>
      push(stream.inner-stream-stack, stream.inner-stream);
      let file = filename.dylan-value;
      if (~ (file-exists?(file) & file-type(file) = #"file"))
        file := find-file-in-path(*cpp-include-path*, filename.dylan-value);
        unless (file)
          signal(make(<file-does-not-exist-error>,
                      locator: filename.dylan-value));
        end unless;
      end if;
      stream.inner-stream :=
        // This should be better worked out.
        make(type-for-copy(stream.inner-stream), // lexer stream
             source-name: filename.dylan-value,
             inner-stream: // pre-lexer
               make(type-for-copy(stream.inner-stream.inner-stream),
                    source-name: filename.dylan-value,
                    inner-stream: // new file stream
                      make(<file-stream>, locator: file)));
      cpp-handle-include-entry(stream);
    <standard-filename> =>
      push(stream.inner-stream-stack, stream.inner-stream);
      let file = find-file-in-path(*cpp-include-path*, filename.dylan-value);
      unless (file)
        signal(make(<file-does-not-exist-error>,
                    locator: filename.dylan-value));
      end unless;
      stream.inner-stream :=
        // This should be better worked out.
        make(type-for-copy(stream.inner-stream), // lexer stream
             source-name: filename.dylan-value,
             inner-stream: // pre-lexer
               make(type-for-copy(stream.inner-stream.inner-stream),
                    source-name: filename.dylan-value,
                    inner-stream: // new file stream
                      make(<file-stream>, locator: file)));
      cpp-handle-include-entry(stream);
    <empty-token> =>
      cpp-error(stream, "#include not followed by a valid filename");
    otherwise =>
      cpp-error(stream,  "unexpected return from read-include-filename");
  end select;
  stream.current-token :=  make(<new-line>);
end method;

define open primary class <pragma-token> (<token>)
  constant slot parser-tag :: <symbol> = #"pragma-token";
  constant slot lexer-string :: <string> = "";
  slot pragma-body :: <sequence>, init-keyword: pragma-body:;
end class;

define method read-cpp-pragma (stream :: <cpp-stream>) => ();
  let the-pragma-body = make(<token-list>);
  let result = #f;
  for (expanded-token = read-element(stream, return-new-line?: #t)
         then read-element(stream, return-new-line?: #t),
       until: instance?(expanded-token, <new-line>))
    push-last(the-pragma-body, expanded-token);
  finally
    // Make the pragma token and push it onto the expanded tokens stack.
    result := cpp-handle-pragma(stream, as(<list>, the-pragma-body));
  end for;
  if (result & instance?(result, <token>))
    push(stream.expanded-tokens, result);
  end if;
end method;

define open generic cpp-handle-include-entry (the-stream :: <cpp-stream>)
 => ();

define open generic cpp-handle-include-exit (the-stream :: <cpp-stream>)
 => ();

define method cpp-handle-include-entry (the-stream :: <cpp-stream>)
 => ()
  // do nothing by default
end method;

define method cpp-handle-include-exit (the-stream :: <cpp-stream>)
 => ()
  // do nothing by default
end method;

define open generic cpp-handle-pragma
    (the-stream :: <cpp-stream>, the-pragma-body :: <list>)
 => (result :: false-or(<token>));

define method cpp-handle-pragma
    (the-stream :: <cpp-stream>, the-pragma-body :: <list>) =>
    (result :: false-or(<pragma-token>))
  // ignore the pragma by doing nothing
  #f
end method;

define method get-simple-macro-definition
    (stream :: <cpp-stream>) => (result :: <token-list>);
  let result = make(<token-list>);
  until (instance?(stream.current-token, <macro-terminator>))
    unless (instance?(stream.current-token, <space>))
      push-last(result, stream.current-token)
    end unless;
    get-next-token(stream);
  end until;
  result
end method;


define method define-simple-macro(stream :: <cpp-stream>,
                                  macro-name :: <identifier>)
  stream.macro-definitions[macro-name.lexer-string]
    := make(<simple-macro>, name: macro-name,
            definition: get-simple-macro-definition(stream));
end method;

define method define-function-macro(stream :: <cpp-stream>,
                                    macro-name :: <identifier>) => ();
  let formal-parameters =
    get-macro-formal-parameters(stream);
  let macro-definition =
    get-function-macro-definition(stream, formal-parameters);
  stream.macro-definitions[macro-name.lexer-string]
    := make(<function-macro>, name: macro-name,
            number-of-arguments: size(formal-parameters),
            formal-parameters: formal-parameters,
            definition: macro-definition);
end method;

// The current element is still the open parenthesis.  Formal parameters
// can only be individual identifiers separated by commas.
define method get-macro-formal-parameters
    (stream) => (result :: <sequence>);
  let result = make(<stretchy-vector>);
  get-next-token(stream); // skip the open parenthesis
  until (instance?(stream.current-token, <close-parenthesis>))
    select (stream.current-token by instance?)
      <identifier> => add!(result, stream.current-token);
      <comma>, <space> => ; // do nothing
      otherwise =>
        cpp-error(stream, "unexpected token in macro formal parameters");
    end select;
    get-next-token(stream);
  end until;
  result
end method;

define method \=
    (token-1 :: <token>, token-2 :: <token>) => (result :: <boolean>);
  ((object-class(token-1) = object-class(token-2))
     & (token-1.lexer-string = token-2.lexer-string))
end method;

define method get-parameter-offset
    (candidate-token :: <identifier>, formal-parameters :: <sequence>)
 => (offset :: false-or(<parameter-offset-marker>));
  let index = find-key(formal-parameters,
                       curry(\=, candidate-token));
  if (index)
    make(<parameter-offset-marker>,
         offset: index, formal-parameter-name: candidate-token)
  else
    #f
  end if
end method;

define method get-function-macro-definition
    (stream :: <cpp-stream>, formal-parameters :: <sequence>)
 => (result :: <token-list>);
  let result = make(<token-list>);
  // current-token is still ")" so skip it
  get-next-token(stream);
  until (instance?(stream.current-token, <macro-terminator>))
    select (stream.current-token by instance?)
      <space> => ; // skip it
      <identifier> => // check if is a formal parameter
        let formal-parameter-offset
          = get-parameter-offset(stream.current-token, formal-parameters);
        push-last(result,
                  if (formal-parameter-offset)
                    formal-parameter-offset
                  else
                    stream.current-token
                  end if);
      otherwise =>
        push-last(result, stream.current-token);
    end select;
    get-next-token(stream);
  end until;
  result
end method;

// Since read returns macro expanded tokens, unread pushes expanded tokens.
// There is another possible operation which would be unread-ing unexpanded
// tokens.  Melange cpp  unreads unexpanded tokens a lot. I think our
// version is designed so this should not be necessary.  In any case that
// would be pushing elements onto the unexpanded-tokens stack or the
// inner-stream, not the expanded-tokens stack.
define method unread-element
    (stream :: <cpp-stream>, token :: <token>) => (result :: <token>);
  push(stream.expanded-tokens, token);
end method;

define method make-test-cpp-stream (string)
  make(<ansi-cpp-stream>,
       source-stream: "test-cpp stream",
       inner-stream:
         make(<ansi-C-lexer>,
              source-name: "test-cpp stream",
              inner-stream:
                make(<pre-lexer>,
                     source-name: "test-cpp stream",
                     inner-stream:
                       make(<string-stream>,
                            direction: #"input",
                            contents: string))))
end method;

define method test-cpp (the-string :: <string>)
 => (result-tokens :: <token-list>)
  let the-stream = make-test-cpp-stream(the-string);
  let result-tokens = make(<token-list>);
  let current-token = read-element(the-stream);
  until (instance?(current-token, <eoi>))
    push-last(result-tokens, current-token);
    current-token := read-element(the-stream);
  end until;
  close(the-stream);
  result-tokens
end method;

define method make-test-cpp-file-stream (file-name :: <string>)
 => (result :: <cpp-stream>);
  let file-string =
    concatenate("/u/toby/dylan/lib/collage/cpp/tests/include-files/",
                file-name);
  make(<cpp-stream>,
       source-name: file-name,
       inner-stream:
         make(<ansi-C-lexer>,
              source-name: file-name,
              inner-stream:
                make(<pre-lexer>,
                     source-name: file-name,
                     inner-stream:
                       make(<file-stream>, locator: file-string))))
end method;
