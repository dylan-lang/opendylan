Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $any-char-set
  = begin
      let set = make(<bit-set>, upper-bound-hint: 256);
      for (symbol :: <integer> from 0 below 256)
        set-add!(set, symbol);
      end for;
      set
    end;

define function parse-glob-pattern
    (string :: <byte-string>)
 => (node :: <regular-expression>);
  iterate loop (node :: <regular-expression>
                  = make(<epsilon-regular-expression>),
                i :: <integer> = 0)
    if (i < string.size)
      let char = string[i];
      select (char)
        // ? - match any single character
        '?' =>
          loop(make(<concatenation-regular-expression>,
                    head: node,
                    tail: make(<symbol-set-regular-expression>,
                               symbol-set: $any-char-set)),
               i + 1);

        // * - match zero or more characters
        '*' =>
          loop(make(<concatenation-regular-expression>,
                    head: node,
                    tail: make(<closure-regular-expression>,
                               of: make(<symbol-set-regular-expression>,
                                        symbol-set: $any-char-set))),
               i + 1);

        // [chars] / [^chars] - match any single character in a character
        // set or character set complement
        '[' =>
          let cclass-set = make(<bit-set>, upper-bound-hint: 256);
          let pos = i + 1;
          let complement? = if (pos < string.size & string[pos] = '^')
                              i := i + 1;
                              pos := pos + 1;
                              #t;
                            else #f end if;
          while (pos < string.size & (string[pos] ~== ']' | pos = i + 1))
            if (pos + 2 < string.size & string[pos + 1] == '-')
              for (symbol :: <integer> from as(<integer>, string[pos])
                     to as(<integer>, string[pos + 2]))
                set-add!(cclass-set, symbol);
              end for;
              pos := pos + 3;
            else
              set-add!(cclass-set, as(<integer>, string[pos]));
              pos := pos + 1;
            end;
          end while;
          if (pos = string.size)
            error("closing ']' missing in regexp '%s'", string);
          end if;
          if (complement?)
            let complement-set = make(<bit-set>, upper-bound-hint: 256);
            for (symbol :: <integer> from 0 below 256)
              unless (member?(symbol, cclass-set))
                add!(complement-set, symbol)
              end;
            end for;
            loop(make(<concatenation-regular-expression>,
                    head: node,
                    tail: make(<symbol-set-regular-expression>,
                               symbol-set: complement-set)),
                 pos + 1);
          else
            loop(make(<concatenation-regular-expression>,
                    head: node,
                    tail: make(<symbol-set-regular-expression>,
                               symbol-set: cclass-set)),
                 pos + 1);
          end;

        '\\' =>
          if (i + 1 < string.size)
            loop(make(<concatenation-regular-expression>,
                      head: node,
                      tail: make(<symbol-regular-expression>,
                                 symbol: as(<integer>, string[i + 1]))),
                 i + 2);
          else
            error("glob pattern '%s' ends in '\\'", string);
          end if;
        otherwise =>
          loop(make(<concatenation-regular-expression>,
                    head: node,
                    tail: make(<symbol-regular-expression>,
                               symbol: as(<integer>, char))),
               i + 1);
      end select;
    else
      node
    end
  end iterate
end function;

define function glob-match-function
    (pattern :: <byte-string>, #rest more-patterns)
 => (matcher :: <function>);
  let regex = parse-glob-pattern(pattern);

  for (pattern in more-patterns)
    regex := make(<union-regular-expression>,
                  union1: regex,
                  union2: parse-glob-pattern(pattern));
  end for;
  
  let dfa
    = regular-expression-dfa(make(<concatenation-regular-expression>,
                                  head: regex,
                                  tail:
                                    make(<match-accept-regular-expression>)),
                             transition-collection-class:
                               <simple-object-vector>,
                             transition-collection-size: 256,
                             state-class: <match-dfa-state>);
  
  method (match-string :: <byte-string>) => (match? :: <boolean>);
    block (return)
      for (char in match-string,
           state = dfa
             then state.regular-expression-dfa-state-transitions[as(<integer>, char)]
             | return(#f))
      finally
        state.match-dfa-state-accepting?
      end for;
    end block
  end method;
end function;

define function jam-builtin-glob
    (jam :: <jam-state>, directories :: <sequence>, patterns :: <sequence>)
 => (result :: <sequence>);
  let match? = apply(glob-match-function, patterns);
  let result = make(<stretchy-vector>);

  for (directory-name in directories)
    do-directory(method (directory :: <directory-locator>, name, type)
                   if (match?(name))
                     add!(result,
                          as(<byte-string>,
                             merge-locators(as(<file-locator>, name),
                                            directory)));
                   end if;
                 end method,
                 as(<directory-locator>, directory-name));
  end for;
  result
end function;

