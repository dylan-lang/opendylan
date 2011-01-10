Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $regular-expression-cache :: <string-table>
  = make(<string-table>);

define function parse-regular-expression
    (string :: <byte-string>)
 => (node :: <regular-expression>);
  local
    method parse-regexp0
        (string :: <byte-string>, start :: <integer>)
     => (node :: <regular-expression>, pos :: <integer>);
      let (node :: <regular-expression>, pos :: <integer>)
        = parse-regexp1(string, start);
      while (pos < string.size & string[pos] == '|')
        let (new-node :: <regular-expression>, new-pos :: <integer>)
          = parse-regexp1(string, pos + 1);
        node := make(<union-regular-expression>,
                     union1: node, union2: new-node);
        pos := new-pos;
      end while;
      values(node, pos);
    end,
  
  method parse-regexp1
      (string :: <byte-string>, start :: <integer>)
   => (node :: <regular-expression>, pos :: <integer>);
    let (node :: <regular-expression>, pos :: <integer>)
      = parse-regexp2(string, start);
    while (pos < string.size & string[pos] ~== '|' & string[pos] ~== ')')
      let (new-node :: <regular-expression>, new-pos :: <integer>)
        = parse-regexp2(string, pos);
      node := make(<concatenation-regular-expression>,
                   head: node, tail: new-node);
      pos := new-pos;
    end while;
    values(node, pos);
  end,
  
  method parse-regexp2
      (string :: <byte-string>, start :: <integer>)
   => (node :: <regular-expression>, pos :: <integer>);
    let (node :: <regular-expression>, pos :: <integer>)
      = parse-regexp3(string, start);
    if (pos < string.size)
      if (string[pos] == '*')
        let new-node = make(<closure-regular-expression>, of: node);
        values(new-node, pos + 1);
      elseif (string[pos] == '+')
        let new-node = make(<concatenation-regular-expression>,
                            head: node,
                            tail: make(<closure-regular-expression>,
                                       of: copy-regular-expression(node)));
        values(new-node, pos + 1);
      elseif (string[pos] == '?')
        let new-node = make(<union-regular-expression>,
                            union1: make(<epsilon-regular-expression>),
                            union2: node);
        values(new-node, pos + 1);
      else
        values(node, pos);
      end if;
    else
      values(node, pos);
    end if;
  end,
  method parse-regexp3
      (string :: <byte-string>, start :: <integer>)
   => (node :: <regular-expression>, pos :: <integer>);
    if (start >= string.size)
      error("regexp missing at end of '%s'", string);
    else
      if (string[start] == '(')
        let (node :: <regular-expression>, pos :: <integer>)
          = parse-regexp0(string, start + 1);
        if (pos >= string.size | string[pos] ~== ')')
          error("closing ')' missing in regular expression '%s'", string);
        else
          values(node, pos + 1);
        end if;
      elseif (string[start] == '\\' & start < string.size - 1)
        values(make(<symbol-regular-expression>,
                    symbol: as(<integer>, string[start + 1])),
               start + 2);
      elseif (string[start] == '.')
        let dot-set = make(<bit-set>, upper-bound-hint: 256);
        for (symbol :: <integer> from 0 below 256)
          if (symbol ~= as(<integer>, '\n'))
            set-add!(dot-set, symbol);
          end;
        end for;
        values(make(<symbol-set-regular-expression>, symbol-set: dot-set),
               start + 1);
      elseif (string[start] == '[')
        let cclass-set = make(<bit-set>, upper-bound-hint: 256);
        let pos = start + 1;
        let complement? = if (pos < string.size & string[pos] = '^')
                            start := start + 1;
                            pos := pos + 1;
                            #t;
                          else #f end if;
        while (pos < string.size & (string[pos] ~== ']' | pos = start + 1))
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
          values(make(<symbol-set-regular-expression>,
                      symbol-set: complement-set),
                 pos + 1);
        else
          values(make(<symbol-set-regular-expression>, symbol-set: cclass-set),
                 pos + 1);
        end;
      else
        values(make(<symbol-regular-expression>,
                    symbol: as(<integer>, string[start])),
               start + 1);
      end if;
    end if;
  end;

  let cached-node = element($regular-expression-cache, string, default: #f);
  if (cached-node)
    cached-node
  else
    let (node :: <regular-expression>, pos :: <integer>)
      = parse-regexp0(string, 0);
    if (pos < string.size)
      error("regular expression \"%s\" ended prematurely at position %d",
            string, pos)
    end if;
    $regular-expression-cache[string] := node
  end if;
end function;
            
define class <match-accept-regular-expression> (<accept-regular-expression>)
  // no slots
end class;
            
define class <match-dfa-state> (<regular-expression-dfa-state>)
  slot match-dfa-state-accepting? :: <boolean> = #f;
end class;  

define sealed method do-regular-expression-dfa-state-position
    (state :: <regular-expression-dfa-state>,
     position :: <match-accept-regular-expression>,
     #key deterministic? = #f)
 => ();
  state.match-dfa-state-accepting? := #t;
end method;

define function jam-builtin-match
    (jam :: <jam-state>, regexps :: <sequence>, strings :: <sequence>)
 => (result :: <sequence>);
  error("the Match built-in rule is not yet implemented");
  #[]
end function;
