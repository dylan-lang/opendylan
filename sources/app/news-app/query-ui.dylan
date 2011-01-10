language: infix-dylan
module: news-app
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* query reading/writing: */

define variable *operators* :: <list> = #(#"and", #"or", #"not");

/*
define method add-query-string (key :: <string>, target-str)
  append-string!(target-str, key);
end;

define method add-query-string (key :: <list>, target-str)
  append-string!(target-str, "(");
  append-string!(target-str, as(<string>, head(key)));
  for (k in tail(key))
    append-string!(target-str, " ");
    add-query-string(k, target-str);
  end;
  append-string!(target-str, ")");
end;

define method query-search-string (query :: <query>)
  let str :: <stretchy-byte-string> = make(<stretchy-byte-string>, size: 255);
  size(str) := 0;
  
  add-query-string(query.search-keys, str);
  str;
end;
*/

define method parse-query-error(message, str, #key start = 0, stop = size(str))
  error("Invalid Query String %s: %s %s\n"
        "Queries must be parenthesized lists where the first element is "
        "an operator such as \"and\", \"or\", or \"not\"",
        copy-sequence(str, start: start, end: stop),
        message)
end method parse-query-error;

define method parse-query (str, start, #key stop = size(str))
  => (query-list, next-pos)
  if ((str[start] = '('))
    let (op-str, next-start) = nth-word(str, 0, start: start + 1);
    let op = as(<symbol>, op-str);
    if (member?(op, *operators*))
      let op-list = #();
      block (exit)
        while (next-start < stop)
          let (word, next-start2) = nth-word(str, 0, start: next-start);
          if (size(word) = 0)
            exit();
          elseif (word[0] = '(')
            let (new-stuff, new-start) = parse-query(str, next-start, stop: stop);
            if (new-stuff)
              op-list := add!(op-list, new-stuff);
              next-start := new-start + 1;
            else // parse-error detected and already reported:
              exit();
            end;
          elseif (last(word) = ')')
            let last-real-character
              = for (i from word.size - 2 to 0 by -1, while: word[i] == ')')
                finally i
                end;
            let new-word = copy-sequence(word, end: last-real-character);
            op-list := add!(op-list, new-word);
            next-start := next-start2;
            exit();
          else
            op-list := add!(op-list, word);
            next-start := next-start2 + 1;
          end;
        end while;
      end block;
      values(pair(op, reverse!(op-list)), next-start);
    else
      parse-query-error("Bad query operator: ", op-str);
    end;
  else
    parse-query-error("Bad query string: ", str, start: start, stop: stop - 1);
  end;
end;

/*
define method query-groups-string (query :: <query>, #key add-new-lines?)
  let str :: <stretchy-byte-string> = make(<stretchy-byte-string>, size: 255);
  size(str) := 0;
  
  for (g in query.news-groups)
    append-string!(str, g);
    if (add-new-lines?)
      append-string!(str, ",\n");
    else
      append-string!(str, ", ");
    end;
  end for;
  if (size(str) > 0)
    size(str) := size(str) - 2; // remove last comma.
  end;
  str;
end;

define method parse-query-groups (str, #key start = 0, stop = size(str))
  let next-start = start;
  let str = replace-elements!(str, method(x)
                                     x = '\n';
                                   end,
                              always(' '));
  let group-list = #();
  block (exit)
    while (next-start < stop)
      let (word, next-start2) = nth-word(str, 0, start: next-start);
      if (size(word) = 0)
        exit();
      elseif (word = ",")
        next-start := next-start2 + 1; // skip it.
      else
        if (last(word) = ',')
          group-list := add!(group-list, remove!(word, ',', count: 1));
        else
          group-list := add!(group-list, word);
        end;
        next-start := next-start2 + 1;
      end;
    end while;
  end block;
  values(reverse!(group-list), next-start);
end;
*/
