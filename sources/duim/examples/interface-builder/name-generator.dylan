Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Name generator

define class <name-generator> (<object>)
  slot %table :: <table> = make(<table>);
  slot name-separator :: <character> = '-',
    init-keyword: name-separator:;
end class <name-generator>;

define method make-nth-name 
    (generator :: <name-generator>, name :: <string>, number :: <integer>)
 => (nth-name :: <string>)
  let separator = name-separator(generator);
  format-to-string("%s%s%d",
                   name,
                   separator,
                   number)
end method make-nth-name;

define method generate-next-name
    (generator :: <name-generator>, name :: <string>,
     #key remove-number?)
 => (next-name :: <string>)
  let root-name
    = if (remove-number?)
        split-name-and-number(name, generator.name-separator)
      else
        name
      end;
  let number = last-generated-number(generator, name) + 1; 
  use-nth-name(generator, root-name, number);
  make-nth-name(generator, root-name, number)
end method generate-next-name;

define method last-generated-number
    (generator :: <name-generator>, name :: <string>)
 => (number :: false-or(<integer>))
  let key = as(<symbol>, name);
  element(generator.%table, key, default: 0)
end method last-generated-number;

define method use-nth-name
    (generator :: <name-generator>, name :: <string>, number :: <integer>)
 => ()
  let key = as(<symbol>, name);
  let table = generator.%table;
  let old-number = element(table, key, default: #f);
  if (~old-number | number > old-number)
    table[key] := number
  end;
end method use-nth-name;

define method free-nth-name
    (generator :: <name-generator>, name :: <string>, number :: <integer>)
 => ()
  let key = as(<symbol>, name);
  let table = generator.%table;
  let old-number = element(table, key, default: #f);
  if (old-number & number = old-number)
    table[key] := number - 1
  end;
end method free-nth-name;

define method use-name
    (generator :: <name-generator>, name :: <string>) => ()
  let separator = name-separator(generator);
  let (key, number) = split-name-and-number(name, separator);
  use-nth-name(generator, key, number)
end method use-name;

define method free-name
    (generator :: <name-generator>, name :: <string>) => ()
  let separator = name-separator(generator);
  let (key, number) = split-name-and-number(name, separator);
  free-nth-name(generator, key, number)
end method free-name;

define method split-name-and-number 
    (name :: <string>, separator :: <character>)
 => (key :: <string>, number :: false-or(<integer>))
  let last-separator-position
    = block (return)
        for (i from size(name) - 1 to 0)
          if (name[i] = separator)
            return(i)
          end
        end
      end;
  let number 
    = if (last-separator-position < size(name) - 1)
         copy-sequence(name, start: last-separator-position)
      end;
  if (number)
    values(copy-sequence(name, end: last-separator-position),
           as(<integer>, number))
  else
    values(name, #f)
  end
end method split-name-and-number;
