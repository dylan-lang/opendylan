Module:    linker-support
Synopsis:  Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $linker-mangle-data
  = vector(#('-', '_'), #('!', 'X'), #('$', 'D'), #('%', 'P'), #('*', 'T'), 
           #('/', 'S'), #('<', 'L'), #('>', 'G'), #('?', 'Q'), #('+', 'A'),
           #('&', 'B'), #('^', 'C'), #('_', 'U'), #('@', 'O'), #('=', 'E'),
           #('~', 'N'));

define variable *linker-mangle-table* = #f;

define method initialize-mangler-table () => ()
  unless (*linker-mangle-table*)
    *linker-mangle-table* := make(<table>);
    for (mangle in $linker-mangle-data)
      *linker-mangle-table*[mangle[0]] := mangle[1];
    end for;
  end unless;
end method;

define method mangle(name :: <string>) => (mangled-name :: <string>)
  initialize-mangler-table();
  let new-name = as-lowercase(copy-sequence(name));
  for (c in new-name,
       pos from 0)
    let new-c = element(*linker-mangle-table*, c, default: #f);
    if (new-c)
      new-name[pos] := new-c;
    end if;
  end for;
  new-name
end method;
