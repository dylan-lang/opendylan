Module:   dfmc-reader
Synopsis: A name lookup table that respects hygiene.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <variable-name-table> (<mutable-explicit-key-collection>)
  constant slot entries :: <stretchy-object-vector>
     = make(<stretchy-object-vector>);
end class;

define method size (table :: <variable-name-table>) => (size :: <integer>)
  table.entries.size
end method;

define method element 
    (table :: <variable-name-table>, name :: <name-fragment>, 
       #key default = unsupplied())
 => (element-or-default)
  let entries = entries(table);
  let sz      = size(entries);
  iterate walk (index :: <integer> = 0)
    if (index >= sz)
      if (supplied?(default))
        default
      else
        error("No name matching %= found in variable name table %= and "
              "no default supplied.",
              name, table);
      end;
    else
      let test-name = entries[index];
      if (same-name-when-local?(name, test-name))
	entries[index + 1];
      else
        walk(index + 2);
      end;
    end;
  end;
end method;

define method element-setter
    (value, table :: <variable-name-table>, name :: <name-fragment>)
 => (value)
  let entries = entries(table);
  let sz      = size(entries);
  iterate walk (index :: <integer> = 0)
    if (index >= sz)
      add!(entries, name);
      add!(entries, value);
      value
    else
      let test-name = entries[index];
      if (same-name-when-local?(name, test-name))
	entries[index + 1] := value;
      else
        walk(index + 2);
      end;
    end;
  end;
end method;

define inline function table-next-state
    (collection :: <variable-name-table>, state :: <integer>)
 => (state :: <integer>)
  state + 2
end function;

define inline function table-finished-state?
    (collection :: <variable-name-table>, 
     state :: <integer>, limit :: <integer>)
 => (result :: <boolean>)
  state = limit
end function;

define inline function table-current-key
    (collection :: <variable-name-table>, state :: <integer>) 
 => (result :: <variable-name-fragment>)
  collection[state]
end function;

define inline function table-current-element
    (collection :: <variable-name-table>, state :: <integer>) => (result)
  collection[state + 1]
end function;

define inline function table-current-element-setter
    (new-value, collection :: <variable-name-table>, state :: <integer>) 
 => (result)
  collection[state + 1] := new-value;
end function;

define inline function table-copy-state
    (collection :: <variable-name-table>, state :: <integer>) 
 => (state :: <integer>)
  state
end function;

define sealed inline method forward-iteration-protocol 
    (collection :: <variable-name-table>)
    => (initial-state :: <integer>, limit :: <integer>,
        next-state :: <function>, finished-state? :: <function>,
        current-key :: <function>,
        current-element :: <function>, current-element-setter :: <function>,
        copy-state :: <function>);
  values(0,
	 size(collection.entries),
	 table-next-state,
	 table-finished-state?,
	 table-current-key,
	 table-current-element,
	 table-current-element-setter,
	 table-copy-state)
end method forward-iteration-protocol;
