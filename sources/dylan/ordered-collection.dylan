Module:    internal
Author:    Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Ordered explicit key collections.
//
// This doesn't support deletion of elements, and it's also not threads safe


define abstract class <ordered-key-collection>
    (<mutable-explicit-key-collection>, <stretchy-collection>)
  constant slot key-sequence :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
end;

define sealed domain key-sequence(<ordered-key-collection>);

define sealed generic ordered-mapping
    (tab :: <ordered-key-collection>) => (table :: <explicit-key-collection>);

define sealed inline method size (tab :: <ordered-key-collection>)
 => (size :: <integer>)
  tab.ordered-mapping.size
end method size;

define sealed inline method remove-all-keys! (tab :: <ordered-key-collection>)
  remove-all-keys!(tab.ordered-mapping);
  tab.key-sequence.size := 0;
end method remove-all-keys!;

define sealed inline method element
    (tab :: <ordered-key-collection>, key, #key default = unsupplied())
 => (key-or-default :: <object>)
  if (supplied?(default))
    element(tab.ordered-mapping, key, default: default)
  else
    element(tab.ordered-mapping, key)
  end;
end method element;


define sealed inline method element-setter
    (value , tab :: <ordered-key-collection>, key) => (value)
  let table = tab.ordered-mapping;
  if (not-found?(element(table, key, default: not-found())))
    add!(tab.key-sequence, key);
  end;
  table[key] := value;
end;

define sealed inline method forward-iteration-protocol
    (tab :: <ordered-key-collection>)
 => (init :: <integer>, lim :: <integer>,
     next :: <function>,
     finished? :: <function>,
     key :: <function>,
     elt :: <function>,
     elt-setter :: <function>,
     copy :: <function>)
  local method next (tab, index :: <integer>)
	  index + 1
	end;
  local method finished? (tab, index :: <integer>, size :: <integer>)
	  index == size
	end;
  local method key (tab :: <ordered-key-collection>, index :: <integer>)
	  tab.key-sequence[index]
	end;
  local method elt (tab :: <ordered-key-collection>, index :: <integer>)
	  element(tab.ordered-mapping, tab.key-sequence[index])
	end;
  local method elt-setter (value, tab :: <ordered-key-collection>, index :: <integer>)
	  tab.ordered-mapping[tab.key-sequence[index]] := value
	end;
  local method copy (tab, index :: <integer>)
	  index
	end;
  values(0, tab.key-sequence.size,
	 next, finished?, key, elt, elt-setter, copy)
end;


define class <ordered-object-table> (<ordered-key-collection>)
  constant slot ordered-mapping :: <object-table> = make(<object-table>),
    init-keyword: table:;
end;

define class <ordered-string-table> (<ordered-key-collection>)
  constant slot ordered-mapping :: <string-table> = make(<string-table>),
    init-keyword: table:;
end;


define class <ordered-object-set> (<ordered-key-collection>)
  constant slot ordered-mapping :: <object-set> = make(<object-set>),
    init-keyword: set:;
end;

define sealed inline method member? 
    (object, tab :: <ordered-object-set>, #key test)
  => (bool :: <boolean>)
  member?(object, tab.ordered-mapping)
end;

define sealed inline method add! (tab :: <ordered-object-set>, key)
  => (tab :: <ordered-object-set>)
  let set = tab.ordered-mapping;
  unless (member?(key, set))
    add!(tab.key-sequence, key);
  end;
  add!(set, key);
  tab
end;
