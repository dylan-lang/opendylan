Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Implement [part of] the collection protocol for <element>

/// Note that this only deals with the element nodes of a DOM tree
/// All other node types are effectively ignored
/// The implication is that, e.g., 'size(child-nodes(elt))' may not
/// return the same value as 'size(elt)', etc.

define sealed method element
    (elt :: <element>, key :: <DOM-string>, #key default = $unsupplied)
 => (child :: type-union(<element>, <object>))
  block (return)
    local method find-key (elt :: <element>)
	    return(elt)
	  end method;
    do-elements-by-tag-name(find-key, elt, name: key);
    if (supplied?(default))
      default
    else 
      error(make(<not-found-error>, 
		 format-string: "No such element %= in %=", 
		 format-arguments: vector(key, elt)))
    end
  end
end method element;

// We don't provide 'element-setter' because the key is part of
// the child element...
define sealed method element-setter
    (child :: <object>, elt :: <element>, key :: <DOM-string>)
 => (child :: <object>)
  error("You can't set element children this way")
end method element-setter;

define sealed method size
    (elt :: <element>) => (size :: <integer>)
  let children :: <node-list> = child-nodes(elt);
  count(method (c) instance?(c, <element>) end, children)
end method size;

define sealed method empty?
    (elt :: <element>) => (empty? :: <boolean>)
  let children :: <node-list> = child-nodes(elt);
  count(method (c) instance?(c, <element>) end, children) = 0
end method empty?;


/// Implement [part of] the sequence protocol for <element>

define sealed method element
    (elt :: <element>, index :: <integer>, #key default = $unsupplied)
 => (child :: type-union(<element>, <object>))
  //--- The new Chris Double semantics just returns the n'th child
  if (index >= elt.child-nodes.size)
    if (supplied?(default))
      default
    else
      error(make(<not-found-error>, 
		 format-string: "No element with index %d in %=", 
		 format-arguments: vector(index, elt)))
    end
  else
    elt.child-nodes[index]
  end
  /*
  //--- The old Scott McKay semantics returns the n'th descendent element
  //--- Which is the desirable behavior?
  block (return)
    let n :: <integer> = -1;		// don't count 'elt' itself...
    local method find-key (elt :: <element>)
	    if (index = n) return(elt)
	    else inc!(n) end
	  end method;
    do-elements-by-tag-name(find-key, elt);
    if (supplied?(default))
      default
    else 
      error(make(<not-found-error>, 
		 format-string: "No element with index %d in %=", 
		 format-arguments: vector(index, elt)))
    end
  end
  */
end method element;

// We don't provide 'element-setter' because the key is part of
// the child element...
define sealed method element-setter
    (child :: <object>, elt :: <element>, key :: <integer>)
 => (child :: <object>)
  error("You can't set element children this way")
end method element-setter;

define sealed method add!
    (elt :: <element>, child :: <element>) => (elt :: <element>)
  append-child(elt, child);
  elt
end method add!;

define sealed method add
    (elt :: <element>, child :: <element>) => (elt :: <element>)
  error("Use 'clone-node' and 'add!' instead of 'add'")
end method add;

define sealed method remove!
    (elt :: <element>, child :: <element>, #key test, count) => (elt :: <element>)
  ignore(test, count);
  remove-child(elt, child);
  elt
end method remove!;

define sealed method remove
    (elt :: <element>, child :: <element>, #key test, count) => (elt :: <element>)
  ignore(test, count);
  error("Use 'clone-node' and 'remove!' instead of 'remove'")
end method remove;


/// Implement [part of] the array protocol for <element>

define sealed method aref
    (elt :: <element>, #rest indices)
 => (child :: type-union(<element>, <object>))
  apply(element-aref, elt, indices)
end method aref;

define sealed inline method element-aref
    (elt :: <element>, key :: <DOM-string>, index :: <integer>)
 => (child :: false-or(<element>))
  block (return)
    let n :: <integer> = if (tag-name(elt) = key) -1 else 0 end;
    local method find-key (elt :: <element>)
	    if (index = n) return(elt)
	    else inc!(n) end
	  end method;
    do-elements-by-tag-name(find-key, elt, name: key);
    #f
  end
end method element-aref;

// We don't provide 'aref-setter' because the key is part of
// the child element...
define sealed method aref-setter
    (child :: <object>, elt :: <element>, #rest indices)
 => (child :: <object>)
  error("You can't set element children this way")
end method aref-setter;


/// Iteration

define sealed inline method forward-iteration-protocol
    (elt :: <element>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(element-initial-state(elt),
	 element-limit-state(elt),
	 element-next-state,
	 element-finished-state?,
	 element-current-key,
	 element-current-element,
	 element-current-element-setter,
	 element-copy-state)
end method forward-iteration-protocol;

define function element-initial-state
    (elt :: <element>) => (initial-state :: <integer>)
  let children :: <node-list> = child-nodes(elt);
  position-if(children, method (c) instance?(c, <element>) end, start: 0)
  | size(children)
end function element-initial-state;

define function element-limit-state
    (elt :: <element>) => (limit-state :: <integer>)
  let children :: <node-list> = child-nodes(elt);
  size(children)
end function element-limit-state;

define inline function element-next-state
    (elt :: <element>, state :: <integer>) => (next-state :: <integer>)
  let children :: <node-list> = child-nodes(elt);
  position-if(children, method (c) instance?(c, <element>) end, start: state + 1)
  | size(children)
end function element-next-state;

define inline function element-finished-state?
    (elt :: <element>, state :: <integer>, limit :: <integer>)
 => (finished? :: <boolean>)
  state = limit
end function element-finished-state?;

define inline function element-current-key
    (elt :: <element>, state :: <integer>) => (key :: <integer>)
  state
end function element-current-key;

define inline function element-current-element
    (elt :: <element>, state :: <integer>) => (child :: <element>)
  let children :: <node-list> = child-nodes(elt);
  children[state]
end function element-current-element;

define inline function element-current-element-setter
    (child :: false-or(<element>), elt :: <element>, state :: <integer>) => ()
  error("You can't set element children this way")
end function element-current-element-setter;

define function element-copy-state
    (elt :: <element>, state :: <integer>) => (new-state :: <integer>)
  state
end function element-copy-state;


/// Trampolines from documents

define sealed method element
    (doc :: <document>, key :: type-union(<DOM-string>, <integer>),
     #key default = $unsupplied)
 => (child :: type-union(<element>, <object>))
  let elt = document-element(doc);
  if (elt)
    element(elt, key, default: default)
  elseif (supplied?(default))
    default
  else 
    error(make(<not-found-error>, 
	       format-string: "No top-level element in document %=",
	       format-arguments: vector(doc)))
  end
end method element;

define sealed method element
    (doc :: <html-document>, key :: type-union(<DOM-string>, <integer>),
     #key default = $unsupplied)
 => (child :: type-union(<element>, <object>))
  let elt = body(doc);
  if (elt)
    element(elt, key, default: default)
  elseif (supplied?(default))
    default
  else
    error(make(<not-found-error>, 
	       format-string: "No BODY element in document %=",
	       format-arguments: vector(doc)))
  end
end method element;

define sealed method element-setter
    (child :: <object>, doc :: <document>, key :: <DOM-string>)
 => (child :: <object>)
  error("You can't set document children this way")
end method element-setter;

define sealed method aref
    (doc :: <document>, #rest indices)
 => (child :: type-union(<element>, <object>))
  let elt = document-element(doc);
  if (elt)
    apply(element-aref, elt, indices)
  else
    error(make(<not-found-error>, 
	       format-string: "No top-level element in document %=",
	       format-arguments: vector(doc)))
  end
end method aref;

define sealed method aref
    (doc :: <html-document>, #rest indices)
 => (child :: type-union(<element>, <object>))
  let elt = body(doc);
  if (elt)
    apply(element-aref, elt, indices)
  else
    error(make(<not-found-error>, 
	       format-string: "No BODY element in document %=",
	       format-arguments: vector(doc)))
  end
end method aref;

define sealed method aref-setter
    (child :: <object>, doc :: <document>, #rest indices)
 => (child :: <object>)
  error("You can't set element children this way")
end method aref-setter;
