Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple case-sensitive "plists" for named node maps

// 'properties' must contain only nodes that have both names and value.
// This is not really at all like a Lisp-style property list...
define sealed method get-property
    (properties :: <stretchy-object-vector>,
     name :: <DOM-string>, namespace :: false-or(<DOM-string>))
 => (value :: false-or(<node>))
  let name      :: <DOM-string>           = intern-string(name);
  let namespace :: false-or(<DOM-string>) = namespace & intern-string(namespace);
  block (return)
    let length :: <integer> = size(properties);
    without-bounds-checks
      for (i :: <integer> from 0 below length)
	let node :: <node> = properties[i];
	let node-name :: <node-name> = node.%name;
	when (if (namespace)
		name == node-name.%local & namespace == node-name.%namespace
	      else
		name == node-name.%name
	      end)
	  return(node)
	end
      end
    end;
    #f
  end
end method get-property;

define sealed method set-property!
    (properties :: <stretchy-object-vector>,
     key :: <DOM-string>, namespace :: false-or(<DOM-string>),
     value :: <node>)
 => (old-value :: false-or(<node>))
  let name      :: <DOM-string>           = intern-string(key);
  let namespace :: false-or(<DOM-string>) = namespace & intern-string(namespace);
  block (return)
    let length :: <integer> = size(properties);
    without-bounds-checks
      for (i :: <integer> from 0 below length)
	let node :: <node> = properties[i];
	let node-name :: <node-name> = node.%name;
	when (if (namespace)
		name == node-name.%local & namespace == node-name.%namespace
	      else
		name == node-name.%name
	      end)
	  properties[i] := value;
	  return(node)
	end
      end
    end;
    add!(properties, value);
    #f
  end
end method set-property!;

define sealed method remove-property!
    (properties :: <stretchy-object-vector>,
     key :: <DOM-string>, namespace :: false-or(<DOM-string>))
 => (old-value :: false-or(<node>))
  let name      :: <DOM-string>           = intern-string(key);
  let namespace :: false-or(<DOM-string>) = namespace & intern-string(namespace);
  let length :: <integer> = size(properties);
  let j      :: <integer> = 0;
  let old-value = #f;
  without-bounds-checks
    for (i :: <integer> from 0 below length)
      let node :: <node> = properties[i];
      let node-name :: <node-name> = node.%name;
      if (if (namespace)
	      name == node-name.%local & namespace == node-name.%namespace
	    else
	      name == node-name.%name
	    end)
	old-value := node
      else
	properties[j] := properties[i];
	inc!(j);
      end
    end
  end;
  size(properties) := j;
  old-value
end method remove-property!;


/// Named node maps

//
// interface NamedNodeMap {
//   readonly attribute unsigned long        length;
//   Node                      getNamedItem(in DOMString name);
//   Node                      setNamedItem(in Node arg)
//                                          raises(DOMException);
//   Node                      removeNamedItem(in DOMString name)
//                                             raises(DOMException);
//   Node                      item(in unsigned long index);
//   Node                      getNamedItemNS(in DOMString namespaceURI, 
//                                            in DOMString localName);
//   Node                      setNamedItemNS(in Node arg)
//                                            raises(DOMException);
//   Node                      removeNamedItemNS(in DOMString namespaceURI, 
//                                               in DOMString localName)
//                                               raises(DOMException);
// };
//

define sealed class <named-node-map> (<object>)
  sealed constant slot %nodes :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed constant slot owner-node :: false-or(<node>),
    required-init-keyword: node:;
end class <named-node-map>;

define sealed domain make (singleton(<named-node-map>));
define sealed domain initialize (<named-node-map>);

// We exclude the owner node from the equality comparison because the two
// named node maps might be equal clones that belong to different nodes
//---*** This doesn't work if the two vectors are in different orders
define sealed method \=
    (map1 :: <named-node-map>, map2 :: <named-node-map>) => (equal? :: <boolean>)
  map1 == map2
  | (  size(map1.%nodes) = size(map2.%nodes)
     & every?(\=, map1.%nodes, map2.%nodes))
end method \=;

define sealed method get-named-item
    (node-map :: <named-node-map>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (node :: false-or(<node>))
  get-property(node-map.%nodes, name, namespace)
end method get-named-item;

define sealed method set-named-item
    (node-map :: <named-node-map>, node :: <node>,
     #key namespace? :: <boolean> = #f)
 => (old-node :: false-or(<node>))
  unless (owner-document(node) == owner-document(owner-node(node-map)))
    error(make(<wrong-document-error>,
	       format-string: "The named node map %= doesn't belong to the node %=",
	       format-arguments: vector(node-map, node)))
  end;
  if (namespace?)
    set-property!(node-map.%nodes, local-name(node), namespace-uri(node), node)
  else
    set-property!(node-map.%nodes, node-name(node), #f, node)
  end
end method set-named-item;

define sealed method remove-named-item
    (node-map :: <named-node-map>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (old-node :: false-or(<node>))
  //--- The DOM spec is unclear about whether to signal a <not-found-error>
  let old-node = remove-property!(node-map.%nodes, name, namespace);
  old-node
end method remove-named-item;

define sealed inline method item
    (node-map :: <named-node-map>, index :: <integer>)
 => (item :: false-or(<node>))
  let length = size(node-map.%nodes);
  index < length
  & node-map.%nodes[index]
end method item;

define sealed inline method length
    (node-map :: <named-node-map>) => (length :: <integer>)
  size(node-map.%nodes)
end method length;
