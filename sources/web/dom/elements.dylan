Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Elements

//
// interface Element : Node {
//   readonly attribute  DOMString            tagName;
//   DOMString                 getAttribute(in DOMString name);
//   void                      setAttribute(in DOMString name, 
//                                          in DOMString value)
//                                          raises(DOMException);
//   void                      removeAttribute(in DOMString name)
//                                             raises(DOMException);
//   Attr                      getAttributeNode(in DOMString name);
//   Attr                      setAttributeNode(in Attr newAttr)
//                                              raises(DOMException);
//   Attr                      removeAttributeNode(in Attr oldAttr)
//                                                 raises(DOMException);
//   NodeList                  getElementsByTagName(in DOMString name);
//   DOMString                 getAttributeNS(in DOMString namespaceURI, 
//                                            in DOMString localName);
//   void                      setAttributeNS(in DOMString namespaceURI, 
//                                            in DOMString qualifiedName, 
//                                            in DOMString value)
//                                            raises(DOMException);
//   void                      removeAttributeNS(in DOMString namespaceURI, 
//                                               in DOMString localName)
//                                               raises(DOMException);
//   Attr                      getAttributeNodeNS(in DOMString namespaceURI, 
//                                                in DOMString localName);
//   Attr                      setAttributeNodeNS(in Attr newAttr)
//                                                raises(DOMException);
//   NodeList                  getElementsByTagNameNS(in DOMString namespaceURI, 
//                                                    in DOMString localName);
//   boolean                   hasAttribute(in DOMString name);
//   boolean                   hasAttributeNS(in DOMString namespaceURI, 
//                                            in DOMString localName);
// 
// };
//

define sealed class <element>
    (<non-leaf-node-mixin>, <node>,
     // Include these so that we can provide collection and sequence
     // access functions to elements and their children
     <array>, <mutable-sequence>, <mutable-explicit-key-collection>)
  sealed slot %attributes :: false-or(<named-node-map>) = #f;
  keyword type: = $element-node;
end class <element>;

define sealed method \=
    (elt1 :: <element>, elt2 :: <element>) => (equal? :: <boolean>)
  elt1 == elt2
  | (  next-method()
     & (elt1.%attributes | #[]) = (elt2.%attributes | #[]))
end method \=;


define sealed method do-clone-node
    (node :: <element>, new-node :: <element>) => ()
  // Deep-copy the attributes into the new node
  let attrs = node.%attributes;
  when (attrs)
    let attrs :: <named-node-map> = attrs;	// tighten the type
    let document  = owner-document(new-node);
    let new-attrs = make(<named-node-map>, node: new-node);
    new-node.%attributes := new-attrs;
    for (i :: <integer> from 0 below length(attrs))
      let attr     = item(attrs, i);
      let new-attr = make(<attribute>,
			  document: document,
			  name:  attr.%name,
			  value: attr.%value);
      //---*** What should we pass as the value of 'namespace?'
      set-named-item(new-attrs, new-attr)
    end
  end
end method do-clone-node;

define constant $element-child-types :: <simple-object-vector>
  = vector($element-node,
	   $processing-instruction-node,
	   $comment-node,
	   $text-node,
	   $CDATA-section-node,
	   $entity-reference-node);
  
define sealed method node-accepts-child?
    (node :: <element>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  ignore(replace?);
  node ~== child			// prevent recursion
  & member?(node-type(child), $element-child-types)
end method node-accepts-child?;

define sealed inline method tag-name
    (elt :: <element>) => (name :: <DOM-string>)
  node-name(elt)
end method tag-name;


// Don't cons the <named-node-map> for the attributes until we need it
define sealed method attributes
    (elt :: <element>) => (attributes :: <named-node-map>)
  elt.%attributes
  | (elt.%attributes := make(<named-node-map>, node: elt))
end method attributes;

define sealed method has-attributes?
    (elt :: <element>) => (has-attributes? :: <boolean>)
  elt.%attributes & ~empty?(elt.%attributes)
end method has-attributes?;

define sealed method has-attribute?
    (elt :: <element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (has-attribute? :: <boolean>)
  get-attribute-node(elt, name, namespace: namespace) ~== #f
end method has-attribute?;


define sealed method do-attributes
    (function :: <function>, elt :: <element>) => ()
  let attrs = elt.%attributes;
  when (attrs)
    //--- This knows too much about how <named-node-map> is implemented
    let attrs  :: <stretchy-object-vector> = attrs.%nodes;
    let length :: <integer> = size(attrs);
    without-bounds-checks
      for (i :: <integer> from 0 below length)
	let attr :: <attribute> = attrs[i];
	function(attr)
      end
    end
  end;
  #f
end method do-attributes;


define sealed method get-attribute
    (elt :: <element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (_value :: <DOM-string>)
  let attrs     :: false-or(<named-node-map>) = elt.%attributes;
  let attribute :: false-or(<attribute>)
    = attrs & get-named-item(attrs, name, namespace: namespace);
  //---*** How are we supposed to return the default value?
  if (attribute) value(attribute) else as(<DOM-string>, "") end
end method get-attribute;

define sealed method get-attribute-node
    (elt :: <element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (attribute :: false-or(<attribute>))
  let attrs     :: false-or(<named-node-map>) = elt.%attributes;
  let attribute :: false-or(<attribute>)
    = attrs & get-named-item(attrs, name, namespace: namespace);
  attribute
end method get-attribute-node;


define sealed method set-attribute
    (elt :: <element>, name :: <DOM-string>, _value :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f) => ()
  check-read-only(elt);
  let attrs     :: <named-node-map> = attributes(elt);
  let attribute :: false-or(<attribute>)
    = get-named-item(attrs, name, namespace: namespace);
  if (attribute)
    value(attribute) := _value
  else
    let attribute :: <attribute>
      = create-attribute(owner-document(elt), name, namespace: namespace);
    value(attribute) := _value;
    set-named-item(attrs, attribute, namespace?: namespace ~= #f);
    node-parent(attribute) := elt
  end
end method set-attribute;

define sealed method set-attribute-node
    (elt :: <element>, new-attribute :: <attribute>,
     #key namespace? :: <boolean> = #f)
 => (old-attribute :: false-or(<attribute>))
  check-read-only(elt);
  let name      = if (namespace?) local-name(new-attribute)    else name(new-attribute) end;
  let namespace = if (namespace?) namespace-uri(new-attribute) else #f end;
  let attrs     :: <named-node-map> = attributes(elt);
  let attribute :: false-or(<attribute>)
    = get-named-item(attrs, name, namespace: namespace);
  set-named-item(attrs, new-attribute, namespace?: namespace?);
  node-parent(new-attribute) := elt;
  attribute
end method set-attribute-node;


define sealed method remove-attribute
    (elt :: <element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (attribute :: false-or(<attribute>))
  check-read-only(elt);
  let attrs     :: false-or(<named-node-map>) = elt.%attributes;
  let attribute :: false-or(<attribute>)
    = attrs & get-named-item(attrs, name, namespace: namespace);
  when (attribute)
    //---*** If the attribute has a default value, insert the default
    //---*** rather than calling 'remove-named-item'
    remove-named-item(attrs, name, namespace: namespace)
  end;
  attribute
end method remove-attribute;

define sealed method remove-attribute-node
    (elt :: <element>, attribute :: <attribute>,
     #key namespace? :: <boolean> = #f)
 => (attribute :: false-or(<attribute>))
  check-read-only(elt);
  let name      = if (namespace?) local-name(attribute)    else name(attribute) end;
  let namespace = if (namespace?) namespace-uri(attribute) else #f end;
  let attrs     :: false-or(<named-node-map>) = elt.%attributes;
  let attribute :: false-or(<attribute>)
    = attrs & get-named-item(attrs, name, namespace: namespace);
  unless (attribute)
    error(make(<not-found-error>,
	       format-string: "There is no attribute %= for element %=",
	       format-arguments: vector(attribute, elt)))
  end;
  remove-named-item(attrs, name, namespace: namespace);
  attribute
end method remove-attribute-node;


define sealed method get-elements-by-tag-name
    (elt :: <element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (elements :: <node-list>)
  let elements :: <node-list> = make(<node-list>);
  local method add-element! (elt :: <element>)
	  add!(elements, elt)
	end method;
  do-elements-by-tag-name(add-element!, elt,
			  name:      if (name      = "*") #f else name      end,
			  namespace: if (namespace = "*") #f else namespace end);
  elements
end method get-elements-by-tag-name;


// Returns a new string containing the concatenated text data of all the
// text or cdata elements under 'elt'
define sealed method text
    (elt :: <element>) => (text :: false-or(<DOM-string>))
  let found? = #f;			// return #f if no text elements
  let n :: <integer> = 0;
  do-nodes(method (node :: <node>)
	     when (node-type(node) = $text-node | node-type(node) = $cdata-section-node)
	       found? := #t;
	       inc!(n, length(node))
	     end
	   end method, elt);
  when (found?)
    let result :: <DOM-string> = make(<DOM-string>, size: n);
    let i :: <integer> = 0;
    do-nodes(method (node :: <node>)
	       when (node-type(node) = $text-node | node-type(node) = $cdata-section-node)
		 let n :: <integer> = length(node);
		 copy-string-into!(data(node), 0, n, result, i);
		 inc!(i, n)
	       end
	     end method, elt);
    result
  end
end method text;
  

/// HTML element

//
// interface HTMLElement : Element {
//            attribute  DOMString            id;
//            attribute  DOMString            title;
//            attribute  DOMString            lang;
//            attribute  DOMString            dir;
//            attribute  DOMString            className;
// };
// 

// Notice that most of the attributes defined in the IDL above do not
// appear as slots in the class.  Instead, they will get stored in the
// 'attributes' of the element.  This is true of all HTML elements.
define sealed class <html-element> (<element>)
  // We keep this as a slot instead of an attribute because when someone
  // want to access it, the access has to be reasonably fast
  sealed slot html/id :: <DOM-string> = "";
end class <html-element>;

define sealed method \=
    (elt1 :: <html-element>, elt2 :: <html-element>) => (equal? :: <boolean>)
  elt1 == elt2
  | (  next-method()
     & html/id(elt1) = html/id(elt2))
end method \=;

define sealed method get-attribute
    (elt :: <html-element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (_value :: <DOM-string>)
  next-method(elt, as-uppercase(name), namespace: namespace)
end method get-attribute;

define sealed method set-attribute
    (elt :: <html-element>, name :: <DOM-string>, _value :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f) => ()
  next-method(elt, as-uppercase(name), _value, namespace: namespace)
end method set-attribute;

define sealed method remove-attribute
    (elt :: <html-element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f) => ()
  next-method(elt, as-uppercase(name), namespace: namespace)
end method remove-attribute;

define sealed method get-attribute-node
    (elt :: <html-element>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (attribute :: false-or(<attribute>))
  next-method(elt, as-uppercase(name), namespace: namespace)
end method get-attribute-node;


define sealed method do-elements-by-tag-name
    (function :: <function>, elt :: <html-element>,
     #key name      :: false-or(<DOM-string>) = #f,
          namespace :: false-or(<DOM-string>) = #f) => ()
  when (instance?(elt, <html-element>)
	// Compare names with 'string-equal?' because HTML tag names are not case-sensitive
	& (if (namespace)
	    ((~name | string-equal?(local-name(elt), name))
             & (~namespace | namespace-uri(elt) = namespace))
	   else
	    (~name | string-equal?(node-name(elt), name))
	   end))
    function(elt)
  end;
  for (child in child-nodes(elt))
    do-elements-by-tag-name(function, child, name: name, namespace: namespace)
  end
end method do-elements-by-tag-name;
