Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Attributes

//
// interface Attr : Node {
//   readonly attribute  DOMString            name;
//   readonly attribute  boolean              specified;
//            attribute  DOMString            value;
//   readonly attribute Element          ownerElement;
// };
//

// We use 'node-parent' to get from the attribute to its node, but
// strictly speaking, this is not correct since attributes aren't
// really part of the document tree.  Fortunately, 'node-parent'
// isn't part of the real DOM API, so things work out OK.
define sealed class <attribute>
    (<non-leaf-node-mixin>, <node>)
  sealed slot %default-value :: false-or(<DOM-string>) = #f,
    init-keyword: default-value:;
  keyword type: = $attribute-node;
end class <attribute>;

define constant $attribute-child-types :: <simple-object-vector>
  = vector($text-node,
	   $entity-reference-node);
  
define sealed method node-accepts-child?
    (node :: <attribute>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  ignore(replace?);
  member?(node-type(child), $attribute-child-types)
end method node-accepts-child?;

define sealed inline method name
    (attribute :: <attribute>) => (name :: <DOM-string>)
  node-name(attribute)
end method name;

define sealed inline method value
    (attribute :: <attribute>) => (_value :: <DOM-string>)
  node-value(attribute) | ""
end method value;

define sealed inline method value-setter
    (_value :: false-or(<DOM-string>), attribute :: <attribute>)
 => (_value :: false-or(<DOM-string>))
  node-value(attribute) := _value
end method value-setter;

define sealed inline method default-value
    (attribute :: <attribute>) => (_value :: <DOM-string>)
  attribute.%default-value | ""
end method default-value;

define sealed inline method default-value-setter
    (_value :: false-or(<DOM-string>), attribute :: <attribute>)
 => (_value :: false-or(<DOM-string>))
  attribute.%default-value := _value
end method default-value-setter;

define sealed inline method specified?
    (attribute :: <attribute>) => (specified? :: <boolean>)
  // We mark attributes as unspecified if their 'node-value' is #f
  node-value(attribute) ~== #f
end method specified?;


define sealed inline method owner-element
    (attribute :: <attribute>) => (owner :: false-or(<element>))
  node-parent(attribute)
end method owner-element;

define sealed inline method owner-element-setter
    (owner :: false-or(<element>), attribute :: <attribute>)
 => (owner :: false-or(<element>))
  node-parent(attribute) := owner
end method owner-element-setter;


define sealed inline method parent-node
    (attribute :: <attribute>) => (parent :: false-or(<node>))
  #f
end method parent-node;

// Attributes aren't really part of the document tree; instead, they hang
// from the 'attributes' slot of <element>
define sealed method next-sibling
    (attribute :: <attribute>) => (node :: false-or(<node>))
  #f
end method next-sibling;

define sealed method previous-sibling
    (attribute :: <attribute>) => (node :: false-or(<node>))
  #f
end method previous-sibling;


define sealed method set-named-item
    (node-map :: <named-node-map>, attribute :: <attribute>,
     #key namespace? :: <boolean> = #f)
 => (old-attribute :: false-or(<attribute>))
  when (node-parent(attribute) & node-parent(attribute) ~== owner-node(node-map))
    error(make(<inuse-attribute-error>,
	       format-string: "The attribute %= is already in use by the node %=",
	       format-arguments: vector(attribute, node-parent(attribute))))
  end;
  next-method()
end method set-named-item;


/// Attribute utilities

define sealed method inherited-attribute
    (node :: <node>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (attribute :: false-or(<attribute>))
  block (return)
    let node :: false-or(<node>) = node;	// loosen up the type...
    while (node)
      let type = node-type(node);
      when (type = $element-node & has-attributes?(node))
	let attrs     :: <named-node-map> = attributes(node);
	let attribute :: false-or(<attribute>)
	  = get-named-item(attrs, name, namespace: namespace);
	when (attribute)
	  return(attribute)
	end
      end;
      // 'node-parent' is the owning element for attributes,
      // and the parent node for all other nodes...
      node := node-parent(node)
    end;
    #f
  end
end method inherited-attribute;

define sealed method xml-base-uri
    (node :: <node>, document-base-uri :: <DOM-string>)
 => (base-uri :: <DOM-string>)
  let attr :: false-or(<attribute>)
    = inherited-attribute(node, "base", namespace: $xml-namespace-uri);
  (attr & value(attr)) | document-base-uri
end method xml-base-uri;

define sealed method xml-language
    (node :: <node>) => (language :: false-or(<DOM-string>))
  let attr :: false-or(<attribute>)
    = inherited-attribute(node, "lang", namespace: $xml-namespace-uri);
  attr & value(attr)
end method xml-language;

define sealed method xml-preserve-space?
    (node :: <node>) => (preserve? :: <boolean>)
  let attr :: false-or(<attribute>)
    = inherited-attribute(node, "space", namespace: $xml-namespace-uri);
  attr & value(attr) = "preserve"
end method xml-preserve-space?;

define sealed method namespace-attribute?
    (node :: <attribute>) => (namespace? :: <boolean>)
  let name = node-name(node);
  (size(name) = 5 | (size(name) > 5 & name[5] == ':'))
  & string-equal?(name, "xmlns", end1: 5)
end method namespace-attribute?;
