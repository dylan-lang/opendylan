Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Document fragments

//
// interface DocumentFragment : Node {
// };
//

define sealed class <document-fragment>
    (<root-node-mixin>, <node>)
  keyword type: = $document-fragment-node;
  keyword document: = #f;
end class <document-fragment>;


define constant $document-fragment-child-types :: <simple-object-vector>
  = vector($element-node,
	   $processing-instruction-node,
	   $comment-node,
	   $text-node,
	   $CDATA-section-node,
	   $entity-reference-node);
  
define sealed method node-accepts-child?
    (node :: <document-fragment>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  ignore(replace?);
  member?(node-type(child), $document-fragment-child-types)
end method node-accepts-child?;

// When 'where' is #f, this inserts at the end
define sealed method insert-before
    (node :: <abstract-node>, child :: <document-fragment>, where :: false-or(<node>))
 => (child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, child);
  when (where & ~position(child-nodes(node), where))
    error(make(<not-found-error>,
	       format-string: "The reference node %= isn't a child of %=",
	       format-arguments: vector(where, node)))
  end;
  let children :: <node-list> = child-nodes(child);
  if (every?(method (c) node-accepts-child?(node, c) end, children))
    // Insert each child of the fragment 
    let document = owner-document(node);
    for (child in children)
      quick-insert-child-at!(node, child, before: where | #"end");
      note-node-added(document, child)
    end;
    children.size := 0;
    child
  else
    error(make(<hierarchy-request-error>,
               format-string: "You can't add the child %= to %=",
               format-arguments: vector(child, node)))
  end
end method insert-before;

// Like 'insert-child-at!', but we use it for the "batch" insertions
// needed by fragments; in particular, it doesn't do any error-checking
// and it doesn't remove the child from its fragment parent
define sealed method quick-insert-child-at!
    (node :: <abstract-node>, child :: <node>,
     #key before :: type-union(<node>, one-of(#"start", #"end")) = #"end") => ()
  let children :: <node-list> = child-nodes(node);
  let index :: <integer>
    = select (before)
	#"start"  => 0;
	#"end"    => length(children);
	otherwise => position(children, before);
      end;
  insert-at!(children, child, index);
  let prev = (index > 0) & children[index - 1];
  let next = (index < length(children) - 1) & children[index + 1];
  previous-sibling(child) := prev;
  next-sibling(child)     := next;
  next & (previous-sibling(next) := child);
  prev & (next-sibling(prev)     := child);
  node-parent(child) := node
end method quick-insert-child-at!;

define sealed method append-child
    (node :: <abstract-node>, child :: <document-fragment>)
 => (child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, child);
  let children :: <node-list> = child-nodes(child);
  if (every?(method (c) node-accepts-child?(node, c) end, children))
    let document = owner-document(node);
    for (child in children)
      // Append each child of the fragment 
      quick-insert-child-at!(node, child, before: #"end");
      note-node-added(document, child)
    end;
    children.size := 0;
    child
  else
    error(make(<hierarchy-request-error>,
               format-string: "You can't add the child %= to %=",
               format-arguments: vector(child, node)))
  end
end method append-child;

define sealed method replace-child
    (node :: <abstract-node>, new-child :: <document-fragment>, old-child :: <node>)
 => (old-child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, new-child);
  let children :: <node-list> = child-nodes(new-child);
  if (every?(method (c) node-accepts-child?(node, c, replace?: #t) end, children))
    // Replace the old child with each child of the fragment
    let where = next-sibling(old-child);
    remove-child(node, old-child);
    let document = owner-document(node);
    note-node-removed(document, old-child);
    for (child in children)
      quick-insert-child-at!(node, child, before: where);
      note-node-added(document, child)
    end;
    children.size := 0;
    old-child
  else
    error(make(<hierarchy-request-error>,
               format-string: "You can't replace the child %= with %= in %=",
               format-arguments: vector(old-child, new-child, node)))
  end
end method replace-child;


/// Documents

//
// interface Document : Node {
//   readonly attribute  DocumentType         doctype;
//   readonly attribute  DOMImplementation    implementation;
//   readonly attribute  Element              documentElement;
//            attribute DOMString             actualEncoding;
//            attribute DOMString             encoding;
//            attribute boolean               standalone;
//            attribute boolean               strictErrorChecking;
//            attribute DOMString             version;
//   Element                   createElement(in DOMString tagName)
//                                           raises(DOMException);
//   DocumentFragment          createDocumentFragment();
//   Text                      createTextNode(in DOMString data);
//   Comment                   createComment(in DOMString data);
//   CDATASection              createCDATASection(in DOMString data)
//                                                raises(DOMException);
//   ProcessingInstruction     createProcessingInstruction(in DOMString target, 
//                                                         in DOMString data)
//                                                         raises(DOMException);
//   Attr                      createAttribute(in DOMString name)
//                                             raises(DOMException);
//   EntityReference           createEntityReference(in DOMString name)
//                                                   raises(DOMException);
//   NodeList                  getElementsByTagName(in DOMString tagname);
//   Node                      importNode(in Node importedNode, 
//                                        in boolean deep)
//                                        raises(DOMException);
//   Element                   createElementNS(in DOMString namespaceURI, 
//                                             in DOMString qualifiedName)
//                                             raises(DOMException);
//   Attr                      createAttributeNS(in DOMString namespaceURI, 
//                                               in DOMString qualifiedName)
//                                               raises(DOMException);
//   NodeList                  getElementsByTagNameNS(in DOMString namespaceURI, 
//                                                    in DOMString localName);
//   Element                   getElementById(in DOMString elementId);
//   Node                      adoptNode(in Node source)
//                                       raises(DOMException);
//   void                      setBaseURI(in DOMString baseURI)
//                                        raises(DOMException);
// };
//

define open abstract class <document>
    (<root-node-mixin>, <node>,
     <array>, <mutable-sequence>, <mutable-explicit-key-collection>)
  //---*** Shouldn't doctype just be one of the children?
  sealed slot doctype :: false-or(<document-type>) = #f;
  sealed constant slot implementation :: <DOM-implementation> = make(<DOM-implementation>),
    init-keyword: implementation:;
  sealed slot base-uri :: false-or(<DOM-string>) = #f,
    init-keyword: base-uri:;
  sealed slot encoding :: false-or(<DOM-string>) = #f,
    init-keyword: encoding:;
  sealed slot actual-encoding :: false-or(<DOM-string>) = #f,
    init-keyword: actual-encoding:;
  sealed slot standalone? :: <boolean> = #f,
    init-keyword: standalone?:;
  sealed slot strict-error-checking? :: <boolean> = #t,
    init-keyword: strict-error-checking?:;
  sealed slot version :: false-or(<DOM-string>) = #f,
    init-keyword: version:;
  //---*** What should this table really be keyed on?
  sealed constant slot %node-names :: <string-table> = make(<string-table>);
  keyword type: = $document-node;
end class <document>;

define sealed method initialize
    (document :: <document>, #key)
  next-method();
  // A document is its own owner, I guess...
  owner-document(document) := document
end method initialize;

define sealed method \=
    (doc1 :: <document>, doc2 :: <document>) => (equal? :: <boolean>)
  doc1 == doc2
  | (  next-method()
     & doctype(doc1)        = doctype(doc2)
     & implementation(doc1) = implementation(doc2))
end method \=;


// A concrete class for XML documents
define sealed class <xml-document> (<document>)
  keyword document: = #f;
end class <xml-document>;

define sealed method make
    (class == <document>, #rest initargs, #key)
 => (document :: <xml-document>)
  apply(make, <xml-document>, initargs)
end method make;


define sealed method do-clone-node
    (node :: <document>, new-node :: <document>) => ()
  when (doctype(node))
    doctype(new-node) := clone-node(doctype(node), #f)
  end
end method do-clone-node;

define constant $document-child-types :: <simple-object-vector>
  = vector($element-node,
	   $processing-instruction-node,
	   $comment-node,
	   $document-type-node);
  
define sealed method node-accepts-child?
    (node :: <document>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  let type = node-type(child);
  select (type)
    $element-node =>
      // Documents allow only a single top-level <element>
      replace? | document-element(node) == #f;
    $text-node =>
      // Ignore top-level whitespace in documents
      every?(xml-whitespace?, data(child)) & #"ignore";
    otherwise =>
      member?(type, $document-child-types);
  end
end method node-accepts-child?;

define sealed method document-element
    (document :: <document>) => (elt :: false-or(<element>))
  find-element(child-nodes(document), method (c) instance?(c, <element>) end,
	       failure: #f)
end method document-element;


define sealed method adopt-node
    (document :: <document>, node :: <node>)
 => (node :: false-or(<node>))
  do-adopt-node(document, node, #t)
end method adopt-node;

define sealed method do-adopt-node
    (document :: <document>, node :: <node>, deep? :: <boolean>)
 => (node :: false-or(<node>))
  //---*** Implement this by recursively setting all the owner-document slots
end method do-adopt-node;

define sealed method import-node
    (document :: <document>, node :: <node>, deep? :: <boolean>)
 => (node :: false-or(<node>))
  do-adopt-node(document, clone-node(node, deep?), deep?)
end method import-node;


// Not in the DOM spec, but we need it to create documents somehow
define sealed inline method create-xml-document
    (#key name :: false-or(<DOM-string>) = #f,
	  namespace :: false-or(<DOM-string>) = #f,
	  doctype :: false-or(<document-type>) = #f)
 => (document :: <xml-document>)
  let document = make(<xml-document>, 
		      name: "#document");	// not #"document"!
  when (doctype)
    append-child(document, doctype)
  end;
  when (name)
    append-child(document, create-element(document, name, namespace: namespace))
  end;
  document
end method create-xml-document;

define sealed inline method create-element
    (document :: <document>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (element :: <element>)
  make(<element>,
       document: document,
       name: make-node-name(name, namespace: namespace, document: document))
end method create-element;

define sealed inline method create-document-fragment
    (document :: <document>)
 => (fragment :: <document-fragment>)
  make(<document-fragment>,
       document: document,
       name: "#document-fragment")	// not #"document-fragment"!
end method create-document-fragment;

define sealed inline method create-text-node
    (document :: <document>, data :: <DOM-String>)
 => (text :: <text>)
  // Be careful not to capture string data, because there are DOM API
  // functions that can smash it
  make(<text>,
       document: document,
       name:  "#text",		// not #"text"!
       value: copy-string(data))
end method create-text-node;

define sealed inline method create-CDATA-section
    (document :: <document>, data :: <DOM-String>)
 => (section :: <CDATA-section>)
  make(<CDATA-section>,
       document: document,
       name:  "#cdata-section",	// not #"cdata-section"!
       value: copy-string(data))
end method create-CDATA-section;

define sealed inline method create-comment
    (document :: <document>, data :: <DOM-String>)
 => (comment :: <comment>)
  make(<comment>,
       document: document,
       name:  "#comment",		// not #"comment"!
       value: copy-string(data))
end method create-comment;

define sealed inline method create-processing-instruction
    (document :: <document>, target :: <DOM-string>, data :: <DOM-string>)
 => (instruction :: <processing-instruction>)
  make(<processing-instruction>,
       document: document,
       name:  target,
       value: data)
end method create-processing-instruction;

define sealed inline method create-attribute
    (document :: <document>, name :: <DOM-string>,
     #key value :: false-or(<DOM-string>) = #f,
	  namespace :: false-or(<DOM-string>) = #f)
 => (attribute :: <attribute>)
  make(<attribute>,
       document: document,
       name:  make-node-name(name, namespace: namespace, document: document),
       value: value)
end method create-attribute;

define sealed inline method create-entity-reference
    (document :: <document>, name :: <DOM-string>)
 => (reference :: <entity-reference>)
  make(<entity-reference>,
       document: document,
       name: name)
end method create-entity-reference;


//--- This is a pretty slow way to do this, maybe keep a table instead?
define sealed method get-element-by-id
    (document :: <document>, id :: <DOM-string>)
 => (element :: false-or(<element>))
  block (return)
    local method do-attrs (elt :: <element>)
		   do-attributes(method (attr)
				   when (id-attribute?(document, attr)
					 // '=' because XML tag names are case-sensitive
					 & node-value(attr) = id)
				     return(elt)
                                   end
				 end method, elt)
	  end method;
    do-elements(do-attrs, document);
    #f
  end    
end method get-element-by-id;

define sealed method id-attribute?
    (document :: <document>, attr :: <attribute>)
 => (id? :: <boolean>)
  #f
end method id-attribute?;

define sealed method get-elements-by-tag-name
    (document :: <document>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (elements :: <node-list>)
  let elements :: <node-list> = make(<node-list>);
  local method add-element! (elt :: <element>)
	  add!(elements, elt)
	end method;
  do-elements-by-tag-name(add-element!, document,
			  name:      if (name      = "*") #f else name      end,
			  namespace: if (namespace = "*") #f else namespace end);
  elements
end method get-elements-by-tag-name;

// 'get-elements-by-tag-name' canonicalizes "*" in the name or namespace,
// but that's a (required) kludge which we don't do in this function...
define sealed method do-elements-by-tag-name
    (function :: <function>, node :: <node>, 
     #key name      :: false-or(<DOM-string>) = #f,
	  namespace :: false-or(<DOM-string>) = #f) => ()
  when (instance?(node, <element>)
	// Compare names with '=' because XML tag names are case-sensitive
	& (if (namespace)
	    ((~name | local-name(node) = name)
             & (~namespace | namespace-uri(node) = namespace))
	   else
	    (~name | node-name(node) = name)
	   end))
    function(node)
  end;
  for (child in child-nodes(node))
    do-elements-by-tag-name(function, child, name: name, namespace: namespace)
  end;
  #f
end method do-elements-by-tag-name;

define sealed method do-elements
    (function :: <function>, node :: <node>) => ()
  when (instance?(node, <element>))
    function(node)
  end;
  for (child in child-nodes(node))
    do-elements(function, child)
  end;
  #f
end method do-elements;


/// Document types

//
// interface DocumentType : Node {
//   readonly attribute  DOMString            name;
//   readonly attribute  NamedNodeMap         entities;
//   readonly attribute  NamedNodeMap         notations;
//   readonly attribute DOMString             publicId;
//   readonly attribute DOMString             systemId;
//   readonly attribute DOMString             internalSubset;
// };
//

define sealed class <document-type>
    (<leaf-node-mixin>, <node>)
  sealed slot entities  :: false-or(<named-node-map>) = #f;
  sealed slot notations :: false-or(<named-node-map>) = #f;
  sealed slot public-id :: false-or(<DOM-string>) = #f,
    init-keyword: public-id:;
  sealed slot system-id :: false-or(<DOM-string>) = #f,
    init-keyword: system-id:;
  sealed slot internal-subset :: false-or(<DOM-string>) = #f,
    init-keyword: internal-subset:;
  keyword type: = $document-type-node;
end class <document-type>;

define sealed method initialize
    (doctype :: <document-type>, #key)
  next-method();
  entities(doctype)  := make(<named-node-map>, node: doctype);
  notations(doctype) := make(<named-node-map>, node: doctype);
end method initialize;

define sealed method \=
    (node1 :: <document-type>, node2 :: <document-type>) => (equal? :: <boolean>)
  node1 == node2
  | (  next-method()
     & public-id(node1) = public-id(node2)
     & system-id(node1) = system-id(node2)
     & entities(node1)  = entities(node2)
     & notations(node1) = notations(node2))
end method \=;


define sealed method set-entity
    (doctype :: <document-type>, entity :: <entity>) => ()
  set-named-item(entities(doctype), entity)
end method set-entity;

define sealed method set-notation
    (doctype :: <document-type>, notation :: <notation>) => ()
  set-named-item(entities(doctype), notation)
end method set-notation;


define sealed method clone-node
    (node :: <document-type>, deep? :: <boolean>)
 => (new-node :: <document-type>)
  error(make(<invalid-state-error>,
	     format-string: "You can't clone a <document-type> node"))
end method clone-node;


define sealed inline method name
    (doctype :: <document-type>) => (name :: <DOM-string>)
  node-name(doctype)
end method name;


define sealed method note-node-added
    (document :: <document>, node :: <node>) => ()
  #f
end method note-node-added;

define sealed method note-node-removed
    (document :: <document>, node :: <node>) => ()
  #f
end method note-node-removed;


/// HTML documents

//
// interface HTMLDocument : Document {
//            attribute  DOMString            title;
//   readonly attribute  DOMString            referrer;
//   readonly attribute  DOMString            domain;
//   readonly attribute  DOMString            URL;
//            attribute  HTMLElement          body;
//   readonly attribute  HTMLCollection       images;
//   readonly attribute  HTMLCollection       applets;
//   readonly attribute  HTMLCollection       links;
//   readonly attribute  HTMLCollection       forms;
//   readonly attribute  HTMLCollection       anchors;
//            attribute  DOMString            cookie;
//   void                      open();
//   void                      close();
//   void                      write(in DOMString text);
//   void                      writeln(in DOMString text);
//   Element                   getElementById(in DOMString elementId);
//   NodeList                  getElementsByName(in DOMString elementName);
// };
//

define sealed class <html-document> (<document>)
  sealed slot referrer :: <DOM-string> = "";
  sealed slot domain   :: <DOM-string> = "";
  sealed slot url      :: <DOM-string>,
    required-init-keyword: url:;
  sealed slot cookie   :: <DOM-string> = "";
  // The next five cache the results of 'anchor', 'applet', etc.
  sealed slot %anchors :: false-or(<html-collection>) = #f;
  sealed slot %applets :: false-or(<html-collection>) = #f;
  sealed slot %forms   :: false-or(<html-collection>) = #f;
  sealed slot %images  :: false-or(<html-collection>) = #f;
  sealed slot %links   :: false-or(<html-collection>) = #f;
  sealed virtual constant slot title :: <DOM-string>;
  sealed virtual constant slot body  :: false-or(<node>);
  keyword document: = #f;
end class <html-document>;

define sealed method \=
    (doc1 :: <html-document>, doc2 :: <html-document>) => (equal? :: <boolean>)
  doc1 == doc2
  | (  next-method()
     & referrer(doc1) = referrer(doc2)
     & domain(doc1)   = domain(doc2)
     & url(doc1)      = url(doc2)
     & cookie(doc1)   = cookie(doc2))
end method \=;


define sealed inline method create-html-document
    (url :: <DOM-string>)
 => (document :: <html-document>)
  make(<html-document>,
       name: "#document",		// not #"document"!
       url:  copy-string(url));
end method create-html-document;

define constant $allow-non-html-tags :: one-of(#t, #f, #"warn") = #"warn";

define sealed inline method create-element
    (document :: <html-document>, tag-name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (element :: <html-element>)
  ignore(namespace);
  let tag-name = as-uppercase(tag-name);
  let valid?   = element($html-element-names, tag-name, default: #f);
  case
    valid? =>
      make(<html-element>,
	   document: document,
	   name: tag-name);
    $allow-non-html-tags =>
      when ($allow-non-html-tags == #"warn")
        //--- What should we use for this?
        //--- warn("The tag %s is not a valid HTML tag", tag-name)
      end;
      make(<html-element>,
	   document: document,
	   name: tag-name);
    otherwise =>
      error(make(<not-supported-error>,
	         format-string: "The tag %s is not a valid HTML tag",
	         format-arguments: vector(tag-name)))
  end
end method create-element;

define sealed inline method create-CDATA-section
    (document :: <html-document>, data :: <DOM-String>)
 => (section :: <CDATA-section>)
  error(make(<not-supported-error>,
	     format-string: "You can't create a CDATA section in an HTML document"))
end method create-CDATA-section;

define sealed inline method create-processing-instruction
    (document :: <html-document>, target :: <DOM-string>, data :: <DOM-string>)
 => (instruction :: <processing-instruction>)
  error(make(<not-supported-error>,
	     format-string: "You can't create a processing instruction in an HTML document"))
end method create-processing-instruction;

define sealed inline method create-entity-reference
    (document :: <html-document>, name :: <DOM-string>)
 => (reference :: <entity-reference>)
  error(make(<not-supported-error>,
	     format-string: "You can't create an entity reference in an HTML document"))
end method create-entity-reference;


define sealed method document-element
    (document :: <html-document>) => (elt :: false-or(<element>))
  block (return)
    local method find-html (_html :: <element>)
	    return(_html)
	  end method;
    do-elements-by-tag-name(find-html, document, name: "HTML");
    #f
  end
end method document-element;

define sealed method title
    (document :: <html-document>) => (title :: <DOM-string>)
  block (return)
    local method find-title (_title :: <element>)
	    return(text(_title) | "")
	  end method;
    do-elements-by-tag-name(find-title, document, name: "TITLE");
    ""
  end
end method title;

define sealed method body
    (document :: <html-document>) => (body :: false-or(<html-element>))
  block (return)
    local method find-body (_body :: <element>)
	    return(_body)
	  end method;
    do-elements-by-tag-name(find-body, document, name: "BODY");
    #f
  end
end method body;


//--- This is a pretty slow way to do this, maybe keep a table instead?
define sealed method get-element-by-id
    (document :: <html-document>, id :: <DOM-string>)
 => (element :: false-or(<element>))
  block (return)
    local method do-attrs (elt :: <element>)
		   do-attributes(method (attr)
				   when (id-attribute?(document, attr)
					 // '=' because XML tag names are case-sensitive
					 & string-equal?(node-value(attr), id))
				     return(elt)
                                   end
				 end method, elt)
	  end method;
    do-elements(do-attrs, document);
    #f
  end
end method get-element-by-id;

define sealed method id-attribute?
    (document :: <html-document>, attr :: <attribute>)
 => (id? :: <boolean>)
  string-equal?(node-name(attr), "ID")
end method id-attribute?;

define sealed method get-elements-by-name
    (document :: <html-document>, element-name :: <DOM-string>)
 => (elements :: <node-list>)
  //---*** Implement this
  make(<node-list>, size: 0)
end method get-elements-by-name;


define macro cached-html-elements-definer
  { define cached-html-elements ?name:name (?tag-name:expression) end }
    => { define sealed method ?name
	     (document :: <html-document>) => (?name :: <html-collection>)
	   document. "%" ## ?name
	   | begin
	       let result :: <html-collection> = make(<html-collection>, size: 0);
	       local method add-one (elt :: <element>)
		       add!(result, elt)
		     end method;
	       do-elements-by-tag-name(add-one, document, name: ?tag-name);
	       document."%" ## ?name := result;
	       result
	     end
	 end method ?name; }
end macro cached-html-elements-definer;

define cached-html-elements anchors ("A")      end;
define cached-html-elements applets ("APPLET") end;
define cached-html-elements forms   ("FORM")   end;
define cached-html-elements images  ("IMG")    end;
define cached-html-elements links   ("LINK")   end;

define sealed method note-node-added
    (document :: <html-document>, elt :: <html-element>) => ()
  select (tag-name(elt) by \=)
    "A"       => document.%anchors := #f;
    "APPLET"  => document.%applets := #f;
    "FORM"    => document.%forms   := #f;
    "IMG"     => document.%images  := #f;
    "LINK"    => document.%links   := #f;
    otherwise => #f;
  end
end method note-node-added;

define sealed method note-node-removed
    (document :: <html-document>, elt :: <html-element>) => ()
  select (tag-name(elt) by \=)
    "A"       => document.%anchors := #f;
    "APPLET"  => document.%applets := #f;
    "FORM"    => document.%forms   := #f;
    "IMG"     => document.%images  := #f;
    "LINK"    => document.%links   := #f;
    otherwise => #f;
  end
end method note-node-removed;
