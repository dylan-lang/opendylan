Module:       DOM-Tests
Synopsis:     Tests for DOM
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DOM level 1 (core) tests

/// Basic classes and functions

define test condition-classes-test ()
  check-equal("<index-size-error> has correct code",
	      exception-code(make(<index-size-error>)), $index-size-error);
  check-equal("<string-size-error> has correct code",
	      exception-code(make(<string-size-error>)), $string-size-error);
  check-equal("<hierarchy-request-error> has correct code",
	      exception-code(make(<hierarchy-request-error>)), $hierarchy-request-error);
  check-equal("<wrong-document-error> has correct code",
	      exception-code(make(<wrong-document-error>)), $wrong-document-error);
  check-equal("<invalid-character-error> has correct code",
	      exception-code(make(<invalid-character-error>)), $invalid-character-error);
  check-equal("<no-data-allowed-error> has correct code",
	      exception-code(make(<no-data-allowed-error>)), $no-data-allowed-error);
  check-equal("<no-modification-allowed-error> has correct code",
	      exception-code(make(<no-modification-allowed-error>)), $no-modification-allowed-error);
  check-equal("<not-found-error> has correct code",
	      exception-code(make(<not-found-error>)), $not-found-error);
  check-equal("<not-supported-error> has correct code",
	      exception-code(make(<not-supported-error>)), $not-supported-error);
  check-equal("<inuse-attribute-error> has correct code",
	      exception-code(make(<inuse-attribute-error>)), $inuse-attribute-error);
end test condition-classes-test;

define test implementation-class-test ()
  check-true("Only one instance of <DOM-implementation>",
	     make(<DOM-implementation>) == make(<DOM-implementation>));
  check-true("Implementation has \"xml\" and is 1.0",
	     has-feature?(make(<DOM-implementation>), "xml", "1.0"));
  check-true("Implementation has \"XML\" and is 1.0",
	     has-feature?(make(<DOM-implementation>), "XML", "1.0"));
  check-true("Implementation has \"html\" and is 1.0",
	     has-feature?(make(<DOM-implementation>), "html", "1.0"));
  check-true("Implementation has \"HTML\" and is 1.0",
	     has-feature?(make(<DOM-implementation>), "HTML", "1.0"));
end test implementation-class-test;

define function make-attr (name, value)
  make(<attribute>, name: name, value: value, document: #f)
end function make-attr;

define test plist-test ()
  // Empty plist
  let plist = make(<stretchy-vector>);
  let foo1 = make-attr("foo", "1");
  let foo2 = make-attr("foo", "2");
  check-equal("Can't find \"foo\" in an empty plist",
	      get-property(plist, "foo", #f), #f);
  check-equal("Former value for \"foo\" is #f",
	      set-property!(plist, "foo", #f, foo1), #f);
  check-equal("Value for \"foo\" is now 1",
	      get-property(plist, "foo", #f), foo1);
  check-equal("Former value for \"foo\" is 1",
	      set-property!(plist, "foo", #f, foo2), foo1);
  check-equal("Value for \"foo\" is now 2",
	      get-property(plist, "foo", #f), foo2);
  check-equal("Value for \"FOO\" is #f",
	      get-property(plist, "FOO", #f), #f);
  check-equal("Value for \"foo\" on removal is 2",
	      remove-property!(plist, "foo", #f), foo2);
  check-equal("Length of plist is 0",
	      size(plist), 0);
  // Non-empty plist
  let plist = make(<stretchy-vector>);
  let foo99 = make-attr("foo", "99");
  let bar1  = make-attr("bar", "1");
  let bar2  = make-attr("bar", "1");
  set-property!(plist, "foo", #f, foo99);
  check-equal("Can't find \"bar\" in an almost-empty plist",
	     get-property(plist, "bar", #f), #f);
  check-equal("Former value for \"bar\" is #f",
	      set-property!(plist, "bar", #f, bar1), #f);
  check-equal("Value for \"bar\" is now 1",
	      get-property(plist, "bar", #f), bar1);
  check-equal("Former value for \"bar\" is 1",
	      set-property!(plist, "bar", #f, bar2), bar1);
  check-equal("Value for \"bar\" is now 2",
	      get-property(plist, "bar", #f), bar2);
  check-equal("Value for \"BAR\" is #f",
	      get-property(plist, "BAR", #f), #f);
  check-equal("Value for \"bar\" on removal is 2",
	      remove-property!(plist, "bar", #f), bar2);
  check-equal("Length of plist is 1",
	      size(plist), 1);
end test plist-test;

define test namespace-plist-test ()
  // Non-empty plist
  let plist = make(<stretchy-vector>);
  let foo99 = make-attr("mine:foo", "99");
  let foo98 = make-attr("mine:foo", "98");
  let foo66 = make-attr("yours:foo", "66");
  set-property!(plist, "foo", "mine", foo99);
  set-property!(plist, "foo", "yours", foo66);
  check-equal("Can't find \"bar\" in an almost-empty namespace plist",
	     get-property(plist, "bar", #f), #f);
  check-equal("Value for \"foo\" in namespace plist is 99",
	      get-property(plist, "foo", #f), foo99);
  check-equal("Can't find \"xsl:foo\" in namespace plist",
	      get-property(plist, "foo", "xsl"), #f);
  check-equal("Value for \"mine:foo\" is 99",
	      get-property(plist, "foo", "mine"), foo99);
  check-equal("Value for \"yours:foo\" is 66",
	      get-property(plist, "foo", "yours"), foo66);
  check-equal("Former value for \"mine:foo\" is 99",
	      set-property!(plist, "foo", "mine", foo98), foo99);
  check-equal("Value for \"mine:foo\" is now 98",
	      get-property(plist, "foo", "mine"), foo98);
  check-equal("Value for \"yours:foo\" on removal is 66",
	      remove-property!(plist, "foo", "yours"), foo66);
  check-equal("Value for \"mine:foo\" is still 98",
	      get-property(plist, "foo", "mine"), foo98);
  check-equal("Length of namespace plist is 1",
	      size(plist), 1);
end test namespace-plist-test;

define suite classes-suite ()
  test condition-classes-test;
  test implementation-class-test;
  test plist-test;
  test namespace-plist-test;
end suite classes-suite;


/// DOM object creation

define test object-creation-test ()
  // Document creation
  let document = create-xml-document();
  check-equal("Document node type is $document-node",
	      node-type(document), $document-node);
  check-true("Document type is #f",
	     doctype(document) == #f);
  check-true("Document implementation is right",
	     implementation(document) == make(<DOM-implementation>));
  check-equal("Document's node name is \"#document\"",
	     node-name(document), "#document");
  check-equal("Document's node value is #f",
	      node-value(document), #f);
  // Element creation
  let elt = create-element(document, "test");
  check-equal("Element node type is $element-node",
	      node-type(elt), $element-node);
  check-true("Element's document is correct",
	     owner-document(elt) == document);
  check-equal("Element's node name is \"test\"",
	      node-name(elt), "test");
  check-equal("Element's node value is #f",
	      node-value(elt), #f);
  // Fragment creation
  let fragment = create-document-fragment(document);
  check-equal("Fragment node type is $document-fragment-node",
	      node-type(fragment), $document-fragment-node);
  check-true("Fragment's document is correct",
	     owner-document(fragment) == document);
  check-equal("Fragment's node name is \"#document-fragment\"",
	      node-name(fragment), "#document-fragment");
  check-equal("Fragment's node value is #f",
	      node-value(fragment), #f);
  // Text creation
  let text = create-text-node(document, "testing 1 2 3");
  check-equal("Text node type is $text-node",
	      node-type(text), $text-node);
  check-true("Text's document is correct",
	     owner-document(text) == document);
  check-equal("Text's node name is \"#text\"",
	      node-name(text), "#text");
  check-equal("Text's node value is correct",
	      node-value(text), "testing 1 2 3");
  // Attribute creation
  let attribute = create-attribute(document, "Base");
  check-equal("Attribute node type is $attribute-node",
	      node-type(attribute), $attribute-node);
  check-true("Attribute's document is correct",
	     owner-document(attribute) == document);
  check-equal("Attribute's node name is \"Base\"",
	      node-name(attribute), "Base");
  check-equal("Attribute's node initial value is correct",
	      node-value(attribute), #f);
end test object-creation-test;

define suite object-creation-suite ()
  test object-creation-test;
end suite object-creation-suite;


/// Tree surgery

define test node-insertion-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let bold     = create-element(document, "bold");
  let italic   = create-element(document, "italic");
  let roman    = create-element(document, "roman");
  let tt       = create-element(document, "tt");
  let nope     = create-element(document, "nope");
  check-false("Document has no children",
	      has-child-nodes?(document));
  check-false("Body has no children",
	      has-child-nodes?(body));
  // Start by appending nodes
  append-child(document, body);
  append-child(body,     bold);
  append-child(body,     italic);
  check-true("Document now has children",
	      has-child-nodes?(document));
  check-true("Body now has children",
	      has-child-nodes?(body));
  check-equal("Document has one child",
	      length(child-nodes(document)), 1);
  check-true("Document has correct child",
	     child-nodes(document)[0] == body);
  check-true("Body element has correct parent",
	     parent-node(body) == document);
  check-equal("Body has two children",
	      length(child-nodes(body)), 2);
  check-true("Body has correct children",
	     child-nodes(body)[0] == bold
	       & child-nodes(body)[1] == italic);
  check-true("Bold element has correct parent",
	     parent-node(bold) == body);
  check-true("Italic element has correct parent",
	     parent-node(italic) == body);
  check-true("First child of body is bold element",
	     first-child(body) == bold);
  check-true("Last child of body is italic element",
	     last-child(body) == italic);
  check-true("Previous sibling of bold is #f",
	     previous-sibling(bold) == #f);
  check-true("Previous sibling of italic is bold",
	     previous-sibling(italic) == bold);
  check-true("Next sibling of bold is italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is #f",
	     next-sibling(italic) == #f);
  // Then insert some in the middle
  insert-before(body, roman, bold);
  insert-before(body, tt, #f);
  check-equal("Body now has four children",
	      length(child-nodes(body)), 4);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == roman
	       & child-nodes(body)[1] == bold
	       & child-nodes(body)[2] == italic
	       & child-nodes(body)[3] == tt);
  check-true("Roman element has correct parent",
	     parent-node(roman) == body);
  check-true("TT element has correct parent",
	     parent-node(tt) == body);
  check-true("First child of body is now roman element",
	     first-child(body) == roman);
  check-true("Last child of body is now TT element",
	     last-child(body) == tt);
  check-true("Previous sibling of roman is #f",
	     previous-sibling(roman) == #f);
  check-true("Previous sibling of bold is roman",
	     previous-sibling(bold) == roman);
  check-true("Previous sibling of italic is bold",
	     previous-sibling(italic) == bold);
  check-true("Previous sibling of TT is italic",
	     previous-sibling(tt) == italic);
  check-true("Next sibling of roman is bold",
	     next-sibling(roman) == bold);
  check-true("Next sibling of bold is italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is TT",
	     next-sibling(italic) == tt);
  check-true("Next sibling of TT is #f",
	     next-sibling(tt) == #f);
  check-condition("Document can only have one element", 
		  <hierarchy-request-error>, 
		  append-child(document, nope));
end test node-insertion-test;

define test node-deletion-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let bold     = create-element(document, "bold");
  let italic   = create-element(document, "italic");
  let roman    = create-element(document, "roman");
  let tt       = create-element(document, "tt");
  append-child(document, body);
  append-child(body,     bold);
  append-child(body,     italic);
  insert-before(body, roman, bold);
  insert-before(body, tt, #f);
  remove-child(body, bold);
  remove-child(body, italic);
  check-equal("Body now has two children",
	      length(child-nodes(body)), 2);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == roman
	       & child-nodes(body)[1] == tt);
  check-true("Roman element has correct parent",
	     parent-node(roman) == body);
  check-true("TT element has correct parent",
	     parent-node(tt) == body);
  check-true("First child of body is now roman element",
	     first-child(body) == roman);
  check-true("Last child of body is now TT element",
	     last-child(body) == tt);
  check-true("Previous sibling of roman is #f",
	     previous-sibling(roman) == #f);
  check-true("Previous sibling of TT is roman",
	     previous-sibling(tt) == roman);
  check-true("Next sibling of roman is TT",
	     next-sibling(roman) == tt);
  check-true("Next sibling of TT is #f",
	     next-sibling(tt) == #f);
end test node-deletion-test;

define test node-replace-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let bold     = create-element(document, "bold");
  let italic   = create-element(document, "italic");
  let roman    = create-element(document, "roman");
  let tt       = create-element(document, "tt");
  // Replace with explicit calls to 'replace-child'
  append-child(document, body);
  append-child(body,     bold);
  append-child(body,     italic);
  replace-child(body, roman,  bold);
  replace-child(body, tt,     italic);
  check-equal("Body now has two children",
	      length(child-nodes(body)), 2);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == roman
	       & child-nodes(body)[1] == tt);
  check-true("Roman element has correct parent",
	     parent-node(roman) == body);
  check-true("TT element has correct parent",
	     parent-node(tt) == body);
  check-true("First child of body is now roman element",
	     first-child(body) == roman);
  check-true("Last child of body is now TT element",
	     last-child(body) == tt);
  check-true("Previous sibling of roman is #f",
	     previous-sibling(roman) == #f);
  check-true("Previous sibling of TT is roman",
	     previous-sibling(tt) == roman);
  check-true("Next sibling of roman is TT",
	     next-sibling(roman) == tt);
  check-true("Next sibling of TT is #f",
	     next-sibling(tt) == #f);
  // Replace by calling 'append' on an element that's already there...
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let bold     = create-element(document, "bold");
  let italic   = create-element(document, "italic");
  append-child(document, body);
  append-child(body,     italic);		// start with 'italic' at the start
  append-child(body,     bold);
  append-child(body,     italic);		// this should move 'italic' to the end
  check-equal("Body has two children",
	      length(child-nodes(body)), 2);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == bold
	       & child-nodes(body)[1] == italic);
  check-true("Bold element has correct parent",
	     parent-node(bold) == body);
  check-true("Italic element has correct parent",
	     parent-node(italic) == body);
  check-true("First child of body is now bold element",
	     first-child(body) == bold);
  check-true("Last child of body is now italic element",
	     last-child(body) == italic);
  check-true("Previous sibling of bold is now #f",
	     previous-sibling(bold) == #f);
  check-true("Previous sibling of italic is now bold",
	     previous-sibling(italic) == bold);
  check-true("Next sibling of bold is now italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is now #f",
	     next-sibling(italic) == #f);
end test node-replace-test;

define test node-conditions-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let leaf     = create-text-node(document, "some leaf text");
  let elt      = create-element(document, "item");
  append-child(document, body);
  append-child(body,     leaf);
  check-condition("Can't insert into a leaf",
		  <hierarchy-request-error>, 
		  insert-before(leaf, elt, #f));
  check-condition("Can't append to a leaf",
		  <hierarchy-request-error>, 
		  append-child(leaf, elt));
  check-condition("Can't replace in a leaf",
		  <hierarchy-request-error>, 
		  replace-child(leaf, elt, elt));
  check-condition("Can't remove from a leaf",
		  <hierarchy-request-error>, 
		  remove-child(leaf, elt));
  //---*** Need some tests that fail 'node-accepts-child?'
  //---*** Need some tests that fail 'check-owners'
end test node-conditions-test;

define test fragments-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let bold     = create-element(document, "bold");
  let italic   = create-element(document, "italic");
  let roman    = create-element(document, "roman");
  let tt       = create-element(document, "tt");
  let more     = create-element(document, "more");
  let less     = create-element(document, "less");
  let frag1    = create-document-fragment(document);
  let frag2    = create-document-fragment(document);
  let frag3    = create-document-fragment(document);
  append-child(document, body);
  append-child(frag1, bold);
  append-child(frag1, italic);
  append-child(frag2, roman);
  append-child(frag2, tt);
  append-child(frag3, more);
  append-child(frag3, less);
  append-child(body, frag1);
  check-equal("Body has two children",
	      length(child-nodes(body)), 2);
  check-true("Body has correct children",
	     child-nodes(body)[0] == bold
	       & child-nodes(body)[1] == italic);
  check-true("Bold element has correct parent",
	     parent-node(bold) == body);
  check-true("Italic element has correct parent",
	     parent-node(italic) == body);
  check-true("First child of body is bold element",
	     first-child(body) == bold);
  check-true("Last child of body is italic element",
	     last-child(body) == italic);
  check-true("Previous sibling of bold is #f",
	     previous-sibling(bold) == #f);
  check-true("Previous sibling of italic is bold",
	     previous-sibling(italic) == bold);
  check-true("Next sibling of bold is italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is #f",
	     next-sibling(italic) == #f);
  // Insert a fragment
  insert-before(body, frag2, bold);
  check-equal("Body now has four children",
	      length(child-nodes(body)), 4);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == roman
	       & child-nodes(body)[1] == tt
	       & child-nodes(body)[2] == bold
	       & child-nodes(body)[3] == italic);
  check-true("Roman element has correct parent",
	     parent-node(roman) == body);
  check-true("TT element has correct parent",
	     parent-node(tt) == body);
  check-true("First child of body is now roman element",
	     first-child(body) == roman);
  check-true("Last child of body is still italic element",
	     last-child(body) == italic);
  check-true("Previous sibling of roman is #f",
	     previous-sibling(roman) == #f);
  check-true("Previous sibling of TT is roman",
	     previous-sibling(tt) == roman);
  check-true("Previous sibling of bold is TT",
	     previous-sibling(bold) == tt);
  check-true("Previous sibling of italic is bold",
	     previous-sibling(italic) == bold);
  check-true("Next sibling of roman is TT",
	     next-sibling(roman) == tt);
  check-true("Next sibling of TT is bold",
	     next-sibling(tt) == bold);
  check-true("Next sibling of bold is italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is #f",
	     next-sibling(italic) == #f);
  // Replace with fragment
  replace-child(body, frag3,  tt);
  check-equal("Body now has five children",
	      length(child-nodes(body)), 5);
  check-true("Body still has correct children",
	     child-nodes(body)[0] == roman
	       & child-nodes(body)[1] == more
	       & child-nodes(body)[2] == less
	       & child-nodes(body)[3] == bold
	       & child-nodes(body)[4] == italic);
  check-true("More element has correct parent",
	     parent-node(more) == body);
  check-true("Less element has correct parent",
	     parent-node(less) == body);
  check-true("First child of body is still roman element",
	     first-child(body) == roman);
  check-true("Last child of body is still italic element",
	     last-child(body) == italic);
  check-true("Previous sibling of roman is #f",
	     previous-sibling(roman) == #f);
  check-true("Previous sibling of more is roman",
	     previous-sibling(more) == roman);
  check-true("Previous sibling of less is more",
	     previous-sibling(less) == more);
  check-true("Previous sibling of bold is less",
	     previous-sibling(bold) == less);
  check-true("Previous sibling of italic is bold",
	     previous-sibling(italic) == bold);
  check-true("Next sibling of roman is more",
	     next-sibling(roman) == more);
  check-true("Next sibling of more is less",
	     next-sibling(more) == less);
  check-true("Next sibling of less is bold",
	     next-sibling(less) == bold);
  check-true("Next sibling of bold is italic",
	     next-sibling(bold) == italic);
  check-true("Next sibling of italic is #f",
	     next-sibling(italic) == #f);
end test fragments-test;

define test node-copying-test ()
  // Test shallow copying
  let document = create-xml-document();
  check-true("Shallow-copied documents are 'nodes-equal?'",
	     nodes-equal?(document, clone-node(document, #f)));
  check-true("Shallow-copied documents are '\\='",
	     document = clone-node(document, #f));
  check-true("Shallow-copied documents don't have identical owner documents",
	     owner-document(document) ~== owner-document(clone-node(document, #f)));
  let document = create-xml-document();
  let body     = create-element(document, "body");
  append-child(document, body);
  check-true("Shallow-copied elements with no attributes are 'nodes-equal?'",
	     nodes-equal?(body, clone-node(body, #f)));
  check-true("Shallow-copied elements with no attributes are '\\='",
	     body = clone-node(body, #f));
  check-true("Shallow-copied elements have identical owner documents",
	     owner-document(body) == owner-document(clone-node(body, #f)));
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let text     = create-text-node(document, "This is some text");
  let node-map = make(<named-node-map>, node: body);
  let attr1    = create-attribute(document, "Base");
  let attr2    = create-attribute(document, "Package");
  append-child(document, body);
  append-child(body, text);
  set-named-item(node-map, attr1);
  set-named-item(node-map, attr2);
  value(attr1) := "10";
  value(attr2) := "Common-Lisp";
  check-true("Shallow-copied elements with attributes and a child are 'nodes-equal?'",
	     nodes-equal?(body, clone-node(body, #f)));
  check-false("Shallow-copied elements with attributes and a child are not '\\='",
	      body = clone-node(body, #f));
  check-true("Shallow-copied text nodes are 'nodes-equal?'",
	     nodes-equal?(text, clone-node(text, #f)));
  check-true("Shallow-copied text nodes are '\\='",
	     text = clone-node(text, #f));
  // Test deep copying
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let text     = create-text-node(document, "This is some text");
  let node-map = make(<named-node-map>, node: body);
  let attr1    = create-attribute(document, "Base");
  let attr2    = create-attribute(document, "Package");
  append-child(document, body);
  append-child(body, text);
  set-named-item(node-map, attr1);
  set-named-item(node-map, attr2);
  value(attr1) := "10";
  value(attr2) := "Common-Lisp";
  // Start by deep-copying the body
  let _body = clone-node(body, #t);
  check-true("Deep-copied elements with attributes are 'nodes-equal?'",
	     nodes-equal?(body, _body));
  check-true("Deep-copied elements have identical owner documents",
	     owner-document(body) == owner-document(_body));
  check-true("Deep-copied element children are 'nodes-equal?'",
	     length(child-nodes(body)) = length(child-nodes(_body))
	       & item(child-nodes(body), 0) ~== item(child-nodes(_body), 0)
	       & nodes-equal?(item(child-nodes(body), 0), item(child-nodes(_body), 0)));
  check-true("Deep-copied elements with attributes are '\\='",
	     body = _body);
  // Now try deep-copying the whole document
  let _document = clone-node(document, #t);
  let _body     = item(child-nodes(_document), 0);
  check-true("Deep-copied documents are 'nodes-equal?'",
	     nodes-equal?(document, _document));
  check-true("Deep-copied document elements with attributes are 'nodes-equal?'",
	     nodes-equal?(body, _body));
  check-true("Deep-copied document elements have non-identical owner documents",
	     owner-document(body) ~== owner-document(_body));
  check-true("Deep-copied element children are 'nodes-equal?'",
	     length(child-nodes(body)) = length(child-nodes(_body))
	       & item(child-nodes(body), 0) ~== item(child-nodes(_body), 0)
	       & nodes-equal?(item(child-nodes(body), 0), item(child-nodes(_body), 0)));
  check-true("Deep-copied documents are '\\='",
	     document = _document);
end test node-copying-test;

define method nodes-equal?
    (node1 :: <node>, node2 :: <node>) => (equal? :: <boolean>)
    node-name(node1)  = node-name(node2)
  & node-value(node1) = node-value(node2)
end method nodes-equal?;

define method nodes-equal?
    (doc1 :: <document>, doc2 :: <document>) => (equal? :: <boolean>)
    next-method()
  & doctype(doc1) = doctype(doc2)
  & implementation(doc1) = implementation(doc2)
end method nodes-equal?;

define method nodes-equal?
    (elt1 :: <element>, elt2 :: <element>) => (equal? :: <boolean>)
    next-method()
  & block (fail)
      let attrs1 = attributes(elt1);
      let attrs2 = attributes(elt2);
      when (length(attrs1) ~= length(attrs2)) fail() end;
      for (i :: <integer> from 0 below length(attrs1))
	let attr1 = item(attrs1, i);
	let attr2 = item(attrs2, i);
	when (name(attr1) ~= name(attr2) | value(attr1) ~= value(attr2))
	  fail()
	end
      end;
      #t
    end;
end method nodes-equal?;

define method nodes-equal?
    (cdata1 :: <character-data>, cdata2 :: <character-data>) => (equal? :: <boolean>)
    next-method()
  & data(cdata1) = data(cdata2)
end method nodes-equal?;

define suite tree-surgery-suite ()
  test node-insertion-test;
  test node-deletion-test;
  test node-replace-test;
  test node-conditions-test;
  test fragments-test;
  test node-copying-test;
end suite tree-surgery-suite;


/// Elements and attributes

define test named-node-map-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let node-map = make(<named-node-map>, node: body);
  let attr1    = create-attribute(document, "Base");
  let attr2    = create-attribute(document, "Package");
  append-child(document, body);
  check-false("No item named 'Base' yet",
	      get-named-item(node-map, "Base"));
  check-false("No previous item named 'Base'",
	      set-named-item(node-map, attr1));
  check-true("Found 'Base'",
	     get-named-item(node-map, "Base") == attr1);
  check-true("Removed 'Base'",
	     remove-named-item(node-map, "Base") == attr1);
  check-false("No previous item named 'Base'",
	      set-named-item(node-map, attr1));
  check-false("No previous item named 'Package'",
	      set-named-item(node-map, attr2));
  check-true("Found 'Base'",
	     get-named-item(node-map, "Base") == attr1);
  check-true("Found 'Package'",
	     get-named-item(node-map, "Package") == attr2);
  //---*** Need some tests for <inuse-attribute-error>
end test named-node-map-test;

define test attributes-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let node-map = make(<named-node-map>, node: body);
  let attr1    = create-attribute(document, "Base");
  let attr2    = create-attribute(document, "Package");
  append-child(document, body);
  set-named-item(node-map, attr1);
  set-named-item(node-map, attr2);
  check-true("'Base' attribute starts as empty",
	     value(attr1) = "");
  check-true("'Package' attribute starts as empty",
	     value(attr2) = "");
  check-true("'Base' attribute starts as unspecified",
	     ~specified?(attr1));
  check-true("'Package' attribute starts as unspecified",
	     ~specified?(attr2));
  value(attr1) := "10";
  value(attr2) := "Common-Lisp";
  check-true("'Base' attribute now \"10\"",
	     value(attr1) = "10");
  check-true("'Package' attribute now \"Common-Lisp\"",
	     value(attr2) = "Common-Lisp");
  check-true("'Base' attribute now specified",
	     specified?(attr1));
  check-true("'Package' attribute now specified",
	     specified?(attr2));
end test attributes-test;

define test elements-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  append-child(document, body);
  check-true("'Base' attribute starts as empty",
	     get-attribute(body, "Base") = "");
  check-true("'Package' attribute starts as empty",
	     get-attribute(body, "Package") = "");
  set-attribute(body, "Base", "10");
  set-attribute(body, "Package", "Common-Lisp");
  check-true("'Base' attribute now \"10\"",
	     get-attribute(body, "Base") = "10");
  check-true("'Package' attribute now \"Common-Lisp\"",
	     get-attribute(body, "Package") = "Common-Lisp");
  //---*** Need some more tests for attributes
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let p1       = create-element(document, "paragraph");
  let p2       = create-element(document, "paragraph");
  let p1s1     = create-element(document, "sentence");
  let p1s2     = create-element(document, "sentence");
  let p2s1     = create-element(document, "sentence");
  let p2s2     = create-element(document, "sentence");
  append-child(document, body);
  append-child(body,     p1);
  append-child(body,     p2);
  append-child(p1,       p1s1);
  append-child(p1,       p1s2);
  append-child(p2,       p2s1);
  append-child(p2,       p2s2);
  let body-elts = get-elements-by-tag-name(document, "body");
  let para-elts = get-elements-by-tag-name(document, "paragraph");
  let sent-elts = get-elements-by-tag-name(document, "sentence");
  let all-elts  = get-elements-by-tag-name(document, "*");
  check-true("Found the body elements",
	     size(body-elts) = 1
	       & body-elts[0] == body);
  check-true("Found the paragraph elements",
	     size(para-elts) = 2
	       & para-elts[0] == p1
	       & para-elts[1] == p2);
  check-true("Found the sentence elements",
	     size(sent-elts) = 4
	       & sent-elts[0] == p1s1
	       & sent-elts[1] == p1s2
	       & sent-elts[2] == p2s1
	       & sent-elts[3] == p2s2);
  check-true("Found all the elements",
	     size(all-elts) = 7
	       & all-elts[0] == body
	       & all-elts[1] == p1
	       & all-elts[2] == p1s1
	       & all-elts[3] == p1s2
	       & all-elts[4] == p2
	       & all-elts[5] == p2s1
	       & all-elts[6] == p2s2);
  // Add text to the sentences and see what happens
  let t1 = create-text-node(document, "Knock knock.");
  let t2 = create-text-node(document, "Who's there?");
  let t3 = create-text-node(document, "Interrupting cow.");
  let t4 = create-text-node(document, "Moo!");
  append-child(p1s1, t1);
  append-child(p1s2, t2);
  append-child(p2s1, t3);
  append-child(p2s2, t4);
  check-true("'text' for p1s1 is the same as the 'data' for t1",
	     text(p1s1) = data(t1));
  check-true("'text' for p1s2 is the same as the 'data' for t2",
	     text(p1s2) = data(t2));
  check-true("'text' for p2s1 is the same as the 'data' for t3",
	     text(p2s1) = data(t3));
  check-true("'text' for p2s2 is the same as the 'data' for t4",
	     text(p2s2) = data(t4));
end test elements-test;

define test collections-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let p1       = create-element(document, "paragraph");
  let p2       = create-element(document, "paragraph");
  let p1s1     = create-element(document, "sentence");
  let p1s2     = create-element(document, "sentence");
  let p2s1     = create-element(document, "sentence");
  let p2s2     = create-element(document, "sentence");
  append-child(document, body);
  append-child(body,     p1);
  append-child(body,     p2);
  append-child(p1,       p1s1);
  append-child(p1,       p1s2);
  append-child(p2,       p2s1);
  append-child(p2,       p2s2);
  // Checks that use 'element' with an integer index
  check-true("0'th element of the body is paragraph 1",
	     body[0] == p1);
  check-true("1'st element of the body is paragraph 2",
	     body[1] == p2);
  check-true("0'th element of paragraph 1 is sentence 1",
	     p1[0] == p1s1);
  check-true("1'st element of paragraph 1 is sentence 2",
	     p1[1] == p1s2);
  check-true("0'th element of paragraph 2 is sentence 1",
	     p2[0] == p2s1);
  check-true("1'st element of paragraph 2 is sentence 2",
	     p2[1] == p2s2);
  // Checks that use 'element' with a tag name
  check-true("\"paragraph\" element of body is paragraph 1",
	     body["paragraph"] == p1);
  check-true("\"sentence\" element of paragraph 1 is its sentence 1",
	     p1["sentence"] == p1s1);
  check-true("\"sentence\" element of paragraph 2 is its sentence 1",
	     p2["sentence"] == p2s1);
  // Checks that use 'aref' with a tag name and an index
  check-true("body[\"paragraph\", 0] is paragraph 1",
	     body["paragraph", 0] == p1);
  check-true("body[\"paragraph\", 1] is paragraph 2",
	     body["paragraph", 1] == p2);
  check-true("p1[\"sentence\", 0] is sentence 1",
	     p1["sentence", 0] == p1s1);
  check-true("p1[\"sentence\", 1] is sentence 2",
	     p1["sentence", 1] == p1s2);
  check-true("p2[\"sentence\", 0] is sentence 1",
	     p2["sentence", 0] == p2s1);
  check-true("p2[\"sentence\", 1] is sentence 2",
	     p2["sentence", 1] == p2s2);
  // Check the document trampolines for the above
  check-true("0'th element of the document is paragraph 1 of the body",
	     document[0] == p1);
  check-true("1'st element of the document is paragraph 2 of the body",
	     document[1] == p2);
  check-true("\"paragraph\" element of the document is paragraph 1",
	     document["paragraph"] == p1);
  check-true("doc[\"paragraph\", 0] is paragraph 1",
	     document["paragraph", 0] == p1);
  check-true("doc[\"paragraph\", 1] is paragraph 2",
	     document["paragraph", 1] == p2);
end test collections-test;

define suite elements-suite ()
  test named-node-map-test;
  test attributes-test;
  test elements-test;
  test collections-test;
end suite elements-suite;


/// Character data nodes

define test character-data-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let text     = create-text-node(document, "initial text");
  append-child(document, body);
  append-child(body,     text);
  check-true("Initial text OK",
	     data(text) = "initial text");
  data(text) := "0123456789";
  check-true("New text OK",
	     data(text) = "0123456789");
  insert-data(text, 0, "aaa");
  insert-data(text, 5 + 3, "bbb");
  append-data(text, "ccc");
  check-true("Text OK after insertions",
	     data(text) = "aaa01234bbb56789ccc");
  replace-data(text, 0, 3, "A");
  replace-data(text, 5 + 1, 3, "B");
  replace-data(text, 10 + 1 + 1, 3, "C");
  check-true("Text OK after replacements",
	     data(text) = "A01234B56789C");
  delete-data(text, 0, 1);
  delete-data(text, 5, 1);
  delete-data(text, 10, 1);
  check-true("Text OK after deletions",
	     data(text) = "0123456789");
  check-true("Substring OK",
	     substring-data(text, 2, 4) = "2345");
  //---*** Need some tests that generate errors in the above functions
  // Test text node splitting
  let old-text = text;
  let new-text = split-text(text, 5);
  check-true("Old node text OK",
	     data(old-text) = "01234");
  check-true("Old node text OK",
	     data(new-text) = "56789");
  check-true("First child of body is old text",
	     first-child(body) == old-text);
  check-true("Last child of body is new text",
	     last-child(body) == new-text);
  check-true("Previous sibling of new text is old text",
	     previous-sibling(new-text) == old-text);
  check-true("Next sibling of old text is new text",
	     next-sibling(old-text) == new-text);
end test character-data-test;

define test normalize-test ()
  let document = create-xml-document();
  let body     = create-element(document, "body");
  let para     = create-element(document, "paragraph");
  let s1       = create-element(document, "sentence");
  let s2       = create-element(document, "sentence");
  let s3       = create-element(document, "sentence");
  let s4       = create-element(document, "sentence");
  let s1t1     = create-text-node(document, "This  ");	// two spaces
  let s1t2     = create-text-node(document, "is  ");	// two spaces
  let s1t3     = create-text-node(document, "a ");
  let s1t4     = create-text-node(document, "test.");
  let s2t1     = create-text-node(document, "Ceci  ");	// two spaces
  let s2t2     = create-text-node(document, "n'est");
  let s2cd     = create-CDATA-section(document, "-ce ");
  let s2t3     = create-text-node(document, "pas  ");	// two spaces
  let s2t4     = create-text-node(document, "un test.");
  let s3t1     = create-text-node(document, "  one ");
  let s3t2     = create-text-node(document, " two  ");
  let s4t1     = create-text-node(document, " ");
  let s4t2     = create-text-node(document, " ");
  append-child(document, body);
  append-child(body,     para);
  append-child(para,     s1);
  append-child(para,     s2);
  append-child(para,     s3);
  append-child(para,     s4);
  append-child(s1,       s1t1);
  append-child(s1,       s1t2);
  append-child(s1,       s1t3);
  append-child(s1,       s1t4);
  append-child(s2,       s2t1);
  append-child(s2,       s2t2);
  append-child(s2,       s2cd);
  append-child(s2,       s2t3);
  append-child(s2,       s2t4);
  append-child(s3,       s3t1);
  append-child(s3,       s3t2);
  append-child(s4,       s4t1);
  append-child(s4,       s4t2);
  check-true("First sentence initially OK",
	     text(s1) = "This  is  a test.");		// watch the double spaces!
  check-true("Second sentence initially OK",
	     text(s2) = "Ceci  n'est-ce pas  un test.");// watch the double spaces!
  check-true("Third sentence initially OK",
	     text(s3) = "  one  two  ");		// watch the double spaces!
  check-true("Fourth sentence initially OK",
	     text(s4) = "  ");				// watch the double spaces!
  normalize(s1);
  normalize(s2);
  normalize(s3);
  normalize(s4);
  check-true("First sentence normalized",
	     text(s1) = "This is a test.");		// double spaces now gone
  check-true("Second sentence normalized",
	     text(s2) = "Ceci n'est-ce pas un test.");	// double spaces now gone
  check-true("Third sentence normalized",
	     text(s3) = "one two");			// double spaces now gone
  check-true("Fourth sentence now empty",
	     text(s4) = #f);				// all gone
  check-equal("First sentence now has one child",
	      length(child-nodes(s1)), 1);
  check-true("First sentence's sole child is now the original first text node",
	     child-nodes(s1)[0] == s1t1);
  check-equal("Second sentence now has three children",
	      length(child-nodes(s2)), 3);
  check-true("Second sentence's first child is now the original first text node",
	     child-nodes(s2)[0] == s2t1);
  check-true("Second sentence's second child is now the original cdata node",
	     child-nodes(s2)[1] == s2cd);
  check-true("Second sentence's third child is now the original third text node",
	     child-nodes(s2)[2] == s2t3);
  check-equal("Third sentence now has one child",
	      length(child-nodes(s3)), 1);
  check-true("Third sentence's sole child is now the original first text node",
	     child-nodes(s3)[0] == s3t1);
  check-equal("Fourth sentence now has no children",
	      length(child-nodes(s4)), 0);
end test normalize-test;

define suite character-data-suite ()
  test character-data-test;
  test normalize-test;
end suite character-data-suite;


/// The DOM test suite...

define suite dom-test-suite ()
  suite classes-suite;
  suite object-creation-suite;
  suite tree-surgery-suite;
  suite elements-suite;
  suite character-data-suite;
end suite dom-test-suite;

define function do-dom-tests (#key debug? = #f)
  perform-suite(dom-test-suite, debug?: debug?)
end function do-dom-tests;

do-dom-tests();
