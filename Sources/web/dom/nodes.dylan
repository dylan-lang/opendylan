Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Node names

define constant $interned-strings :: <string-table> = make(<string-table>);

// Strings are globally interned
define function intern-string
    (string :: <DOM-string>, #key copy? :: <boolean> = #t)
 => (interned-string :: <DOM-string>)
  let interned-string = element($interned-strings, string, default: #f);
  interned-string
  | begin
      let interned-string = if (copy?) copy-string(string) else string end;
      $interned-strings[string] := interned-string;
      interned-string
    end
end function intern-string;


// Node names are interned in the current document
define sealed class <node-name> (<object>)
  sealed slot %name      :: <DOM-string> = "";	// the fully qualified name
  sealed slot %local     :: <DOM-string> = "";	// the local name
  sealed slot %prefix    :: <DOM-string> = "";	// the prefix
  sealed slot %namespace :: <DOM-string> = "";	// the namespace URI
end class <node-name>;

define sealed method initialize
    (node-name :: <node-name>,
     #key name :: false-or(<DOM-string>) = #f, namespace :: false-or(<DOM-string>) = #f)
  //---*** Signal <invalid-character-error> if the name contains a bogus character
  next-method();
  node-name.%name      := if (name)      intern-string(name)      else "" end;
  node-name.%namespace := if (namespace) intern-string(namespace) else "" end;
  let colon = name & position(name, ':');
  if (colon)
    node-name.%prefix := intern-string(copy-string(name, end: colon), copy?: #f);
    node-name.%local  := intern-string(copy-string(name, start: colon + 1), copy?: #f);
    when (namespace
	  & node-name.%prefix = "xml"
	  & namespace ~= $xml-namespace-uri)
      error(make(<namespace-error>,
		 format-string: "You may use the 'xml' prefix only for the true XML namespace URI"))
    end
  else
    node-name.%local := node-name.%name
  end
end method initialize;

define sealed domain make (subclass(<node-name>));
define sealed domain initialize (<node-name>);

define sealed method \=
    (name1 :: <node-name>, name2 :: <node-name>) => (equal? :: <boolean>)
  name1 == name2
  | (  name1.%namespace == #f
     & name2.%namespace == #f
     & name1.%name == name2.%name)
  | (  name1.%prefix    == name2.%prefix
     & name1.%local     == name2.%local
     & name1.%namespace == name2.%namespace)
end method \=;


define variable $empty-node-name :: <node-name> = make(<node-name>);

define function make-node-name
    (name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f,
          document  :: false-or(<document>) = #f)
 => (node-name :: <node-name>)
  if (document)
    //---*** What should this table really be keyed on?
    let node-name = element(document.%node-names, name, default: #f);
    node-name
    | begin
        let node-name = make(<node-name>, name: name, namespace: namespace);
        document.%node-names[name] := node-name;
        node-name
      end
  else
    make(<node-name>, name: name, namespace: namespace)
  end
end function make-node-name;


/// Node lists

//
// interface NodeList {
//   readonly attribute  unsigned long        length;
//   Node                      item(in unsigned long index);
// };
//

// Using 'limited(<stretchy-vector>, of: <node>)' just hoses performance...
define constant <node-list> = <stretchy-object-vector>;

define sealed inline method item
    (node-list :: <node-list>, index :: <integer>)
 => (item :: false-or(<node>))
  // DOM requires that we defeat bounds-checking...
  (index < length(node-list)) & node-list[index]
end method item;

define sealed inline method length
    (node-list :: <node-list>) => (length :: <integer>)
  size(node-list)
end method length;


/// Nodes

//
// interface Node {
//   // NodeType
//   const unsigned short ELEMENT_NODE           = 1;
//   const unsigned short ATTRIBUTE_NODE         = 2;
//   const unsigned short TEXT_NODE              = 3;
//   const unsigned short CDATA_SECTION_NODE     = 4;
//   const unsigned short ENTITY_REFERENCE_NODE  = 5;
//   const unsigned short ENTITY_NODE            = 6;
//   const unsigned short PROCESSING_INSTRUCTION_NODE = 7;
//   const unsigned short COMMENT_NODE           = 8;
//   const unsigned short DOCUMENT_NODE          = 9;
//   const unsigned short DOCUMENT_TYPE_NODE     = 10;
//   const unsigned short DOCUMENT_FRAGMENT_NODE = 11;
//   const unsigned short NOTATION_NODE          = 12;
// 
//   readonly attribute  DOMString            nodeName;
//            attribute  DOMString            nodeValue;
//                                                  // raises(DOMException) on setting
//                                                  // raises(DOMException) on retrieval
//   readonly attribute  unsigned short       nodeType;
//   readonly attribute  Node                 parentNode;
//   readonly attribute  NodeList             childNodes;
//   readonly attribute  Node                 firstChild;
//   readonly attribute  Node                 lastChild;
//   readonly attribute  Node                 previousSibling;
//   readonly attribute  Node                 nextSibling;
//   readonly attribute  NamedNodeMap         attributes;
//   readonly attribute  Document             ownerDocument;
//   readonly attribute DOMString             namespaceURI;
//            attribute DOMString             prefix;
//   readonly attribute DOMString             localName;
//   readonly attribute DOMString             baseURI;
//            attribute DOMString             textContent;
//   readonly attribute DOMKey                key;
//   Node                      insertBefore(in Node newChild, 
//                                          in Node refChild)
//                                          raises(DOMException);
//   Node                      replaceChild(in Node newChild, 
//                                          in Node oldChild)
//                                          raises(DOMException);
//   Node                      removeChild(in Node oldChild)
//                                         raises(DOMException);
//   Node                      appendChild(in Node newChild)
//                                         raises(DOMException);
//   boolean                   hasChildNodes();
//   Node                      cloneNode(in boolean deep);
//   void                      normalize();
//   boolean                   isSupported(in DOMString feature, 
//                                         in DOMString version);
//   boolean                   hasAttributes();
//   enum DocumentOrder {
//     DOCUMENT_ORDER_PRECEDING,
//     DOCUMENT_ORDER_FOLLOWING,
//     DOCUMENT_ORDER_SAME,
//     DOCUMENT_ORDER_UNORDERED
//     };
//   DocumentOrder             compareDocumentOrder(in Node other)
//                                                  raises(DOMException);
//   enum TreePosition {
//     TREE_POSITION_PRECEDING,
//     TREE_POSITION_FOLLOWING,
//     TREE_POSITION_ANCESTOR,
//     TREE_POSITION_DESCENDANT,
//     TREE_POSITION_SAME,
//     TREE_POSITION_UNORDERED
//     };
//   TreePosition               compareTreePosition(in Node other)
//                                                 raises(DOMException);
//   boolean                    isSameNode(in Node other);
//   DOMString                  lookupNamespacePrefix(in DOMString namespaceURI);
//   DOMString                  lookupNamespaceURI(in DOMString prefix);
//   void                       normalizeNS();
//   boolean                    equalsNode(in Node arg, 
//                                         in boolean deep);
// };
//

define open abstract primary class <abstract-node> (<object>)
end class <abstract-node>;

define open abstract primary class <node> (<abstract-node>)
  sealed slot %name  :: <node-name> = $empty-node-name;
  sealed slot %value :: false-or(<DOM-string>) = #f,
    init-keyword: value:;
  sealed each-subclass slot node-type :: <integer> = 0,
    init-keyword: type:;
  sealed slot node-parent :: false-or(<node>) = #f;
  sealed constant slot child-nodes :: <node-list> = make(<node-list>, size: 0);
  sealed slot previous-sibling :: false-or(<node>) = #f;
  sealed slot next-sibling     :: false-or(<node>) = #f;
  sealed slot owner-document   :: false-or(<document>),
    required-init-keyword: document:;
end class <node>;

define sealed method initialize
    (node :: <node>, #key name, document :: false-or(<document>))
  next-method();
  select (name by instance?)
    <DOM-string> =>
      node.%name := make-node-name(name, document: document);
    <node-name> =>
      node.%name := name;
  end
end method initialize;

define sealed domain make (subclass(<node>));
define sealed domain initialize (<node>);

// Are the two nodes (recursively) equal, except for the owner document?
// We exclude the owner document from the equality comparison because
// 'clone-node' of a document creates a new owner document while otherwise
// preserving equality everyplace else
define sealed method \=
    (node1 :: <node>, node2 :: <node>) => (equal? :: <boolean>)
  node1 == node2
  | (  node-type(node1)  = node-type(node2)
     & node1.%name       = node2.%name
     & node1.%value      = node2.%value
     & length(child-nodes(node1)) = length(child-nodes(node2))
     & every?(\=, child-nodes(node1), child-nodes(node2)))
end method \=;


// The API function just returns the contents of the slot
// We do this so that attributes can safely use the 'node-parent'
// slot without polluting the 'parent-node' API
define sealed inline method parent-node
    (node :: <node>) => (parent :: false-or(<node>))
  node-parent(node)
end method parent-node;

define sealed method base-uri
    (node :: <node>) => (base-uri :: false-or(<DOM-string>))
  base-uri(owner-document(node))
end method base-uri;

define sealed inline method is-supported?
    (node :: <node>,
     feature :: <DOM-string>, version :: false-or(<DOM-string>))
 => (is-supported? :: <boolean>)
  has-feature?($DOM-implementation, feature, version)
end method is-supported?;


define constant $element-node           :: <integer> = 1;
define constant $attribute-node         :: <integer> = 2;
define constant $text-node              :: <integer> = 3;
define constant $cdata-section-node     :: <integer> = 4;
define constant $entity-reference-node  :: <integer> = 5;
define constant $entity-node            :: <integer> = 6;
define constant $processing-instruction-node :: <integer> = 7;
define constant $comment-node           :: <integer> = 8;
define constant $document-node          :: <integer> = 9;
define constant $document-type-node     :: <integer> = 10;
define constant $document-fragment-node :: <integer> = 11;
define constant $notation-node          :: <integer> = 12;


// These three mixins are for writing methods on...
define open abstract class <leaf-node-mixin> (<abstract-node>)
end class <leaf-node-mixin>;

define open abstract class <non-leaf-node-mixin> (<abstract-node>)
end class <non-leaf-node-mixin>;

define open abstract class <root-node-mixin> (<abstract-node>)
end class <root-node-mixin>;


// Only <element> has any attributes, so the default method returns #f
define sealed method attributes
    (node :: <node>)
 => (attributes :: false-or(<named-node-map>))
  #f
end method attributes;  

define sealed method has-attributes?
    (node :: <node>) => (has-attributes? :: <boolean>)
  #f
end method has-attributes?;


define sealed inline method node-name
    (node :: <node>) => (name :: <DOM-string>)
  let node-name :: <node-name> = node.%name;
  node-name.%name
end method node-name;

define sealed inline method node-value
    (node :: <node>) => (value :: false-or(<DOM-string>))
  node.%value
end method node-value;

define sealed inline method namespace-uri
    (node :: <node>) => (namespace :: <DOM-string>)
  let node-name :: <node-name> = node.%name;
  node-name.%namespace
end method namespace-uri;

define sealed inline method prefix
    (node :: <node>) => (namespace :: <DOM-string>)
  let node-name :: <node-name> = node.%name;
  node-name.%prefix
end method prefix;

define sealed inline method local-name
    (node :: <node>) => (namespace :: <DOM-string>)
  let node-name :: <node-name> = node.%name;
  node-name.%local
end method local-name;

define sealed method node-value-setter
    (value :: false-or(<DOM-string>), node :: <node>)
 => (value :: false-or(<DOM-string>))
  check-read-only(node);
  node.%value := value
end method node-value-setter;

define function check-read-only
    (node :: <node>) => ()
  when (node-read-only?(node))
    error(make(<no-modification-allowed-error>,
	       format-string: "The node %= is read-only",
	       format-arguments: vector(node)))
  end
end function check-read-only;

define sealed inline method node-read-only?
    (node :: <node>) => (read-only? :: <boolean>)
  //--- How are you suppose to set this attribute?
  #f
end method node-read-only?;


define sealed method first-child
    (node :: <node>) => (child :: false-or(<node>))
  let children :: <node-list> = child-nodes(node);
  ~empty?(children) & children[0]
end method first-child;

define sealed method last-child
    (node :: <node>) => (child :: false-or(<node>))
  let children :: <node-list> = child-nodes(node);
  ~empty?(children) & children[size(children) - 1]
end method last-child;

define sealed method has-child-nodes?
    (node :: <node>) => (has-children? :: <boolean>)
  let children :: <node-list> = child-nodes(node);
  ~empty?(children)
end method has-child-nodes?;


define sealed method do-nodes
    (function :: <function>, node :: <node>, 
     #key type :: false-or(<integer>) = #f) => ()
  when (~type | node-type(node) = type)
    function(node)
  end;
  for (child in child-nodes(node))
    do-nodes(function, child, type: type)
  end
end method do-nodes;


// When 'where' is #f, this inserts at the end
define sealed method insert-before
    (node :: <abstract-node>, child :: <node>, where :: false-or(<node>))
 => (child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, child);
  select (node-accepts-child?(node, child))
    #t =>
      insert-child-at!(node, child, before: where | #"end");
      note-node-added(owner-document(node), child);
      child;
    #"ignore" => 
      #f;
    otherwise =>
      error(make(<hierarchy-request-error>,
		 format-string: "You can't add the child %= to %=",
		 format-arguments: vector(child, node)));
  end
end method insert-before;

define sealed method insert-before
    (node :: <leaf-node-mixin>, child :: <node>, where :: false-or(<node>))
 => (child :: false-or(<node>))
  error(make(<hierarchy-request-error>,
	     format-string: "You can't add a child to the leaf node %=",
             format-arguments: vector(node)))
end method insert-before;

define sealed method insert-child-at!
    (node :: <abstract-node>, child :: <node>,
     #key before :: type-union(<node>, one-of(#"start", #"end")) = #"end") => ()
  let children :: <node-list> = child-nodes(node);
  when (child == before)
    error(make(<hierarchy-request-error>,
	       format-string: "You can't use the new node %= as the reference node",
	       format-arguments: vector(child)))
  end;
  when (instance?(before, <node>) & ~position(children, before))
    error(make(<not-found-error>,
	       format-string: "The reference node %= isn't a child of %=",
	       format-arguments: vector(before, node)))
  end;
  when (node-parent(child))
    remove-child(node-parent(child), child)
  end;
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
end method insert-child-at!;

define function check-owners
    (node :: <node>, child :: <node>) => ()
  when (owner-document(child) & owner-document(node) ~== owner-document(child))
    error(make(<wrong-document-error>,
	       format-string: "The parent node %= and child node %= don't come from the same document",
	       format-arguments: vector(node, child)))
  end
end function check-owners;


define sealed method append-child
    (node :: <abstract-node>, child :: <node>)
 => (child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, child);
  select (node-accepts-child?(node, child))
    #t =>
      insert-child-at!(node, child, before: #"end");
      note-node-added(owner-document(node), child);
      child;
    #"ignore" =>
      #f;
    otherwise =>
      error(make(<hierarchy-request-error>,
		 format-string: "You can't add the child %= to %=",
		 format-arguments: vector(child, node)));
  end
end method append-child;

define sealed method append-child
    (node :: <leaf-node-mixin>, child :: <node>)
 => (child :: false-or(<node>))
  error(make(<hierarchy-request-error>,
             format-string: "You can't add a child to the leaf node %=",
             format-arguments: vector(node)))
end method append-child;


define sealed method replace-child
    (node :: <abstract-node>, new-child :: <node>, old-child :: <node>)
 => (old-child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, new-child);
  select (node-accepts-child?(node, new-child, replace?: #t))
    #t =>
      let children :: <node-list> = child-nodes(node);
      when (new-child == old-child)
	error(make(<hierarchy-request-error>,
		   format-string: "You can't replace the node %= with itself",
		   format-arguments: vector(new-child)))
      end;
      when (~position(children, old-child))
	error(make(<not-found-error>,
		   format-string: "The reference node %= isn't a child of %=",
		   format-arguments: vector(old-child, node)))
      end;
      when (node-parent(new-child))
	remove-child(node-parent(new-child), new-child)
      end;
      let index :: <integer> = position(children, old-child);
      children[index] := new-child;
      let prev = previous-sibling(old-child);
      let next = next-sibling(old-child);
      previous-sibling(new-child) := prev;
      next-sibling(new-child)     := next;
      next & (previous-sibling(next) := new-child);
      prev & (next-sibling(prev)     := new-child);
      node-parent(new-child)      := node;
      previous-sibling(old-child) := #f;
      next-sibling(old-child)     := #f;
      node-parent(old-child)      := #f;
      note-node-added(owner-document(node), new-child);
      note-node-removed(owner-document(node), old-child);
      old-child;
    #"ignore" =>
      #f;
    otherwise =>
      error(make(<hierarchy-request-error>,
		 format-string: "You can't replace the child %= with %= in %=",
		 format-arguments: vector(old-child, new-child, node)));
  end
end method replace-child;

define sealed method replace-child
    (node :: <leaf-node-mixin>, new-child :: <node>, old-child :: <node>)
 => (old-child :: false-or(<node>))
  error(make(<hierarchy-request-error>,
             format-string: "You can't replace a child to the leaf node %=",
             format-arguments: vector(node)))
end method replace-child;


define sealed method remove-child
    (node :: <abstract-node>, child :: <node>)
 => (child :: false-or(<node>))
  check-read-only(node);
  check-owners(node, child);
  let children :: <node-list> = child-nodes(node);
  let index = position(children, child);
  unless (index)
    error(make(<not-found-error>,
	       format-string: "The node %= isn't a child of %=",
	       format-arguments: vector(child, node)))
  end;
  remove-at!(children, index);
  let prev = previous-sibling(child);
  let next = next-sibling(child);
  prev & (next-sibling(prev)     := next);
  next & (previous-sibling(next) := prev);
  previous-sibling(child) := #f;
  next-sibling(child)     := #f;
  node-parent(child)      := #f;
  note-node-removed(owner-document(node), child);
  child
end method remove-child;

define sealed method remove-child
    (node :: <leaf-node-mixin>, child :: <node>)
 => (child :: false-or(<node>))
  error(make(<hierarchy-request-error>,
             format-string: "You can't remove a child from the leaf node %=",
             format-arguments: vector(node)))
end method remove-child;


// The idea here is to concatenate all adjacent text nodes under 'node',
// normalizing the whitespace of the text nodes
define sealed method normalize
    (node :: <node>) => ()
  let preserve? :: <boolean> = #f;	// the state of 'preserve whitespace'
  let known?    :: <boolean> = #f;	// whether we know the state yet...
  let child = first-child(node);
  while (child)
    select (node-type(child))
      $text-node =>
	let text :: <text> = child;
	while ((child := next-sibling(text)) & node-type(child) = $text-node)
	  append-data(text, data(child));
	  remove-child(node, child)
	end;
	unless (known?)
	  preserve? := xml-preserve-space?(node);
	  known?    := #t
	end;
	unless (preserve?)
	  normalize-text(text);
	  when (size(data(text)) = 0)
	    remove-child(node, text)
	  end
	end;
      $element-node =>
	normalize(child);
	child := next-sibling(child);
      otherwise =>
	child := next-sibling(child);
    end
  end
end method normalize;


define sealed method clone-node
    (node :: <node>, deep? :: <boolean>)
 => (new-node :: <node>)
  // First copy the primary attributes
  let new-node = make(object-class(node),
		      document: owner-document(node),
		      name:  node.%name,
		      value: node.%value & copy-string(node.%value));
  // Then copy the per-subclass attributes
  do-clone-node(node, new-node);
  // Only then can we consider doing a deep-copy
  when (deep?)
    deep-copy-node(node, new-node)
  end;
  new-node
end method clone-node;

define sealed method do-clone-node
    (node :: <node>, new-node :: <node>) => ()
  #f
end method do-clone-node;

define sealed method deep-copy-node
    (node :: <node>, new-node :: <node>) => ()
  for (child in child-nodes(node))
    let new-child = clone-node(child, #f);
    owner-document(new-child) := owner-document(new-node);
    append-child(new-node, new-child);
    deep-copy-node(child, new-child)
  end
end method deep-copy-node;


define constant $document-order-preceding = #"preceding";
define constant $document-order-following = #"following";
define constant $document-order-same      = #"same";
define constant $document-order-unordered = #"unordered";

define constant <document-order> = one-of($document-order-preceding,
					  $document-order-following,
					  $document-order-same,
					  $document-order-unordered);

define sealed method compare-document-order
    (node1 :: <node>, node2 :: <node>) => (order :: <document-order>)
  //---*** Implement this
  $document-order-unordered
end method compare-document-order;


define constant $tree-position-preceding  = #"preceding";
define constant $tree-position-following  = #"following";
define constant $tree-position-ancestor   = #"ancestor";
define constant $tree-position-descendant = #"descendant";
define constant $tree-position-same       = #"same";
define constant $tree-position-unordered  = #"unordered";

define constant <tree-position> = one-of($tree-position-preceding,
					 $tree-position-following,
					 $tree-position-ancestor,
					 $tree-position-descendant,
					 $tree-position-same,
					 $tree-position-unordered);

define sealed method compare-tree-position
    (node1 :: <node>, node2 :: <node>) => (order :: <document-order>)
  //---*** Implement this
  $tree-position-unordered
end method compare-tree-position;


/// HTML collections

//
// interface HTMLCollection {
//   readonly attribute  unsigned long        length;
//   Node                      item(in unsigned long index);
//   Node                      namedItem(in DOMString name);
// };
//

define constant <html-collection> = <node-list>;

define sealed method named-item
    (collection :: <html-collection>, name :: <DOM-string>,
     #key namespace :: false-or(<DOM-string>) = #f)
 => (item :: false-or(<node>))
  let index
    =   position(collection, name, test: method (x, name)   html/id(x) = name end)
      | position(collection, name, test: method (x, name) node-name(x) = name end);
  index & collection[index]
end method named-item;
