Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Entities

//
// interface Entity : Node {
//   readonly attribute  DOMString            publicId;
//   readonly attribute  DOMString            systemId;
//   readonly attribute  DOMString            notationName;
//            attribute DOMString             actualEncoding;
//            attribute DOMString             encoding;
//            attribute DOMString             version;
// };
//

define sealed class <entity>
    (<non-leaf-node-mixin>, <node>)
  sealed slot public-id :: false-or(<DOM-string>) = #f,
    init-keyword: public-id:;
  sealed slot system-id :: false-or(<DOM-string>) = #f,
    init-keyword: system-id:;
  sealed slot notation-name :: false-or(<DOM-string>) = #f,
    init-keyword: notation-name:;
  sealed slot encoding :: false-or(<DOM-string>) = #f,
    init-keyword: encoding:;
  sealed slot actual-encoding :: false-or(<DOM-string>) = #f,
    init-keyword: actual-encoding:;
  sealed slot version :: false-or(<DOM-string>) = #f,
    init-keyword: version:;
  keyword type: = $entity-node;
end class <entity>;

define sealed method \=
    (entity1 :: <entity>, entity2 :: <entity>) => (equal? :: <boolean>)
  entity1 == entity2
  | (  next-method()
     & public-id(entity1)     = public-id(entity2)
     & system-id(entity1)     = system-id(entity2)
     & notation-name(entity1) = notation-name(entity2))
end method \=;


define sealed method do-clone-node
    (node :: <entity>, new-node :: <entity>) => ()
  // No need to copy the strings, because they're not meant to be mutable
  public-id(new-node)     := public-id(node);
  system-id(new-node)     := system-id(node);
  notation-name(new-node) := notation-name(node)
end method do-clone-node;

define constant $entity-child-types :: <simple-object-vector>
  = vector($element-node,
	   $processing-instruction-node,
	   $comment-node,
	   $text-node,
	   $CDATA-section-node,
	   $entity-reference-node);
  
define sealed method node-accepts-child?
    (node :: <entity>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  ignore(replace?);
  member?(node-type(child), $entity-child-types)
end method node-accepts-child?;


/// Entity references

//
// interface EntityReference : Node {
// };
//

define sealed class <entity-reference>
    (<non-leaf-node-mixin>, <node>)
  keyword type: = $entity-reference-node;
end class <entity-reference>;

define constant $entity-reference-child-types :: <simple-object-vector>
  = vector($element-node,
	   $processing-instruction-node,
	   $comment-node,
	   $text-node,
	   $CDATA-section-node,
	   $entity-reference-node);
  
define sealed method node-accepts-child?
    (node :: <entity-reference>, child :: <node>, #key replace? = #f)
 => (accepts-child? :: one-of(#f, #t, #"ignore"))
  ignore(replace?);
  node ~== child			// prevent recursion
  & member?(node-type(child), $entity-reference-child-types)
end method node-accepts-child?;
