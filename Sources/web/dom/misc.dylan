Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Notation

//
// interface Notation : Node {
//   readonly attribute  DOMString            publicId;
//   readonly attribute  DOMString            systemId;
// };
//

define sealed class <notation>
    (<leaf-node-mixin>, <node>)
  sealed slot public-id :: false-or(<DOM-string>) = #f,
    init-keyword: public-id:;
  sealed slot system-id :: false-or(<DOM-string>) = #f,
    init-keyword: system-id:;
  keyword type: = $notation-node;
end class <notation>;

define sealed method \=
    (notation1 :: <notation>, notation2 :: <notation>) => (equal? :: <boolean>)
  notation1 == notation2
  | (  next-method()
     & public-id(notation1) = public-id(notation2)
     & system-id(notation1) = system-id(notation2))
end method \=;

define sealed method do-clone-node
    (node :: <notation>, new-node :: <notation>) => ()
  // No need to copy the strings, because they're not meant to be mutable
  public-id(new-node) := public-id(node);
  system-id(new-node) := system-id(node)
end method do-clone-node;


/// Processing instructions

//
// interface ProcessingInstruction : Node {
//   readonly attribute  DOMString            target;
//            attribute  DOMString            data;
//                                       // raises(DOMException) on setting
// };
//

define sealed class <processing-instruction>
    (<leaf-node-mixin>, <node>)
end class <processing-instruction>;

define sealed method make
    (class == <processing-instruction>,
     #rest keys, #key target, data, #all-keys)
 => (instruction :: <processing-instruction>)
  apply(next-method, class, name: target, value: data, keys)
end method make;

define sealed inline method target
    (instruction :: <processing-instruction>) => (target :: <DOM-string>)
  node-name(instruction)
end method target;

define sealed inline method data
    (instruction :: <processing-instruction>) => (data :: <DOM-string>)
  node-value(instruction)
end method data;
