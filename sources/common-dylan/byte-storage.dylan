Module:       common-dylan-internals
Author:       Peter S. Housel
              Bruce Mitchener, Jr.
Copyright:    Original Code is Copyright 1995-2011 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.

///
/// WITH-STACK-BYTE-STORAGE
///

// TODO: Detect this usage pattern in dfmc-optimizer and replace it
// with a real stack-local allocation

define macro with-stack-byte-storage
  { with-stack-byte-storage (?:name, ?size:expression) ?:body end }
  => { begin
         let storage-size :: <integer> = ?size;
         let ?name :: <machine-word> = primitive-manual-allocate(storage-size);
         block ()
           ?body
         cleanup
           primitive-manual-free(?name);
         end;
       end }
end macro;

///
/// BYTE-STORAGE-ADDRESS
///

define open generic byte-storage-address
    (the-buffer)
 => (result-address :: <machine-word>);

define constant <byte-vector-like> = type-union(<byte-string>, <byte-vector>);

define sealed inline method byte-storage-address
    (the-buffer :: <byte-vector-like>)
 => (result-address :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (primitive-repeated-slot-as-raw
          (the-buffer, primitive-repeated-slot-offset(the-buffer))))
end method;

///
/// WITH-OBJECT-BYTE-STORAGE
///

define macro with-object-byte-storage
  { with-object-byte-storage (?:name = ?object:expression) ?:body end }
    => { begin
           let pinned-object = primitive-pin-object(?object);
           block ()
             let ?name :: <machine-word> = byte-storage-address(pinned-object);
             ?body
           cleanup
             primitive-unpin-object(pinned-object);
           end
         end }
end macro;

