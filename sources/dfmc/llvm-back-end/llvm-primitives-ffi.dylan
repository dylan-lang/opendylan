Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// C-FFI

define side-effect-free stateless dynamic-extent mapped &primitive-descriptor primitive-unwrap-c-pointer
    (pointer :: <C-pointer>) => (p :: <raw-c-pointer>);
  let ptr = op--getslotptr(be, pointer, #"<C-pointer>", #"raw-pointer-address");
  ins--load(be, ptr, alignment: back-end-word-size(be))
end;

define side-effect-free stateless dynamic-extent mapped-result &runtime-primitive-descriptor primitive-wrap-c-pointer
    (wrapper :: <mm-wrapper>, pointer :: <raw-c-pointer>) => (p :: <C-pointer>);
  let class :: <&class> = dylan-value(#"<C-pointer>");
  let alloc
    = call-primitive(be, primitive-alloc-leaf-descriptor,
                     instance-storage-bytes(be, class),
                     wrapper);
  let result = op--object-pointer-cast(be, pointer, class);
  let slot-ptr
    = op--getslotptr(be, result, class, #"raw-pointer-address");
  ins--store(be, pointer, slot-ptr, alignment: back-end-word-size(be));
  result
end;

define macro raw-accessor-primitive-definer
  { define raw-accessor-primitive (?be:name, ?:name, ?raw-value-type:name) }
     => { define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor "primitive-" ## ?name ## "-at"
              (pointer :: <raw-pointer>,
               offset :: <raw-integer>,
	       byte-offset :: <raw-integer>)
           => (value :: ?raw-value-type);
            //---*** Fill this in...
          end;
         define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor 
             "primitive-" ## ?name ## "-at-setter"
             (new :: ?raw-value-type,
              pointer :: <raw-pointer>,
              offset :: <raw-integer>,
	      byte-offset :: <raw-integer>)
          => (value :: ?raw-value-type);
           //---*** Fill this in...
         end }
end;

define raw-accessor-primitive(be, c-double, <raw-c-double>);
define raw-accessor-primitive(be, c-float, <raw-c-float>);
define raw-accessor-primitive(be, c-pointer, <raw-c-pointer>);
define raw-accessor-primitive(be, c-signed-char, <raw-c-signed-char>);
define raw-accessor-primitive(be, c-signed-int, <raw-c-signed-int>);
define raw-accessor-primitive(be, c-signed-long, <raw-c-signed-long>);
define raw-accessor-primitive(be, c-signed-short, <raw-c-signed-long>);
define raw-accessor-primitive(be, c-unsigned-char, <raw-c-unsigned-char>);
define raw-accessor-primitive(be, c-unsigned-int, <raw-c-unsigned-int>);
define raw-accessor-primitive(be, c-unsigned-long, <raw-c-unsigned-long>);
define raw-accessor-primitive(be, c-unsigned-short, <raw-c-unsigned-short>);

define macro raw-field-primitive-definer
  { define raw-field-primitive (?be:name, ?:name, ?raw-value-type:name) }
    => { define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor "primitive-" ## ?name ## "-field"
             (pointer :: <raw-pointer>,
	      byte-offset :: <raw-integer>,
	      bit-offset :: <raw-integer>,
	      bit-size :: <raw-integer>)
          => (value :: ?raw-value-type);
           //---*** Fill this in...
         end;
         define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor 
             "primitive-" ## ?name ## "-field-setter"
             (new :: ?raw-value-type,
              pointer :: <raw-pointer>,
              byte-offset :: <raw-integer>,
              bit-offset :: <raw-integer>,
              bit-size :: <raw-integer>)
          => (value :: ?raw-value-type);
           //---*** Fill this in...
         end }
end macro;

define raw-field-primitive (be, c-unsigned, <raw-c-unsigned-long>);
define raw-field-primitive (be, c-signed, <raw-c-signed-long>);
define raw-field-primitive (be, c-int, <raw-c-unsigned-int>);
