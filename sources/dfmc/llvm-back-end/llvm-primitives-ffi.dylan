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
  { define raw-accessor-primitive (?be:name, ?:name, ?raw-value-type:name,
                                   ?sext:expression) }
     => { define side-effect-free stateless dynamic-extent &primitive-descriptor "primitive-" ## ?name ## "-at"
              (pointer :: <raw-pointer>,
               offset :: <raw-integer>,
	       byte-offset :: <raw-integer>)
           => (value :: ?raw-value-type);
            let pointer
              = if (instance?(pointer.llvm-value-type, <llvm-integer-type>))
                  ins--inttoptr(?be, pointer, $llvm-i8*-type)
                else
                  pointer
                end;
            let type = llvm-reference-type(?be, dylan-value(?#"raw-value-type"));
            let pointer-byte-off = ins--gep(?be, pointer, byte-offset);
            let pointer-cast
              = ins--bitcast(?be, pointer-byte-off, llvm-pointer-to(?be, type));
            let pointer-off = ins--gep(?be, pointer-cast, offset);
            let value = ins--load(?be, pointer-off);
            if (instance?(type, <llvm-integer-type>)
                  & type.llvm-integer-type-width < back-end-word-size(?be) * 8)
              if (?sext)
                ins--sext(?be, value, ?be.%type-table["iWord"])
              else
                ins--zext(?be, value, ?be.%type-table["iWord"])
              end if
            else
              value
            end if;
          end;
         define side-effecting stateless dynamic-extent &primitive-descriptor
             "primitive-" ## ?name ## "-at-setter"
             (new :: ?raw-value-type,
              pointer :: <raw-pointer>,
              offset :: <raw-integer>,
	      byte-offset :: <raw-integer>)
          => (new :: ?raw-value-type);
            let pointer
              = if (instance?(pointer.llvm-value-type, <llvm-integer-type>))
                  ins--inttoptr(?be, pointer, $llvm-i8*-type)
                else
                  pointer
                end;
            let type = llvm-reference-type(?be, dylan-value(?#"raw-value-type"));
            let pointer-byte-off = ins--gep(?be, pointer, byte-offset);
            let pointer-cast
              = ins--bitcast(?be, pointer-byte-off, llvm-pointer-to(?be, type));
            let pointer-off = ins--gep(?be, pointer-cast, offset);
            ins--store(?be, new, pointer-off);
            new
         end }
end;

define raw-accessor-primitive(be, c-double, <raw-c-double>, #f);
define raw-accessor-primitive(be, c-float, <raw-c-float>, #f);
define raw-accessor-primitive(be, c-pointer, <raw-c-pointer>, #f);
define raw-accessor-primitive(be, c-signed-char, <raw-c-signed-char>, #t);
define raw-accessor-primitive(be, c-signed-int, <raw-c-signed-int>, #t);
define raw-accessor-primitive(be, c-signed-long, <raw-c-signed-long>, #t);
define raw-accessor-primitive(be, c-signed-short, <raw-c-signed-short>, #t);
define raw-accessor-primitive(be, c-ssize-t, <raw-c-ssize-t>, #t);
define raw-accessor-primitive(be, c-unsigned-char, <raw-c-unsigned-char>, #f);
define raw-accessor-primitive(be, c-unsigned-int, <raw-c-unsigned-int>, #f);
define raw-accessor-primitive(be, c-unsigned-long, <raw-c-unsigned-long>, #f);
define raw-accessor-primitive(be, c-unsigned-short, <raw-c-unsigned-short>, #f);
define raw-accessor-primitive(be, c-size-t, <raw-c-size-t>, #f);

define macro raw-field-primitive-definer
  { define raw-field-primitive (?be:name, ?:name, ?raw-value-type:name, ?sext:expression) }
    => { define side-effect-free stateless dynamic-extent &primitive-descriptor "primitive-" ## ?name ## "-field"
             (pointer :: <raw-pointer>,
	      byte-offset :: <raw-integer>,
	      bit-offset :: <raw-integer>,
	      bit-size :: <raw-integer>)
          => (value :: ?raw-value-type);
           let pointer
             = if (instance?(pointer.llvm-value-type, <llvm-integer-type>))
                 ins--inttoptr(?be, pointer, $llvm-i8*-type)
               else
                 pointer
               end;
           let type
             = llvm-reference-type(?be, dylan-value(?#"raw-value-type"));
           let pointer-byte-off = ins--gep(?be, pointer, byte-offset);
           let pointer-cast
             = ins--bitcast(?be, pointer-byte-off, llvm-pointer-to(?be, type));
           let value = ins--load(?be, pointer-cast);
           let shifted = ins--lshr(?be, value, bit-offset);
           let mask = ins--sub(?be, ins--shl(?be, 1, bit-size), 1);
           if (?sext)
             // Hacker's Delight (2nd ed.) section 2-6 (Sign Extension),
             // first formulation:
             //   logand(shifted + sign-bit, mask) - sign-bit
             let sign-bit = ins--shl(?be, 1, ins--sub(?be, bit-size, 1));
             ins--sub(?be,
                      ins--and(?be, ins--add(?be, shifted, sign-bit), mask),
                      sign-bit)
           else
             ins--and(?be, shifted, mask)
           end if
         end;
         define side-effecting stateless dynamic-extent &primitive-descriptor
             "primitive-" ## ?name ## "-field-setter"
             (new :: ?raw-value-type,
              pointer :: <raw-pointer>,
              byte-offset :: <raw-integer>,
              bit-offset :: <raw-integer>,
              bit-size :: <raw-integer>)
          => (value :: ?raw-value-type);
           let pointer
             = if (instance?(pointer.llvm-value-type, <llvm-integer-type>))
                 ins--inttoptr(?be, pointer, $llvm-i8*-type)
               else
                 pointer
               end;
           let type = llvm-reference-type(?be, dylan-value(?#"raw-value-type"));
           let pointer-byte-off = ins--gep(?be, pointer, byte-offset);
           let pointer-cast
             = ins--bitcast(?be, pointer-byte-off, llvm-pointer-to(?be, type));
           let value = ins--load(?be, pointer-cast);
           let new-shifted = ins--shl(?be, new, bit-offset);
           let mask = ins--sub(?be, ins--shl(?be, 1, bit-size), 1);
           let mask-shifted = ins--shl(?be, mask, bit-offset);
           let anti-mask-shifted = ins--xor(?be, mask-shifted, -1);
           let value-masked = ins--and(?be, value, anti-mask-shifted);
           ins--store(?be, ins--or(?be, value-masked, new-shifted), pointer-cast);
           new
         end }
end macro;

define raw-field-primitive (be, c-unsigned, <raw-c-unsigned-long>, #f);
define raw-field-primitive (be, c-signed, <raw-c-signed-long>, #t);
define raw-field-primitive (be, c-int, <raw-c-unsigned-int>, #f);
