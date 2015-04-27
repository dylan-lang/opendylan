Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* 
 * HARP PRIMITIVES
 */


/* NULL PRIMITIVE */

define method op--nop
    (back-end :: <harp-back-end>, #rest junk) => ()
end method op--nop;

define method op--ignore-result (ins--thing :: <function>)
  method (back-end :: <harp-back-end>, result, #rest wanted-args) => ()
    apply(ins--thing, back-end, wanted-args);
  end method;
end method op--ignore-result;



/* LOW-LEVEL ACCESSOR PRIMITIVES */


define open generic op--load-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ();

define open generic op--store-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ();


define method op--load-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset,
     #key tagged?) => ()
  let offset =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, bytes%(back-end, scaled-index), offset);
    end;

  ins--ld(back-end, result, base, offset);

end method op--load-index;


define method op--store-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset,
     #key tagged?) => ()
  let offset =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, bytes%(back-end, scaled-index), offset);
    end;

  ins--st(back-end, value, base, offset);
  ins--move(back-end, result, value);

end method op--store-index;


define open generic op--load-byte-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ();

define open generic op--store-byte-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ();


define method op--load-byte-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--ldb(back-end, result, base, offset);

end method op--load-byte-index;


define method op--store-byte-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--stb(back-end, value, base, offset);
  ins--move(back-end, result, value);

end method op--store-byte-index;



define open generic op--load-bit-index
    (back-end :: <harp-back-end>, result, base, index, offset, bit) => ();

define open generic op--store-bit-index
    (back-end :: <harp-back-end>, result, value, base, index, offset, bit) => ();

define method op--load-bit-index
    (back-end :: <harp-back-end>, result, base, index, offset, bit) => ()
  let offset = op--add(back-end, #f, index, offset);

  ins--ld(back-end, result, base, offset);
  ins--lsr(back-end, result, result, bit);
  ins--and(back-end, result, result, 1);

end method op--load-bit-index;

define method op--store-bit-index
    (back-end :: <harp-back-end>, result, value, base, index, offset, bit) => ()
  let offset = op--add(back-end, #f, index, offset);
  let word = make-n-register(back-end);

  ins--ld(back-end, word, base, offset);
  select (value)
    0 => 
      ins--unset-bit(back-end, word, word, bit);
    1 => 
      ins--set-bit(back-end, word, word, bit);
    otherwise =>
      let set-bit-tag = make-tag(back-end);
      let done-tag = make-tag(back-end);
      ins--bne(back-end, set-bit-tag, value, 0);
      ins--unset-bit(back-end, word, word, bit);
      ins--bra(back-end, done-tag);      
      ins--tag(back-end, set-bit-tag);
      ins--set-bit(back-end, word, word, bit);
      ins--tag(back-end, done-tag);
  end select;
  ins--st(back-end, word, base, offset);

  ins--move(back-end, result, value);

end method op--store-bit-index;




define open generic op--load-signed-byte-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset);

define open generic op--store-signed-byte-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset);


define method op--load-signed-byte-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--ldb-signed(back-end, result, base, offset);

end method op--load-signed-byte-index;

define method op--store-signed-byte-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-byte-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-byte-index;


define open generic op--load-half-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ();

define open generic op--store-half-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ();


define method op--load-half-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--ldh(back-end, result, base, offset);

end method op--load-half-index;


define method op--store-half-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--sth(back-end, value, base, offset);
  ins--move(back-end, result, value);

end method op--store-half-index;


define open generic op--load-signed-half-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ();

define open generic op--store-signed-half-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ();


define method op--load-signed-half-index
    (back-end :: <harp-back-end>, result, base, scaled-index, offset) => ()
  let offset = op--add(back-end, #f, scaled-index, offset);

  ins--ldh-signed(back-end, result, base, offset);

end method op--load-signed-half-index;

define method op--store-signed-half-index
    (back-end :: <harp-back-end>, result, value, base, scaled-index, offset) => ()

  op--store-half-index(back-end, result, value, base, scaled-index, offset);

end method op--store-signed-half-index;

define open generic op--load-float-index
    (back-end :: <harp-back-end>, result, object, scaled-index, offset,
     #key tagged?) => ();

define open generic op--store-float-index
    (back-end :: <harp-back-end>, result, value, object, scaled-index, offset,
     #key tagged?) => ();


define method op--load-float-index
    (back-end :: <harp-back-end>, result, object, scaled-index, offset,
     #key tagged?) => ()

  let temp =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, bytes%(back-end, scaled-index), offset);
    end;

  ins--fld(back-end, result, object, temp);

end method op--load-float-index;


define method op--store-float-index
    (back-end :: <harp-back-end>, result, value, object, scaled-index, offset,
     #key tagged?) => ()

  let temp =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, bytes%(back-end, scaled-index), offset);
    end;

  ins--fst(back-end, value, object, temp);
  ins--fmove(back-end, result, value);

end method op--store-float-index;


define open generic op--load-dfloat-index
    (back-end :: <harp-back-end>, result, object, scaled-index, offset,
     #key tagged?) => ();

define open generic op--store-dfloat-index
    (back-end :: <harp-back-end>, result, value, object, scaled-index, offset,
     #key tagged?) => ();


define method op--load-dfloat-index
    (back-end :: <harp-back-end>, result, object, scaled-index, offset,
     #key tagged?) => ()

  let temp =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, double%(back-end, scaled-index), offset);
    end;

  ins--dld(back-end, result, object, temp);

end method op--load-dfloat-index;


define method op--store-dfloat-index
    (back-end :: <harp-back-end>, result, value, object, scaled-index, offset,
     #key tagged?) => ()

  let temp =
    if (tagged? & instance?(offset, <integer>))
      op--add(back-end, #f, scaled-index, unset-tag-bit(offset));
    else
      op--add(back-end, #f, double%(back-end, scaled-index), offset);
    end;

  ins--dst(back-end, value, object, temp);
  ins--dmove(back-end, result, value);

end method op--store-dfloat-index;
/*
// single to double float conversion because language does not
// support double floats

define method op--load-dfloat-index
    (back-end :: <harp-back-end>, result, object, scaled-index, offset) => ()

  let temp = make-n-register(back-end);
  let sf-temp = make-sf-register(back-end);

  op--load-float-index(back-end, sf-temp, object, scaled-index, offset);
  ins--single-to-double-float(back-end, result, sf-temp);

end method op--load-dfloat-index;


define method op--store-dfloat-index
    (back-end :: <harp-back-end>, result, value, object, scaled-index, offset) => ()

  let sf-temp = make-sf-register(back-end);

  ins--double-to-single-float(back-end, sf-temp, value);
  op--store-float-index(back-end, result, sf-temp, object, scaled-index, offset);

end method op--store-dfloat-index;
*/


/* VECTOR PRIMITIVES */

define method op--vector-element
    (back-end :: <harp-back-end>, result, vector, index) => ()

  op--load-index(back-end,
		 result,
		 vector,
		 op--raw(back-end, #f, index),
		 bytes%(back-end, 2));

end method op--vector-element;


define method op--vector-element-setter
    (back-end :: <harp-back-end>, result, new-value, vector, index) => ()

  op--store-index(back-end,
		  result,
		  new-value,
		  vector,
		  op--raw(back-end, #f, index),
		  bytes%(back-end, 2));

end method op--vector-element-setter;


define method op--vector-size
    (back-end :: <harp-back-end>, result, vector) => (result)

  ins--load(back-end, result, vector, bytes%(back-end, 1));

  result;

end method op--vector-size;


define method op--vector-as-raw
    (back-end :: <harp-back-end>, result, vector) => (result)

  ins--add(back-end, result, vector, bytes%(back-end, 2));

  result;

end method op--vector-as-raw;


define method op--string-as-raw
    (back-end :: <harp-back-end>, result, vector) => (result)

  // TEMPORARY KLUDGE - to compensate for the lack of a Leaf Object
  // pool, read & write back the wrapper of the string

  let wrapper = make-g-register(back-end);

  ins--ld(back-end, wrapper, vector, 0);
  ins--st(back-end, wrapper, vector, 0);

  op--vector-as-raw(back-end, result, vector);

end method op--string-as-raw;



/* OBJECT REPRESENTATION PRIMITIVES AND SUPPORT */
 

define method op--init-slot-element
    (back-end :: <harp-back-end>, result, object, position) => ()

  op--load-index(back-end,
		 result,
		 object,
		 position,
		 bytes%(back-end, 1));

end method op--init-slot-element;

define method op--slot-element
    (back-end :: <harp-back-end>, result, object, position) => ()

  
  let dylan-library? = *compiling-dylan?*;

  if (#f)
    op--init-slot-element(back-end, result, object, position);
  else
    let value = make-register(back-end);
    let unbound-tag = make-tag(back-end);
    let done-tag = make-tag(back-end);

    op--load-index(back-end,
		   value,
		   object,
		   position,
		   bytes%(back-end, 1));

    ins--beq(back-end, unbound-tag, value, op--dylan-constant-ref(back-end, $dylan-unbound));
    ins--bra(back-end, done-tag);

    ins--tag(back-end, unbound-tag);
    let tagged-position =
      op--integer(back-end, #f, op--raw(back-end, #f, bytes%(back-end, position)));

    ins--call(back-end,
	      op--dylan-constant-ref(back-end, $dylan-unbound-instance-slot-iep),
	      push-arguments(back-end, list(object, tagged-position)),
	      nlx-tags: *live-nlx-tags*);

    ins--tag(back-end, done-tag);
    ins--move(back-end, result, value);
  end if;
end method op--slot-element;


define method op--slot-element-setter
    (back-end :: <harp-back-end>, result, new-value, object, position) => ()

  let op--store-index = op--store-index%(new-value);

  op--store-index(back-end,
		  result,
		  new-value,
		  object,
		  position,
		  bytes%(back-end, 1));

end method op--slot-element-setter;


define method op--repeated-slot-element
    (back-end :: <harp-back-end>, result, object, base-position, position,
     #key tagged?) => ()

  let op--load-index = op--load-index%(result);

  if (instance?(base-position, <integer>))
    op--load-index(back-end,
		   result,
		   object,
		   position,
		   bytes%(back-end, base-position + 1),
		   tagged?: tagged?);
  else
    let new-position = op--add(back-end, #f, base-position, position);

    op--load-index(back-end,
		   result,
		   object,
		   new-position,
		   bytes%(back-end, 1));
  end if;

end method op--repeated-slot-element;

define method op--repeated-slot-element-setter
    (back-end :: <harp-back-end>, result, new-value, object, base-position, position,
     #key tagged?) => ()

  let op--store-index = op--store-index%(new-value);

  if (instance?(base-position, <integer>))
    op--store-index(back-end,
		    result,
		    new-value,
		    object,
		    position,
		    bytes%(back-end, base-position + 1),
		    tagged?: tagged?);
  else
    let new-position = op--add(back-end, #f, base-position, position);

    op--store-index(back-end,
		    result,
		    new-value,
		    object,
		    new-position,
		    bytes%(back-end, 1));
  end if;

end method op--repeated-slot-element-setter;


define method op--repeated-slot-offset(back-end :: <harp-back-end>, result, instance) => ()

  let wrapper = make-n-register(back-end);
  let fixed-part = make-n-register(back-end);
  let n-slots = make-n-register(back-end);

  ins--load(back-end, wrapper, instance, 0);
  ins--load(back-end, fixed-part, wrapper, bytes%(back-end, 3));
  ins--asr(back-end, n-slots, fixed-part, 2);
  ins--add(back-end, result, n-slots, 2);

end method op--repeated-slot-offset;


define method op--repeated-slot-as-raw(back-end :: <harp-back-end>, result, x, offset) => ()

    ins--add(back-end, result, x, bytes%(back-end, offset));

end method op--repeated-slot-as-raw;


define method op--object-class(back-end :: <harp-back-end>, result, object) => (result)
  let result = result | make-g-register(back-end);
  let wrapper = make-g-register(back-end);
  let iclass = make-g-register(back-end);

  ins--load(back-end, wrapper, object, 0);
  ins--load(back-end, iclass, wrapper, bytes%(back-end, 1));
  ins--load(back-end, result, iclass, bytes%(back-end, 2));

  result
end method op--object-class;


define inline method unset-tag-bit(tagged-offset :: <integer>)
 => (untagged-offset :: <integer>)
  tagged-offset - 1
end method;

define method op--byte-element
    (back-end :: <harp-back-end>, result, base, offset, index,
     #key tagged?) => ()
  let (base, offset, index) =
    if (tagged?)
      let new-index = make-n-register(back-end);
      ins--asr(back-end, new-index, index, 2);
      values(base, bytes%(back-end, offset + 1), new-index)
    else
      values(op--slots(back-end, base), bytes%(back-end, offset), index)
    end if;

  op--load-byte-index(back-end, result, base, index, offset)

end method op--byte-element;


define method op--byte-element-setter
    (back-end :: <harp-back-end>, result, value, base, offset, index,
     #key tagged?) => ()
  let (base, offset, index) =
    if (tagged?)
      let new-index = make-n-register(back-end);
      ins--asr(back-end, new-index, index, 2);
      values(base, bytes%(back-end, offset + 1), new-index)
    else
      values(op--slots(back-end, base), bytes%(back-end, offset), index)
    end if;

  op--store-byte-index(back-end, result, value, base, index, offset)

end method op--byte-element-setter;

define method op--double-byte-element
    (back-end :: <harp-back-end>, result, base, offset, index,
     #key tagged?) => ()
  let (base, offset, index) =
    if (tagged?)
      let new-index = make-n-register(back-end);
      ins--asr(back-end, new-index, index, 2);
      values(base, half%(back-end, offset) + bytes%(back-end, 1), new-index)
    else
      values(op--slots(back-end, base), half%(back-end, offset), index)
    end if;

  op--load-half-index(back-end, result, base, index, offset)

end method op--double-byte-element;


define method op--double-byte-element-setter
    (back-end :: <harp-back-end>, result, value, base, offset, index,
     #key tagged?) => ()
  let (base, offset, index) =
    if (tagged?)
      let new-index = make-n-register(back-end);
      ins--asr(back-end, new-index, index, 2);
      values(base, half%(back-end, offset) + bytes%(back-end, 1), new-index)
    else
      values(op--slots(back-end, base), half%(back-end, offset), index)
    end if;

  op--store-half-index(back-end, result, value, base, index, offset)

end method op--double-byte-element-setter;


define method op--bit-element
    (back-end :: <harp-back-end>, result, base, offset, byte-offset, bit-offset) => ()

  op--load-bit-index(back-end, result, base, bytes%(back-end, offset), byte-offset, bit-offset);

end method op--bit-element;


define method op--bit-element-setter
    (back-end :: <harp-back-end>, result, value, base, offset, byte-offset, bit-offset) => ()

  op--store-bit-index(back-end, result, value, base, bytes%(back-end, offset), byte-offset, bit-offset);

end method op--bit-element-setter;


define method op--load-bit-field(back-end :: <harp-back-end>, result, pointer, bit-offset, bit-size) => ()
  ins--ldbits(back-end, result, pointer, bit-offset, bit-size);
end method op--load-bit-field;


define method op--store-bit-field(back-end :: <harp-back-end>, result, new-field, pointer, bit-offset, bit-size) => ()
  ins--stbits(back-end, pointer, bit-offset, bit-size, new-field);
  ins--move(back-end, result, new-field);
end method op--store-bit-field;


define method op--extract-bits(back-end :: <harp-back-end>, result, bit-offset, bit-size, x) => ()
  ins--extract-bits(back-end, result, x, bit-offset, bit-size);
end method op--extract-bits;


define method op--set-bits(back-end :: <harp-back-end>, result, new-field, bit-offset, bit-size, x) => ()
  ins--set-bits(back-end, result, x, bit-offset, bit-size, new-field);
end method op--set-bits;


define method op--replace!(back-end :: <harp-back-end>, result, dst, dst-base-offset, dst-offset, src, src-base-offset, src-offset, size) => ()
  let dst-offset = op--add(back-end, #f, dst-base-offset, dst-offset);
  let src-offset = op--add(back-end, #f, src-base-offset, src-offset);

  ins--copy-words-down-w(back-end,
			 op--add(back-end, #f, dst, bytes%(back-end, dst-offset)),
			 op--add(back-end, #f, src, bytes%(back-end, src-offset)),
			 size);
  ins--move(back-end, result, dst);
end method op--replace!;


define open generic op--replace-bytes!(back-end :: <harp-back-end>, result, dst, dst-base-offset, dst-offset, src, src-base-offset, src-offset, size) => ();

define method op--replace-bytes!(back-end :: <harp-back-end>, result, dst, dst-base-offset, dst-offset, src, src-base-offset, src-offset, size) => ()
  let dst-offset = op--add(back-end, #f, bytes%(back-end, dst-base-offset), dst-offset);
  let src-offset = op--add(back-end, #f, bytes%(back-end, src-base-offset), src-offset);

  ins--copy-words-down(back-end,
		       op--add(back-end, #f, dst, dst-offset),
		       op--add(back-end, #f, src, src-offset),
		       size);
  ins--move(back-end, result, dst);
end method op--replace-bytes!;


define method op--fill!(back-end :: <harp-back-end>, result, dst, base-offset, offset, size, value) => ()

    let dst-offset = op--add(back-end, #f, base-offset, offset);

    ins--fill-words-w(back-end,
		      op--add(back-end, #f, dst, bytes%(back-end, dst-offset)),
		      size,
		      value);
    ins--move(back-end, result, dst);

end method op--fill!;


define method op--fill-bytes!(back-end :: <harp-back-end>, result, dst, base-offset, offset, size, value) => ()

    let dst-offset = op--add(back-end, #f, bytes%(back-end, base-offset), offset);

    ins--fill-bytes(back-end,
		    op--add(back-end, #f, dst, dst-offset),
		    size,
		    value);
    ins--move(back-end, result, dst);

end method op--fill-bytes!;



/* SMALL-INTEGER PRIMITIVES */


define method op--integer-negate(back-end :: <harp-back-end>, result, argument) => ()

  ins--sub(back-end, result, 0, argument);

end method op--integer-negate;


define method op--integer-negatev(back-end :: <harp-back-end>, ov-tag :: <tag>, result, argument) => ()

  ins--subv(back-end, ov-tag, result, 0, argument);

end method op--integer-negatev;


/* GENERIC FLOAT */

define method op--convert-from-float
    (ins--convert-from-float :: <function>) => (op :: <function>)

  method(back-end :: <harp-back-end>, result, float) => ()

      ins--set-rounding-mode(back-end, #"truncate");
      ins--convert-from-float(back-end, result, float);
      ins--set-rounding-mode(back-end, #"default");

  end method;

end method op--convert-from-float;



/* SINGLE-FLOAT PRIMITIVES */


define method op--single-float-negate(back-end :: <harp-back-end>, result, argument) => ()

  ins--fsub(back-end, result, 0, argument);

end method op--single-float-negate;


define method op--single-float-expt
    (back-end :: <harp-back-end>, result, base, power) => ()
  let log-base = make-sf-register(back-end);
  let temp = make-sf-register(back-end);

  ins--floge(back-end, log-base, base);
  ins--fmul(back-end, temp, power, log-base);
  ins--fetox(back-end, result, temp);

end method op--single-float-expt;



/* DOUBLE-FLOAT PRIMITIVES */


define method op--double-float-negate(back-end :: <harp-back-end>, result, argument) => ()

  ins--dsub(back-end, result, 0, argument);

end method op--double-float-negate;


define method op--double-float-expt
    (back-end :: <harp-back-end>, result, base, power) => ()
  let log-base = make-df-register(back-end);
  let temp = make-df-register(back-end);

  ins--dloge(back-end, log-base, base);
  ins--dmul(back-end, temp, power, log-base);
  ins--detox(back-end, result, temp);

end method op--double-float-expt;



/* MACHINE-WORD PRIMITIVES */


define method op--mulxh(back-end :: <harp-back-end>, high, x, y) => ()

  ins--mulx(back-end, #f, high, x, y);

end method op--mulxh;


define method op--floorxq(back-end :: <harp-back-end>, quotient, dividend, divisor) => ()

  ins--floorx(back-end, quotient, #f, dividend, divisor);

end method op--floorxq;


define method op--floorxr(back-end :: <harp-back-end>, remainder, dividend, divisor) => ()

  ins--floorx(back-end, #f, remainder, dividend, divisor);

end method op--floorxr;


define method op--ceilingxq(back-end :: <harp-back-end>, quotient, dividend, divisor) => ()

  ins--ceilingx(back-end, quotient, #f, dividend, divisor);

end method op--ceilingxq;


define method op--ceilingxr(back-end :: <harp-back-end>, remainder, dividend, divisor) => ()

  ins--ceilingx(back-end, #f, remainder, dividend, divisor);

end method op--ceilingxr;


define method op--roundxq(back-end :: <harp-back-end>, quotient, dividend, divisor) => ()

  ins--roundx(back-end, quotient, #f, dividend, divisor);

end method op--roundxq;


define method op--roundxr(back-end :: <harp-back-end>, remainder, dividend, divisor) => ()

  ins--roundx(back-end, #f, remainder, dividend, divisor);

end method op--roundxr;


define method op--truncatexq(back-end :: <harp-back-end>, quotient, dividend, divisor) => ()

  ins--truncatex(back-end, quotient, #f, dividend, divisor);

end method op--truncatexq;


define method op--truncatexr(back-end :: <harp-back-end>, remainder, dividend, divisor) => ()

  ins--truncatex(back-end, #f, remainder, dividend, divisor);

end method op--truncatexr;


define method op--aslh(back-end :: <harp-back-end>, high, x, shift) => ()

  ins--aslx(back-end, #f, high, x, shift);

end method op--aslh;


define method op--aslxvl(back-end :: <harp-back-end>, ov-tag :: <tag>, low, x, shift) => ()

  ins--aslxv(back-end, ov-tag, low, #f, x, shift);

end method op--aslxvl;


define method op--neg-trap(back-end :: <harp-back-end>, result, x) => ()

  ins--sub-trap(back-end, result, 0, x);

end method op--neg-trap;


define method op--abs-trap(back-end :: <harp-back-end>, result, x) => ()

  let done-tag = make-tag(back-end);
  let abs-tag = make-tag(back-end);

  ins--move(back-end, result, x);
  ins--bge(back-end, done-tag, x, 0);
  ins--sub-trap(back-end, result, 0, x);
  ins--tag(back-end, done-tag);

end method op--abs-trap;


define method op--floorxxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--floorxx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--floorxxq;


define method op--floorxxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--floorxx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--floorxxr;


define method op--ceilingxxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--ceilingxx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--ceilingxxq;


define method op--ceilingxxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--ceilingxx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--ceilingxxr;


define method op--roundxxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--roundxx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--roundxxq;


define method op--roundxxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--roundxx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--roundxxr;


define method op--truncatexxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--truncatexx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--truncatexxq;


define method op--truncatexxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--truncatexx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--truncatexxr;


define method op--divxxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--divxx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--divxxq;


define method op--divxxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--divxx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--divxxr;


define method op--muluxh(back-end :: <harp-back-end>, high, x, y) => ()

  ins--mulux(back-end, #f, high, x, y);

end method op--muluxh;


define method op--divuxxq(back-end :: <harp-back-end>, quotient, dividend-low, dividend-high, divisor) => ()

  ins--divuxx(back-end, quotient, #f, dividend-low, dividend-high, divisor);

end method op--divuxxq;


define method op--divuxxr(back-end :: <harp-back-end>, remainder, dividend-low, dividend-high, divisor) => ()

  ins--divuxx(back-end, #f, remainder, dividend-low, dividend-high, divisor);

end method op--divuxxr;


define method op--lslxh(back-end :: <harp-back-end>, high, x, shift) => ()

  ins--lslx(back-end, #f, high, x, shift);

end method op--lslxh;


define method op--lslxxh(back-end :: <harp-back-end>, high, x-low, x-high, shift) => ()

  ins--lslxx(back-end, #f, high, x-low, x-high, shift);

end method op--lslxxh;


define method op--lsrxxl(back-end :: <harp-back-end>, low, x-low, x-high, shift) => ()

  ins--lsrxx(back-end, low, #f, x-low, x-high, shift);

end method op--lsrxxl;


define method op--lsrxxh(back-end :: <harp-back-end>, high, x-low, x-high, shift) => ()

  ins--lsrxx(back-end, #f, high, x-low, x-high, shift);

end method op--lsrxxh;


define method op--word-size(back-end :: <harp-back-end>, result) => ()

  ins--move(back-end, result, bytes%(back-end, 1));

end method op--word-size;


define method op--abs(back-end :: <harp-back-end>, result, x) => ()

  let done-tag = make-tag(back-end);

  ins--move(back-end, result, x);
  ins--bge(back-end, done-tag, x, 0);
  ins--sub(back-end, result, 0, x);
  ins--tag(back-end, done-tag);

end method op--abs;


define method op--absv(back-end :: <harp-back-end>, ov-tag :: <tag>, result, x) => ()

  let done-tag = make-tag(back-end);

  ins--move(back-end, result, x);
  ins--bge(back-end, done-tag, x, 0);
  ins--subv(back-end, ov-tag, result, 0, x);
  ins--tag(back-end, done-tag);

end method op--absv;


define method op--logbit-set(back-end :: <harp-back-end>, result, bit, x) => ()

  ins--set-bit(back-end, result, x, bit);

end method op--logbit-set;


define method op--logbit-clear(back-end :: <harp-back-end>, result, bit, x) => ()

  ins--unset-bit(back-end, result, x, bit);

end method op--logbit-clear;


/* CALLING CONVENTION PRIMITIVES */


define method op--function-parameter(back-end :: <harp-back-end>, result) => ()

  ins--move(back-end, result, back-end.registers.reg-function);

end method op--function-parameter;

define method op--environment-parameter
    (back-end :: <harp-back-end>, #key indirections = #[]) 
    => (register :: <virtual-register>)
  let result = back-end.cg-variables.cg-temporaries[0];

  if (result)
    result
  else
    let register = make-g-register(back-end, indirections: indirections);
    ins--move(back-end, register, back-end.registers.reg-environment);
    back-end.cg-variables.cg-temporaries[0] := register;
  end if;

end method op--environment-parameter;


define method op--next-methods-parameter(back-end :: <harp-back-end>, result) => ()

  ins--move(back-end, result, back-end.cg-variables.next-methods-vreg);

end method op--next-methods-parameter;



/* VALUES PRIMITIVES */


define method op--mv-get-rest-at(back-end :: <harp-back-end>, result, first-value, offset) => ()

  ins--move(back-end, back-end.registers.reg-arg0, first-value);
  ins--move(back-end, back-end.registers.reg-mlist, offset);
  ins--call(back-end, $primitive-heap-vector-remaining-values.runtime-reference,
	    back-end.registers.arguments-passed-in-registers);
  ins--move(back-end, result, back-end.registers.reg-result);

  result;

end method op--mv-get-rest-at;


define method op--mv-set-rest-at(back-end :: <harp-back-end>, result, values-vector, offset == 0) => ()


  ins--move(back-end, back-end.registers.reg-arg0, values-vector);
  ins--call(back-end, $primitive-set-mv-from-vector.runtime-reference,
	    back-end.registers.arguments-passed-in-registers);
  ins--move(back-end, result, back-end.registers.reg-result);

  result;

end method op--mv-set-rest-at;


define method op--mv-set-rest-at(back-end :: <harp-back-end>, result, values-vector, offset) => ()


  let vector-size = op--raw(back-end, #f, op--vector-size(back-end, make-register(back-end), values-vector));
  let vector = op--vector-as-raw(back-end, make-register(back-end), values-vector);
  let $multiple-values-area = op--ld-mv-area-address(back-end);
  let multiple-values-area = make-n-register(back-end);
  let true-tag = make-tag(back-end);
  let done-tag = make-tag(back-end);
  let count = make-n-register(back-end);

  ins--rem(back-end, "mv-set-rest-at");
  ins--add(back-end, multiple-values-area, $multiple-values-area, bytes%(back-end, offset));
  ins--copy-words-down-w(back-end, multiple-values-area, vector, vector-size);
  ins--add(back-end, count, vector-size, offset);
  op--store-multiple-values-count(back-end, count);
  ins--beq(back-end, true-tag, count, 0);
  ins--load(back-end, result, $multiple-values-area, 0);
  ins--bra(back-end, done-tag);
  ins--tag(back-end, true-tag);
  ins--move(back-end, result, $false);
  ins--tag(back-end, done-tag);

  result;

end method op--mv-set-rest-at;


/* OVERFLOW PRIMITIVES */


define method op--do-with-overflow(ins--dov :: <function>) => (op :: <function>)

  method(back-end :: <harp-back-end>, result, overflow?, #rest args)

      let ov-tag = make-tag(back-end);
      let done-tag = make-tag(back-end);

      apply(ins--dov, back-end, ov-tag, result, args);
      ins--move(back-end, overflow?, $false);
      ins--bra(back-end, done-tag);
      ins--tag(back-end, ov-tag);
      ins--move(back-end, overflow?, $true);
      ins--tag(back-end, done-tag);

  end method;

end method op--do-with-overflow;


define method op--do-with-overflow2(ins--dov :: <function>) => (op :: <function>)

  method(back-end :: <harp-back-end>, result1, result2, overflow?, #rest args)

      let ov-tag = make-tag(back-end);
      let done-tag = make-tag(back-end);

      apply(ins--dov, back-end, ov-tag, result1, result2, args);
      ins--move(back-end, overflow?, $false);
      ins--bra(back-end, done-tag);
      ins--tag(back-end, ov-tag);
      ins--move(back-end, overflow?, $true);
      ins--tag(back-end, done-tag);

  end method;

end method op--do-with-overflow2;


/* COMPARISON PRIMITIVES */


define macro op--primitive-predicate
  { op--primitive-predicate(?ins:name) }
  =>
  {
    method(back-end :: <harp-back-end>, result, #rest operands)
     => (test-result :: <test-result>)

	make(<test-result>,
	     branch: opposite-instruction(?ins),
	     operands: operands,
	     result: result,
	     continue: make-tag(back-end));
       
    end method;
   }

end macro op--primitive-predicate;


define macro opposite-instructions-definer
  { define opposite-instructions ?ins1:name ?ins2:name }
    =>
  {
    define constant "$" ## ?ins1 ## "-opposite" = "ins--" ## ?ins2;
    define constant "$" ## ?ins2 ## "-opposite" = "ins--" ## ?ins1;
  }
end macro;

define macro opposite-instruction
  { opposite-instruction("ins--" ## ?ins:name) }
    =>
  {
    "$" ## ?ins ## "-opposite"
  }
end macro;

define opposite-instructions beq bne;   
define opposite-instructions blt bge;
define opposite-instructions bgt ble;
define opposite-instructions blo bhs;
define opposite-instructions bhi bls;
define opposite-instructions fbeq fbne;
define opposite-instructions fblt fbge;
define opposite-instructions dbeq dbne;
define opposite-instructions dblt dbge;
define opposite-instructions dynamic-bit dynamic-nbit;

// Not referenced or exported.  https://github.com/dylan-lang/opendylan/issues/561
ignore($bge-opposite,
       $fbne-opposite,
       $fbge-opposite,
       $dbne-opposite,
       $dbge-opposite,
       $dynamic-nbit-opposite);

define method op--true?(back-end :: <harp-back-end>, result, x) => (result :: <test-result>)

  make(<test-result>,
       branch: ins--beq,
       operands: list(x, $false),
       result: result,
       continue: make-tag(back-end));

end method op--true?;


define method op--false?(back-end :: <harp-back-end>, result, x) => (result :: <test-result>)

  make(<test-result>,
       branch: ins--bne,
       operands: list(x, $false),
       result: result,
       continue: make-tag(back-end));

end method op--false?;

define method op--true(back-end :: <harp-back-end>, result, #rest args) => (result :: <test-result>)

  make(<test-result>,
       branch: #f,
       result: result,
       continue: make-tag(back-end));

end method op--true;

define method op--raw-as-boolean(back-end :: <harp-back-end>, result, x) => (result :: <test-result>)

  make(<test-result>,
       branch: ins--beq,
       operands: list(x, 0),
       result: result,
       continue: make-tag(back-end));

end method op--raw-as-boolean;


define method op--boolean-as-raw(back-end :: <harp-back-end>, result, x) => ()

  let false-tag = make-tag(back-end);
  let done-tag = make-tag(back-end);

  ins--beq(back-end, false-tag, x, $false);

  ins--move(back-end, result, 1);
  ins--bra(back-end, done-tag);

  ins--tag(back-end, false-tag);
  ins--move(back-end, result, 0);

  ins--tag(back-end, done-tag);

end method op--boolean-as-raw;


define method op--compare-bytes(back-end :: <harp-back-end>, result, b1, o1, b2, o2, len) => (result :: <test-result>)
  op--compare-memory(back-end, ins--bne-bytes, result, b1, o1, b2, o2, len);
end method op--compare-bytes;


define method op--compare-words(back-end :: <harp-back-end>, result, b1, o1, b2, o2, len) => (result :: <test-result>)
  op--compare-memory(back-end, ins--bne-words, result, b1, o1, b2, o2, len);
end method op--compare-words;


define method op--compare-memory(back-end :: <harp-back-end>, op--compare :: <function>, result, b1, o1, b2, o2, len) => (result :: <test-result>)

  let mem1 = make-n-register(back-end);
  let mem2 = make-n-register(back-end);

  ins--add(back-end, mem1, b1, o1);
  ins--add(back-end, mem2, b2, o2);

  make(<test-result>,
       branch: op--compare,
       operands: list(mem1, mem2, len),
       result: result,
       continue: make-tag(back-end));

end method op--compare-memory;


define method op--check-range(back-end :: <harp-back-end>, tag :: <tag>, x, l, h) => ()

  ins--blt(back-end, tag, x, l);
  ins--bge(back-end, tag, x, h);

end method op--check-range;

define method op--range-check(back-end :: <harp-back-end>, result, #rest operands) => (result :: <test-result>)

  make(<test-result>,
       branch: op--check-range, 
       operands: operands,
       result: result,
       continue: make-tag(back-end));

end method op--range-check;


define inline method op--instance?(back-end :: <harp-back-end>, result, x, y) => ()
  let function = make-g-register(back-end);

  ins--load(back-end, function, y, back-end.type-instancep-function-offset);
  call-primitive(back-end, result, function, x, y);

end method op--instance?;


define method op--logbit?(back-end :: <harp-back-end>, result, index, x) => (result :: <test-result>)

  make(<test-result>,
       branch: opposite-instruction(ins--dynamic-bit),
       operands: list(x, index),
       result: result,
       continue: make-tag(back-end));

end method op--logbit?;

define method op--logbit?(back-end :: <harp-back-end>, result, index :: <integer>, x :: <integer>) => ()

  ins--move(back-end, result,
	    if (logbit?(index, x)) $true
	    else $false end);

end method op--logbit?;


define method op--integer?(back-end :: <harp-back-end>, result, x) => (result :: <test-result>)

  make(<test-result>,
       branch: 
	 method(back-end :: <harp-back-end>, tag, x)
	     let temp = make-n-register(back-end);

	     ins--and(back-end, temp, x, 3);
	     ins--bne(back-end, tag, temp, 1);
	 end method,
       operands: list(x),
       result: result,
       continue: make-tag(back-end));

end method op--integer?;


define method op--unwrap-c-pointer(back-end :: <harp-back-end>, result, pointer) => ()

//  op--as-raw(back-end, result, op--as-raw(back-end, #f, pointer));
  op--as-raw(back-end, result, pointer);

end method op--unwrap-c-pointer;


define constant $integer-no-of-bits = 29;

define method make-mask(bit-offset :: <integer>, bit-size :: <integer>) => (mask)
  let no-of-bits = bit-offset + bit-size;
  if (no-of-bits > $integer-no-of-bits)
    generic-ash(generic-lognot(generic-ash(-1, bit-size)), bit-offset)
  else
    ash(lognot(ash(-1, bit-size)), bit-offset)
  end if;
end method;

define method make-word-mask(mask, no-of-bits :: <integer>) => (word-mask)
  if (no-of-bits > $integer-no-of-bits)
    generic-logand(mask, #xffffffff)
  else
    mask
  end if
end method;

define method op--at-field
    (back-end :: <harp-back-end>, result, pointer, byte-offset, 
     bit-offset :: <integer>, bit-size :: <integer>) => ()
  ins--ld(back-end, result, pointer, byte-offset);
  ins--lsr(back-end, result, result, bit-offset);
  ins--and(back-end, result, result,
	   make-word-mask(make-mask(0, bit-size), bit-size));
end method;

define method op--at-field-setter
    (back-end :: <harp-back-end>, result, new, pointer, byte-offset, 
     bit-offset :: <integer>, bit-size :: <integer>) => ()
  let old-word = make-n-register(back-end);
  let word = make-n-register(back-end);
  ins--and(back-end, word, new,
	   make-word-mask(make-mask(0, bit-size), bit-size));
  ins--asl(back-end, word, word, bit-offset);
  ins--ld(back-end, old-word, pointer, byte-offset);

  let no-of-bits = bit-offset + bit-size;
  let mask = 
    begin
      let mask = make-mask(bit-offset, bit-size);
      if (no-of-bits > $integer-no-of-bits)
	make-word-mask(generic-lognot(mask), no-of-bits)
      else
	lognot(mask)
      end if
    end;
  ins--and(back-end, old-word, old-word, mask);

  ins--or(back-end, word, word, old-word);
  ins--st(back-end, word, pointer, byte-offset);
  ins--move(back-end, result, word);
end method;


define method op--debug-message
    (back-end :: <harp-back-end>, result, debug-string, #rest arguments)

  apply(call-primitive,
	back-end,
	result,
	$primitive-debug-message.runtime-reference,
	debug-string,
	arguments.size,
	arguments);

end method;


/* THREADS PRIMITIVES */


define open generic op--wait-for-simple-lock(back-end :: <harp-back-end>, result, lock, #key zmilsecs);
define open generic op--wait-for-simple-lock-timed(back-end :: <harp-back-end>, result, lock, zmilsecs);
define open generic op--release-simple-lock(back-end :: <harp-back-end>, result, lock);
define open generic op--wait-for-recursive-lock(back-end :: <harp-back-end>, result, lock, #key zmilsecs);
define open generic op--wait-for-recursive-lock-timed(back-end :: <harp-back-end>, result, lock, zmilsecs);
define open generic op--release-recursive-lock(back-end :: <harp-back-end>, result, lock);
define open generic op--write-thread-variable(back-end :: <harp-back-end>, result, variable-handle, new-value);
define open generic op--read-thread-variable(back-end :: <harp-back-end>, result, variable-handle);

define open generic op--allocation-count(back-end :: <harp-back-end>, result) => ();
define open generic op--initialize-allocation-count(back-end :: <harp-back-end>, result) => ();


define macro harp-transcendental-function-definer
  { define harp-transcendental-function ?:name}
    =>
  {
    define open generic "op--" ## ?name(be :: <harp-back-end>, #rest args) => ();

    define method "op--" ## ?name(be :: <harp-back-end>, #rest args) => ()
      apply("ins--" ## ?name, be, args);
    end method;
  }

  { define harp-transcendental-function (?:name, ?names:*) }
    =>
  {
   define harp-transcendental-function ?name;
   define harp-transcendental-function (?names)
  }

  { define harp-transcendental-function () }  => {}
end macro;

define harp-transcendental-function
  (floge, fetox, fsin, fcos, ftan, fasin, facos, fatan, dloge, detox, dsin, dcos, dtan, dasin, dacos, datan);


/* ALLOCATION PRIMITIVES */

define method op--allocate-filled
    (type :: <symbol>) => (op :: <function>)
  method
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, rep-fill) => ()

  let ($alloc-rf, $alloc-s-rf, sizeof%) =
    select (type)
      #"dylan-object" =>
	values($primitive-alloc-rf, $primitive-alloc-s-rf, bytes%);
      #"raw-half-word" =>
	values($primitive-alloc-leaf-rhf, $primitive-alloc-s-rhf, half%);
      #"raw-double-word" =>
	values($primitive-alloc-leaf-rdwf, $primitive-alloc-s-rdwf, double%);
      #"raw-single-float" =>
	values($primitive-alloc-leaf-rsff, $primitive-alloc-s-rsff, bytes%);
      #"raw-double-float" =>
	values($primitive-alloc-leaf-rdff, $primitive-alloc-s-rdff, double%);
      #"raw-object" =>
	values($primitive-alloc-leaf-rf, $primitive-alloc-s-rf, bytes%);
    end select;
  let byte-size =
    op--add(back-end, #f,
	    bytes%(back-end, word-size),
	    sizeof%(back-end, rep-size));
  let (allocator, args) =
    if (rep-slot == 0)
      select (num-fixed)
	0 => 
	  values($primitive-alloc, #[]);
	1 => 
	  values($primitive-alloc-s1, vector(fill));
	2 => 
	  values($primitive-alloc-s2, vector(fill, fill));
	otherwise =>
	  values($primitive-alloc-s, vector(num-fixed, fill));
      end select;
    else
      select (num-fixed)
	0 => 
	  values($alloc-rf, vector(rep-size, rep-slot, rep-fill));
	otherwise =>
	  values($alloc-s-rf, vector(num-fixed, fill, rep-size, rep-slot, rep-fill));
      end select;
    end if;
  apply(call-c-primitive, back-end, result, allocator, byte-size, wrapper, args);

  end method;
end method;


define method op--allocate-exact-awl
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, assoc) => ()
  op--allocate-awl(back-end, result, word-size, wrapper, 
		   num-fixed, fill, rep-size, rep-slot, assoc, #t);
end method;

define method op--allocate-weak-awl
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, assoc) => ()
  op--allocate-awl(back-end, result, word-size, wrapper, 
		   num-fixed, fill, rep-size, rep-slot, assoc, #f);
end method;


define method op--allocate-awl
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, assoc, exact? :: <boolean>) => ()

  let byte-size = bytes%(back-end, word-size);
  let (allocator, args) =
    if (num-fixed == 1)
      // The only slot is the assoc slot - so only fill the repeated data
      values(if (exact?) 
	       $primitive-alloc-exact-awl-rf 
	     else $primitive-alloc-weak-awl-rf 
	     end if,
	     vector(rep-size, rep-slot, fill));
    else
      values(if (exact?) 
	       $primitive-alloc-exact-awl-s-r 
	     else $primitive-alloc-weak-awl-s-r 
	     end if,
	     vector(num-fixed, fill, rep-size, rep-slot));
    end if;
  apply(call-c-primitive, back-end, result, allocator, byte-size, wrapper, assoc, args);
end method;


// Byte allocators:
// Assume that these can be allocated in the leaf pool. This is essential for
// strings. If we ever need an object with both traceable slots and a byte 
// repeated slot, then we'll need additional allocation primitives.


define method op--byte-allocate-leaf-filled
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, rep-fill) => ()
  // NB: No zero termination -- it's not used for strings.

  let tot-size = op--round-up-to-word(back-end, 
				      op--add(back-end, #f, 
					      bytes%(back-end, word-size), 
					      rep-size));
  if (rep-slot == 0)
    call-c-primitive(back-end, result, $primitive-alloc-leaf-s, tot-size, wrapper, 
		     num-fixed, fill);
  else
    call-c-primitive(back-end, result, $primitive-alloc-leaf-s-rbf, tot-size, wrapper, 
		     num-fixed, fill, rep-size, rep-slot, rep-fill);
  end if;
end method;


define method op--byte-allocate-leaf-filled-terminated
    (back-end :: <harp-back-end>, result, word-size, byte-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot) => ()

  let tot-size = op--round-up-to-word(back-end, 
				      op--add(back-end, #f, 
					      bytes%(back-end, word-size), 
					      byte-size));
  let byte-fill = op--raw(back-end, #f, fill);
  let (allocator, args) =
    select (num-fixed)
      0 =>   // optimize this case for strings
	values($primitive-alloc-leaf-rbfz, 
	       vector(rep-size, rep-slot, byte-fill));
      otherwise =>
	values($primitive-alloc-leaf-s-rbfz, 
	       vector(num-fixed, fill, rep-size, rep-slot, byte-fill));
    end select;
  apply(call-c-primitive, back-end, result, allocator, tot-size, wrapper, args);
end method;

define method op--byte-allocate-filled
    (back-end :: <harp-back-end>, result, word-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot, rep-fill) => ()
  // NB: No zero termination -- it's not used for strings.

  let tot-size = op--round-up-to-word(back-end, 
				      op--add(back-end, #f, 
					      bytes%(back-end, word-size), 
					      rep-size));
  if (rep-slot == 0)
    call-c-primitive(back-end, result, $primitive-alloc-s, tot-size, wrapper, 
		     num-fixed, fill);
  else
    call-c-primitive(back-end, result, $primitive-alloc-s-rbf, tot-size, wrapper, 
		     num-fixed, fill, rep-size, rep-slot, rep-fill);
  end if;
end method;


define method op--byte-allocate-filled-terminated
    (back-end :: <harp-back-end>, result, word-size, byte-size, wrapper, 
     num-fixed, fill, rep-size, rep-slot) => ()

  let tot-size = op--round-up-to-word(back-end, 
				      op--add(back-end, #f, 
					      bytes%(back-end, word-size), 
					      byte-size));
  let byte-fill = op--raw(back-end, #f, fill);
  let (allocator, args) =
    select (num-fixed)
      0 =>   // optimize this case; leaf pool if no fixed slots
	values($primitive-alloc-leaf-rbfz, 
	       vector(rep-size, rep-slot, byte-fill));
      otherwise =>
	values($primitive-alloc-s-rbfz, 
	       vector(num-fixed, fill, rep-size, rep-slot, byte-fill));
    end select;
  apply(call-c-primitive, back-end, result, allocator, tot-size, wrapper, args);
end method;


define method op--round-up-to-word
    (back-end :: <harp-back-end>, val) => (aligned-val) 
  let res = make-n-register(back-end);
  ins--add(back-end, res, val, 3);
  ins--and(back-end, res, res, ash(-1, 2));
  res;
end method;

define method op--round-up-to-word
    (back-end :: <harp-back-end>, val :: <integer>) => (aligned-val) 
  logand(val + 3, ash(-1, 2));
end method;


/* MAPPINGS */


define function &call-primitive(primitive :: <string>, reference) => (call-primitive :: <function>)
  let primitive-name =
    reference
    | make-runtime-reference(harp-raw-mangle(as-lowercase(primitive)));

  method(back-end :: <harp-back-end>, result, #rest arguments) => ()
      apply(call-primitive, back-end, result, primitive-name, arguments);
  end method;

end function;

define constant $primitive-descriptors = make(<object-table>);

define function install-primitive-descriptor (name, descriptor)
  $primitive-descriptors[name] := descriptor;
end function;

define inline function lookup-primitive-descriptor (name) => (res :: <primitive-descriptor>)
  $primitive-descriptors[name]
end function;

define macro &primitive-descriptor-definer
  { define &primitive-descriptor ?:name, 
      #key ?emitter:expression = #f,
           ?mapping:expression = #f,
           ?reference:expression = #f}
    => { define constant ?name ## "-descriptor" =
           begin
             let emitter = ?emitter | &call-primitive(?"name", ?reference);
	     make(<primitive-descriptor>, emitter: emitter);
           end;
         install-primitive-descriptor(?#"name", ?name ## "-descriptor"); }
end macro;

define macro &local-primitive-descriptor-definer
  { define &local-primitive-descriptor ?:name, 
      #key ?emitter:expression = #f}
    => { define constant ?name ## "-descriptor" =
           begin
             let emitter = ?emitter | &call-primitive(?"name", #f);
	     make(<primitive-descriptor>, emitter: emitter);
           end; }
end macro;

define macro &constant-primitive-descriptor-definer
  { define &constant-primitive-descriptor ?:name = ?primitive:* }
    => { define constant ?name ## "-descriptor" =
           ?primitive ## "-descriptor";
         install-primitive-descriptor(?#"name", ?name ## "-descriptor"); }
end macro;

define function &call-c-primitive(primitive :: <string>) => (call-primitive :: <function>)
  let primitive-name =
    make-c-runtime-reference(harp-raw-mangle(as-lowercase(primitive)));

  method(back-end :: <harp-back-end>, result, #rest arguments) => ()
      apply(call-c-primitive, back-end, result, primitive-name, arguments);
  end method;

end function;

define macro &c-primitive-descriptor-definer
  { define &c-primitive-descriptor ?:name, 
      #key ?emitter:expression = #f,
           ?mapping:expression = #f}
    => { define constant ?name ## "-descriptor" =
           begin
             let emitter =  ?emitter | &call-c-primitive(?"name");
	     make(<primitive-descriptor>, emitter: emitter);
           end;
         install-primitive-descriptor(?#"name", ?name ## "-descriptor"); }
end macro;


define inline method named-emitter (name :: <symbol>) => (emitter :: <function>)
  let descriptor = lookup-primitive-descriptor(name);
  primitive-emitter(descriptor)
end method;

define method emitter (primitive :: <&primitive>) => (emitter :: <function>)
  let name = primitive.primitive-descriptor-getter-name;
  named-emitter(name)
end method;

// Support
define &primitive-descriptor primitive-break;
//define &constant-primitive-descriptor primitive-invoke-debugger = primitive-break;
define &primitive-descriptor primitive-invoke-debugger;
define &primitive-descriptor primitive-inside-debugger?;
define &primitive-descriptor primitive-debug-message, emitter: op--debug-message;

// Machine
define &primitive-descriptor primitive-word-size, emitter: op--word-size;
define &primitive-descriptor primitive-header-size, emitter: op--word-size;
    
// Allocation.
define &primitive-descriptor primitive-allocate;
define &primitive-descriptor primitive-allocate-wrapper;
define &primitive-descriptor primitive-byte-allocate-filled, emitter: op--byte-allocate-filled;
define &primitive-descriptor primitive-byte-allocate-filled-terminated, emitter: op--byte-allocate-filled-terminated;
define &primitive-descriptor primitive-byte-allocate-leaf-filled, emitter: op--byte-allocate-leaf-filled;
define &primitive-descriptor primitive-byte-allocate-leaf-filled-terminated, emitter: op--byte-allocate-leaf-filled-terminated;
define &primitive-descriptor primitive-allocate-in-awl-pool, emitter: op--allocate-exact-awl;
define &primitive-descriptor primitive-allocate-weak-in-awl-pool, emitter: op--allocate-weak-awl;
define &primitive-descriptor primitive-object-allocate-filled, emitter: op--allocate-filled(#"dylan-object");
define &primitive-descriptor primitive-double-byte-allocate-filled, emitter: op--allocate-filled(#"raw-half-word");
define &primitive-descriptor primitive-word-allocate-filled, emitter: op--allocate-filled(#"raw-object");
define &primitive-descriptor primitive-double-word-allocate-filled, emitter: op--allocate-filled(#"raw-double-word");
define &primitive-descriptor primitive-single-float-allocate-filled, emitter: op--allocate-filled(#"raw-single-float");
define &primitive-descriptor primitive-double-float-allocate-filled, emitter: op--allocate-filled(#"raw-double-float");
define &primitive-descriptor primitive-untraced-allocate;
define &primitive-descriptor primitive-manual-allocate;
define &primitive-descriptor primitive-manual-free;
define &c-primitive-descriptor primitive-set-class-breakpoint;
define &c-primitive-descriptor primitive-clear-class-breakpoint;
define &c-primitive-descriptor primitive-display-class-breakpoints;

// Accessors
define &primitive-descriptor primitive-element, emitter: op--load-index;
define &primitive-descriptor primitive-element-setter, emitter: op--store-index;
define &primitive-descriptor primitive-byte-element, emitter: op--byte-element;
define &primitive-descriptor primitive-byte-element-setter, emitter: op--byte-element-setter;
define &primitive-descriptor primitive-bit-element, emitter: op--bit-element;
define &primitive-descriptor primitive-bit-element-setter, emitter: op--bit-element-setter;
define &primitive-descriptor primitive-bit-field, emitter: op--load-bit-field;
define &primitive-descriptor primitive-bit-field-setter, emitter: op--store-bit-field;
define &primitive-descriptor primitive-fill!, emitter: op--fill!;
define &primitive-descriptor primitive-fill-bytes!, emitter: op--fill-bytes!;
define &primitive-descriptor primitive-replace!, emitter: op--replace!;
define &primitive-descriptor primitive-replace-bytes!, emitter: op--replace-bytes!;

// GC
define &primitive-descriptor primitive-pin-object, emitter: ins--move;
define &primitive-descriptor primitive-unpin-object, emitter: op--ignore-result(ins--force-u);

define &primitive-descriptor primitive-allocation-count, emitter: op--allocation-count;
define &primitive-descriptor primitive-initialize-allocation-count, emitter: op--initialize-allocation-count;
define &c-primitive-descriptor primitive-begin-heap-alloc-stats;
define &c-primitive-descriptor primitive-end-heap-alloc-stats;

define &c-primitive-descriptor primitive-mps-finalize;
define &c-primitive-descriptor primitive-mps-finalization-queue-first;
define &c-primitive-descriptor primitive-mps-park;
define &c-primitive-descriptor primitive-mps-clamp;
define &c-primitive-descriptor primitive-mps-release;
define &c-primitive-descriptor primitive-mps-collect;
define &c-primitive-descriptor primitive-mps-collection-stats;
define &c-primitive-descriptor primitive-mps-enable-gc-messages;
define &c-primitive-descriptor primitive-mps-committed;

define &c-primitive-descriptor primitive-mps-begin-ramp-alloc;
define &c-primitive-descriptor primitive-mps-end-ramp-alloc;

define &c-primitive-descriptor primitive-mps-begin-ramp-alloc-all;
define &c-primitive-descriptor primitive-mps-end-ramp-alloc-all;

define &c-primitive-descriptor primitive-mps-ld-reset;
define &c-primitive-descriptor primitive-mps-ld-add;
define &c-primitive-descriptor primitive-mps-ld-merge;
define &c-primitive-descriptor primitive-mps-ld-isstale;

// Support for keyboard-break handling

define &c-primitive-descriptor primitive-keyboard-interrupt-signaled;
define &c-primitive-descriptor primitive-keyboard-interrupt-signaled-setter;
define &c-primitive-descriptor primitive-keyboard-interrupt-polling;
define &c-primitive-descriptor primitive-keyboard-interrupt-polling-setter;
define &c-primitive-descriptor primitive-keyboard-interrupt-polling-thread;
define &c-primitive-descriptor primitive-keyboard-interrupt-polling-thread-setter;

// DLL Support

define &primitive-descriptor primitive-runtime-module-handle, reference: $primitive-runtime-module-handle;

// Byte Character.
define &primitive-descriptor primitive-byte-character-as-raw, emitter: op--raw;
define &primitive-descriptor primitive-raw-as-byte-character, emitter: op--character;
define &primitive-descriptor primitive-unicode-character-as-raw, emitter: op--raw;
define &primitive-descriptor primitive-raw-as-unicode-character, emitter: op--unicode-character;

// Small integer.
    
// Big Integer.

// Machine Integer.

// Unsigned-Machine Integer.

// Address.

// Pointer.
define &primitive-descriptor primitive-cast-pointer-as-raw, emitter: ins--move;
define &primitive-descriptor primitive-cast-raw-as-pointer, emitter: ins--move;

// Machine-words
define &primitive-descriptor primitive-integer?, emitter: op--integer?;
define &primitive-descriptor primitive-machine-word-equal?, emitter: op--primitive-predicate(ins--beq);
define &primitive-descriptor primitive-machine-word-not-equal?, emitter: op--primitive-predicate(ins--bne);
define &primitive-descriptor primitive-machine-word-less-than?, emitter: op--primitive-predicate(ins--blt);
define &primitive-descriptor primitive-machine-word-greater-than?, emitter: op--primitive-predicate(ins--bgt);
define &primitive-descriptor primitive-machine-word-not-greater-than?, emitter: op--primitive-predicate(ins--ble);

define &primitive-descriptor primitive-wrap-machine-word;
define &primitive-descriptor primitive-unwrap-machine-word, emitter: op--as-raw;

define &primitive-descriptor primitive-box-integer, emitter: op--integer;
define &primitive-descriptor primitive-unbox-integer, emitter: op--raw;
define &primitive-descriptor primitive-cast-integer-as-raw, emitter: ins--move;
define &primitive-descriptor primitive-cast-raw-as-integer, emitter: ins--move;
define &primitive-descriptor primitive-wrap-abstract-integer;
define &primitive-descriptor primitive-wrap-unsigned-abstract-integer;
define &primitive-descriptor primitive-unwrap-abstract-integer;

define &primitive-descriptor primitive-machine-word-logand, emitter: ins--and;
define &primitive-descriptor primitive-machine-word-logior, emitter: ins--or;
define &primitive-descriptor primitive-machine-word-logxor, emitter: ins--eor;
define &primitive-descriptor primitive-machine-word-lognot, emitter: ins--not;
define &primitive-descriptor primitive-machine-word-logbit?, emitter: op--logbit?;
define &primitive-descriptor primitive-machine-word-logbit-set, emitter: op--logbit-set;
define &primitive-descriptor primitive-machine-word-logbit-clear, emitter: op--logbit-clear;

define &primitive-descriptor primitive-machine-word-count-low-zeros;
define &primitive-descriptor primitive-machine-word-count-high-zeros;

define &primitive-descriptor primitive-machine-word-add, emitter: ins--add;
define &primitive-descriptor primitive-machine-word-add-with-overflow, emitter: op--do-with-overflow(ins--addv);

define &primitive-descriptor primitive-machine-word-subtract, emitter: ins--sub;
define &primitive-descriptor primitive-machine-word-subtract-with-overflow, emitter: op--do-with-overflow(ins--subv);

define &primitive-descriptor primitive-machine-word-multiply-low, emitter: ins--muls;
define &primitive-descriptor primitive-machine-word-multiply-high, emitter: op--mulxh;
define &primitive-descriptor primitive-machine-word-multiply-low/high, emitter: ins--mulx;
define &primitive-descriptor primitive-machine-word-multiply-low-with-overflow, emitter: op--do-with-overflow(ins--mulv);
define &primitive-descriptor primitive-machine-word-multiply-with-overflow, emitter: op--do-with-overflow2(ins--mulxv);

define &primitive-descriptor primitive-machine-word-negative, emitter: op--integer-negate;
define &primitive-descriptor primitive-machine-word-negative-with-overflow, emitter: op--do-with-overflow(op--integer-negatev);

define &primitive-descriptor primitive-machine-word-abs, emitter: op--abs;
define &primitive-descriptor primitive-machine-word-abs-with-overflow, emitter: op--do-with-overflow(op--absv);

define &primitive-descriptor primitive-machine-word-floor/-quotient, emitter: op--floorxq;
define &primitive-descriptor primitive-machine-word-floor/-remainder, emitter: op--floorxr;
define &primitive-descriptor primitive-machine-word-floor/, emitter: ins--floorx;

define &primitive-descriptor primitive-machine-word-ceiling/-quotient, emitter: op--ceilingxq;
define &primitive-descriptor primitive-machine-word-ceiling/-remainder, emitter: op--ceilingxr;
define &primitive-descriptor primitive-machine-word-ceiling/, emitter: ins--ceilingx;

define &primitive-descriptor primitive-machine-word-round/-quotient, emitter: op--roundxq;
define &primitive-descriptor primitive-machine-word-round/-remainder, emitter: op--roundxr;
define &primitive-descriptor primitive-machine-word-round/, emitter: ins--roundx;

define &primitive-descriptor primitive-machine-word-truncate/-quotient, emitter: op--truncatexq;
define &primitive-descriptor primitive-machine-word-truncate/-remainder, emitter: op--truncatexr;
define &primitive-descriptor primitive-machine-word-truncate/, emitter: ins--truncatex;

define &primitive-descriptor primitive-machine-word-divide-quotient, emitter: ins--divs;
define &primitive-descriptor primitive-machine-word-divide-remainder, emitter: ins--mods;
define &primitive-descriptor primitive-machine-word-divide, emitter: ins--divx;

define &primitive-descriptor primitive-machine-word-shift-left-low, emitter: ins--asl;
define &primitive-descriptor primitive-machine-word-shift-left-high, emitter: op--aslh;
define &primitive-descriptor primitive-machine-word-shift-left-low/high, emitter: ins--aslx;
define &primitive-descriptor primitive-machine-word-shift-left-low-with-overflow, emitter: op--do-with-overflow(op--aslxvl);
define &primitive-descriptor primitive-machine-word-shift-left-with-overflow, emitter: op--do-with-overflow2(ins--aslxv);
define &primitive-descriptor primitive-machine-word-shift-right, emitter: ins--asr;

define &primitive-descriptor primitive-machine-word-add-signal-overflow, emitter: ins--add-trap;
define &primitive-descriptor primitive-machine-word-subtract-signal-overflow, emitter: ins--sub-trap;
define &primitive-descriptor primitive-machine-word-multiply-signal-overflow, emitter: ins--muls-trap;
define &primitive-descriptor primitive-machine-word-negative-signal-overflow, emitter: op--neg-trap;
define &primitive-descriptor primitive-machine-word-abs-signal-overflow, emitter: op--abs-trap;
define &primitive-descriptor primitive-machine-word-shift-left-signal-overflow, emitter: ins--asl-trap;

define &primitive-descriptor primitive-machine-word-double-floor/-quotient, emitter: op--floorxxq;
define &primitive-descriptor primitive-machine-word-double-floor/-remainder, emitter: op--floorxxr;
define &primitive-descriptor primitive-machine-word-double-floor/, emitter: ins--floorxx;

define &primitive-descriptor primitive-machine-word-double-ceiling/-quotient, emitter: op--ceilingxxq;
define &primitive-descriptor primitive-machine-word-double-ceiling/-remainder, emitter: op--ceilingxxr;
define &primitive-descriptor primitive-machine-word-double-ceiling/, emitter: ins--ceilingxx;

define &primitive-descriptor primitive-machine-word-double-round/-quotient, emitter: op--roundxxq;
define &primitive-descriptor primitive-machine-word-double-round/-remainder, emitter: op--roundxxr;
define &primitive-descriptor primitive-machine-word-double-round/, emitter: ins--roundxx;

define &primitive-descriptor primitive-machine-word-double-truncate/-quotient, emitter: op--truncatexxq;
define &primitive-descriptor primitive-machine-word-double-truncate/-remainder, emitter: op--truncatexxr;
define &primitive-descriptor primitive-machine-word-double-truncate/, emitter: ins--truncatexx;

define &primitive-descriptor primitive-machine-word-double-divide-quotient, emitter: op--divxxq;
define &primitive-descriptor primitive-machine-word-double-divide-remainder, emitter: op--divxxr;
define &primitive-descriptor primitive-machine-word-double-divide, emitter: ins--divxx;

define &primitive-descriptor primitive-machine-word-unsigned-less-than?, emitter: op--primitive-predicate(ins--blo);
define &primitive-descriptor primitive-machine-word-unsigned-not-less-than?, emitter: op--primitive-predicate(ins--bhs);
define &primitive-descriptor primitive-machine-word-unsigned-greater-than?, emitter: op--primitive-predicate(ins--bhi);
define &primitive-descriptor primitive-machine-word-unsigned-not-greater-than?, emitter: op--primitive-predicate(ins--bls);

define &primitive-descriptor primitive-machine-word-unsigned-add-with-carry, emitter: ins--addcx;
define &primitive-descriptor primitive-machine-word-unsigned-subtract-with-borrow, emitter: ins--subcx;

define &primitive-descriptor primitive-machine-word-unsigned-multiply-high, emitter: op--muluxh;
define &primitive-descriptor primitive-machine-word-unsigned-multiply, emitter: ins--mulux;

define &primitive-descriptor primitive-machine-word-unsigned-divide-quotient, emitter: ins--divu;
define &primitive-descriptor primitive-machine-word-unsigned-divide-remainder, emitter: ins--modu;
define &primitive-descriptor primitive-machine-word-unsigned-divide, emitter: ins--divux;

define &primitive-descriptor primitive-machine-word-unsigned-rotate-left, emitter: ins--rol;
define &primitive-descriptor primitive-machine-word-unsigned-rotate-right, emitter: ins--ror;
define &primitive-descriptor primitive-machine-word-unsigned-shift-right, emitter: ins--lsr;

define &primitive-descriptor primitive-machine-word-unsigned-double-divide-quotient, emitter: op--divuxxq;
define &primitive-descriptor primitive-machine-word-unsigned-double-divide-remainder, emitter: op--divuxxr;
define &primitive-descriptor primitive-machine-word-unsigned-double-divide, emitter: ins--divuxx;

define &primitive-descriptor primitive-machine-word-unsigned-shift-left-high, emitter: op--lslxh;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left-high, emitter: op--lslxxh;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left, emitter: ins--lslxx;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-low, emitter: op--lsrxxl;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-high, emitter: op--lsrxxh;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right, emitter: ins--lsrxx;

define &primitive-descriptor primitive-machine-word-bit-field-extract, emitter: op--extract-bits;
define &primitive-descriptor primitive-machine-word-bit-field-deposit, emitter: op--set-bits;

// single-float
define &primitive-descriptor primitive-single-float-as-raw, emitter: op--sf-as-raw;
define &primitive-descriptor primitive-raw-as-single-float;
define &primitive-descriptor primitive-single-float-as-integer, emitter: op--convert-from-float(ins--convert-from-single-float);
define &primitive-descriptor primitive-integer-as-single-float, emitter: ins--convert-to-single-float;
define &primitive-descriptor primitive-single-float-as-double-integer, emitter: op--convert-from-float(ins--convert-from-single-float-x);
define &primitive-descriptor primitive-double-integer-as-single-float, emitter: ins--convert-to-single-float-x;
define &primitive-descriptor primitive-cast-single-float-as-machine-word, emitter: ins--move-from-sfreg;
define &primitive-descriptor primitive-cast-machine-word-as-single-float, emitter: ins--move-to-sfreg;
define &primitive-descriptor primitive-single-float-negate, emitter: op--single-float-negate;
define &primitive-descriptor primitive-single-float-add, emitter: ins--fadd;
define &primitive-descriptor primitive-single-float-subtract, emitter: ins--fsub;
define &primitive-descriptor primitive-single-float-multiply, emitter: ins--fmul;
define &primitive-descriptor primitive-single-float-divide, emitter: ins--fdiv;
define &primitive-descriptor primitive-single-float-equals?, emitter: op--primitive-predicate(ins--fbeq);
define &primitive-descriptor primitive-single-float-less-than?, emitter: op--primitive-predicate(ins--fblt);
define &primitive-descriptor primitive-single-float-sqrt, emitter: ins--fsqrt;
define &primitive-descriptor primitive-single-float-log, emitter: op--floge;
define &primitive-descriptor primitive-single-float-exp, emitter: op--fetox;
define &primitive-descriptor primitive-single-float-expt, emitter: op--single-float-expt;
define &primitive-descriptor primitive-single-float-sin, emitter: op--fsin;
define &primitive-descriptor primitive-single-float-cos, emitter: op--fcos;
define &primitive-descriptor primitive-single-float-tan, emitter: op--ftan;
define &primitive-descriptor primitive-single-float-asin, emitter: op--fasin;
define &primitive-descriptor primitive-single-float-acos, emitter: op--facos;
define &primitive-descriptor primitive-single-float-atan, emitter: op--fatan;

// double-float
define &primitive-descriptor primitive-double-float-as-raw, emitter: op--df-as-raw;
define &primitive-descriptor primitive-raw-as-double-float;
define &primitive-descriptor primitive-double-float-as-integer, emitter: op--convert-from-float(ins--convert-from-double-float);
define &primitive-descriptor primitive-integer-as-double-float, emitter: ins--convert-to-double-float;
define &primitive-descriptor primitive-double-float-as-double-integer, emitter: op--convert-from-float(ins--convert-from-double-float-x);
define &primitive-descriptor primitive-double-integer-as-double-float, emitter: ins--convert-to-double-float-x;
define &primitive-descriptor primitive-cast-double-float-as-machine-words, emitter: ins--move-from-dfreg;
define &primitive-descriptor primitive-cast-machine-words-as-double-float, emitter: ins--move-to-dfreg;
define &primitive-descriptor primitive-double-float-negate, emitter: op--double-float-negate;
define &primitive-descriptor primitive-double-float-add, emitter: ins--dadd;
define &primitive-descriptor primitive-double-float-subtract, emitter: ins--dsub;
define &primitive-descriptor primitive-double-float-multiply, emitter: ins--dmul;
define &primitive-descriptor primitive-double-float-divide, emitter: ins--ddiv;
define &primitive-descriptor primitive-double-float-equals?, emitter: op--primitive-predicate(ins--dbeq);
define &primitive-descriptor primitive-double-float-less-than?, emitter: op--primitive-predicate(ins--dblt);
define &primitive-descriptor primitive-double-float-sqrt, emitter: ins--dsqrt;
define &primitive-descriptor primitive-double-float-log, emitter: op--dloge;
define &primitive-descriptor primitive-double-float-exp, emitter: op--detox;
define &primitive-descriptor primitive-double-float-expt, emitter: op--double-float-expt;
define &primitive-descriptor primitive-double-float-sin, emitter: op--dsin;
define &primitive-descriptor primitive-double-float-cos, emitter: op--dcos;
define &primitive-descriptor primitive-double-float-tan, emitter: op--dtan;
define &primitive-descriptor primitive-double-float-asin, emitter: op--dasin;
define &primitive-descriptor primitive-double-float-acos, emitter: op--dacos;
define &primitive-descriptor primitive-double-float-atan, emitter: op--datan;

// float conversions
define &primitive-descriptor primitive-single-float-as-double, emitter: ins--single-to-double-float;
define &primitive-descriptor primitive-double-float-as-single, emitter: ins--double-to-single-float;

// Checks
define &primitive-descriptor primitive-instance?, emitter: emit-instance-check;
define &primitive-descriptor primitive-type-check, reference: $primitive-type-check;
define &primitive-descriptor primitive-range-check, emitter: op--range-check;

// Comparisons.
define &primitive-descriptor primitive-raw-as-boolean, emitter: op--raw-as-boolean;
define &primitive-descriptor primitive-boolean-as-raw, emitter: op--boolean-as-raw;
define &primitive-descriptor primitive-as-boolean, emitter: op--true?;
define &primitive-descriptor primitive-not, emitter: op--false?;
define &primitive-descriptor primitive-id?, emitter: op--primitive-predicate(ins--beq);
define &primitive-descriptor primitive-not-id?, emitter: op--primitive-predicate(ins--bne);
define &primitive-descriptor primitive-compare-bytes, emitter: op--compare-bytes;
define &primitive-descriptor primitive-compare-words, emitter: op--compare-words;

// Repeated Slots.
define &primitive-descriptor primitive-repeated-slot-as-raw, emitter: op--repeated-slot-as-raw;
define &primitive-descriptor primitive-repeated-slot-offset, emitter: op--repeated-slot-offset;

// Vector.
define &primitive-descriptor primitive-vector, reference: $primitive-vector;
define &primitive-descriptor primitive-copy-vector;
define &primitive-descriptor primitive-vector-element, emitter: op--vector-element;
define &primitive-descriptor primitive-vector-element-setter, emitter: op--vector-element-setter;
define &primitive-descriptor primitive-vector-size, emitter: op--vector-size;
define &primitive-descriptor primitive-vector-as-raw, emitter: op--vector-as-raw;
define &primitive-descriptor primitive-raw-as-vector;

// String.
define &primitive-descriptor primitive-strlen;
define &primitive-descriptor primitive-string-as-raw, emitter: op--string-as-raw;
define &primitive-descriptor primitive-raw-as-string;

// Instance.
define &primitive-descriptor primitive-object-class, emitter: op--object-class;
define &primitive-descriptor primitive-slot-value, emitter: op--slot-element;
define &primitive-descriptor primitive-slot-value-setter, emitter: op--slot-element-setter;
define &primitive-descriptor primitive-repeated-slot-value, emitter: op--repeated-slot-element;
define &primitive-descriptor primitive-repeated-slot-value-setter, emitter: op--repeated-slot-element-setter;
define &primitive-descriptor primitive-initialized-slot-value, emitter: op--init-slot-element;

// Calling Convention.
define &primitive-descriptor primitive-function-parameter, emitter: op--function-parameter;
define &primitive-descriptor primitive-next-methods-parameter, emitter: op--next-methods-parameter;
define &primitive-descriptor primitive-set-generic-function-entrypoints;
define &primitive-descriptor primitive-set-accessor-method-xep;
define &primitive-descriptor primitive-callable-as-engine-node?, emitter: op--true;

// Apply.
define &primitive-descriptor primitive-xep-apply;
define &primitive-descriptor primitive-mep-apply, reference: $primitive-mep-apply;
define &primitive-descriptor primitive-mep-apply-with-optionals, reference: $primitive-mep-apply-with-optionals;
define &primitive-descriptor primitive-engine-node-apply-with-optionals;
define &primitive-descriptor primitive-iep-apply;
define &primitive-descriptor primitive-apply, reference: $primitive-apply;

// Discriminator/Engine-node Initialization
define &primitive-descriptor primitive-initialize-engine-node;
define &primitive-descriptor primitive-initialize-discriminator;

// Multiple-Values.
define &primitive-descriptor primitive-values,
                             emitter: method(back-end, result, values)
					  op--mv-set-rest-at(back-end, result, values, 0);
				      end method;

// Symbol boot.
define &primitive-descriptor primitive-resolve-symbol;
define &primitive-descriptor primitive-string-as-symbol;
define &primitive-descriptor primitive-preboot-symbols;

// Terminal

// Operating System
define &primitive-descriptor primitive-exit-application;


define macro at-primitive-definer
  {define at-primitive ?:name, 
      #key ?load:expression = #f,  ?store:expression = #f }
    => { define &primitive-descriptor "primitive-" ## ?name ## "-at", emitter: ?load;
         define &primitive-descriptor "primitive-" ## ?name ## "-at-setter", emitter: ?store
       }
end;

/*
define at-primitive single-float,   load: op--load-float-index,   store: op--store-float-index;
define at-primitive double-float,   load: op--load-dfloat-index,   store: op--store-dfloat-index;
define at-primitive extended-float, load: op--load-dfloat-index,   store: op--store-dfloat-index;
*/

// C ffi types

define at-primitive c-unsigned-char,   load: op--load-byte-index,   store: op--store-byte-index;
define at-primitive c-signed-char,     load: op--load-signed-byte-index,   store: op--store-signed-byte-index;
define at-primitive c-char,            load: op--load-byte-index,   store: op--store-byte-index;
define at-primitive c-unsigned-short,  load: op--load-half-index,   store: op--store-half-index;
define at-primitive c-signed-short,    load: op--load-signed-half-index,   store: op--store-signed-half-index;
define at-primitive c-unsigned-long,   load: op--load-index,   store: op--store-index;
define at-primitive c-signed-long,     load: op--load-index,   store: op--store-index;
define at-primitive c-unsigned-int,    load: op--load-index,   store: op--store-index;
define at-primitive c-signed-int,      load: op--load-index,   store: op--store-index;
define at-primitive c-float,           load: op--load-float-index,     store: op--store-float-index;
define at-primitive c-double,          load: op--load-dfloat-index,     store: op--store-dfloat-index;
define at-primitive c-long-double,     load: op--load-dfloat-index,     store: op--store-dfloat-index;
define at-primitive c-pointer,         load: op--load-index,   store: op--store-index;
define at-primitive c-size-t,          load: op--load-index,   store: op--store-index;
define at-primitive c-ssize-t,         load: op--load-index,   store: op--store-index;

define macro c-primitive-field-definer
  {define c-primitive-field ?:name }
    => { define &primitive-descriptor "primitive-" ## ?name ## "-field", emitter: op--at-field;
         define &primitive-descriptor
             "primitive-" ## ?name ## "-field-setter", emitter: op--at-field-setter
       }
end;

define c-primitive-field c-unsigned;
define c-primitive-field c-signed;
define c-primitive-field c-int;

define &primitive-descriptor primitive-unwrap-c-pointer, emitter: op--unwrap-c-pointer;
define &primitive-descriptor primitive-wrap-c-pointer;


// C Thread Primitives

define &c-primitive-descriptor primitive-make-thread;
define &c-primitive-descriptor primitive-destroy-thread;
define &c-primitive-descriptor primitive-thread-join-single;
define &c-primitive-descriptor primitive-thread-join-multiple;
define &c-primitive-descriptor primitive-thread-yield;
define &c-primitive-descriptor primitive-current-thread;
define &c-primitive-descriptor primitive-wait-for-simple-lock, emitter: op--wait-for-simple-lock;
define &c-primitive-descriptor primitive-wait-for-recursive-lock, emitter: op--wait-for-recursive-lock;
define &c-primitive-descriptor primitive-wait-for-semaphore;
define &c-primitive-descriptor primitive-wait-for-notification;
define &c-primitive-descriptor primitive-wait-for-simple-lock-timed, emitter: op--wait-for-simple-lock-timed;
define &c-primitive-descriptor primitive-wait-for-recursive-lock-timed, emitter: op--wait-for-recursive-lock-timed;
define &c-primitive-descriptor primitive-wait-for-semaphore-timed;
define &c-primitive-descriptor primitive-wait-for-notification-timed;
define &c-primitive-descriptor primitive-release-simple-lock, emitter: op--release-simple-lock;
define &c-primitive-descriptor primitive-release-recursive-lock, emitter: op--release-recursive-lock;
define &c-primitive-descriptor primitive-release-semaphore;
define &c-primitive-descriptor primitive-release-notification;
define &c-primitive-descriptor primitive-release-all-notification;
define &c-primitive-descriptor primitive-make-recursive-lock;
define &c-primitive-descriptor primitive-destroy-recursive-lock;
define &c-primitive-descriptor primitive-make-simple-lock;
define &c-primitive-descriptor primitive-destroy-simple-lock;
define &c-primitive-descriptor primitive-owned-simple-lock;
define &c-primitive-descriptor primitive-owned-recursive-lock;
define &c-primitive-descriptor primitive-make-semaphore;
define &c-primitive-descriptor primitive-destroy-semaphore;
define &c-primitive-descriptor primitive-make-notification;
define &c-primitive-descriptor primitive-destroy-notification;
define &c-primitive-descriptor primitive-sleep;
/*
define &c-primitive-descriptor primitive-assign-atomic-memory;
define &c-primitive-descriptor primitive-conditional-update-memory;
*/
define &c-primitive-descriptor primitive-allocate-thread-variable;
define &c-primitive-descriptor primitive-read-thread-variable, emitter: op--read-thread-variable;
define &c-primitive-descriptor primitive-write-thread-variable, emitter: op--write-thread-variable;
define &c-primitive-descriptor primitive-initialize-current-thread;
define &c-primitive-descriptor primitive-detach-thread;
define &c-primitive-descriptor primitive-initialize-special-thread;
define &c-primitive-descriptor primitive-unlock-simple-lock;
define &c-primitive-descriptor primitive-unlock-recursive-lock;

define &primitive-descriptor primitive-sequence-point, emitter: op--nop;
define &primitive-descriptor primitive-synchronize-side-effects, emitter: op--nop;
