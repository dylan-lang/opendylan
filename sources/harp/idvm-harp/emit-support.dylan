module:    idvm-harp
Synopsis:  instruction emission support for the IDVM backend
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The high level instruction definitions for the IDVM all 
/// actually do their work by calling the generic functions in
/// this file. The idea is that the choice of emission (e.g. via 
/// DOSS, or an assembler) can be made by specializing these
/// functions, along with the assembler support functions.



/// The top-level emission functions


/// Emitting opcodes

define method emit-idvm-opcode
     (be :: <idvm-back-end>, opcode :: <idvm-opcode>) => ()
  be.opcode-offset := 0;
  emit-1(be, opcode);
end method;



/// Emitting full-word operands

define method emit-idvm-operand 
    (be :: <idvm-back-end>, operand :: <object>) => ()
  increment-opcode-offset(be);
  emit-1(be, convert-operand(be, operand));
end method;

define method emit-idvm-operand
     (be :: <idvm-back-end>, operand :: <constant-reference>) => ()
  increment-opcode-offset(be);
  // emit-constant-ref(be, operand);
  // For IDVM, we just put the <constant-reference> directly 
  // into the code stream, for DOSS dumping.
  emit-1(be, operand);
end method;

define method emit-idvm-operand
    (be :: <idvm-back-end>, operand :: <tag>) => ()
  increment-opcode-offset(be);
  emit-sdi(be, make-normal-sdi(be, operand));
end method;



/// Emitting HILO encoded operands


// By default, for a hilo encoding we emit an <idvm-hilo> into the code stream

define method emit-idvm-hilo
    (be :: <idvm-back-end>, hi, lo) => ()
  emit-1(be, make-idvm-hilo(be, hi, lo));
end method;

// But if the hilo encoding contains a SDI, then we have to put
// the SDI into the code stream, and place an <idvm-hilo> object in
// the SDI's code-fragment slot. The <idvm-hilo> object initially
// refers to the <tag> - but this will be updated when the SDI is
// resolved.

define method emit-idvm-hilo
    (be :: <idvm-back-end>, hi :: <tag>, lo) => ()
  let hilo = make-idvm-hilo(be, hi, lo);
  let sdi =  make-hi-hilo-sdi(be, hi, hilo);
  emit-sdi(be, sdi);
end method;

define method emit-idvm-hilo
    (be :: <idvm-back-end>, hi, lo :: <tag>) => ()
  let hilo = make-idvm-hilo(be, hi, lo);
  let sdi =  make-lo-hilo-sdi(be, lo, hilo);
  emit-sdi(be, sdi);
end method;

// Some instructions (e.g. unwind-protect) encode 2 SDIs into
// a single <idvm-hilo> field. In this case, the first SDI
// returns a code-size of 0.

define method emit-idvm-hilo
    (be :: <idvm-back-end>, hi :: <tag>, lo :: <tag>) => ()
  let hilo = make-idvm-hilo(be, hi, lo);
  let sdi1 = make-lo-of-two-hilo-sdi(be, lo, hilo);
  let sdi2 = make-hi-hilo-sdi(be, hi, hilo);
  emit-sdi(be, sdi1);
  emit-sdi(be, sdi2);
end method;


define method make-idvm-hilo
    (be :: <idvm-back-end>, hi, lo) => (res :: <idvm-hilo>)
  increment-opcode-offset(be);
  make(<idvm-hilo>, hi: convert-operand(be, hi), lo: convert-operand(be, lo));
end method;




/// Operand conversion


// convert-operand allows for registers where we require the locals index

define method convert-operand
    (be :: <idvm-back-end>, operand :: <spill>) => (res :: <integer>)
  local-index(be, operand);
end method;

define method convert-operand
    (be :: <idvm-back-end>, operand == env) => (res :: <integer>)
  local-index(be, operand);
end method;

define method convert-operand
    (be :: <idvm-back-end>, operand) => (res :: <object>)
  operand;
end method;




/// SDI support

// Normal-sdi is the used for unencoded spans
//
define method normal-sdi
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>) => (res)
  if (codep)
    list(span);
  else
    1;
  end if;
end method;

// For spans encoded in HILO objects, when the SDI finally gets resolved, 
// we update the HILO object itself so that it no longer circularly refers
// to the SDI, but contains the span instead. Unless the SDI corresponds to 
// the first of two encodings in a HILO object, we return the HILO object 
// itself as the code.


// hi-hilo-sdi is used for spans encoded in the hi field of a HILO
//
define method hi-hilo-sdi
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>) => (res)
  if (codep)
    let hilo :: <idvm-hilo> = self.new-sdi-code-fragment;
    hilo.idvm-hi := span;    // replace the lo field with the span
    list(hilo);
  else
    1;
  end if;
end method;

// lo-hilo-sdi is used for spans encoded in the lo field of a HILO
//
define method lo-hilo-sdi
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>) => (res)
  if (codep)
    let hilo :: <idvm-hilo> = self.new-sdi-code-fragment;
    hilo.idvm-lo := span;    // replace the lo field with the span
    list(hilo);
  else
    1;
  end if;
end method;

// lo-of-two-hilo-sdi is used for spans encoded in the lo field of a HILO
// which has two spans
//
define method lo-of-two-hilo-sdi
    (self :: <new-sdi>, span :: <integer>, codep :: <boolean>) => (res)
  if (codep)
    let hilo :: <idvm-hilo> = self.new-sdi-code-fragment;
    hilo.idvm-lo := span;    // replace the lo field with the span
    #();
  else
    0;
  end if;
end method;


define constant normal-sdi-methods         = vector(normal-sdi);
define constant hi-hilo-sdi-methods        = vector(hi-hilo-sdi);
define constant lo-hilo-sdi-methods        = vector(lo-hilo-sdi);
define constant lo-of-two-hilo-sdi-methods = vector(lo-of-two-hilo-sdi);


define method make-normal-sdi 
    (be :: <idvm-back-end>, operand :: <tag>) => (res :: <new-sdi>)
  make-general-sdi(be, operand, normal-sdi-methods, 1, #f);
end method;

define method make-hi-hilo-sdi 
    (be :: <idvm-back-end>, operand :: <tag>, hilo :: <idvm-hilo>) 
    => (res :: <new-sdi>)
  make-general-sdi(be, operand, hi-hilo-sdi-methods, 1, hilo);
end method;

define method make-lo-hilo-sdi 
    (be :: <idvm-back-end>, operand :: <tag>, hilo :: <idvm-hilo>) 
    => (res :: <new-sdi>)
  make-general-sdi(be, operand, lo-hilo-sdi-methods, 1, hilo);
end method;

define method make-lo-of-two-hilo-sdi 
    (be :: <idvm-back-end>, operand :: <tag>, hilo :: <idvm-hilo>) 
    => (res :: <new-sdi>)
  make-general-sdi(be, operand, lo-of-two-hilo-sdi-methods, 0, hilo);
end method;

define method make-general-sdi
    (be :: <idvm-back-end>, 
    operand :: <tag>, methods :: <simple-object-vector>, 
    size :: <integer>, hilo-object) 
    => (res :: <new-sdi>)
  let opcode-offset = be.opcode-offset;
  let dest-offset = opcode-offset - 1;
  make(<new-sdi>, dest-tag: operand,
		  dest-offset: dest-offset,
                  cached-size: size,
                  method-index: 0,
		  method-vector: methods,
                  code-fragment: hilo-object)
end method;


/// Management of offsets relative to opcodes

// In order to decode the relative offsets for tags, we need to know
// the offset of a tag operand from the associated opcode. We use
// the variables slot asm-line-pos for this purpose.

define method opcode-offset 
    (be :: <idvm-back-end>) => (offset :: <integer>)
  let vars = be.variables;
  vars.asm-line-pos
end method;

define method opcode-offset-setter 
    (offset :: <integer>, be :: <idvm-back-end>) => (offset :: <integer>)
  let vars = be.variables;
  vars.asm-line-pos := offset
end method;

define method increment-opcode-offset (be :: <idvm-back-end>) => ()
  be.opcode-offset := be.opcode-offset + 1;
end method;



/// The basic increment size for the IDVM backend is 1. This corresponds
/// to a word, not a byte.

define method labelled-constant-increment
     (backend :: <idvm-back-end>) => <integer>;
  1;
end method;

define method code-item-increment
     (backend :: <idvm-back-end>) => <integer>;
  1;
end method;
