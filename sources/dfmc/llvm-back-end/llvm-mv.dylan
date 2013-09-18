Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <llvm-mv> (<object>)
end class;

// Multiple values represented as local SSA values
define class <llvm-local-mv> (<llvm-mv>)
  constant slot llvm-mv-fixed :: <sequence>,
    init-value: #[], init-keyword: fixed:;
  constant slot llvm-mv-rest :: false-or(<llvm-value>),
    init-value: #f, init-keyword: rest:;
end class;

// Extract a single value
define method op--mv-extract
    (back-end :: <llvm-back-end>, mv :: <llvm-local-mv>, index :: <integer>)
 => (value :: <llvm-value>);
  let fixed = mv.llvm-mv-fixed;
  if (index < fixed.size)
    fixed[index]
  elseif (mv.llvm-mv-rest)
    error("mv-extract beyond fixed with rest")
  else
    let module = back-end.llvm-builder-module;
    emit-reference(back-end, module, &false)
  end if
end method;

// Extract values beyond the given index in a rest vector
define method op--mv-extract-rest
    (back-end :: <llvm-back-end>, mv :: <llvm-local-mv>, index :: <integer>)
 => (value :: <llvm-value>);
  let module = back-end.llvm-builder-module;
  if (index = mv.llvm-mv-fixed.size)
    mv.llvm-mv-rest
      | emit-reference(back-end, module, dylan-value(#"%empty-vector"))
  elseif (~mv.llvm-mv-rest)
    // FIXME this can be simplified
    let rest-count = mv.llvm-mv-fixed.size - index;
    if (rest-count <= 0)
      emit-reference(back-end, module, dylan-value(#"%empty-vector"))
    else
      let rest-vector = op--allocate-vector(back-end, rest-count);
      for (i from index below mv.llvm-mv-fixed.size)
        call-primitive(back-end, primitive-vector-element-setter-descriptor,
                       mv.llvm-mv-fixed[i], rest-vector,
                       llvm-back-end-value-function(back-end, i));
      end for;
      rest-vector
    end if
  else
    error("op--mv-extract-rest is hard %= (%d, %=) [%d]",
          mv, mv.llvm-mv-fixed.size, mv.llvm-mv-rest.true?, index)
  end if
end method;

// Multiple values stored in the TEB MV area
define class <llvm-global-mv> (<llvm-mv>)
  constant slot llvm-mv-struct :: <llvm-value>,
    required-init-keyword: struct:;
  constant slot llvm-mv-maximum :: <integer>,
    init-keyword: maximum:, init-value: $maximum-value-count;
end class;

define method op--mv-extract
    (back-end :: <llvm-back-end>, mv :: <llvm-global-mv>, index :: <integer>)
 => (value :: <llvm-value>);
  if (index = 0)
    ins--extractvalue(back-end, mv.llvm-mv-struct, 0)
  elseif (index < mv.llvm-mv-maximum)
    let ptr = op--teb-getelementptr(back-end, #"teb-mv-area", index);
    ins--load(back-end, ptr)
  else
    let module = back-end.llvm-builder-module;
    emit-reference(back-end, module, &false)
  end if
end method;

// Extract global values into a rest vector
define method op--mv-extract-rest
    (back-end :: <llvm-back-end>, mv :: <llvm-global-mv>, index :: <integer>)
 => (value :: <llvm-value>);
  let count = ins--extractvalue(back-end, mv.llvm-mv-struct, 1);
  let raw-integer-type
    = llvm-reference-type(back-end, dylan-value(#"<raw-integer>"));
  let rest-vector
    = call-primitive(back-end, primitive-mv-extract-rest-descriptor,
                     llvm-back-end-value-function(back-end, index),
                     ins--zext(back-end, count, raw-integer-type));
  if (index = 0)
    // Fill in the 0th value
    let primary = ins--extractvalue(back-end, mv.llvm-mv-struct, 0);
    call-primitive(back-end, primitive-vector-element-setter-descriptor,
                   primary, rest-vector,
                   llvm-back-end-value-function(back-end, 0));
  end if;
  rest-vector
end method;

define side-effect-free stateful indefinite-extent auxiliary &runtime-primitive-descriptor primitive-mv-extract-rest
    (index :: <raw-integer>, count :: <raw-integer>)
 => (box :: <simple-object-vector>);
  let module = be.llvm-builder-module;
  let word-size = back-end-word-size(be);
  let raw-integer-type = llvm-reference-type(be, dylan-value(#"<raw-integer>"));

  // Basic blocks
  let entry-bb = be.llvm-builder-basic-block;
  let vector-bb = make(<llvm-basic-block>);
  let loop-head-bb = make(<llvm-basic-block>);
  let loop-tail-bb = make(<llvm-basic-block>);
  let return-bb = make(<llvm-basic-block>);

  let rest-count = ins--sub(be, count, index);
  let empty?-cmp = ins--icmp-sle(be, rest-count, 0);
  ins--br(be, empty?-cmp, return-bb, vector-bb);

  // Allocate a <simple-object-vector>
  ins--block(be, vector-bb);
  let rest-vector = op--allocate-vector(be, rest-count);

  // Store values from the global area into it
  let mv-base = op--teb-getelementptr(be, #"teb-mv-area", index);

  // If index is 0, skip over the first element (the caller will fill it in)
  let index-zero?-cmp = ins--icmp-eq(be, index, 0);
  let start = ins--select(be, index-zero?-cmp, 1, 0);
  ins--br(be, loop-head-bb);

  ins--block(be, loop-head-bb);
  let i-placeholder
    = make(<llvm-symbolic-value>, type: raw-integer-type, name: "i");
  let i = ins--phi*(be, start, vector-bb, i-placeholder, loop-tail-bb);
  let cmp = ins--icmp-ult(be, i, rest-count);
  ins--br(be, cmp, loop-tail-bb, return-bb);

  // Copy the value from the MV area to the vector
  ins--block(be, loop-tail-bb);
  let mv-ptr = ins--gep(be, mv-base, i);
  let value = ins--load(be, mv-ptr, alignment: word-size);
  call-primitive(be, primitive-vector-element-setter-descriptor,
                 value, rest-vector, i);

  let next-i = ins--add(be, i, 1);
  i-placeholder.llvm-placeholder-value-forward := next-i;
  ins--br(be, loop-head-bb);

  // Return
  ins--block(be, return-bb);
  let empty-vector
    = emit-reference(be, module, dylan-value(#"%empty-vector"));
  ins--phi*(be,
            empty-vector, entry-bb,
            rest-vector, loop-head-bb);
end;

// Emit the construction of an MV struct
define method op--global-mv-struct
    (back-end :: <llvm-back-end>,
     primary :: <llvm-value>,
     count :: <llvm-value>)
 => (struct :: <llvm-value>);
  let undef-struct
    = make(<llvm-undef-constant>,
           type: llvm-reference-type(back-end, back-end.%mv-struct-type));
  llvm-constrain-type(primary.llvm-value-type, $llvm-object-pointer-type);
  let value-struct
    = ins--insertvalue(back-end, undef-struct, primary, 0);
  llvm-constrain-type(count.llvm-value-type, $llvm-i8-type);
  ins--insertvalue(back-end, value-struct, count, 1)
end method;

// Copy out of the MV area
define method op--copy-from-mv-area
    (back-end :: <llvm-back-end>, index, dst, mv-area-count)
 => ();
  let copy-bb = make(<llvm-basic-block>);
  let continue-bb = make(<llvm-basic-block>);

  let cmp = ins--icmp-sgt(back-end, mv-area-count, 0);
  ins--br(back-end, cmp, copy-bb, continue-bb);

  // Copy spill values to the MV area
  ins--block(back-end, copy-bb);
  let word-size = back-end-word-size(back-end);
  let dst-cast = ins--bitcast(back-end, dst, $llvm-i8*-type);
  let ptr = op--teb-getelementptr(back-end, #"teb-mv-area", index);
  let ptr-cast = ins--bitcast(back-end, ptr, $llvm-i8*-type);
  let byte-count = ins--mul(back-end, mv-area-count, word-size);
  ins--call-intrinsic(back-end, "llvm.memcpy",
                      vector(dst-cast, ptr-cast, byte-count,
                             i32(word-size), $llvm-false));
  ins--br(back-end, continue-bb);

  ins--block(back-end, continue-bb);
end method;

// Copy into the MV area
define method op--copy-into-mv-area
    (back-end :: <llvm-back-end>, index, src, mv-area-count)
 => ();
  let copy-bb = make(<llvm-basic-block>);
  let continue-bb = make(<llvm-basic-block>);

  let cmp = ins--icmp-sgt(back-end, mv-area-count, 0);
  ins--br(back-end, cmp, copy-bb, continue-bb);

  // Copy spill values to the MV area
  ins--block(back-end, copy-bb);
  let word-size = back-end-word-size(back-end);
  let src-cast = ins--bitcast(back-end, src, $llvm-i8*-type);
  let ptr = op--teb-getelementptr(back-end, #"teb-mv-area", index);
  let ptr-cast = ins--bitcast(back-end, ptr, $llvm-i8*-type);
  let byte-count = ins--mul(back-end, mv-area-count, word-size);
  ins--call-intrinsic(back-end, "llvm.memcpy",
                      vector(ptr-cast, src-cast, byte-count,
                             i32(word-size), $llvm-false));
  ins--br(back-end, continue-bb);

  ins--block(back-end, continue-bb);
end method;

// Save a multiple-value temporary to be restored later
define method op--protect-temporary
    (back-end :: <llvm-back-end>, temp, value)
 => (spill :: <llvm-value>);
  // Nothing to do
  emit-reference(back-end, back-end.llvm-builder-module, &false)
end method;

define method op--protect-temporary
    (back-end :: <llvm-back-end>, temp :: <multiple-value-temporary>,
     mv :: <llvm-global-mv>)
 => (spill :: <llvm-value>);
  if (temp.required-values > 1 | temp.rest-values?)
    let copy-bb = make(<llvm-basic-block>);
    let continue-bb = make(<llvm-basic-block>);

    // Allocate a local spill area
    let maximum-count
      = if (temp.rest-values?)
          mv.llvm-mv-maximum - 1
        else
          temp.required-values - 1
        end;
    let word-size = back-end-word-size(back-end);
    let spill = ins--alloca(back-end, $llvm-object-pointer-type, maximum-count);

    // Determine how many values are in the TEB MV area
    let count = ins--extractvalue(back-end, mv.llvm-mv-struct, 1);
    let count-ext = ins--zext(back-end, count, back-end.%type-table["iWord"]);
    let mv-area-count = ins--sub(back-end, count-ext, 1);

    // Copy into the spill area
    op--copy-from-mv-area(back-end, 1, spill, mv-area-count);
    spill
  else
    emit-reference(back-end, back-end.llvm-builder-module, &false)
  end if;
end method;

define method op--restore-temporary
    (back-end :: <llvm-back-end>, temp, value, spill :: <llvm-value>)
 => ();
  // Nothing to do
end method;

define method op--restore-temporary
    (back-end :: <llvm-back-end>, temp :: <multiple-value-temporary>,
     mv :: <llvm-global-mv>, spill :: <llvm-value>)
 => ();
  if (temp.required-values > 1 | temp.rest-values?)

    // Determine how many values need to be restored to the TEB MV area
    let count = ins--extractvalue(back-end, mv.llvm-mv-struct, 1);
    let count-ext = ins--zext(back-end, count, back-end.%type-table["iWord"]);
    let mv-area-count = ins--sub(back-end, count-ext, 1);

    // Copy them back
    op--copy-into-mv-area(back-end, 1, spill, mv-area-count)
  end if;
end method;

// Store an MV into a Bind Exit Frame for non-local return
define method op--set-bef-value
    (back-end :: <llvm-back-end>, bef :: <llvm-value>,
     mv :: <llvm-global-mv>)
 => ();
  let word-size = back-end-word-size(back-end);
  let bef-type = llvm-reference-type(back-end, back-end.llvm-bef-struct-type);
  let bef-cast = ins--bitcast(back-end, bef, llvm-pointer-to(back-end, bef-type));

  // Store the value count
  let count = ins--extractvalue(back-end, mv.llvm-mv-struct, 1);
  let count-ext = ins--zext(back-end, count, back-end.%type-table["iWord"]);
  let count-ptr
    = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-count");
  ins--store(back-end, count-ext, count-ptr);

  // Store the primary value
  let primary = ins--extractvalue(back-end, mv.llvm-mv-struct, 0);
  let primary-ptr
    = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-area", 0);
  ins--store(back-end, primary, primary-ptr);

  let copy-bb = make(<llvm-basic-block>);
  let continue-bb = make(<llvm-basic-block>);

  // Determine how many values are in the TEB MV area
  // (i.e., everything beyond the primary value)
  let mv-area-count = ins--sub(back-end, count-ext, 1);

  // Copy them
  let dst-ptr
    = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-area", 1);
  op--copy-into-mv-area(back-end, 1, dst-ptr, mv-area-count);
end method;

define method op--set-bef-value
    (back-end :: <llvm-back-end>, bef :: <llvm-value>,
     mv :: <llvm-local-mv>)
 => ();
  let word-size = back-end-word-size(back-end);
  let rest-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let bef-type = llvm-reference-type(back-end, back-end.llvm-bef-struct-type);
  let bef-cast
    = ins--bitcast(back-end, bef, llvm-pointer-to(back-end, bef-type));

  let (value-count, rest-cast, rest-count)
    = if (mv.llvm-mv-rest)
        let rest-cast
          = op--object-pointer-cast(back-end, mv.llvm-mv-rest, rest-class);
        let rest-count
          = call-primitive(back-end, primitive-vector-size-descriptor,
                           rest-cast);
        values(ins--add(back-end, mv.llvm-mv-fixed.size, rest-count),
               rest-cast,
               rest-count)
      else
        values(mv.llvm-mv-fixed.size, #f, #f)
      end if;

  // Store the value count
  let count-ptr
    = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-count");
  ins--store(back-end, value-count, count-ptr);

  // Store the fixed values
  for (value in mv.llvm-mv-fixed, index from 0)
    let value-ptr
      = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-area", index);
    ins--store(back-end, value, value-ptr);
  end for;

  // Store the rest values
  if (mv.llvm-mv-rest)
    let byte-count = ins--mul(back-end, rest-count, word-size);

    let dst-ptr
      = op--bef-getelementptr(back-end, bef-cast, #"bef-mv-area",
                              mv.llvm-mv-fixed.size);
    let dst-cast = ins--bitcast(back-end, dst-ptr, $llvm-i8*-type);

    let repeated-slot-ptr
      = op--getslotptr(back-end, rest-cast, rest-class, #"vector-element", 0);
    let src-cast = ins--bitcast(back-end, repeated-slot-ptr, $llvm-i8*-type);

    ins--call-intrinsic(back-end, "llvm.memcpy",
                        vector(dst-cast, src-cast, byte-count,
                               i32(word-size), $llvm-false));
  end if;
end method;


define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-bef-values
    (bind-exit-frame :: <raw-pointer>) => (#rest values);
  let word-size = back-end-word-size(be);

  let bef-type = llvm-reference-type(be, be.llvm-bef-struct-type);
  let bef-cast
    = ins--bitcast(be, bind-exit-frame, llvm-pointer-to(be, bef-type));

  let copy-bb = make(<llvm-basic-block>);
  let continue-bb = make(<llvm-basic-block>);

  // Retrieve the value count
  let count-ptr
    = op--bef-getelementptr(be, bef-cast, #"bef-mv-count");
  let count = ins--load(be, count-ptr);

  // Determine how many values belong in the TEB MV area
  let mv-area-count = ins--sub(be, count, 1);

  // Copy values to MV area
  let src-ptr
    = op--bef-getelementptr(be, bef-cast, #"bef-mv-area", 1);
  op--copy-into-mv-area(be, 1, src-ptr, mv-area-count);

  // Retrieve the primary value
  let primary-ptr
    = op--bef-getelementptr(be, bef-cast, #"bef-mv-area", 0);
  let primary = ins--load(be, primary-ptr);

  let count-trunc = ins--trunc(be, count, $llvm-i8-type);
  op--global-mv-struct(be, primary, count-trunc)
end;


/// values primitive

define side-effect-free stateless dynamic-extent mapped-parameter &primitive-descriptor primitive-values
    (arguments :: <simple-object-vector>) => (#rest values);
  let module = be.llvm-builder-module;

  let entry-block = be.llvm-builder-basic-block;
  let return-primary = make(<llvm-basic-block>);
  let return-common = make(<llvm-basic-block>);

  // Read the vector size
  let count = call-primitive(be, primitive-vector-size-descriptor, arguments);

  // Is it empty?
  let cmp = ins--icmp-eq(be, count, 0);
  ins--br(be, cmp, return-common, return-primary);

  ins--block(be, return-primary);
  let mv-area-count = ins--sub(be, count, 1);
  let src-ptr
    = op--getslotptr(be, arguments, #"<simple-object-vector>",
                     #"vector-element", 1);
  op--copy-into-mv-area(be, 1, src-ptr, mv-area-count);
  let value0
    = call-primitive(be, primitive-vector-element-descriptor,
                     arguments, llvm-back-end-value-function(be, 0));
  let primary-block = be.llvm-builder-basic-block;
  ins--br(be, return-common);

  ins--block(be, return-common);
  let primary = ins--phi*(be,
                          emit-reference(be, module, &false), entry-block,
                          value0, primary-block);
  let count-trunc = ins--trunc(be, count, $llvm-i8-type);
  op--global-mv-struct(be, primary, count-trunc)
end;
