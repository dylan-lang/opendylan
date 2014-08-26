Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Non-Local Exits and Cleanups

define class <nlx-info> (<object>)
  constant slot nlx-landingpad :: <llvm-basic-block>,
    required-init-keyword: landingpad:;
  constant slot nlx-resume :: <llvm-basic-block>,
    required-init-keyword: resume:;
  constant slot nlx-resume-phi-operands :: <stretchy-vector>,
    required-init-keyword: resume-phi-operands:;
end class;

define thread variable *live-nlx* :: false-or(<nlx-info>) = #f;

// Use invoke if we are within an <unwind-protect>/<bind-exit>, or
// call otherwise
define method op--call
    (be :: <llvm-back-end>, fnptrval :: <llvm-value>, args :: <sequence>,
     #rest options, #key, #all-keys)
 => (instruction :: <llvm-instruction>);
  if (*live-nlx*)
    let next = make(<llvm-basic-block>);
    let instruction
      = apply(ins--invoke, be, next, *live-nlx*.nlx-landingpad, fnptrval, args,
              options);
    ins--block(be, next);
    instruction
  else
    apply(ins--call, be, fnptrval, args, options)
  end if
end method;

// Exception handling personality function

define constant $llvm-eh-personality-function-type
  = make(<llvm-function-type>,
         return-type: $llvm-i32-type,
         parameter-types: #(),
         varargs?: #t);

define constant $llvm-opendylan-eh-personality-function
  = make(<llvm-function>,
         name: "__opendylan_personality_v0",
         type: make(<llvm-pointer-type>,
                    pointee: $llvm-eh-personality-function-type),
         arguments: #(),
         linkage: #"external");

define constant $llvm-eh-landingpad-type
  = make(<llvm-struct-type>, elements: vector($llvm-i8*-type, $llvm-i32-type));

define method op--landingpad
    (back-end :: <llvm-back-end>, clauses :: <sequence>,
     #key cleanup?)
 => (instruction :: <llvm-instruction>)
  ins--landingpad(back-end,
                  $llvm-eh-landingpad-type,
                  $llvm-opendylan-eh-personality-function,
                  clauses, cleanup?: cleanup?)
end method;

define method op--resume
    (back-end :: <llvm-back-end>, resume-value :: <llvm-value>)
 => ();
  if (*live-nlx*)
    add!(*live-nlx*.nlx-resume-phi-operands, resume-value);
    add!(*live-nlx*.nlx-resume-phi-operands, back-end.llvm-builder-basic-block);
    ins--br(back-end, *live-nlx*.nlx-resume);
  else
    ins--resume(back-end, resume-value);
  end if;
end;


/// Bind Exit Frame

define method initialize-bef-struct-type (back-end :: <llvm-back-end>) => ()
  back-end.llvm-bef-struct-type
    := make(<&raw-struct-type>,
	    debug-name: "BEF",
	    options: #[],
	    members:
	      vector(// Offset 0: Current bind-exit frame on stack
		     make(<raw-aggregate-ordinary-member>,
			  name: #"bef-frame-pointer",
                          raw-type: dylan-value(#"<raw-pointer>")),
                     // Offset 1: Type ID representing the block
                     make(<raw-aggregate-ordinary-member>,
                          name: #"bef-typeid",
                          raw-type: dylan-value(#"<raw-pointer>")),
		     // Offset 2: MV count
		     make(<raw-aggregate-ordinary-member>,
		     	  name: #"bef-mv-count",
                          raw-type: dylan-value(#"<raw-integer>")),
		     // Offset 3: MV area
		     make(<raw-aggregate-array-member>,
			  name: #"bef-mv-area",
			  array-length: $maximum-value-count,
                          raw-type: dylan-value(#"<raw-pointer>"))));

  // Record BEF structure field indicies
  for (member in back-end.llvm-bef-struct-type.raw-aggregate-members,
       index from 0)
    back-end.%raw-struct-field-index[member.member-name] := i32(index);
  end for;
end method;

define method op--bef-getelementptr
    (be :: <llvm-back-end>, bef :: <llvm-value>, field :: <symbol>,
     #rest indices)
 => (pointer :: <llvm-value>);
  let index = be.%raw-struct-field-index[field];
  apply(ins--gep-inbounds, be, bef, 0, index, indices)
end method;

// Instantiate a global constant object representing the given bind-exit site
define method op--typeid
    (back-end :: <llvm-back-end>, entry-state :: <entry-state>)
 => (typeid :: <llvm-value>);
  let name = hygienic-mangle(back-end,
                             entry-state.name,
                             entry-state.frame-offset);
  let typeid
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, $llvm-object-pointer-type),
           initializer: make(<llvm-null-constant>,
                             type: $llvm-object-pointer-type),
           constant?: #t,
           linkage: #"internal",
           alignment: back-end-word-size(back-end));
  llvm-builder-define-global(back-end, name, typeid)
end method;

define method op--allocate-bef
    (back-end :: <llvm-back-end>, typeid :: <llvm-constant-value>)
 => (bef :: <llvm-value>);
  let bef-type = llvm-reference-type(back-end, back-end.llvm-bef-struct-type);
  let bef-ptr = ins--alloca(back-end, bef-type, i32(1),
                            alignment: back-end-word-size(back-end));

  // Store the current frame pointer in the bind exit frame
  let frame-pointer
    = ins--call-intrinsic(back-end, "llvm.frameaddress", vector(i32(0)));
  let bef-framepointer-slot-ptr
    = op--bef-getelementptr(back-end, bef-ptr, #"bef-frame-pointer");
  ins--store(back-end, frame-pointer, bef-framepointer-slot-ptr);

  // Store the typeid constant in the bind exit frame
  let typeid-cast
    = make(<llvm-cast-constant>, operator: #"BITCAST",
           type: $llvm-object-pointer-type,
           operands: vector(typeid));
  let bef-typeid-slot-ptr
    = op--bef-getelementptr(back-end, bef-ptr, #"bef-typeid");
  ins--store(back-end, typeid-cast, bef-typeid-slot-ptr);

  ins--bitcast(back-end, bef-ptr,
               llvm-reference-type(back-end, dylan-value(#"<raw-pointer>")));
end method;

define side-effecting stateful indefinite-extent can-unwind auxiliary &c-primitive-descriptor primitive-nlx
    (bind-exit-frame :: <raw-pointer>) => ();
