Module: dfmc-llvm-linker
Author: Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Variables

define method emit-definition
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <module-binding>)
 => ()
  let name = emit-name(back-end, m, o);
  let linkage
    = if (model-externally-visible?(o)) #"external" else #"internal" end;

  // Find variable initializer value
  let value = binding-model-or-hollow-object(o, default: unfound());
  let initializer
    = if (found?(value))
        emit-reference(back-end, m, value);
      else
        emit-reference(back-end, m, &unbound);
      end;
  llvm-constrain-type(llvm-value-type(initializer), $llvm-object-pointer-type);

  let thread-local = o.binding-thread? & llvm-thread-local-support?(back-end);

  let global
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, $llvm-object-pointer-type),
           initializer: initializer,
           constant?: #f,
           linkage: linkage,
	   thread-local: thread-local,
           alignment: back-end-word-size(back-end),
           section: llvm-section-name(back-end, #"variables"));
  llvm-builder-define-global(back-end, name, global);
end method;

define method emit-extern
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <module-binding>)
 => ()
  let name = emit-name(back-end, m, o);
  let thread-local = o.binding-thread? & llvm-thread-local-support?(back-end);
  let global
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, $llvm-object-pointer-type),
           constant?: #f,
           linkage: #"external",
	   thread-local: thread-local);
  llvm-builder-define-global(back-end, name, global);
end method;

// Code

define method emit-definition
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&iep>)
 => ();
  let name = emit-name(back-end, module, o);
  if (o.code)
    llvm-builder-define-global(back-end, name, o.code);
    unless (*loose-mode?*)
      o.code := #f
    end unless
  else
    error("No code generated for %s (%s)", o, name);
  end if;
end method;

define method emit-definition
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&kernel-ep>) => ()
  // do nothing
end method;

define method emit-definition
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&mep>) => ()
  // do nothing
end method;

define method emit-extern
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&iep>) => ();
  let name = emit-name(back-end, module, o);
  let function-type = llvm-lambda-type(back-end, o);
  let function
    = make(<llvm-function>,
           name: name,
           type: llvm-pointer-to(back-end, function-type),
           arguments: #(),
           linkage: #"external",
           calling-convention: llvm-calling-convention(back-end, o));
  llvm-builder-define-global(back-end, name, function);
end method;

define method emit-extern
    (back-end :: <llvm-back-end>, module :: <llvm-module>,
     ep :: <&shared-entry-point>)
 => ();
  let (desc :: <llvm-entry-point-descriptor>, count :: false-or(<integer>))
    = llvm-entry-point-info(back-end, ep);
  llvm-entry-point-function(back-end, desc, count);
end method;

// FFI

define method emit-extern
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&c-function>)
 => ();
  let name = o.binding-name;
  if (name & ~llvm-builder-global-defined?(back-end, name))
    let calling-convention
      = llvm-c-function-calling-convention(back-end, o);

    let function-type = llvm-c-function-type(back-end, o);
    let args
      = map(method (arg-type, index)
              make(<llvm-argument>, type: arg-type, index: index)
            end,
            function-type.llvm-function-type-parameter-types, range(from: 0));
    let global
      = make(<llvm-function>,
             linkage: #"external",
             name: primitive.binding-name,
             type: llvm-pointer-to(back-end, function-type),
             arguments: args,
             calling-convention: calling-convention);
    llvm-builder-define-global(back-end, name, global);
  end if;
end method;

define method emit-extern
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&c-variable>)
 => ();
  unless (llvm-builder-global-defined?(back-end, o.name))
    let global
      = make(<llvm-global-variable>,
             name: o.name,
             constant?: #f,
             linkage: if (o.dll-import?) #"dllimport" else #"external" end,
             type: $llvm-i8*-type);
    llvm-builder-define-global(back-end, o.name, global);
  end unless;
end method;

// Generic objects

define method emit-definition 
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o) => ()
  let name = emit-name(back-end, module, o);
  let object = emit-object(back-end, module, o);
  let linkage
    = if (o.model-definition 
            | instance?(o, <&mm-wrapper>)
            | instance?(o, <&singular-terminal-engine-node>))
        #"external"
      else
        #"internal"
      end;
  let global
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, llvm-value-type(object)),
           initializer: object,
           constant?: #f,       // FIXME
           linkage: linkage,
           section: emit-definition-section(back-end, o));
  llvm-builder-define-global(back-end, name, global);
end method;
  
define sideways method emit-object
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o)
 => (object :: <llvm-constant-value>)
  let class = &object-class(o);

  let struct-type = llvm-object-type(back-end, o);

  let struct-type-elements = struct-type.llvm-struct-type-elements;
  let struct-elements
    = make(<simple-object-vector>, size: struct-type-elements.size);

  // Wrapper slot
  let wrapper-name = emit-name(back-end, module, ^class-mm-wrapper(class));
  struct-elements[0]
    := make(<llvm-cast-constant>,
            operator: #"BITCAST",
            type: struct-type-elements[0],
            operands: vector(llvm-builder-global(back-end, wrapper-name)));

  // Ordinary instance slots
  for (slotd in ^instance-slot-descriptors(class), index from 1)
    struct-elements[index]
      := emit-reference(back-end, module, ^slot-value(o, slotd));
  finally
    let rpt = ^repeated-slot-descriptor(class);
    if (rpt)
      struct-elements[index]
        := emit-object-slot(back-end, module, rpt,
                            struct-type-elements[index], o);
    end if;
  end for;

  make(<llvm-aggregate-constant>,
       type: struct-type,
       aggregate-values: struct-elements)
end method;

// Emit the items in a repeated slot as an array constant
define method emit-object-slot
    (back-end :: <llvm-back-end>, module :: <llvm-module>, 
     slotd :: <&repeated-slot-descriptor>, type :: <llvm-type>, o)
 => ();
  let repeated-size = ^slot-value(o, ^size-slot-descriptor(slotd));
  let repeated-elements = make(<simple-object-vector>, size: repeated-size);

  if (slotd.^slot-type == dylan-value(#"<byte-character>"))
    for (i from 0 below repeated-size)
      repeated-elements[i]
        := llvm-raw-byte-character(back-end,
                                   ^repeated-slot-value(o, slotd, i));
    end;
  else
    for (i from 0 below repeated-size)
      repeated-elements[i]
        := emit-reference(back-end, module, ^repeated-slot-value(o, slotd, i))
    end;
  end if;

  make(<llvm-aggregate-constant>,
       type: type,
       aggregate-values: repeated-elements)
end method;

define method emit-extern
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o) => ()
  let class = &object-class(o);
  let struct-type = llvm-object-type(back-end, o);
  let name = emit-name(back-end, module, o);
  let global
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, struct-type),
           constant?: #f,
           linkage: #"external");
  llvm-builder-define-global(back-end, name, global);
end method;

// Heap objects of these types do not contain mutable slots with
// references to garbage-collected objects, and therefore they can be
// stored in the untraced data section.
define method emit-definition-section
    (back-end :: <llvm-back-end>, 
     o :: type-union(<uninterned-symbol>, <string>,
                     <&machine-word>, <&single-float>, <&double-float>,
                     <&mm-wrapper>, <&signature>))
 => (section-name :: <string>);
  llvm-section-name(back-end, #"untraced-objects");
end method;

// Other objects contain mutable references and need to be traced.
define method emit-definition-section
    (back-end :: <llvm-back-end>, o) => (section-name :: <string>)
  llvm-section-name(back-end, #"objects");
end method;

define method emit-indirection-definition
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o) => ()
  let name = concatenate($indirection-prefix, emit-name(back-end, module, o));
  let global
    = make(<llvm-global-variable>,
           name: name,
           type: llvm-pointer-to(back-end, $llvm-object-pointer-type),
           initializer: emit-reference(back-end, module, o),
           constant?: #f,
           linkage: #"internal",
           alignment: back-end-word-size(back-end),
           section: llvm-section-name(back-end, #"variables"));
  llvm-builder-define-global(back-end, name, global);
end method;

