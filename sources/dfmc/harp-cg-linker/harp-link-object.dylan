Module: dfmc-harp-cg-linker
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// VARIABLES


define method runtime-module-binding-type?
    (binding :: <module-binding>)
 => (binding-type);
  unless (constant?(binding))
    let binding-type = binding.binding-type-model-object;
    let declared? = binding-type ~== dylan-value(#"<object>");
    if (declared?)
      if (binding-type)
        binding-type
      else
	unsupplied()
      end if
    end if
  end unless
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <module-binding>)
 => ();
  let name = emit-reference(back-end, stream, o);
  let export? = model-externally-visible?(o);
  emit-public(back-end, stream, o, name: name, export?: export?);
  let binding-type = o.runtime-module-binding-type?;
  if (binding-type)
    emit-public(back-end, stream, unsupplied(),
                name: concatenate(name, $runtime-module-binding-type-marker),
                export?: export?);
  end if;
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&mep>)
 => ();
  emit-extern(back-end, stream, emit-reference(back-end, stream, o), o, #f);
end method;

// CLASSES

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&class>)
 => ();
  emit-public(back-end, stream, o,
              export?:
                model-externally-visible?(o)
                  & model-externally-visible??(o));
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&mm-wrapper>)
 => ();
  emit-public(back-end, stream, o, export?: #f);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&slot-descriptor>)
 => ();
  emit-public(back-end, stream, o);
end method;

define inline function emit-external-ep
    (back-end :: <harp-back-end>, stream, o)
 => ();
  emit-extern(back-end, stream, emit-reference(back-end, stream, o), o, #f);
end function;

define method emit-forward
    (back-end :: <harp-back-end>, stream,
     o :: <&singular-terminal-engine-node>)
 => ();
  // We have to fudge the externalness of these odd objects.
  model-externally-visible?(o) := #t;
  emit-public(back-end, stream, o);
  emit-external-ep(back-end, stream, o.^engine-node-entry-point);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream,
     o :: <&engine-node>)
 => ();
  emit-external-ep(back-end, stream, o.^engine-node-entry-point);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&generic-function>)
 => ();
  if (o.model-has-definition?)
    let name :: <byte-string> = emit-name(back-end, stream, o);
    let export? = model-externally-visible?(o);
    emit-public(back-end, stream, o, name: name,
		export?: export? & model-externally-visible??(o));
    if (emit-generic-methods-list?(o, export?: export?))
      emit-public(back-end, stream, unsupplied(),
		  name: emit-generic-methods-name(back-end, stream, name),
		  export?: export?);
    end;
  end if;
  emit-external-ep(back-end, stream, o.^xep);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&domain>)
 => ();
  emit-public(back-end, stream, o);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&namespace>)
 => ();
  emit-public(back-end, stream, o);
end method;


define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&method>)
 => ();
  if (o.model-has-definition?)
    emit-public(back-end, stream, o,
		export?:
                  model-externally-visible?(o) & model-externally-visible??(o),
                derived-model-object:
                  (instance?(o, <&lambda>)
                     & make-derived-model-object(o.^iep, $iep-suffix)));
  end if;
  emit-external-ep(back-end, stream, o.^xep);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&keyword-method>)
 => ();
  next-method();
  emit-external-ep(back-end, stream, o.^mep);
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <&bottom-type>)
 => ();
  emit-public(back-end, stream, o);
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <boolean>)
 => ();
  emit-public(back-end, stream, o);
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <mapped-unbound>)
 => ();
  emit-public(back-end, stream, o);
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o == #())
 => ();
  emit-public(back-end, stream, o);
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <vector>)
 => ();
  if (o.model-has-definition?)
    emit-public(back-end, stream, o);
  end if;
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <string>)
 => ();
  if (o = "")
    emit-public(back-end, stream, o);
  end if;
end method emit-forward;

define method emit-uninterned-symbol
    (back-end :: <harp-back-end>, stream, o :: <uninterned-symbol>)
 => (name, model-object);
  let symbol :: <symbol> = as(<symbol>, o.symbol-name);
  if (o.model-has-definition?)
    values($dummy-name, symbol)
  else
    values(emit-reference(back-end, stream, symbol),
           unsupplied())
  end if
end method;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <uninterned-symbol>)
 => ();
  if (o.model-has-definition?)
    emit-public(back-end, stream, o);
  end if;
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o :: <model-properties>)
 => ();
  if (instance?(o.model-definition, <constant-definition>))
    emit-public(back-end, stream, o);
  end if;
end method emit-forward;

define method emit-forward
    (back-end :: <harp-back-end>, stream, o)
 => ();
  // do nothing
end method;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream,
     o :: <module-binding>,
     import? :: <boolean>)
 => ();
 let name = emit-reference(back-end, stream, o);
 let model-library = model-library-description(o);
 emit-extern(back-end, stream, name, o, import?, model-library: model-library);
 let binding-type = o.runtime-module-binding-type?;
 if (binding-type)
   emit-extern(back-end, stream,
	       concatenate(name, $runtime-module-binding-type-marker),
	       unsupplied(), import?, model-library: model-library);
 end if;
end method;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream, o, import? :: <boolean>)
 => ();
  unless (o.direct-object?)
    let name = emit-name(back-end, stream, o);
    emit-extern(back-end, stream, name, o, import?,
                really-import?: import? & model-externally-visible??(o));
  end unless;
end method;


// Override external visibility of model-objects here
//
// This can happen in the following cases:
// 
// - model-objects (functions & classes) derived from their bindings
// - wrappers derived from their classes
// - methods derived from generic-function lists
// - class-constructor methods derived from classes
// - ieps derived from methods
// 
// Only the parent objects are exported/imported across DLL boundary;
// the derived objects will be derived locally at DLL load-time
// from their imported parents
// 

define inline function model-externally-visible?? (o)
 => (external? :: <boolean>)
  ~ model-internal-only?(o);
end;

define method model-internal-only? (o)
  #f
end method;

define method model-internal-only? (o :: <&iep>)
  let f = o.function;
  instance?(f, <&lambda>)
    & f.lambda-runtime-function?
end method;

define method model-internal-only? (o :: <&mm-wrapper>)
  #t
end method;

define method model-internal-only? (o :: <&class>)
  binding-internal-only?(o)
end method;

define method model-internal-only? (o :: <&generic-function>)
  binding-internal-only?(o)
end method;

define inline function binding-internal-only? (o)
  model-variable-binding(o)
end;

define method model-internal-only? (o :: <&initializer-method>)
  #t
end method;

define method model-internal-only? (o :: <&method>)
  let defn = o.model-definition;
  if (defn & form-compile-stage-only?(defn))
    #f
  elseif (instance?(defn, <method-definition>))
    let gf :: <module-binding> = form-variable-binding(defn);
    let gf-model = binding-model-or-hollow-object(gf);

    if (instance?(gf-model, <&generic-function>))
      // Goal reduction: only methods statically added in the
      // defining library of their generic are handled here
      let generic-library = home-library(binding-home(gf));
      let method-library = language-definition(form-library(defn));

      if (generic-library == method-library)
        let num :: <integer> = method-number(defn);
        if (num.generic-method-offset?)
          values(gf-model, num)
        else
          #f
        end;
      else
        #f
      end
    else
      // otherwise attempt to use their bindings
      let internal? = binding-internal-only?(o);
      values(internal?, #f)
    end
  else
    let internal? = binding-internal-only?(o);
    values(internal?, #f)
  end
end method;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream,
     o :: <&c-function>, import? :: <boolean>)
 => ()
  if (o.c-function-name)
    let name = emit-name(back-end, stream, o);
    emit-extern(back-end, stream, name, unsupplied(), import?);
  end if;
end method;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream,
     v :: <&c-variable>, import? :: <boolean>)
 => ();
  let name = c-name(back-end, v.name);
  emit-extern(back-end, stream, name, v, v.dll-import?);
end method;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream,
     o :: <&raw-aggregate-type>, import? :: <boolean>)
 => ();
  // do nothing
end;

define method emit-extern/import
    (back-end :: <harp-back-end>, stream, o :: <&shared-entry-point>, import? :: <boolean>) => ()
  let name = emit-reference(back-end, stream, o);
  emit-extern(back-end, stream, name, o, import?);
end method;

define method emit-definition 
    (back-end :: <harp-back-end>, stream, o :: <module-binding>) => ()
  emit-object(back-end, stream, o);
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&iep>)
 => ();
  if (o.code)
    if (*stream-outputters?*)
      emit-comment(stream, "%=", o.function);
    end if;
    for (c in o.code)
      let externals = c.lambda-externals;
      output-compiled-lambda(back-end, stream, c, debug-info?: *debug-info?*);
      cache-imports-in-lambda(back-end, externals);
    end for;
    unless (*loose-mode?*)
      o.code := #f
    end unless
  else
    error("Code Generation must precede Linking");
  end if;
end method;

define method emit-init-code-definition
    (back-end :: <harp-back-end>, stream, name)
 => ();
  local
    method emit-compiled-lambda (lambda, #key model-object = unsupplied())
      let externals = lambda.lambda-externals;
      output-compiled-lambda(back-end, stream,
                             lambda,
                             section: #"init-code",
                             debug-info?: *debug-info?*,
                             model-object: model-object);
      cache-imports-in-lambda(back-end, externals);
    end method;

  dynamic-bind (*emitting-init-code?* = #t)
    emit-comment(stream, "SYSTEM INIT CODE");
    let system-name = concatenate(name, $system-init-code-tag);
    let system-lambda = emitted-name(as(<symbol>, system-name));
    let system-init-code = *current-heap*.heap-root-system-init-code;
    if (#t)
      let fixups-name = format-to-string("%s_fixups", system-name);
      emit-compiled-lambda(emitted-name(as(<symbol>,
                                           concatenate(fixups-name, "_code"))),
                           model-object:
                             emitted-name(as(<symbol>, fixups-name)));
      for (o in system-init-code, count from 0)
        let init-name = format-to-string("%s_%d", system-name, count);
        emit-compiled-lambda(o.^iep.code,
                             model-object:
                               emitted-name(as(<symbol>, init-name)));
      end for;
      emit-compiled-lambda(system-lambda);
    else
      emit-compiled-lambda(system-lambda);
    end if;

    emit-comment(stream, "USER INIT CODE");
    let user-name = concatenate(name, $user-init-code-tag);
    let user-lambda = emitted-name(as(<symbol>, user-name));
    let user-init-code = *current-heap*.heap-root-init-code;
    if (#t)
      for (o in user-init-code, count from 0)
        let init-name = format-to-string("%s_%d", user-name, count);
	emit-compiled-lambda(o.^iep.code,
			     model-object:
                               emitted-name(as(<symbol>, init-name)));
      end for;
      emit-compiled-lambda(user-lambda);
    else
      emit-compiled-lambda(user-lambda);
    end if;
  end dynamic-bind;
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&kernel-ep>)
 => ();
  // do nothing
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&mep>)
 => ();
  // do nothing
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <runtime-object>)
 => ();
  // do nothing
end method;

define method emit-definition // !@#$ need unifying type
    (back-end :: <harp-back-end>, stream, o)
 => ();
  // Direct objects are always emitted in full at point of reference and
  // are never referred to by name, hence no need for a forward declaration.
  unless (o.direct-object?)
    let (name, model-object) =
      select(o by instance?)
        <uninterned-symbol> =>
          emit-uninterned-symbol(back-end, stream, o);
        otherwise =>
          values($dummy-name, apropo-model-object(o));
      end select;
    
    output-definition(back-end,
                      stream,
                      name,
                      model-object: model-object,
                      section: o.section-for-definition);

    emit-object(back-end, stream, o);

    emit-data-footer(back-end, stream, name, model-object: model-object);
  end unless;
end method;


define method section-for-definition (o :: <object>) => (section :: <symbol>)
  select (o by instance?)
    // Statically dumped objects which are immutable or are only ever
    // mutated by untraceable data may be allocated in the untraced-objects
    // section.
    <uninterned-symbol>, <string>,
    <&machine-word>, <&single-float>, <&double-float>, <&mm-wrapper>,
    <&signature> /* , <&singleton>, <&slot-descriptor> */
      => #"untraced-objects";
    otherwise
      => #"objects";
  end select;
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&generic-function>)
 => ();
  // let req-size =
  //   spec-argument-number-required(signature-spec(o));

  output-definition(back-end,
		    stream,
		    $dummy-name,
		    model-object: o,
		    section: #"objects");
  
  emit-object(back-end, stream, o);

  emit-data-footer(back-end, stream, $dummy-name, model-object: o);

  if (emit-generic-methods-list?(o))
    let name = emit-generic-methods-name(back-end, stream, o);
    output-definition(back-end,
                      stream,
                      name,
                      section: #"variables");
    
    emit-data-item(back-end, stream, o.^generic-function-methods);
    
    emit-data-footer(back-end, stream, name);
  end if;
end method;


// 
// Runtime derivation of generic function methods
// 
// For sealed generic functions, just use the methods
// slot, because this is not expected to change dynamically
// 
// For incremental generics, have to define/export a binding
// for the methods list, and indirect off that
// 

define method emit-generic-methods-list?
    (o :: <&generic-function>,
     #key export?)
 => (emit?);
  let export? = export? | model-externally-visible?(o);
  export?
    & ~ o.^generic-function-sealed?
    & ~ o.^generic-function-methods.empty?
end method;

define constant $generic-methods-suffix    = "GFML";

define method emit-generic-methods-name
    (back-end :: <harp-back-end>, stream, o :: <&generic-function>)
 => (name :: <string>);
  concatenate-as(<byte-string>,
                 emit-name(back-end, stream, o),
                 $generic-methods-suffix)
end method;

define method emit-generic-methods-name
    (back-end :: <harp-back-end>, stream, name :: <byte-string>)
 => (name :: <string>);
    concatenate-as(<byte-string>,
		   name,
		   $generic-methods-suffix);
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&method>)
 => ();
  output-definition(back-end,
                    stream,
                    $dummy-name,
                    model-object: o,
                    section:
                      if (instance?(o, <&closure-method-mixin>)
                            & method-top-level?(o))
                        // special top-level closures are not cloned;
                        // they are mutated by traceable signatures
                        #"objects"
                      else
                        #"untraced-objects"
                      end if);
  emit-object(back-end, stream, o);
  emit-data-footer(back-end, stream, $dummy-name, model-object: o);
end method;

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <&raw-aggregate-type>)
 => ();
  // do nothing
end;

/*
 Alternative, expensive contiguous list dumping of generic function
 methods list at the level of the back-end instead of the Heaper

define method emit-definition
    (back-end :: <harp-back-end>, stream, o :: <list>) => ()
  unless (element(back-end.cg-variables.model-references, o, default: #f))

  element(back-end.cg-variables.model-references, o) := #t;
  output-definition(back-end,
		    stream,
		    $dummy-name,
		    model-object: o,
		    section: #"objects");

  emit-object(back-end, stream, o);

  emit-data-footer(back-end, stream, $dummy-name, model-object: o);

  let o-tail :: <list> = o.tail;
  unless (o-tail.empty?)
    emit-definition(back-end, stream, o-tail);
  end;

  end;
end method;
*/

// INDIRECTION DEFINITIONS

define method emit-indirection-definition
    (back-end :: <harp-back-end>, stream, o :: <object>)
 => ();
  let local-symbol = element(heap-symbols(*current-heap*), o, default: #f);

  if (symbol-emitted?(local-symbol))
    let indirection = local-symbol.cg-indirect-symbol;
    output-definition(back-end,
                      stream,
                      indirection,
                      section: #"variables");
    emit-data-item(back-end, stream, local-symbol.cg-uninterned-symbol);
    emit-data-footer(back-end, stream, indirection);
  end if;
end method;

define sideways method emit-object 
    (back-end :: <harp-back-end>, stream, o :: <module-binding>)
 => (object);
  output-definition(back-end, stream,
		    $dummy-name,
		    model-object: o,
                    section: #"variables");
  let value = binding-model-or-hollow-object(o, default: unfound());
  if (~found?(value))
    // emit-raw-data-item(back-end, stream, 0);  // Could use any value here
    emit-data-item(back-end, stream, &unbound);
  else
    emit-data-item(back-end, stream, value);
  end;
  emit-data-footer(back-end, stream, $dummy-name, model-object: o);
 let binding-type = o.runtime-module-binding-type?;
 if (binding-type)
   let name = emit-reference(back-end, stream, o);
   let type-name = concatenate(name, $runtime-module-binding-type-marker);
   output-definition(back-end, stream,
		     type-name,
		     section: #"variables");
   if (supplied?(binding-type))
    emit-data-item(back-end, stream, binding-type);
   else
    emit-data-item(back-end, stream, #f);
   end if;
   emit-data-footer(back-end, stream, type-name);
 end if;
end method;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <string>)
 => (object);
  let class-wrapper = ^class-mm-wrapper(&object-class(o));
  emit-data-item(back-end, stream, class-wrapper);
  emit-data-item(back-end, stream, o.size);
  unless (o.empty?)
    output-data-byte(back-end, stream, o);
  end unless;
  output-data-byte(back-end, stream, 0)
end method emit-object;

define sideways method emit-object // !@#$ NEED UNIFYING TYPE
    (back-end :: <harp-back-end>, stream, o :: <object>)
 => (object);
  let class = &object-class(o);
  let wrapper = ^class-mm-wrapper(class);
  emit-data-item(back-end, stream, wrapper);
  emit-line-comment(stream, "wrapper");
  for (slotd in ^instance-slot-descriptors(class))
    emit-object-slot(back-end, stream, class, slotd, o);
  end;
  let rpt = ^repeated-slot-descriptor(class);
  if (rpt)
    emit-object-slot(back-end, stream, class, rpt, o);
  end if;
end method;

define method emit-object-slot
    (back-end :: <harp-back-end>, stream, class,
     slotd :: <&any-instance-slot-descriptor>, o)
 => ();
  let the-slot = ^slot-value(o, slotd);
  // just use the iep model for mep models
  let the-slot = 
    select(the-slot by instance?)
      <&mep> =>
	let sig-spec = signature-spec(o);
	if (spec-argument-key?(sig-spec))
	  the-slot
	else
	  o.^iep
        end if;
      otherwise =>
        the-slot;
    end select;

  emit-slot(back-end, stream, the-slot,
            if (*stream-outputters?*) struct-field-name(class, slotd, 0)
            else #f
            end if);
end method;

define method emit-object-slot
    (back-end :: <harp-back-end>, stream, 
     class,
     slotd :: <&repeated-slot-descriptor>,
     o)
 => ();
  let repeated-size = ^slot-value(o, ^size-slot-descriptor(slotd));

  if (slotd.^slot-type == dylan-value(#"<byte-character>"))
    for (i from 0 below repeated-size)
      emit-raw-data-item(back-end, stream,
                         format-to-string("%s",
                                          ^repeated-slot-value(o, slotd, i)));
    end;
  else
    for (i from 0 below repeated-size)
      let value = ^repeated-slot-value(o, slotd, i);
      emit-data-item(back-end, stream, value);

      if (*stream-outputters?*)
        emit-line-comment(stream,
                          " %s[%d] ", 
                          struct-field-name(class, slotd, i),
                          i);
      end if;
    end;
  end if;
end method;

define function emit-slot(back-end :: <harp-back-end>, stream, o, field-name)
  emit-data-item(back-end, stream, o);

  if (*stream-outputters?*)
    emit-line-comment(stream, " %s ", field-name);
  end if;
end function emit-slot;
