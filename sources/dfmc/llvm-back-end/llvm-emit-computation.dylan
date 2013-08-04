Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Object references

define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <object-reference>)
 => (reference :: <llvm-value>)
  emit-indirect-reference(back-end, m, o.reference-value);
end method;

define method emit-indirect-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o)
 => (reference :: <llvm-value>)
  if (load-bound-object?(o))
    let word-size = back-end-word-size(back-end);

    // Locate this object's indirection definition
    let name = concatenate($indirection-prefix, emit-name(back-end, m, o));
    let indirection-definition = llvm-builder-global(back-end, name);
    llvm-constrain-type(indirection-definition.llvm-value-type,
                        llvm-pointer-to(back-end, $llvm-object-pointer-type));

    // Declare as invariant to allow CSE
    let memory
      = make(<llvm-cast-constant>, operator: #"BITCAST",
             type: $llvm-object-pointer-type,
             operands: vector(indirection-definition));
    ins--call-intrinsic(back-end, "llvm.invariant.start",
                        vector(i64(word-size), memory));

    // Retrieve the load-bound value
    ins--load(back-end, indirection-definition, alignment: word-size)
  else
    emit-reference(back-end, m, o)
  end
end method;

define sideways method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     o :: <method-reference>)
 => (reference :: <object>)
  emit-reference(back-end, m, function(o.reference-value))
end method;

define sideways method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     o :: <defined-constant-reference>)
 => (reference :: <object>)
  emit-reference(back-end, m, referenced-binding(o))
end method;


/// Temporary values

define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <temporary>)
 => (reference :: <llvm-value>);
  if (o.environment == *current-environment*)
    temporary-value(o)
  else
    emit-closure-reference(back-end, m, o);
  end if
end method;

define function emit-closure-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <temporary>)
 => (reference :: <llvm-value>);
  let closure = llvm-builder-local(back-end, $function-parameter-name);
  if (method-top-level?(*current-environment*.lambda))
    closure
  else
    let fun = *current-environment*.lambda.function;
    let class :: <&class> = fun.^object-class;

    // Read the closure's environment slot
    let offset = closure-offset(*current-environment*, o);
    unless (offset)
      error("Can't find %= in environment", o);
    end unless;

    let class-type
      = llvm-class-type(back-end, class,
                        repeated-size: closure-size(*current-environment*));
    let closure-cast
      = ins--bitcast(back-end, closure, llvm-pointer-to(back-end, class-type));
    let ptr
      = op--getslotptr(back-end, closure-cast, class,
                       #"environment-element", offset);
    ins--load(back-end, ptr);
  end if;
end function;

define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <stack-vector-temporary>)
 => (reference :: <llvm-value>);
  ins--bitcast(back-end, temporary-value(o), $llvm-object-pointer-type)
end method;

// Local temporary definitions

define method emit-local-tmp-definition
    (back-end :: <llvm-back-end>, tmp :: <temporary>) => ()
  // FIXME nothing?
end method;

define method emit-local-tmp-definition
    (back-end :: <llvm-back-end>, tmp :: <multiple-value-temporary>) => ()
  // FIXME nothing?
end method;

define method emit-local-definition
    (back-end :: <llvm-back-end>, tmp :: <temporary>) => ()
  emit-local-tmp-definition(back-end, tmp);
end method;

define method emit-local-definition
    (back-end :: <llvm-back-end>, tmp :: <stack-vector-temporary>) => ()
  unless (tmp.number-values = 0)
    let module = back-end.llvm-builder-module;

    // Stack-allocate a vector
    let class :: <&class> = dylan-value(#"<simple-object-vector>");
    let class-type
      = llvm-class-type(back-end, class, repeated-size: tmp.number-values);
    let ptr = ins--alloca(back-end, class-type, i32(1),
                          alignment: back-end-word-size(back-end));
    temporary-value(tmp) := ptr;

    // Initialize the wrapper and size slots
    let wrapper-slot-ptr = ins--gep-inbounds(back-end, ptr, 0, i32(0));
    let wrapper-name = emit-name(back-end, module, ^class-mm-wrapper(class));
    let wrapper = llvm-builder-global(back-end, wrapper-name);
    ins--store(back-end, wrapper, wrapper-slot-ptr);

    let size-slot-ptr = op--getslotptr(back-end, ptr, class, #"size");
    let size-ref = emit-reference(back-end, module, tmp.number-values);
    ins--store(back-end, size-ref, size-slot-ptr);
  end;
end method;

define method emit-local-definition
    (back-end :: <llvm-back-end>, tmp :: <lexical-local-variable>) => ()
  emit-local-tmp-definition(back-end, tmp);
end method;

define method emit-local-definition
    (back-end :: <llvm-back-end>, tmp :: <lexical-variable>) => ()
//   if (tmp.cell?)
//     format-emit*
//       (back-end, stream, "\t~* % = ~(tmp_%);\n",
//        $dylan-type-string, tmp, $cell-string, tmp);
//   end if
end method;




// Temporary value transfers

define method emit-transfer
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     dst :: <temporary>, src :: <temporary>)
 => ();
  temporary-value(dst) := temporary-value(src);
end method;

define method emit-transfer
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     dst :: <temporary>, src :: <value-reference>)
 => ();
  temporary-value(dst) := emit-reference(back-end, m, src);
end method;

// Results

define method computation-result
    (back-end :: <llvm-back-end>, c :: <computation>, result :: <llvm-value>)
 => ()
  computation-results(back-end, c, list(result));
end method;

define method computation-results
    (back-end :: <llvm-back-end>, c :: <computation>, results :: <sequence>,
     #key rest-results :: false-or(<llvm-value>))
 => ()
  if (c.temporary & c.temporary.used?)
    emit-result-assignment(back-end, c, c.temporary, results, rest-results);
  end if;
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <temporary>, results :: <sequence>,
     rest-results :: false-or(<llvm-value>))
 => ()
  let value = results.first;
  temporary-value(temp) := value;

  if (named?(temp))
    let name = hygienic-mangle(back-end, temp.name, temp.frame-offset);
    ins--local(back-end, name, value);
  end if;
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <multiple-value-temporary>, results :: <sequence>,
     rest-results :: false-or(<llvm-value>))
 => ()
/*
  if (temp.required-values ~= results.size & ~temp.rest-values?)
    error("MV required-values mismatch (%d vs %d) in %=",
          temp.required-values, results.size, c);
  elseif (~temp.rest-values? & rest-results)
    error("MV rest-values mismatch (%= vs %=) in %=",
          temp.rest-values?, rest-results, c);
  end if;
*/
  temporary-value(temp) := pair(results, rest-results);
end method;

define method merge-result
    (back-end :: <llvm-back-end>, c :: <binary-merge>,
     left-bb :: false-or(<llvm-basic-block>),
     right-bb :: false-or(<llvm-basic-block>))
 => ()
  let temp = c.temporary;
  if (temp & temp.used?)
    emit-merge-assignment(back-end, c, temp, left-bb, right-bb);
  end if;
end;

define method emit-merge-assignment
    (back-end :: <llvm-back-end>, c :: <binary-merge>,
     temp :: <temporary>,
     left-bb :: false-or(<llvm-basic-block>),
     right-bb :: false-or(<llvm-basic-block>))
 => ();
  let module = back-end.llvm-builder-module;
  if (left-bb & right-bb)
    let left-value
      = emit-reference(back-end, module, c.merge-left-value | &false);
    let right-value
      = emit-reference(back-end, module, c.merge-right-value | &false);
    let phi = ins--phi(back-end, left-value, left-bb, right-value, right-bb);
    emit-result-assignment(back-end, c, temp, list(phi), #f);
  elseif (left-bb)
    let left-value
      = emit-reference(back-end, module, c.merge-left-value | &false);
    emit-result-assignment(back-end, c, temp, list(left-value), #f);
  elseif (right-bb)
    let right-value
      = emit-reference(back-end, module, c.merge-right-value | &false);
    emit-result-assignment(back-end, c, temp, list(right-value), #f);
  end if;
end method;

define method emit-merge-assignment
    (back-end :: <llvm-back-end>, c :: <binary-merge>,
     temp :: <multiple-value-temporary>,
     left-bb :: false-or(<llvm-basic-block>),
     right-bb :: false-or(<llvm-basic-block>))
 => ();
  if (left-bb & right-bb)
    let left-mv  :: <pair> = temporary-value(c.merge-left-value);
    let right-mv :: <pair> = temporary-value(c.merge-right-value);
    let results = map(method (left-value, right-value)
                        ins--phi(back-end,
                                 left-value, left-bb,
                                 right-value, right-bb)
                      end,
                      left-mv.head, right-mv.head);
    if (left-mv.tail ~= right-mv.tail)
      error("MV merge mismatch: %=", c);
    end;
    emit-result-assignment(back-end, c, temp, results, left-mv.tail);
  elseif (left-bb)
    let left-mv  :: <pair> = temporary-value(c.merge-left-value);
    emit-result-assignment(back-end, c, temp, left-mv.head, left-mv.tail);
  elseif (right-bb)
    let right-mv :: <pair> = temporary-value(c.merge-right-value);
    emit-result-assignment(back-end, c, temp, right-mv.head, right-mv.tail);
  end if;
end method;


/// Computations

define method emit-computations
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <computation>, last)
 => ()
  for (c = c then c.next-computation, while: c & c ~== last)
    // Update the current source location info
    op--scl(back-end, c);

    BLOCK ()
      // Emit the current computation
      if (back-end.llvm-builder-basic-block)
        emit-computation(back-end, m, c);
      end if;
    EXCEPTION (e :: <error>)
      format(*standard-output*, "computation %s: %s\n", c, e);
      error(e);
    END BLOCK;
  end for;
end method emit-computations;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <computation>) => ()
  // Do nothing
  format(*standard-output*, "// Ignore %=\n", c);
end method;

define method closure? (o :: <&lambda-or-code>) => (closure? :: <boolean>)
  ~lambda-top-level?(o) & (closure-size(o.environment) ~= 0)
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <make-closure>)
 => ();
  let o = function(c.computation-closure-method);
  let sigtmp = c.computation-signature-value;
  let key?   = instance?(o, <&keyword-method>);
  if (closure?(o))
    let init? = computation-init-closure?(c);
    let top-level? = computation-top-level-closure?(c);
    let env = o.environment;
    let closure-inits
      = if (init? & ~top-level?) env.closure else #() end if;
    if (c.closure-has-dynamic-extent?)
      // Stack-allocated closure
      let class
        = if (key?)
            dylan-value(#"<keyword-closure-method>")
          else
            dylan-value(#"<simple-closure-method>");
          end if;

      let template = emit-reference(back-end, m, o);
      let closure-size = closure-size(env);
      let closure
        = op--stack-allocate-closure(back-end, class, template, closure-size);

      for (index from 0, init in closure-inits)
        let value = emit-reference(back-end, m, init);
        let ptr
          = op--getslotptr(back-end, closure, class, #"environment-element",
                           index);
        ins--store(back-end, value, ptr);
      end for;

      if (sigtmp)
        // Dynamic signature
        let signature = emit-reference(back-end, m, sigtmp);
        let signature-ptr
          = op--getslotptr(back-end, closure, class, #"function-signature");
        ins--store(back-end, signature, signature-ptr);
      end if;

      let result = ins--bitcast(back-end, closure, $llvm-object-pointer-type);
      computation-result(back-end, c, result);
    else
      if (sigtmp)
        if (top-level?)
          // Top-level method with a dynamic signature
          let signature = emit-reference(back-end, m, sigtmp);
          let name = emit-name(back-end, m, o);
          let global = llvm-builder-global(back-end, name);
          let class = o.^object-class;
          llvm-constrain-type
            (global.llvm-value-type,
             llvm-pointer-to(back-end, llvm-reference-type(back-end, class)));
          let signature-ptr
            = op--getslotptr(back-end, global, class, #"function-signature");
          ins--store(back-end, signature, signature-ptr);
        else
          let result
            = if (init?)
                // Initialized closure
                let primitive
                  = if (key?)
                      primitive-make-keyword-closure-with-environment-signature-descriptor
                    else
                      primitive-make-closure-with-environment-signature-descriptor
                    end if;
                apply(call-primitive, back-end, primitive,
                      emit-reference(back-end, m, o),
                      emit-reference(back-end, m, sigtmp),
                      closure-size(env),
                      map(curry(emit-reference, back-end, m), closure-inits))
              else
                // Uninitialized closure
                let primitive
                  = if (key?)
                      primitive-make-keyword-closure-signature-descriptor
                    else
                      primitive-make-closure-signature-descriptor
                    end if;
                call-primitive(back-end, primitive,
                               emit-reference(back-end, m, o),
                               emit-reference(back-end, m, sigtmp),
                               closure-size(env));
              end if;
          computation-result(back-end, c, result);
        end if;
      else
        let result
          = if (init?)
              // Initialized closure
              let primitive
                = if (key?)
                    primitive-make-keyword-closure-with-environment-descriptor
                  else
                    primitive-make-closure-with-environment-descriptor
                  end if;
              apply(call-primitive, back-end, primitive,
                    emit-reference(back-end, m, o),
                    closure-size(env),
                    map(curry(emit-reference, back-end, m), closure-inits))
            else
              // Uninitialized closure
              let primitive
                = if (key?)
                    primitive-make-keyword-closure-descriptor
                  else
                    primitive-make-closure-descriptor
                  end if;
              call-primitive(back-end, primitive,
                             emit-reference(back-end, m, o),
                             closure-size(env));
            end if;
        computation-result(back-end, c, result);
      end if;
    end if;
  else
    // Not a closure
    if (sigtmp)
      // Dynamic method signature
      let primitive
        = if (key?)
            primitive-make-keyword-method-with-signature-descriptor
          else
            primitive-make-method-with-signature-descriptor
          end if;
      let result
        = call-primitive(back-end, primitive,
                         emit-reference(back-end, m, o),
                         emit-reference(back-end, m, sigtmp));
      computation-result(back-end, c, result);
    else
      // Ordinary compile-time method signature
      computation-result(back-end, c, emit-reference(back-end, m, o));
    end if
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <initialize-closure>)
 => ()
  let o = function(c.computation-closure-method);
  if (closure?(o))
    let env = o.environment;
    let primitive
      = if (instance?(o, <&keyword-method>))
          primitive-initialize-keyword-closure-descriptor
        else
          primitive-initialize-closure-descriptor
       end if;
    apply(call-primitive, back-end, primitive,
          emit-reference(back-end, m, c.computation-closure),
          closure-size(env),
          map(curry(emit-reference, back-end, m), env.closure))
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <variable-reference>) => ();
  let name = emit-name(back-end, m, c.referenced-binding);
  let global = llvm-builder-global(back-end, name);
  llvm-constrain-type(global.llvm-value-type,
		      llvm-pointer-to(back-end,
				      $llvm-object-pointer-type));
  let value = ins--load(back-end, global);
  let result
    = if (c.referenced-binding.binding-thread?
	    & ~llvm-thread-local-support?(back-end))
        call-primitive(back-end, primitive-read-thread-variable-descriptor,
                       value)
      else
	value
      end if;
  computation-result(back-end, c, result);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <function-call>)
 => ()
  let effective-function = call-effective-function(c);
  emit-call(back-end, m, c, effective-function);
  //if (instance?(c.temporary, <multiple-value-temporary>))
  //  emit-transfer(b, s, d, c.temporary, $global-all-rest);
  //end if;
end method;

define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <function-call>, f)
 => ();
  error("%= calling %=", c, f)
end method;

define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <simple-call>, f :: <&iep>)
 => ();
  let name = emit-name(back-end, m, f);
  let global = llvm-builder-global(back-end, name);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  let call
    = ins--call(back-end, global,
                concatenate(map(curry(emit-reference, back-end, m), c.arguments),
                            vector(undef, undef)),
                type: return-type,
                calling-convention:
                  llvm-calling-convention(back-end, f));
  let primary = ins--extractvalue(back-end, call, 0);
  computation-result(back-end, c, primary); // FIXME
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <simple-call>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <method-call>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <engine-node-call>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <apply>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <method-apply>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <engine-node-apply>) => ()
end method;
*/

define method emit-slot-ptr
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <any-slot-value>)
 => (slot-ptr :: <llvm-value>, instance :: <llvm-value>)
  let instance = emit-reference(back-end, m, c.computation-instance);
  let header-words = dylan-value(#"$number-header-words");

  let slot-descriptor = c.computation-slot-descriptor;
  let slot-owner = slot-descriptor.^slot-owner;

  let offset = c.computation-slot-offset;
  let owner-offset = ^slot-offset(slot-descriptor, slot-owner);

  let slot-ptr
    = if (offset = owner-offset)
        // The slot is where we would expect it to be, so we can
        // access it using getelementptr within the owner's mapped
        // struct type
        let instance-struct
          = ins--bitcast(back-end, instance,
                         llvm-pointer-to
                           (back-end, llvm-class-type(back-end, slot-owner)));
        ins--gep-inbounds(back-end, instance-struct, 0,
                          i32(header-words + offset))
      else
        // The slot is at a different offset, and we don't have access
        // to the actual concrete type: treat the instance as a vector
        // of slots
        let instance-slots
          = ins--bitcast(back-end, instance,
                         llvm-pointer-to(back-end, $llvm-object-pointer-type));
        ins--gep-inbounds(back-end, instance-slots, header-words + offset)
      end if;
  values(slot-ptr, instance)
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <slot-value>) => ()
  let word-size = back-end-word-size(back-end);
  let (slot-ptr :: <llvm-value>, instance :: <llvm-value>)
    = emit-slot-ptr(back-end, m, c);
  let result
    = ins--load(back-end, slot-ptr, alignment: word-size);

  if (~computation-guaranteed-initialized?(c))
    let error-bb = make(<llvm-basic-block>);
    let result-bb = make(<llvm-basic-block>);

    // Compare the slot value against the uninitialized slot marker
    let unbound = emit-reference(back-end, m, &unbound);
    let cmp = ins--icmp-eq(back-end, result, unbound);
    ins--br(back-end, cmp, error-bb, result-bb);

    // Throw an error
    ins--block(back-end, error-bb);
    let uis-iep = dylan-value(#"unbound-instance-slot").^iep;
    let uis-global
      = llvm-builder-global(back-end, emit-name(back-end, m, uis-iep));
    llvm-constrain-type
      (uis-global.llvm-value-type,
       llvm-pointer-to(back-end, llvm-lambda-type(back-end, uis-iep)));
    let offset-ref = emit-reference(back-end, m, c.computation-slot-offset);
    let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
    ins--tail-call(back-end, uis-global,
                   vector(instance, offset-ref, undef, undef),
                   calling-convention:
                     llvm-calling-convention(back-end, uis-iep));
    ins--unreachable(back-end);

    // Not uninitialized
    ins--block(back-end, result-bb);
  end if;
  computation-result(back-end, c, result);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <slot-value-setter>) => ()
  let word-size = back-end-word-size(back-end);
  let new-value = emit-reference(back-end, m, c.computation-new-value);
  let (slot-ptr :: <llvm-value>, instance :: <llvm-value>)
    = emit-slot-ptr(back-end, m, c);
  ins--store(back-end, new-value, slot-ptr, alignment: word-size);
  computation-result(back-end, c, new-value);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <repeated-slot-value>) => ()
  let instance = emit-reference(back-end, m, c.computation-instance);
  let desc = c.computation-slot-descriptor;
  let instance-type = desc.^slot-owner;

  let instance-struct
    = ins--bitcast(back-end, instance,
                   llvm-pointer-to(back-end,
                                   llvm-class-type(back-end, instance-type)));
  let offset = c.computation-slot-offset;
  let index = emit-reference(back-end, m, c.computation-index);
  let index
    = if (c.computation-index-tagged?)
        ins--ashr(back-end, index, $dylan-tag-bits)
      else
        index
      end if;
  let slot-ptr
    = ins--gep-inbounds(back-end, instance-struct, 0,
                        i32(dylan-value(#"$number-header-words") + offset),
                        index);
  let result = ins--load(back-end, slot-ptr);

  let iWord-type = back-end.%type-table["iWord"];
  let type = result.llvm-value-type;
  if ((instance?(type, <llvm-integer-type>)
         & type.llvm-integer-type-width < iWord-type.llvm-integer-type-width))
    let zext = ins--zext(back-end, result, iWord-type);
    computation-result(back-end, c, zext);
  else
    computation-result(back-end, c, result);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <repeated-slot-value-setter>) => ()
  let new-value = emit-reference(back-end, m, c.computation-new-value);
  let instance = emit-reference(back-end, m, c.computation-instance);
  let desc = c.computation-slot-descriptor;
  let instance-type = desc.^slot-owner;

  let instance-struct
    = ins--bitcast(back-end, instance,
                   llvm-pointer-to(back-end,
                                   llvm-class-type(back-end, instance-type)));
  let offset = c.computation-slot-offset;
  let index = emit-reference(back-end, m, c.computation-index);
  let index
    = if (c.computation-index-tagged?)
        ins--ashr(back-end, index, $dylan-tag-bits)
      else
        index
      end if;
  let slot-ptr
    = ins--gep-inbounds(back-end, instance-struct, 0,
                        i32(dylan-value(#"$number-header-words") + offset),
                        index);
  let iWord-type = back-end.%type-table["iWord"];
  let type = llvm-repeated-type(back-end, desc.^slot-type);
  if ((instance?(type, <llvm-integer-type>)
         & type.llvm-integer-type-width < iWord-type.llvm-integer-type-width))
    let trunc = ins--trunc(back-end, new-value, type);
    ins--store(back-end, trunc, slot-ptr);
  else
    llvm-constrain-type(new-value.llvm-value-type, type);
    ins--store(back-end, new-value, slot-ptr);
  end if;
  computation-result(back-end, c, new-value);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <stack-vector>) => ()
  let word-size = back-end-word-size(back-end);
  let class :: <&class> = dylan-value(#"<simple-object-vector>");
  let tmp = temporary-value(c.temporary);
  for (argument in c.arguments, i from 0)
    let ptr = op--getslotptr(back-end, tmp, class, #"vector-element", i);
    ins--store(back-end, emit-reference(back-end, m, argument), ptr,
               alignment: word-size);
  end for;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <primitive-call>) => ()
  let primitive-name = c.primitive.primitive-descriptor-getter-name;
  let descriptor = $llvm-primitive-descriptors[primitive-name];

  let arguments
    = map (curry(emit-reference, back-end, m), c.arguments);
  let (#rest results)
    = apply(descriptor.primitive-mapped-emitter, back-end, arguments);
  computation-results(back-end, c, results);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <assignment>) => ()
  let the-value = emit-reference(back-end, m, c.computation-value);
  let name = emit-name(back-end, m, c.assigned-binding);
  let global = llvm-builder-global(back-end, name);
  if (c.assigned-binding.binding-thread?
	& ~llvm-thread-local-support?(back-end))
    llvm-constrain-type(global.llvm-value-type,
			llvm-pointer-to(back-end, $llvm-object-pointer-type));
    let handle = ins--load(back-end, global);
    call-primitive(back-end, primitive-write-thread-variable-descriptor,
		   handle,
		   the-value);
  else
    llvm-constrain-type(global.llvm-value-type,
                        llvm-pointer-to(back-end, the-value.llvm-value-type));
    ins--store(back-end, the-value, global);
  end if;
  computation-result(back-end, c, the-value);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <definition>) => ()
  let the-value = emit-reference(back-end, m, c.computation-value);
  if (c.assigned-binding.binding-thread?
	& ~llvm-thread-local-support?(back-end))
    let handle
      = call-primitive(back-end, primitive-allocate-thread-variable-descriptor,
		       the-value);
    let name = emit-name(back-end, m, c.assigned-binding);
    let global = llvm-builder-global(back-end, name);
    llvm-constrain-type(global.llvm-value-type,
                        llvm-pointer-to(back-end, handle.llvm-value-type));
    ins--store(back-end, handle, global);
    computation-result(back-end, c, the-value);
  else
    next-method();
  end if;
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <redefinition>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <set!>) => ()
end method;
*/

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <conditional-update!>) => ()
  let the-value = emit-reference(back-end, m, c.computation-value);
/*
  if (c.assigned-binding.binding-locked?)
    let name = emit-name(back-end, m, c.referenced-binding);
    let global = llvm-builder-global(back-end, name);

    // FIXME

    computation-result(back-end, c, the-value);
  else
    error("<conditional-update!>");
  end if;
*/
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <type-definition>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <type-redefinition>) => ()
end method;
*/

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <temporary-transfer-computation>)
 => ()
  emit-transfer(back-end, m, c.temporary, c.computation-value);
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <temporary-transfer>) => ()
end method;
*/

// if

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <if>) => ()
  // Condition test
  let test-value = emit-reference(back-end, m, c.test);
  let cmp = ins--icmp-ne(back-end,
                         test-value,
                         emit-reference(back-end, m, &false));

  // Basic blocks
  let condition-branch-bb = back-end.llvm-builder-basic-block;
  let merge-bb = make(<llvm-basic-block>);

  let merge :: <if-merge> = next-computation(c);
  local
    method dead-branch?
        (branch :: <computation>, ref :: false-or(<value-reference>))
      branch == merge & ~ref
    end method;

  let (consequent-branch-bb, alternative-branch-bb)
    = if (dead-branch?(c.consequent, merge-left-value(merge)))
        // Alternative only
        let alternative-bb = make(<llvm-basic-block>);
        ins--br(back-end, cmp, merge-bb, alternative-bb);

        ins--block(back-end, alternative-bb);
        emit-computations(back-end, m, c.alternative, merge);
        let alternative-branch-bb = back-end.llvm-builder-basic-block;
        if (alternative-branch-bb)
          ins--br(back-end, merge-bb);
        end;

        values(condition-branch-bb, alternative-branch-bb)
      elseif (dead-branch?(c.alternative, merge-right-value(merge)))
        // Consequent only
        let consequent-bb = make(<llvm-basic-block>);
        ins--br(back-end, cmp, consequent-bb, merge-bb);

        ins--block(back-end, consequent-bb);
        emit-computations(back-end, m, c.consequent, merge);
        let consequent-branch-bb = back-end.llvm-builder-basic-block;
        if (consequent-branch-bb)
          ins--br(back-end, merge-bb);
        end;

        values(consequent-branch-bb, condition-branch-bb)
      else
        // Both
        let consequent-bb = make(<llvm-basic-block>);
        let alternative-bb = make(<llvm-basic-block>);
        ins--br(back-end, cmp, consequent-bb, alternative-bb);

        ins--block(back-end, consequent-bb);
        emit-computations(back-end, m, c.consequent, merge);
        let consequent-branch-bb = back-end.llvm-builder-basic-block;
        if (consequent-branch-bb)
          ins--br(back-end, merge-bb);
        end;

        ins--block(back-end, alternative-bb);
        emit-computations(back-end, m, c.alternative, merge);
        let alternative-branch-bb = back-end.llvm-builder-basic-block;
        if (alternative-branch-bb)
          ins--br(back-end, merge-bb);
        end;

        values(consequent-branch-bb, alternative-branch-bb)
      end if;

  // Common control flow (corresponding to <if-merge>)
  if (consequent-branch-bb | alternative-branch-bb)
    ins--block(back-end, merge-bb);
    merge-result(back-end, merge, consequent-branch-bb, alternative-branch-bb);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <if-merge>) => ()
  // Handled within <if> generation; nothing to do here
end method;

// Looping

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <loop>) => ()
  // Basic blocks
  let init-bb = back-end.llvm-builder-basic-block;
  let merge-bb = make(<llvm-basic-block>);

  // Init values
  let initial-values
    = map(method (merge-c :: <loop-merge>)
            emit-reference(back-end, m, merge-c.loop-merge-parameter)
          end,
          c.loop-merges);

  // Branch to loop head
  ins--br(back-end, merge-bb);

  // Loop head PHI instructions
  c.%label := merge-bb;
  ins--block(back-end, merge-bb);
  for (merge-c :: <loop-merge> in c.loop-merges, value in initial-values)
    // Construct the initial phi operands
    let phi-operands = make(<stretchy-object-vector>);
    merge-operands(merge-c.temporary) := phi-operands;
    add-merge-operands(merge-c.temporary, value, init-bb);

    // Insert the phi instruction
    let phi
      = make(<llvm-phi-node>,
             operands: phi-operands,
             metadata: list(back-end.llvm-builder-dbg));
    add!(merge-bb.llvm-basic-block-instructions, phi);
    computation-result(back-end, merge-c, phi);
  end for;

  // Loop body
  emit-computations(back-end, m, c.loop-body, c.next-computation);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <loop-merge>) => ()
  unless (loop-merge-initial?(c))
    emit-transfer(back-end, m, c.temporary, c.loop-merge-parameter);
  end unless;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <loop-call>) => ()
  let loop :: <loop> = loop-call-loop(c);

  for (initial-merge in loop-merges(loop), call-merge in loop-call-merges(c))
    let value = emit-reference(back-end, m, loop-merge-argument(call-merge));

    // Add this value (and its source basic block) to the PHI
    // instruction of the loop
    add-merge-operands(initial-merge.temporary,
                       value, back-end.llvm-builder-basic-block);
  end for;

  // Branch to loop head
  ins--br(back-end, loop.%label);

  // Fake result
  computation-result(back-end, c, emit-reference(back-end, m, &false));
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <end-loop>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <bind-exit>) => ()
/*
  if (c.entry-state.local-entry-state?)
    let exit-bb = make(<llvm-basic-block>);

    emit-computations(back-end, c.body, merge);

    maybe-emit-merge-transfer(back-end, merge, merge-body-value);
    emit-label(back-end, c);
  else
    let merge-tmp = merge.temporary;
    let result = c.temporary.used? & emit-object(back-end, #f, merge-tmp);
    let end-tag = make-tag(back-end);

    let bind-exit-frame = op--start-bind-exit(back-end, c, end-tag);

    dynamic-bind (*live-nlx-tags* = pair(end-tag, *live-nlx-tags*))
      emit-computations(back-end, c.body, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-body-value);
    end dynamic-bind;

    // obliged to set up reg-result for Harp
    if (result)
      ins--move(back-end, back-end.registers.reg-result, result);
    end if;

    ins--tag(back-end, end-tag);
    // Deallocate bind-exit frame by restoring frame pointer
    ins--add(back-end, back-end.registers.reg-stack, bind-exit-frame, $bind-exit-frame-size);
    // in the event of a NLX, must retrieve result from reg-result
    ins--move(back-end, result, back-end.registers.reg-result);
    // note that blocks are converted in all-rest context
    if (result & (required-values(merge-tmp) > 0))
      unspill-multiple-values(back-end, merge-tmp, #f);
    end if;
  end if;
*/
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <exit>) => ()
/*
  if (c.entry-state.local-entry-state?)
    let me-block :: <bind-exit> = c.entry-state.me-block;
    let nc = me-block.next-computation;
    let temp = if (instance?(nc, <bind-exit-merge>)) temporary(nc) end;

    if (temp & used?(temp))
      emit-transfer(back-end, temp, c.computation-value);
    end if;
    emit-goto(back-end, me-block);
  else
    // spill to MV area before doing non-local exit (which saves
    // the MV area values)
    if (instance?(c.computation-value, <multiple-value-temporary>)
        & required-values(c.computation-value) > 1)
      spill-multiple-values(back-end, c.computation-value);
    end if;
    ins--call(back-end, $primitive-nlx.runtime-reference,
	      push-arguments(back-end, list(emit-reference(back-end, #f, c.entry-state),
					    emit-reference(back-end, #f, c.computation-value))));

    // redundant move to get around Harp Live Registers
    let result = emit-destination-reference(back-end, #f, c.temporary);
    emit-result-assignment(back-end, result);
  end if;
*/
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <bind-exit-merge>) => ()
  // FIXME
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <unwind-protect>)
 => ()
  error("<unwind-protect>");
/*
  let protect-temp =  c.protected-temporary;
  let protected-result = emit-reference(back-end, #f, protect-temp);

  let cleanup-tag = make-tag(back-end);
  let continue-tag = make-tag(back-end);

  op--start-unwind-protect(back-end, c, cleanup-tag);

  dynamic-bind (*live-nlx-tags* = pair(cleanup-tag, *live-nlx-tags*))
    emit-computations(back-end, c.body, c.next-computation);
  end dynamic-bind;

  let multiple-values =
    case
      false-reference?(protected-result) => #();
      local-multiple-value-temporary?(protect-temp) =>
        mv-temporary(back-end, protect-temp).mv-elements;
      otherwise => list(protected-result);
    end case;

  op--unwind-protect-cleanup(back-end, c, cleanup-tag, continue-tag);

  ins--tag(back-end, cleanup-tag);
  for (value in multiple-values)
    ins--force-d(back-end, value);
  end for;

  preserving-cleanup-state(back-end)
    emit-computations(back-end, c.cleanups, c.next-computation);
  end;

  ins--end-cleanup(back-end, continue-tag);

  ins--tag(back-end, continue-tag);
*/
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <return>) => ();
  let undef-struct
    = make(<llvm-undef-constant>,
           type: llvm-reference-type(back-end, back-end.%mv-struct-type));
  if (instance?(c.computation-value, <multiple-value-temporary>))
    let mv :: <pair> = temporary-value(c.computation-value);
    if (mv.head.size = 0)      // FIXME
      let value-struct
        = ins--insertvalue(back-end, undef-struct,
                           emit-reference(back-end, m, &false), 0);
      let value-count-struct
        = ins--insertvalue(back-end, value-struct, i8(0), 1);
      ins--ret(back-end, value-count-struct);
    elseif (mv.head.size = 1)
      // FIXME
      let value-struct
        = ins--insertvalue(back-end, undef-struct, mv.head.first, 0);
      let value-count-struct
        = ins--insertvalue(back-end, value-struct, i8(1), 1);
      ins--ret(back-end, value-count-struct);
    else
      error("FIXME: MV return %d", c.computation-value.required-values);
    end if;
  else
    let test-value = emit-reference(back-end, m, c.computation-value);
    ins--ret(back-end, test-value);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <end-exit-block>)
 => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <end-protected-block>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <end-cleanup-block>)
 => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <bind>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <values>) => ()
  // FIXME <values> should do an automatic adjust
  let fixed = map(curry(emit-reference, back-end, m), c.fixed-values);
  let rest = c.rest-value & emit-reference(back-end, m, c.rest-value);
  computation-results(back-end, c, fixed, rest-results: rest);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <extract-single-value>) => ()
  let mv :: <pair> = temporary-value(c.computation-value);
  let vals = mv.head;
  if (c.index < vals.size)
    computation-result(back-end, c, element(vals, c.index));
  else
    error("Not enough fixed (%d) for %=", vals.size, c);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <extract-rest-value>) => ()
  // FIXME
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-spill>) => ()
  // FIXME
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-unspill>) => ()
  // FIXME
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <adjust-multiple-values>)
 => ()
  next-method();  // do transfer
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <adjust-multiple-values-rest>)
 => ()
  next-method();  // do transfer
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <check-type>) => ()
  emit-type-check(back-end,
                  emit-reference(back-end, m, c.computation-value),
                  c.type);
  next-method();  // do transfer
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <keyword-check-type>) => ()
  emit-type-check(back-end,
                  emit-reference(back-end, m, c.computation-value),
                  c.type);
  emit-transfer(back-end, m, c.temporary, c.computation-value);
end method;
*/

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <constrain-type>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <assignment-check-type>) => ()
end method;
*/

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-check-type>) => ()
  let mv :: <pair> = temporary-value(c.computation-value);
  for (value in mv.head, type in c.types)
    emit-type-check(back-end, value, type);
  end for;
  next-method();
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <multiple-value-check-type-rest>)
 => ()
/*
  let mv :: <pair> = temporary-value(c.computation-value);
  for (value in mv.head, type in c.types)
    emit-type-check(back-end, value, type);
  end for;
*/
  next-method();
  // FIXME rest-type
end method;

/*
define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-result-check-type>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-result-check-type-rest>) => ()
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <guarantee-type>) => ()
end method;
*/

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <make-cell>) => ()
  let value = emit-reference(back-end, m, c.computation-value);
  let tmp = c.temporary;
  let rep = cell-representation(cell-type(tmp));
  if (closed-over?(tmp))
    let cell = op--make-closed-over-cell(back-end, rep, value);
    temporary-value(tmp) := cell;
  else
    let type = llvm-reference-type(back-end, rep);

    let alloca = ins--alloca(back-end, type, i32(1));
    if (named?(tmp))
      let local-name = hygienic-mangle(back-end, tmp.name, tmp.frame-offset);
      ins--local(back-end, local-name, alloca);

      let dbg-function = back-end.llvm-back-end-dbg-functions.last;
      let (dbg-file, dbg-line)
        = source-location-dbg-file-line(back-end, c.dfm-source-location);
      let var-type
        = llvm-reference-dbg-type(back-end, cell-type(tmp));
      let function-name
        = back-end.llvm-builder-function.llvm-global-name;
      let v
        = make(<llvm-metadata-node>,
               function-local?: #t,
               node-values: list(alloca));
      let lv
        = llvm-make-dbg-local-variable(#"auto",
                                       dbg-function,
                                       as(<string>, tmp.name),
                                       dbg-file, dbg-line,
                                       var-type,
                                       module: m,
                                       function-name: function-name);
      ins--call-intrinsic(back-end, "llvm.dbg.declare", vector(v, lv));
    end if;
    temporary-value(tmp) := alloca;
    ins--store(back-end, value, alloca);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <get-cell-value>) => ()
  if (closed-over?(c.computation-cell))
    let cell = emit-reference(back-end, m, c.computation-cell);
    let rep = cell-representation(cell-type(c.computation-cell));
    let value = op--get-closed-over-cell(back-end, rep, cell);
    computation-result(back-end, c, value);
  else
    let value = ins--load(back-end, temporary-value(c.computation-cell));
    computation-result(back-end, c, value);
  end if;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <set-cell-value!>) => ()
  let value = emit-reference(back-end, m, c.computation-value);
  if (closed-over?(c.computation-cell))
    let cell = emit-reference(back-end, m, c.computation-cell);
    let rep = cell-representation(cell-type(c.computation-cell));
    op--set-closed-over-cell(back-end, rep, cell, value)
  else
    ins--store(back-end, value, temporary-value(c.computation-cell));
  end if;
  computation-result(back-end, c, value);
end method;

