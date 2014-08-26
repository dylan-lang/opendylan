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
    (back-end :: <llvm-back-end>, c :: <computation>,
     result :: type-union(<llvm-value>, <llvm-mv>))
 => ()
  if (c.temporary & c.temporary.used?)
    emit-result-assignment(back-end, c, c.temporary, result);
  end if;
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <temporary>, result :: <llvm-value>)
 => ()
  temporary-value(temp) := result;

  if (named?(temp) & *temporary-locals?*)
    let name = hygienic-mangle(back-end, temp.name, temp.frame-offset);
    if (instance?(result, <llvm-instruction>)
          & ~llvm-builder-local-defined?(back-end, name))
      ins--local(back-end, name, result);
    end;
  end if;
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <temporary>, result :: <llvm-global-mv>)
 => ()
  let primary = ins--extractvalue(back-end, result.llvm-mv-struct, 0);
  emit-result-assignment(back-end, c, temp, primary);
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <multiple-value-temporary>, result :: <llvm-global-mv>)
 => ();
  if (temp.rest-values?)
    // No maximum
    temporary-value(temp) := result;
  else
    // Limit maximum available values
    temporary-value(temp)
      := make(<llvm-global-mv>,
              struct: result.llvm-mv-struct,
              maximum: temp.required-values);
  end if
end method;

define method emit-result-assignment
    (back-end :: <llvm-back-end>, c :: <computation>,
     temp :: <multiple-value-temporary>, result :: <llvm-local-mv>)
 => ();
  temporary-value(temp) := result;
end method;

define method merge-results
    (back-end :: <llvm-back-end>, c :: <merge>, results :: <vector>)
 => ();
  let module = back-end.llvm-builder-module;
  let temp = c.temporary;
  if (results.empty? | ~temp.used?)
    #f // Do nothing
  elseif (results.size = 2)
    // Only one predecessor block
    emit-transfer(back-end, module, temp, results[0]);
  else
    emit-merge-assignment(back-end, c, temp, results);
  end if;
end method;

define method emit-merge-assignment
    (back-end :: <llvm-back-end>, c :: <merge>,
     temp :: <temporary>, results :: <vector>)
 => ();
  let module = back-end.llvm-builder-module;
  let phi-operands = make(<simple-object-vector>, size: results.size);
  for (i :: <integer> from 0 below results.size by 2)
    with-insert-before-terminator (back-end, results[i + 1])
      phi-operands[i] := emit-reference(back-end, module, results[i]);
    end;
    phi-operands[i + 1] := results[i + 1];
  end for;
  emit-result-assignment(back-end, c, temp, ins--phi(back-end, phi-operands));
end method;

define method emit-merge-assignment
    (back-end :: <llvm-back-end>, c :: <merge>,
     temp :: <multiple-value-temporary>, results :: <vector>)
 => ();
  // Choose a merge strategy:
  // - Merge as a <llvm-local-mv> if any of the merge operands are
  //   <llvm-local-mv> values with two or more values
  // - Otherwise merge as a <llvm-global-mv> if any of the merge operands
  //   are <llvm-global-mv> values
  // - Otherwise merge as a <llvm-local-mv>
  let post-primary? = temp.rest-values? | temp.required-values > 1;
  iterate loop (i :: <integer> = 0, strategy = #f)
    if (i < results.size)
      let mv = temporary-value(results[i]);
      if (instance?(mv, <llvm-local-mv>)
            & post-primary?
            & (mv.llvm-mv-fixed.size > 1 | mv.llvm-mv-rest))
        emit-local-merge-assignment(back-end, c, temp, results);
      elseif (instance?(mv, <llvm-global-mv>))
        loop(i + 2, emit-global-merge-assignment);
      else
        loop(i + 2, strategy);
      end if;
    else
      let strategy = strategy | emit-local-merge-assignment;
      strategy(back-end, c, temp, results);
    end if;
  end iterate;
end method;

define method emit-local-merge-assignment
    (back-end :: <llvm-back-end>, c :: <merge>,
     temp :: <multiple-value-temporary>, results :: <vector>)
 => ();
  // Determine how many phi instructions we need
  let (count, rest?)
    = iterate loop (i :: <integer> = 0, count = temp.required-values,
                    rest? = #f)
        if (i < results.size)
          let mv = temporary-value(results[i]);
          if (instance?(mv, <llvm-local-mv>))
            loop(i + 2,
                 max(count, mv.llvm-mv-fixed.size),
                 rest? | (mv.llvm-mv-rest & temp.rest-values?))
          else
            loop(i + 2, max(count, 1), rest?)
          end if
        else
          values(count, rest?)
        end if
      end;

  // Create phi instructions for fixed values
  let fixed = make(<stretchy-object-vector>);
  for (m from 0 below count)
    let phi-operands = make(<simple-object-vector>, size: results.size);
    for (i :: <integer> from 0 below results.size by 2)
      let mv = temporary-value(results[i]);
      with-insert-before-terminator (back-end, results[i + 1])
        phi-operands[i] := op--mv-extract(back-end, mv, m);
      end;
      phi-operands[i + 1] := results[i + 1];
    end for;
    add!(fixed, ins--phi(back-end, phi-operands));
  end for;

  // Create a phi instruction for rest values if needed
  let rest
    = if (rest?)
        let phi-operands = make(<simple-object-vector>, size: results.size);
        for (i :: <integer> from 0 below results.size by 2)
          let mv = temporary-value(results[i]);
          with-insert-before-terminator (back-end, results[i + 1])
            phi-operands[i] := op--mv-extract-rest(back-end, mv, count);
          end;
          phi-operands[i + 1] := results[i + 1];
        end for;
        ins--phi(back-end, phi-operands);
      end if;

  let mv = make(<llvm-local-mv>, fixed: fixed, rest: rest);
  emit-result-assignment(back-end, c, temp, mv);
end method;

define method emit-global-merge-assignment
    (back-end :: <llvm-back-end>, c :: <merge>,
     temp :: <multiple-value-temporary>, results :: <vector>)
 => ();
  // FIXME set maximum
  let phi-operands = make(<simple-object-vector>, size: results.size);
  for (i :: <integer> from 0 below results.size by 2)
    let mv = temporary-value(results[i]);
    if (instance?(mv, <llvm-local-mv>))
      with-insert-before-terminator (back-end, results[i + 1])
        let value = op--mv-extract(back-end, mv, 0);
        phi-operands[i] := op--global-mv-struct(back-end, value, i8(1));
      end;
    else
      phi-operands[i] := mv.llvm-mv-struct;
    end if;
    phi-operands[i + 1] := results[i + 1];
  end for;

  let phi = ins--phi(back-end, phi-operands);
  let mv = make(<llvm-global-mv>, struct: phi);
  emit-result-assignment(back-end, c, temp, mv);
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
  let call = emit-call(back-end, m, c, effective-function);

  // Mark path unreachable if called function never returns
  let temp = c.temporary;
  if (temp & instance?(type-estimate(temp), <type-estimate-bottom>))
    ins--unreachable(back-end);
  else
    computation-result(back-end, c, make(<llvm-global-mv>, struct: call));
  end if;
end method;

// Call a known top-level method with no further next methods
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <simple-call>, f :: <&iep>)
 => (call :: <llvm-value>);
  let name = emit-name(back-end, m, f);
  let global = llvm-builder-global(back-end, name);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  let env = f.environment;
  let fn
    = if (~env | empty?(env.closure))
        // Not a closure... we don't need to pass the <function> object
        undef
      else
        emit-reference(back-end, m, f);
      end if;
  ins--call(back-end, global,
            concatenate(map(curry(emit-reference, back-end, m), c.arguments),
                        vector(undef, fn)),
            type: return-type,
            calling-convention: llvm-calling-convention(back-end, f))
end method;

// Call a known top-level method with the possibility of next methods
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <method-call>, f :: <&iep>)
 => (call :: <llvm-value>);
  let name = emit-name(back-end, m, f);
  let global = llvm-builder-global(back-end, name);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let undef = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);
  let next
    = if (^next?(function(f)))
        emit-reference(back-end, m, c.next-methods)
      else
        undef
      end if;
  ins--call(back-end, global,
           concatenate(map(curry(emit-reference, back-end, m), c.arguments),
                       vector(next, undef)),
           type: return-type,
           calling-convention: llvm-calling-convention(back-end, f))
end method;

// Method calls
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <method-call>, f)
 => (call :: <llvm-value>);
  let word-size = back-end-word-size(back-end);

  let fn = emit-reference(back-end, m, f);
  let fn-cast = op--object-pointer-cast(back-end, fn, #"<lambda>");
  let mep-slot-ptr = op--getslotptr(back-end, fn-cast, #"<lambda>", #"mep");
  let mep = ins--load(back-end, mep-slot-ptr, alignment: word-size);

  // Cast to the appropriate MEP type
  let parameter-types
    = make(<simple-object-vector>, size: c.arguments.size + 3);
  parameter-types[0] := $llvm-object-pointer-type; // next
  parameter-types[1] := $llvm-object-pointer-type; // function
  parameter-types[2] := back-end.%type-table["iWord"]; // argument count
  fill!(parameter-types, $llvm-object-pointer-type, start: 3);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let mep-type
    = make(<llvm-function-type>,
           return-type: return-type,
           parameter-types: parameter-types,
           varargs?: #f);
  let mep-cast
    = ins--bitcast(back-end, mep, llvm-pointer-to(back-end, mep-type));

  let next
    = emit-reference(back-end, m, c.next-methods);

  ins--call(back-end, mep-cast,
            concatenate(vector(next, fn, c.arguments.size),
                        map(curry(emit-reference, back-end, m),
                            c.arguments)),
           type: return-type,
           calling-convention: $llvm-calling-convention-c)
end method;

// Generic function call through a known <engine-node>
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <engine-node-call>, f :: <&generic-function>)
 => (call :: <llvm-value>);
  let word-size = back-end-word-size(back-end);

  // Retrieve the entry point function from the engine node
  let node = emit-reference(back-end, m, c.engine-node);
  let node-cast = op--object-pointer-cast(back-end, node, #"<engine-node>");
  let node-ep-ptr
    = op--getslotptr(back-end, node-cast,
                     #"<engine-node>", #"engine-node-entry-point");
  let ep = ins--load(back-end, node-ep-ptr, alignment: word-size);

  let fn = emit-reference(back-end, m, f);

  // Cast to the appropriate engine-node entry point type
  let parameter-types
    = make(<simple-object-vector>, size: c.arguments.size + 3);
  parameter-types[0] := $llvm-object-pointer-type; // node
  parameter-types[1] := $llvm-object-pointer-type; // function
  parameter-types[2] := back-end.%type-table["iWord"]; // argument count
  fill!(parameter-types, $llvm-object-pointer-type, start: 3);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let ep-type
    = make(<llvm-function-type>,
           return-type: return-type,
           parameter-types: parameter-types,
           varargs?: #f);
  let ep-cast
    = ins--bitcast(back-end, ep, llvm-pointer-to(back-end, ep-type));

  ins--call(back-end, ep-cast,
            concatenate(vector(node, fn, c.arguments.size),
                        map(curry(emit-reference, back-end, m),
                            c.arguments)),
            type: return-type,
            calling-convention: $llvm-calling-convention-c)
end method;

// Calls to general functions using the XEP
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <simple-call>, f)
 => (call :: <llvm-value>);
  let word-size = back-end-word-size(back-end);

  let fn = emit-reference(back-end, m, f);
  let fn-cast = op--object-pointer-cast(back-end, fn, #"<function>");
  let xep-slot-ptr = op--getslotptr(back-end, fn-cast, #"<function>", #"xep");
  let xep = ins--load(back-end, xep-slot-ptr, alignment: word-size);

  // Cast to the appropriate XEP type
  let parameter-types
    = make(<simple-object-vector>, size: c.arguments.size + 2);
  parameter-types[0] := $llvm-object-pointer-type; // function
  parameter-types[1] := back-end.%type-table["iWord"]; // argument count
  fill!(parameter-types, $llvm-object-pointer-type, start: 2);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  let xep-type
    = make(<llvm-function-type>,
           return-type: return-type,
           parameter-types: parameter-types,
           varargs?: #f);
  let xep-cast
    = ins--bitcast(back-end, xep, llvm-pointer-to(back-end, xep-type));

  ins--call(back-end, xep-cast,
            concatenate(vector(fn, c.arguments.size),
                        map(curry(emit-reference, back-end, m),
                            c.arguments)),
            type: return-type,
            calling-convention: $llvm-calling-convention-c)
end method;

// Possibly congruent calls to a generic function
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <simple-call>, f :: <&generic-function>)
 => (call :: <llvm-value>);
  if (call-congruent?(c))
    let gfn = emit-reference(back-end, m, f);
    op--engine-node-call(back-end, gfn, c.arguments.size,
                         map(curry(emit-reference, back-end, m), c.arguments))
  else
    next-method()
  end if
end method;

// Calls through a function's XEP using apply()
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <apply>, f)
 => (call :: <llvm-value>);
  let apply-xep-entry-point
    = llvm-entry-point-function(back-end, apply-xep-descriptor,
                                c.arguments.size);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  ins--call(back-end, apply-xep-entry-point,
            concatenate(vector(emit-reference(back-end, m, f)),
                        map(curry(emit-reference, back-end, m),
                            c.arguments)),
            type: return-type,
            calling-convention: $llvm-calling-convention-fast)
end method;

// Calls through a function's MEP using apply() where next-methods are known
define method emit-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <method-apply>, f)
 => (call :: <llvm-value>);
  let apply-mep-entry-point
    = llvm-entry-point-function(back-end, apply-mep-descriptor,
                                c.arguments.size);
  let return-type = llvm-reference-type(back-end, back-end.%mv-struct-type);
  ins--call(back-end, apply-mep-entry-point,
            concatenate(vector(emit-reference(back-end, m, c.next-methods),
                               emit-reference(back-end, m, f)),
                        map(curry(emit-reference, back-end, m),
                            c.arguments)),
            type: return-type,
            calling-convention: $llvm-calling-convention-fast)
end method;

define method emit-slot-ptr
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <any-slot-value>)
 => (slot-ptr :: <llvm-value>, slot-type :: <llvm-type>,
     instance :: <llvm-value>)
  let instance = emit-reference(back-end, m, c.computation-instance);
  let header-words = dylan-value(#"$number-header-words");

  let slot-descriptor = c.computation-slot-descriptor;
  let slot-owner = slot-descriptor.^slot-owner;
  let slot-type = llvm-reference-type(back-end, slot-descriptor.^slot-type);

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
                         llvm-pointer-to(back-end, slot-type));
        ins--gep-inbounds(back-end, instance-slots, header-words + offset)
      end if;
  values(slot-ptr, slot-type, instance)
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <slot-value>) => ()
  let word-size = back-end-word-size(back-end);
  let (slot-ptr :: <llvm-value>, slot-type :: <llvm-type>,
       instance :: <llvm-value>) = emit-slot-ptr(back-end, m, c);
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
  let (slot-ptr :: <llvm-value>, slot-type :: <llvm-type>,
       instance :: <llvm-value>) = emit-slot-ptr(back-end, m, c);
  llvm-constrain-type(new-value.llvm-value-type, slot-type);
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
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <primitive-call>)
 => ();
  emit-primitive-call(back-end, m, c, c.primitive)
end method;

define method emit-primitive-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <primitive-call>, primitive :: <&primitive>)
 => ();
  let primitive-name = primitive.primitive-descriptor-getter-name;
  let descriptor
    = element($llvm-primitive-descriptors, primitive-name, default: #f)
    | error("No primitive named %=", primitive-name);

  let arguments
    = map(curry(emit-reference, back-end, m), c.arguments);
  let (#rest results)
    = apply(descriptor.primitive-mapped-emitter, back-end, arguments);
  if (llvm-primitive-values-rest?(back-end, descriptor))
    let mv = make(<llvm-global-mv>, struct: results.first);
    computation-result(back-end, c, mv);
  elseif (results.size = 1)
    computation-result(back-end, c, results.first);
  else
    computation-result(back-end, c, make(<llvm-local-mv>, fixed: results));
  end if;
end method;

define method emit-primitive-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <primitive-call>, primitive :: <&c-function>)
 => ();
  let name = primitive.binding-name;
  let calling-convention
    = llvm-c-function-calling-convention(back-end, primitive);
  let function-type = llvm-c-function-type(back-end, primitive);

  // Call the C function
  let result
    = ins--call(back-end,
                llvm-builder-global(back-end, name),
                map(curry(emit-reference, back-end, m), c.arguments),
                type: function-type.llvm-function-type-return-type,
                calling-convention: calling-convention);
  computation-result(back-end, c, result);
end method;

define method emit-primitive-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <primitive-indirect-call>, primitive :: <&c-function>)
 => ();
  let calling-convention
    = llvm-c-function-calling-convention(back-end, primitive);
  let function-type = llvm-c-function-type(back-end, primitive);
  let function-ptr-type = llvm-pointer-to(back-end, function-type);
  let function
    = ins--bitcast(back-end, emit-reference(back-end, m, c.arguments.first),
                   function-ptr-type);

  // Call the C function
  let result
    = ins--call(back-end, function,
                map(curry(emit-reference, back-end, m),
                    copy-sequence(c.arguments, start: 1)),
                type: function-type.llvm-function-type-return-type,
                calling-convention: calling-convention);
  computation-result(back-end, c, result);
end method;

define method llvm-c-function-calling-convention
    (back-end :: <llvm-back-end>, o :: <&c-function>)
 => (calling-convention :: <integer>);
  $llvm-calling-convention-c
end method;

define method llvm-c-function-calling-convention
    (back-end :: <llvm-x86-windows-back-end>, o :: <&c-function>)
 => (calling-convention :: <integer>);
  if (o.c-modifiers = "__stdcall")
    $llvm-calling-convention-x86-stdcall
  else
    $llvm-calling-convention-c
  end if
end method;

define method emit-primitive-call
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <c-variable-pointer-call>, primitive)
 => ();
  let result = llvm-builder-global(back-end, c.c-variable.name);
  computation-result(back-end, c, result);
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
  let merge-temp = merge.temporary;

  let phi-operands = make(<stretchy-object-vector>);
  merge-operands(merge-temp) := phi-operands;

  let consequent-bb = make(<llvm-basic-block>);
  let alternative-bb = make(<llvm-basic-block>);
  ins--br(back-end, cmp, consequent-bb, alternative-bb);

  ins--block(back-end, consequent-bb);
  emit-computations(back-end, m, c.consequent, merge);
  let consequent-branch-bb = back-end.llvm-builder-basic-block;
  if (consequent-branch-bb)
    add-merge-operands(merge-temp, merge.merge-left-value | &false,
                       consequent-branch-bb);
    ins--br(back-end, merge-bb);
  end;

  ins--block(back-end, alternative-bb);
  emit-computations(back-end, m, c.alternative, merge);
  let alternative-branch-bb = back-end.llvm-builder-basic-block;
  if (alternative-branch-bb)
    add-merge-operands(merge-temp, merge.merge-right-value | &false,
                       alternative-branch-bb);
    ins--br(back-end, merge-bb);
  end;

  // Common control flow (corresponding to <if-merge>)
  if (consequent-branch-bb | alternative-branch-bb)
    ins--block(back-end, merge-bb);
    merge-results(back-end, merge, phi-operands);
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
  let value = c.computation-value;
  do-emit-return(back-end, m, value, temporary-value(value));
end method;

define method do-emit-return
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     temp :: <multiple-value-temporary>, mv :: <llvm-global-mv>)
 => ();
  ins--ret(back-end, mv.llvm-mv-struct);
end method;

define method do-emit-return
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     temp :: <multiple-value-temporary>, mv :: <llvm-local-mv>)
 => ();
  let undef-struct
    = make(<llvm-undef-constant>,
           type: llvm-reference-type(back-end, back-end.%mv-struct-type));
  if (mv.llvm-mv-fixed.empty?)
    let result
      = op--global-mv-struct(back-end,
                             emit-reference(back-end, m, &false),
                             i8(0));
    ins--ret(back-end, result);
  else
    // Store values beyond the first in the MV area
    let count = mv.llvm-mv-fixed.size;
    for (i from 1 below count)
      let ptr = op--teb-getelementptr(back-end, #"teb-mv-area", i);
      ins--store(back-end, mv.llvm-mv-fixed[i], ptr);
    end for;

    if (mv.llvm-mv-rest)
      error("FIXME return llvm-mv-rest")
    end;

    // Return the primary value in the MV struct
    let result
      = op--global-mv-struct(back-end, mv.llvm-mv-fixed.first, i8(count));
    ins--ret(back-end, result);
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
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <values>) => ();
  let (values-rest-constant?, values-rest) = fast-constant-value?(c.rest-value);
  let (fixed, rest)
    = if (values-rest-constant?)
        let values-rest-refs = map(make-object-reference, values-rest);
        values(concatenate(c.fixed-values, values-rest-refs), #f)
      elseif (c.rest-value & instance?(generator(c.rest-value), <stack-vector>))
        let sv-c :: <stack-vector> = generator(c.rest-value);
        values(concatenate(c.fixed-values, sv-c.arguments), #f)
      else
        values(c.fixed-values, c.rest-value)
      end;
  let result
    = make(<llvm-local-mv>,
           fixed: map(curry(emit-reference, back-end, m), fixed),
           rest: rest & emit-reference(back-end, m, rest));
  computation-result(back-end, c, result);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <extract-single-value>) => ();
  let value = c.computation-value;
  computation-result(back-end, c,
                     op--mv-extract(back-end, temporary-value(value), c.index));
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <extract-rest-value>) => ()
  let mv = temporary-value(c.computation-value);
  computation-result(back-end, c, op--mv-extract-rest(back-end, mv, c.index));
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-spill>) => ()
  let value = c.computation-value;
  let spill = op--protect-temporary(back-end, value, temporary-value(value));
  computation-result(back-end, c, spill);
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>, c :: <multiple-value-unspill>) => ()
  let comp = c.computation-value;
  let spill-value
    = if (instance?(comp.generator, <multiple-value-spill>))
        comp.generator.computation-value;
      else
        error("emit-computation <multiple-value-spill>");
      end if;
  op--restore-temporary(back-end, spill-value, temporary-value(spill-value),
                        temporary-value(comp));
  computation-result(back-end, c, temporary-value(spill-value));
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
  do-emit-mv-check-type(back-end, m, c, temporary-value(c.computation-value));
  next-method();
end method;

define method do-emit-mv-check-type
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <multiple-value-check-type>, mv :: <llvm-local-mv>)
 => ();
  for (value in mv.llvm-mv-fixed, type in c.types)
    emit-type-check(back-end, value, type);
  end for;
end method;

define method do-emit-mv-check-type
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <multiple-value-check-type>, mv :: <llvm-global-mv>)
 => ();
  for (index from 0, type in c.types)
    let value = op--mv-extract(back-end, mv, index);
    emit-type-check(back-end, value, type);
  end for;
end method;

define method emit-computation
    (back-end :: <llvm-back-end>, m :: <llvm-module>,
     c :: <multiple-value-check-type-rest>)
 => ()
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
      if (*temporary-locals?*
            & ~llvm-builder-local-defined?(back-end, local-name))
        ins--local(back-end, local-name, alloca);
      end if;

      if (c.dfm-source-location)
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

