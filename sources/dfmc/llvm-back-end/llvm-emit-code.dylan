Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Temporary values

define thread variable *temporary-value-table* = false-or(<object-table>) = #f;

define method temporary-value
    (temporary :: <temporary>)
 => (value :: type-union(<llvm-value>, <llvm-mv>));
  element(*temporary-value-table*, temporary, default: #f)
    | error("No value defined for temporary %s (generator %=)",
            temporary, temporary.generator)
end method;

define method temporary-value-setter
    (value :: type-union(<llvm-value>, <llvm-mv>), temporary :: <temporary>)
 => (value :: type-union(<llvm-value>, <llvm-mv>));
  if (element(*temporary-value-table*, temporary, default: #f))
    error("Temporary %s is already set", temporary)
  else
    *temporary-value-table*[temporary] := value
  end if
end method;

define thread variable *merge-operands-table* :: false-or(<object-table>) = #f;

define method merge-operands-setter
    (operands :: <stretchy-vector>, temporary :: false-or(<temporary>))
 => (operands :: <stretchy-vector>);
  if (element(*merge-operands-table*, temporary, default: #f))
    error("Merge operands for %s is already set", temporary)
  elseif (temporary)
    *merge-operands-table*[temporary] := operands
  end if;
  operands
end method;

define method add-merge-operands
    (temporary :: false-or(<temporary>), value :: <object>, bb :: <llvm-value>)
 => ();
  if (temporary)
    let operands = *merge-operands-table*[temporary];
    add!(operands, value);
    add!(operands, bb)
  end if;
end method;

define thread variable *temporary-locals?* = #t;

define macro without-persistent-temporaries
  { without-persistent-temporaries () ?:body end }
  => { dynamic-bind (*temporary-value-table* = shallow-copy(*temporary-value-table*),
                     *temporary-locals?* = #f,
                     *merge-operands-table* = shallow-copy(*merge-operands-table*))
         ?body;
       end }
end macro;


/// Code emission

define method emit-code
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o,
     #key init? = #f)
 => ();
  // Do nothing
end method;

define constant $extra-parameter-name = ".extra";

define method emit-code
    (back-end :: <llvm-back-end>, module :: <llvm-module>, o :: <&iep>,
     #key init? = #f)
 => ();
  unless (code(o))
    let function-name = emit-name(back-end, module, o);
    let function-type = llvm-lambda-type(back-end, o);

    let fun = o.function;
    let signature = ^function-signature(fun);
    let parameter-types
      = if (signature)
          llvm-signature-types(back-end, o, signature-spec(fun), signature)
        else
          llvm-dynamic-signature-types(back-end, o, signature-spec(fun))
        end if;
    let arguments
      = map(method (arg-type, index, param)
              make(<llvm-argument>,
                   type: arg-type,
                   name: hygienic-mangle(back-end,
                                         param.name, param.frame-offset),
                   index: index)
            end,
            parameter-types,
            range(below: min(parameter-types.size,
                             $entry-point-argument-count)),
            parameters(o));

    let extra
      = if (parameter-types.size > $entry-point-argument-count)
          vector(make(<llvm-argument>,
                      type: llvm-pointer-to(back-end,
                                            $llvm-object-pointer-type),
                      name: $extra-parameter-name,
                      index: arguments.size))
        else
          #[]
        end if;

    let calling-convention-index = arguments.size + extra.size;
    let calling-convention-parameters
      = vector(make(<llvm-argument>,
                    type: $llvm-object-pointer-type,
                    name: $next-methods-parameter-name,
                    index: calling-convention-index),
               make(<llvm-argument>,
                    type: $llvm-object-pointer-type,
                    name: $function-parameter-name,
                    index: calling-convention-index + 1));

    let linkage
      = if (o.model-definition & ~init?) #"external" else #"internal" end;
    let section
      = if (init?) #"init-code" else #"code" end;

    block ()
      o.code
        := back-end.llvm-builder-function
        := make(<llvm-function>,
                name: function-name,
                type: llvm-pointer-to(back-end, function-type),
                arguments: concatenate(arguments,
                                       extra,
                                       calling-convention-parameters),
                linkage: linkage,
                section: llvm-section-name(back-end, section),
                calling-convention:
                  llvm-calling-convention(back-end, o));

      // Emit the entry block
      ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));

      dynamic-bind (*current-environment* = o.environment,
                    *temporary-value-table* = make(<object-table>),
                    *merge-operands-table* = make(<object-table>),
                    *computation-dbg-scope-table* = make(<object-table>))
        // Add function arguments to the function's value table,
        // and to the temporary value mapping
        for (argument in arguments, param in o.parameters)
          ins--local(back-end, argument.llvm-argument-name, argument);
          temporary-value(param) := argument;
        end for;

        // Add extra parameters to the function's value table,
        // and to the temporary value mapping
        if (~empty?(extra))
          ins--local(back-end, extra.first.llvm-argument-name, extra.first);

          for (i from $entry-point-argument-count below o.parameters.size)
            let param = o.parameters[i];
            let name = hygienic-mangle(back-end, param.name, param.frame-offset);
            let offset = i - $entry-point-argument-count;
            let value-ptr = ins--gep-inbounds(back-end, extra.first, offset);
            let value = ins--load(back-end, value-ptr); // FIXME invariant load
            ins--local(back-end, name, value);
            temporary-value(param) := value;
          end for;
        end if;

        // Add calling convention parameters to the function's value table
        for (argument in calling-convention-parameters)
          ins--local(back-end, argument.llvm-argument-name, argument);
        end for;

        // Emit debug information for the function
        emit-lambda-dbg-function(back-end, o);

        // Emit definitions for temporaries
        let e :: <lambda-lexical-environment> = o.environment;
        for (tmp in e.temporaries)
          if (used?(tmp))
            emit-local-definition(back-end, tmp);
          end if;
        end for;

        // Mark the function object as invariant if it is a closure
        if (closure?(o))
          let closure = llvm-builder-local(back-end, $function-parameter-name);
          op--closure-invariant-start(back-end, o, closure);
        end if;

        // Emit the body of the function
        emit-computations(back-end, module, o.body, #f);
      end dynamic-bind;
    cleanup
      back-end.llvm-builder-dbg := #f;
      back-end.llvm-builder-function := #f;
      back-end.llvm-builder-basic-block := #f; // FIXME
    EXCEPTION (e :: <error>)    // FIXME
      back-end.llvm-builder-basic-block := #f;
      back-end.llvm-builder-function.llvm-function-basic-blocks.size := 0;
      remove-all-keys!(back-end.llvm-builder-function.llvm-function-value-table);
      ins--block(back-end, make(<llvm-basic-block>, name: "bb.entry"));
      ins--call-intrinsic(back-end, "llvm.trap", vector());
      ins--unreachable(back-end);
      format(*standard-output*, "emit %s: %s\n", function-name, e);
      force-output(*standard-output*);
    end block;

    // Done with this DFM?
    if (*retract-dfm?*)
      if (lambda-top-level?(o))
	retract-method-dfm(o);
	retract-method-dfm(o.function);
      end if;
    end if;
  end unless;
end method;

// Calling convention for ordinary functions
define method llvm-calling-convention
    (back-end :: <llvm-back-end>, o :: <&iep>)
 => (calling-convention :: <integer>);
  $llvm-calling-convention-fast
end method;

// Calling convention for shared entry points
define method llvm-calling-convention
    (back-end :: <llvm-back-end>, o :: <&shared-entry-point>)
 => (calling-convention :: <integer>);
  $llvm-calling-convention-c
end method;

