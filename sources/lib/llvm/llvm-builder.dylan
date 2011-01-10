Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract primary class <llvm-builder> (<object>)
  constant slot llvm-builder-value-function :: <function>,
    init-value: default-value-function, init-keyword: value-function:;
  slot llvm-builder-module :: false-or(<llvm-module>),
    init-value: #f, init-keyword: module:;
  slot llvm-builder-function :: false-or(<llvm-function>),
    init-value: #f;
  slot llvm-builder-basic-block :: false-or(<llvm-basic-block>),
    init-value: #f;
end class;

define class <llvm-concrete-builder> (<llvm-builder>)
end class;

define sealed method make
    (class == <llvm-builder>, #next next-method, #rest keys, #key, #all-keys)
 => (instance :: <llvm-concrete-builder>);
  apply(next-method, <llvm-concrete-builder>, keys)
end method;


/// Value transformation

define sealed generic default-value-function
    (builder :: <llvm-builder>, value :: <object>) => (result :: <llvm-value>);

define method default-value-function
    (builder :: <llvm-builder>, value :: <object>) => (result :: <llvm-value>);
  error("No llvm-builder value function for %=", value);
end method;

define method default-value-function
    (builder :: <llvm-builder>, value :: <llvm-value>)
 => (value :: <llvm-value>);
  value
end method;

define inline function llvm-builder-value
    (builder :: <llvm-builder>, value :: <object>)
 => (value :: <llvm-value>);
  builder.llvm-builder-value-function(builder, value)
end function;


/// Global variables

define method llvm-builder-define-global
    (builder :: <llvm-builder>, name :: <string>,
     value :: <llvm-constant-value>)
 => (value :: <llvm-constant-value>);
  let global-table = builder.llvm-builder-module.llvm-global-table;
  let definition
    = element(global-table, name, default: #f);
  if (definition)
    if (instance?(definition, <llvm-symbolic-constant>))
      definition.llvm-placeholder-value-forward := value;
      llvm-constrain-type(definition.llvm-value-type, llvm-value-type(value));
    else
      error("value @%s is multiply defined", name);
    end if;
  end if;

  // Record this value
  element(global-table, name) := value
end method;

define method llvm-builder-define-global
    (builder :: <llvm-builder>, name :: <string>,
     value :: <llvm-global-variable>)
 => (value :: <llvm-global-variable>);
  next-method();
  add!(builder.llvm-builder-module.llvm-module-globals, value);
  value
end method;

define method llvm-builder-define-global
    (builder :: <llvm-builder>, name :: <string>, value :: <llvm-function>)
 => (value :: <llvm-function>);
  next-method();
  add!(builder.llvm-builder-module.llvm-module-functions, value);
  value
end method;

define method llvm-builder-define-global
    (builder :: <llvm-builder>, name :: <string>, value :: <llvm-global-alias>)
 => (value :: <llvm-global-alias>);
  next-method();
  add!(builder.llvm-builder-module.llvm-module-aliases, value);
  value
end method;

define method llvm-builder-declare-global
    (builder :: <llvm-builder>, name :: <string>,
     value :: <llvm-constant-value>)
 => (value :: <llvm-constant-value>);
  let global-table = builder.llvm-builder-module.llvm-global-table;
  element(global-table, name, default: #f)
    | llvm-builder-define-global(builder, name, value);
  value
end method;

define function llvm-builder-global
    (builder :: <llvm-builder>, name :: <string>)
 => (value :: <llvm-constant-value>);
  let global-table = builder.llvm-builder-module.llvm-global-table;
  element(global-table, name, default: #f)
    | (element(global-table, name)
         := make(<llvm-symbolic-constant>, name: name))
end function;


/// Local variables

define function ins--local
    (builder :: <llvm-builder>, name :: <string>,
     value :: type-union(<llvm-instruction>, <llvm-basic-block>))
 => (value :: type-union(<llvm-instruction>, <llvm-basic-block>));
  let builder-function
    = builder.llvm-builder-function
    | error("llvm-builder-function is not set");
  let definition
    = element(builder-function.llvm-function-value-table, name, default: #f);
  if (definition)
    if (instance?(definition, <llvm-symbolic-value>))
      definition.llvm-placeholder-value-forward := value;
      llvm-constrain-type(definition.llvm-value-type, llvm-value-type(value));
    else
      error("value %%%s is multiply defined", name);
    end if;
  end if;

  // Record this value
  element(builder-function.llvm-function-value-table, name) := value
end function;

define function llvm-builder-local
    (builder :: <llvm-builder>, name :: <string>)
 => (value :: <llvm-value>);
  let builder-function
    = builder.llvm-builder-function
    | error("llvm-builder-function is not set");
  element(builder-function.llvm-function-value-table, name, default: #f)
    | (element(builder-function.llvm-function-value-table, name)
         := make(<llvm-symbolic-value>, name: name))
end function;


/// Basic block insertion

define function ins--block
    (builder :: <llvm-builder>, basic-block :: <llvm-basic-block>)
 => (basic-block :: <llvm-basic-block>);
  if (builder.llvm-builder-basic-block)
    error("llvm-builder-basic-block already set");
  end if;

  let builder-function
    = builder.llvm-builder-function
    | error("llvm-builder-function is not set");
  add!(builder-function.llvm-function-basic-blocks, basic-block);

  let name = basic-block.llvm-basic-block-name;
  if (name)
    ins--local(builder, name, basic-block);
  end if;

  builder.llvm-builder-basic-block := basic-block
end function;


/// Instruction insertion

define inline method builder-insert
    (builder :: <llvm-builder>, instruction :: <llvm-instruction>)
 => (instruction :: <llvm-instruction>);
  add!(builder.llvm-builder-basic-block.llvm-basic-block-instructions,
       instruction);
  instruction
end method;

define inline method builder-insert
    (builder :: <llvm-builder>, instruction :: <llvm-terminator-instruction>)
 => (instruction :: <llvm-terminator-instruction>);
  next-method();
  builder.llvm-builder-basic-block := #f;
  instruction
end method;


/// Instruction definitions

define macro instruction-set-definer
  { define instruction-set ?clauses end }
    => { ?clauses }

clauses:
  { } => { }

  { binop ?:name; ... }
    => { define inline function "ins--" ## ?name
             (builder :: <llvm-builder>, lhs, rhs, #rest options)
          => (instruction :: <llvm-binop-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          apply(make, <llvm-binop-instruction>,
                                operator: ?#"name",
                                operands: vector(lhs, rhs),
                                options))
         end function;
         ... }

  { cast ?:name; ... }
    => { define inline function "ins--" ## ?name
             (builder :: <llvm-builder>, value, type :: <llvm-type>)
          => (instruction :: <llvm-cast-instruction>);
           builder-insert(builder,
                          make(<llvm-cast-instruction>,
                                operator: ?#"name",
                                operands:
                                 vector(llvm-builder-value(builder, value)),
                                type: type))
         end function;
         ... }

  { icmp ?:name; ... }
    => { define inline function "ins--icmp-" ## ?name
             (builder :: <llvm-builder>, lhs, rhs)
          => (instruction :: <llvm-icmp-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          make(<llvm-icmp-instruction>,
                                predicate: ?#"name",
                                operands: vector(lhs, rhs)))
         end function;
         ... }

  { fcmp ?:name; ... }
    => { define inline function "ins--fcmp-" ## ?name
             (builder :: <llvm-builder>, lhs, rhs)
          => (instruction :: <llvm-fcmp-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          make(<llvm-fcmp-instruction>,
                                predicate: ?#"name",
                                operands: vector(lhs, rhs)))
         end function;
         ... }

  { op ?:name (?params:*) => ?:body ... }
    => { define inline function "ins--" ## ?name
             (?=builder :: <llvm-builder>, ?params)
          => (instruction :: <llvm-instruction>);
           builder-insert(?=builder, ?body)
         end function;
         ... }
end macro;

define instruction-set
  binop add;
  binop fadd;
  binop sub;
  binop fsub;
  binop mul;
  binop fmul;
  binop udiv;
  binop sdiv;
  binop fdiv;
  binop urem;
  binop srem;
  binop frem;
  binop shl;
  binop lshr;
  binop ashr;
  binop and;
  binop or;
  binop xor;

  cast trunc;
  cast zext;
  cast sext;
  cast fptoui;
  cast fptosi;
  cast uitofp;
  cast sitofp;
  cast fptrunc;
  cast fpext;
  cast ptrtoint;
  cast inttoptr;
  cast bitcast;

  icmp eq;
  icmp ne;
  icmp slt;
  icmp sgt;
  icmp sle;
  icmp sge;
  icmp ult;
  icmp ugt;
  icmp ule;
  icmp uge;

  fcmp oeq;
  fcmp one;
  fcmp olt;
  fcmp ogt;
  fcmp ole;
  fcmp oge;
  fcmp ord;
  fcmp uno;
  fcmp ueq;
  fcmp une;
  fcmp ult;
  fcmp ugt;
  fcmp ule;
  fcmp uge;
  fcmp true;
  fcmp false;

  op gep (value, #rest indices)
    => make(<llvm-gep-instruction>,
            operands: map(curry(llvm-builder-value, builder),
                          concatenate(vector(value), indices)));

  op gep-inbounds (value, #rest indices)
    => make(<llvm-gep-instruction>,
            in-bounds?: #t,
            operands: map(curry(llvm-builder-value, builder),
                          concatenate(vector(value), indices)));

  op \select (c, true, false)
    => let true = llvm-builder-value(builder, true);
       let false = llvm-builder-value(builder, false);
       llvm-constrain-type(llvm-value-type(true), llvm-value-type(false));
       make(<llvm-select-instruction>,
            operands: vector(llvm-builder-value(builder, c), true, false));

  op va-arg (va-list :: <llvm-value>, type :: <llvm-type>)
    => make(<llvm-va-arg-instruction>, operands: vector(va-list), type: type);

  op extractelement (vec, index)
    => make(<llvm-extractelement-instruction>,
            operands: vector(llvm-builder-value(builder, vec),
                             llvm-builder-value(builder, index)));

  op insertelement (vec, scalar, index)
   => make(<llvm-insertelement-instruction>,
            operands: vector(llvm-builder-value(builder, vec),
                             llvm-builder-value(builder, scalar),
                             llvm-builder-value(builder, index)));

  op shufflevector (vec1 :: <llvm-value>, vec2 :: <llvm-value>,
                    mask :: <llvm-value>)
    => make(<llvm-shufflevector-instruction>,
            operands: vector(vec1, vec2, mask));

  op phi (#rest operands)
    => let operands = map(curry(llvm-builder-value, builder), operands);
       let type = llvm-value-type(operands[0]);
       llvm-constrain-type(llvm-value-type(operands[1]), $llvm-label-type);
       for (i from 2 below operands.size by 2)
         llvm-constrain-type(llvm-value-type(operands[i]), type);
         llvm-constrain-type(llvm-value-type(operands[i + 1]), $llvm-label-type)
       end for;
       make(<llvm-phi-node>, operands: operands);

  op call (fnptrval :: <llvm-value>, args :: <sequence>, #rest options)
    => let args = map(curry(llvm-builder-value, builder), args);
       let fnptrtype = type-forward(fnptrval.llvm-value-type);
       let return-type
         = if (instance?(fnptrtype, <llvm-pointer-type>))
             let pointee
               = type-forward(fnptrtype.llvm-pointer-type-pointee);
             if (instance?(pointee, <llvm-function-type>))
               for (arg-type in pointee.llvm-function-type-parameter-types,
                    arg in args)
                 llvm-constrain-type(llvm-value-type(arg), arg-type);
               end for;
               type-forward(pointee.llvm-function-type-return-type)
             end if
           end if;
       if (return-type)
         apply(make, <llvm-call-instruction>,
               type: return-type,
               operands: concatenate(vector(fnptrval), args),
               options)
       else
         apply(make, <llvm-call-instruction>,
               operands: concatenate(vector(fnptrval), args),
               options)
       end if;

  op alloca (type :: <llvm-type>, num-elements, #key alignment = 0)
    => let pointer-type
         = make(<llvm-pointer-type>, pointee: type);
       make(<llvm-alloca-instruction>,
            type: pointer-type,
            alignment: alignment,
            operands: vector(llvm-builder-value(builder, num-elements)));

  op load (pointer, #rest options)
    => let ptrtype = type-forward(pointer.llvm-value-type);
       if (instance?(ptrtype, <llvm-pointer-type>))
         apply(make, <llvm-load-instruction>,
               type: type-forward(ptrtype.llvm-pointer-type-pointee),
               operands: vector(llvm-builder-value(builder, pointer)),
               options)
       else
         apply(make, <llvm-load-instruction>,
               operands: vector(llvm-builder-value(builder, pointer)),
               options)
       end if;

  op store (value, pointer, #rest options)
    => apply(make, <llvm-store-instruction>,
             operands: vector(llvm-builder-value(builder, value),
                              llvm-builder-value(builder, pointer)),
             options);

  op insertvalue (aggregate, value, #rest indices)
    => make(<llvm-insert-value-instruction>,
            operands: vector(llvm-builder-value(builder, aggregate),
                             llvm-builder-value(builder, value)),
            indices: indices);

  op extractvalue (aggregate, #rest indices)
    => make(<llvm-extract-value-instruction>,
            operands: vector(llvm-builder-value(builder, aggregate)),
            indices: indices);

  op ret (#rest operands)
    => make(<llvm-return-instruction>,
            operands: map(curry(llvm-builder-value, builder), operands));

  op br (#rest operands)
    => make(<llvm-branch-instruction>,
            operands: map(curry(llvm-builder-value, builder), operands));

  op switch (value, default, #rest jump-table)
    => make(<llvm-switch-instruction>,
            operands: map(curry(llvm-builder-value, builder),
                          concatenate(vector(value, default), jump-table)));

  op invoke (to, unwind, fnptrval, args :: <sequence>, #rest options)
    => apply(make, <llvm-invoke-instruction>,
             operands: map(curry(llvm-builder-value, builder),
                           concatenate(vector(to, unwind, fnptrval), args)),
             options);

  op unwind ()
    => make(<llvm-unwind-instruction>);

  op unreachable ()
    => make(<llvm-unreachable-instruction>);
end instruction-set;

define inline function ins--tail-call
    (builder :: <llvm-builder>, fnptrval :: <llvm-value>, args :: <sequence>,
     #rest options)
 => (instruction :: <llvm-instruction>);
  apply(ins--call, builder, fnptrval, args, tail-call?: #t, options)
end function;

define inline function ins--call-intrinsic
    (builder :: <llvm-builder>, name :: <string>, args :: <sequence>,
     #rest options)
 => (instruction :: <llvm-instruction>);
  let args = map(curry(llvm-builder-value, builder), args);
  let function :: <llvm-function> = $llvm-intrinsic-makers[name](args);
  llvm-builder-declare-global(builder, function.llvm-global-name, function);
  apply(ins--call, builder, function, args,
        attribute-list: function.llvm-function-attribute-list,
        options)
end function;
