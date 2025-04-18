Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010-2018 Gwydion Dylan Maintainers
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
  slot llvm-builder-dbg :: false-or(<llvm-metadata-attachment>),
    init-value: #f;
  constant slot llvm-builder-ctor-entries :: <stretchy-vector>
    = make(<stretchy-object-vector>);
end class;

define class <llvm-concrete-builder> (<llvm-builder>)
end class;

define sealed method make
    (class == <llvm-builder>, #next next-method, #rest keys, #key, #all-keys)
 => (instance :: <llvm-concrete-builder>);
  apply(next-method, <llvm-concrete-builder>, keys)
end method;

define macro with-builder-function
  { with-builder-function(?builder:expression, ?function:expression)
      ?:body
    end }
    => { let builder :: <llvm-builder> = ?builder;
         let save-function = builder.llvm-builder-function;
         let save-bb = builder.llvm-builder-basic-block;
         let save-dbg = builder.llvm-builder-dbg;
         block ()
           builder.llvm-builder-function := ?function;
           builder.llvm-builder-basic-block := #f;
           builder.llvm-builder-dbg := #f;
           ?body
         cleanup
           builder.llvm-builder-dbg := save-dbg;
           builder.llvm-builder-basic-block := save-bb;
           builder.llvm-builder-function := save-function;
         end block }
end macro;


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

define generic llvm-builder-define-global
    (builder :: <llvm-builder>, name :: <string>,
     value :: <llvm-constant-value>)
 => (value :: <llvm-constant-value>);

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
    | llvm-builder-define-global(builder, name, value)
end method;

define function llvm-builder-global
    (builder :: <llvm-builder>, name :: <string>)
 => (value :: <llvm-constant-value>);
  let global-table = builder.llvm-builder-module.llvm-global-table;
  element(global-table, name, default: #f)
    | (element(global-table, name)
         := make(<llvm-symbolic-constant>, name: name))
end function;

define function llvm-builder-global-defined?
    (builder :: <llvm-builder>, name :: <string>)
 => (defined? :: <boolean>);
  let global-table = builder.llvm-builder-module.llvm-global-table;
  let definition
    = element(global-table, name, default: #f);
  definition
    & if (instance?(definition, <llvm-symbolic-constant>))
        slot-initialized?(definition, llvm-placeholder-value-forward)
      else
        #t
      end if
end function;


/// Global constructor entries

define constant $ctor-function-type
  = make(<llvm-function-type>,
         return-type: $llvm-void-type,
         parameter-types: #(),
         varargs?: #f);
define constant $ctor-function-ptr-type
  = make(<llvm-pointer-type>, pointee: $ctor-function-type);

define constant $ctor-struct-type
  = make(<llvm-struct-type>,
         elements: vector($llvm-i32-type,
                          $ctor-function-ptr-type,
                          $llvm-i8*-type));

define constant $null-data
  = make(<llvm-null-constant>, type: $llvm-i8*-type);

define function llvm-builder-add-ctor-entry
    (builder :: <llvm-builder>, priority :: <integer>,
     init-function :: <llvm-value>)
 => ();
  add!(builder.llvm-builder-ctor-entries,
       make(<llvm-aggregate-constant>,
            type: $ctor-struct-type,
            aggregate-values: vector(i32(priority),
                                     init-function,
                                     $null-data)));
end function;

define function llvm-builder-finish-ctor
    (builder :: <llvm-builder>)
 => ();
  unless (empty?(builder.llvm-builder-ctor-entries))
    let entries = copy-sequence(builder.llvm-builder-ctor-entries);
    builder.llvm-builder-ctor-entries.size := 0;

    // Declare the constructors list
    let ctor-type
      = make(<llvm-array-type>,
             size: entries.size,
             element-type: $ctor-struct-type);
    let ctor-global
      = make(<llvm-global-variable>,
             name: "llvm.global_ctors",
             type: make(<llvm-pointer-type>, pointee: ctor-type),
             initializer: make(<llvm-aggregate-constant>,
                               type: ctor-type,
                               aggregate-values: entries),
             constant?: #f,
             linkage: #"appending");
    llvm-builder-define-global(builder,
                               ctor-global.llvm-global-name,
                               ctor-global);
  end unless;
end function;


/// Local variables

define constant <llvm-local-value>
  = type-union(<llvm-instruction>, <llvm-basic-block>, <llvm-argument>);

define function ins--local
    (builder :: <llvm-builder>, name :: <string>,
     value :: <llvm-local-value>)
 => (value :: <llvm-local-value>);
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

define function llvm-builder-local-defined?
    (builder :: <llvm-builder>, name :: <string>)
 => (defined? :: <boolean>);
  let builder-function
    = builder.llvm-builder-function
    | error("llvm-builder-function is not set");
  let value-table = builder-function.llvm-function-value-table;
  let definition
    = element(value-table, name, default: #f);
  definition
    & if (instance?(definition, <llvm-symbolic-constant>))
        slot-initialized?(definition, llvm-placeholder-value-forward)
      else
        #t
      end if
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
    (builder :: <llvm-builder>, instruction :: <llvm-phi-node>)
 => (instruction :: <llvm-instruction>);
  let instructions
    = builder.llvm-builder-basic-block.llvm-basic-block-instructions;
  unless (empty?(instructions)
            | instance?(instructions.last, <llvm-phi-node>))
    error("PHI nodes must be grouped at the top of a basic block")
  end unless;
  next-method()
end method;

define inline method builder-insert
    (builder :: <llvm-builder>, instruction :: <llvm-terminator-instruction>)
 => (instruction :: <llvm-terminator-instruction>);
  next-method();
  builder.llvm-builder-basic-block := #f;
  instruction
end method;

define macro with-insert-before-terminator
  { with-insert-before-terminator(?builder:expression, ?bb:expression)
      ?:body
    end }
    => { // Temporarily switch to the given basic block
	 let current-bb = ?builder.llvm-builder-basic-block;
         let insert-bb = ?bb;
	 ?builder.llvm-builder-basic-block := insert-bb;

	 // Temporarily remove the block terminator
	 let instructions = insert-bb.llvm-basic-block-instructions;
	 let terminator :: <llvm-terminator-instruction> = instructions.last;
	 instructions.size := instructions.size - 1;

	 // Execute the given body
	 block ()
	   ?body
	 cleanup
	   // Put the removed terminator at the end of the current
	   // block (which might have changed)
           builder-insert(?builder, terminator);

           // Restore the original current basic block
           ?builder.llvm-builder-basic-block := current-bb;
	 end block }
end macro;


/// Metadata attachments

define function ins--dbg
    (builder :: <llvm-builder>,
     line-number :: <integer>, column-number :: <integer>,
     scope :: <llvm-metadata>,
     #key inlined-at)
 => ();
  let current-attachment = builder.llvm-builder-dbg;
  if (~current-attachment
        | begin
            let node = current-attachment.llvm-metadata-attachment-metadata;
            line-number ~= node.llvm-DILocation-metadata-line
              | column-number ~= node.llvm-DILocation-metadata-column
              | scope ~= node.llvm-DILocation-metadata-scope
          end)
    let node
      = make(<llvm-DILocation-metadata>,
             line: line-number, column: column-number,
             scope: scope, inlinedAt: inlined-at);
    builder.llvm-builder-dbg
      := make(<llvm-metadata-attachment>,
              kind: $llvm-metadata-kind-dbg,
              metadata: node);
  end if;
end function;

define function llvm-builder-dbg-scope
    (builder :: <llvm-builder>) => (dbg-scope :: false-or(<llvm-metadata>));
  let current-attachment = builder.llvm-builder-dbg;
  let location
    = current-attachment
    & current-attachment.llvm-metadata-attachment-metadata;
  location
    & location.llvm-DILocation-metadata-scope
end function;

define function llvm-builder-dbg-line
    (builder :: <llvm-builder>) => (dbg-line :: false-or(<integer>));
  let current-attachment = builder.llvm-builder-dbg;
  let location
    = current-attachment
    & current-attachment.llvm-metadata-attachment-metadata;
  location
    & location.llvm-DILocation-metadata-line
end function;

define function llvm-builder-dbg-file
    (builder :: <llvm-builder>) => (dbg-file :: false-or(<llvm-metadata>));
  let scope = llvm-builder-dbg-scope(builder);
  scope
    & scope-dbg-file(scope)
end function;

define method scope-dbg-file
    (scope :: <llvm-DILexicalBlock-metadata>)
 => (dbg-file :: false-or(<llvm-metadata>));
  scope.llvm-DILexicalBlock-metadata-file
end method;

define method scope-dbg-file
    (scope :: <llvm-DISubprogram-metadata>)
 => (dbg-file :: false-or(<llvm-metadata>));
  scope.llvm-DISubprogram-metadata-file
end method;

define inline function builder-metadata
    (builder :: <llvm-builder>, metadata :: <list>)
 => (augmented-metadata :: <list>);
  if (builder.llvm-builder-dbg)
    pair(builder.llvm-builder-dbg, metadata)
  else
    metadata
  end if
end function;


/// Instruction definitions

define macro instruction-set-definer
  { define instruction-set ?clauses end }
    => { ?clauses }

clauses:
  { } => { }

  { binop ?:name; ... }
    => { define inline function "ins--" ## ?name
             (builder :: <llvm-builder>, lhs, rhs,
              #rest options, #key metadata :: <list> = #(), #all-keys)
          => (instruction :: <llvm-binop-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          apply(make, <llvm-binop-instruction>,
                                operator: ?#"name",
                                operands: vector(lhs, rhs),
                                metadata: builder-metadata(builder, metadata),
                                options))
         end function;
         ... }

  { cast ?:name; ... }
    => { define inline function "ins--" ## ?name
             (builder :: <llvm-builder>, value, type :: <llvm-type>,
              #key metadata :: <list> = #())
          => (instruction :: <llvm-cast-instruction>);
           builder-insert(builder,
                          make(<llvm-cast-instruction>,
                                operator: ?#"name",
                                operands:
                                 vector(llvm-builder-value(builder, value)),
                                type: type,
                                metadata: builder-metadata(builder, metadata)))
         end function;
         ... }

  { icmp ?:name; ... }
    => { define inline function "ins--icmp-" ## ?name
             (builder :: <llvm-builder>, lhs, rhs,
              #key metadata :: <list> = #())
          => (instruction :: <llvm-icmp-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          make(<llvm-icmp-instruction>,
                                predicate: ?#"name",
                                operands: vector(lhs, rhs),
                                metadata: builder-metadata(builder, metadata)))
         end function;
         ... }

  { fcmp ?:name; ... }
    => { define inline function "ins--fcmp-" ## ?name
             (builder :: <llvm-builder>, lhs, rhs,
              #key metadata :: <list> = #())
          => (instruction :: <llvm-fcmp-instruction>);
           let lhs = llvm-builder-value(builder, lhs);
           let rhs = llvm-builder-value(builder, rhs);
           llvm-constrain-type(llvm-value-type(lhs), llvm-value-type(rhs));
           builder-insert(builder,
                          make(<llvm-fcmp-instruction>,
                               predicate: ?#"name",
                               operands: vector(lhs, rhs),
                               metadata: builder-metadata(builder, metadata)))
         end function;
         ... }

  { atomicrmw ?:name; ... }
    => { define inline function "ins--atomicrmw-" ## ?name
             (builder :: <llvm-builder>, ptr, value, #rest options,
              #key metadata :: <list> = #(), #all-keys)
          => (instruction :: <llvm-atomicrmw-instruction>);
           let ptr = llvm-builder-value(builder, ptr);
           let value = llvm-builder-value(builder, value);
           builder-insert(builder,
                          apply(make, <llvm-atomicrmw-instruction>,
                                operation: ?#"name",
                                operands: vector(ptr, value),
                                metadata: builder-metadata(builder, metadata),
                                options))
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
    => begin
         let gep = make(<llvm-gep-instruction>,
                        operands: map(curry(llvm-builder-value, builder),
                                      concatenate(vector(value), indices)),
                        metadata: builder-metadata(builder, #()));
         gep.llvm-value-type;
         gep
       end;

  op gep-inbounds (value, #rest indices)
    => make(<llvm-gep-instruction>,
            in-bounds?: #t,
            operands: map(curry(llvm-builder-value, builder),
                          concatenate(vector(value), indices)),
            metadata: builder-metadata(builder, #()));

  op \select (c, true, false, #key metadata :: <list> = #())
    => let true = llvm-builder-value(builder, true);
       let false = llvm-builder-value(builder, false);
       llvm-constrain-type(llvm-value-type(true), llvm-value-type(false));
       make(<llvm-select-instruction>,
            operands: vector(llvm-builder-value(builder, c), true, false),
            metadata: builder-metadata(builder, metadata));

  op va-arg (va-list :: <llvm-value>, type :: <llvm-type>,
             #key metadata :: <list> = #())
    => make(<llvm-va-arg-instruction>, operands: vector(va-list), type: type,
            metadata: builder-metadata(builder, metadata));

  op extractelement (vec, index, #key metadata :: <list> = #())
    => make(<llvm-extractelement-instruction>,
            operands: vector(llvm-builder-value(builder, vec),
                             llvm-builder-value(builder, index)),
            metadata: builder-metadata(builder, metadata));

  op insertelement (vec, scalar, index, #key metadata :: <list> = #())
   => make(<llvm-insertelement-instruction>,
            operands: vector(llvm-builder-value(builder, vec),
                             llvm-builder-value(builder, scalar),
                             llvm-builder-value(builder, index)),
            metadata: builder-metadata(builder, metadata));

  op shufflevector (vec1 :: <llvm-value>, vec2 :: <llvm-value>,
                    mask :: <llvm-value>, #key metadata :: <list> = #())
    => make(<llvm-shufflevector-instruction>,
            operands: vector(vec1, vec2, mask),
            metadata: builder-metadata(builder, metadata));

  op phi (operands :: <sequence>)
    => let operands = map(curry(llvm-builder-value, builder), operands);
       let type = llvm-value-type(operands[0]);
       llvm-constrain-type(llvm-value-type(operands[1]), $llvm-label-type);
       for (i from 2 below operands.size by 2)
         llvm-constrain-type(llvm-value-type(operands[i]), type);
         llvm-constrain-type(llvm-value-type(operands[i + 1]), $llvm-label-type)
       end for;
       make(<llvm-phi-node>,
            operands: operands,
            metadata: builder-metadata(builder, #()));

  op landingpad (type :: <llvm-type>, clauses :: <sequence>,
                 #key metadata :: <list> = #(), cleanup? :: <boolean> = #f)
    => make(<llvm-landingpad-instruction>,
            type: type,
            operands: clauses,
            cleanup?: cleanup?,
            metadata: builder-metadata(builder, metadata));

  op call (fnptrval :: <llvm-value>, args :: <sequence>,
           #rest options, #key metadata :: <list> = #(), #all-keys)
    => let args = map(curry(llvm-builder-value, builder), args);
       let return-type = do-constrain-call-type(fnptrval, args);
       if (return-type)
         apply(make, <llvm-call-instruction>,
               type: return-type,
               operands: concatenate(vector(fnptrval), args),
               metadata: builder-metadata(builder, metadata),
               options)
       else
         apply(make, <llvm-call-instruction>,
               operands: concatenate(vector(fnptrval), args),
               metadata: builder-metadata(builder, metadata),
               options)
       end if;

  op alloca (type :: <llvm-type>, num-elements,
             #key alignment :: <integer> = 0, metadata :: <list> = #())
    => let pointer-type
         = make(<llvm-pointer-type>, pointee: type);
       make(<llvm-alloca-instruction>,
            allocated-type: type,
            type: pointer-type,
            alignment: alignment,
            operands: vector(llvm-builder-value(builder, num-elements)),
            metadata: builder-metadata(builder, metadata));

  op load (pointer, #rest options, #key metadata :: <list> = #(), #all-keys)
    => let ptrtype = type-forward(pointer.llvm-value-type);
       if (instance?(ptrtype, <llvm-pointer-type>))
         apply(make, <llvm-load-instruction>,
               type: type-forward(ptrtype.llvm-pointer-type-pointee),
               operands: vector(llvm-builder-value(builder, pointer)),
               metadata: builder-metadata(builder, metadata),
               options)
       else
         apply(make, <llvm-load-instruction>,
               operands: vector(llvm-builder-value(builder, pointer)),
               metadata: builder-metadata(builder, metadata),
               options)
       end if;

  op store (value, pointer, #rest options,
            #key metadata :: <list> = #(), #all-keys)
    => begin
         let value = llvm-builder-value(builder, value);
         let pointer = llvm-builder-value(builder, pointer);
         let ptrtype = type-forward(pointer.llvm-value-type);
         if (instance?(ptrtype, <llvm-pointer-type>))
           llvm-constrain-type(ptrtype.llvm-pointer-type-pointee,
                               llvm-value-type(value));
         end if;
         apply(make, <llvm-store-instruction>,
               operands: vector(value, pointer),
               metadata: builder-metadata(builder, metadata),
               options)
       end;

  op cmpxchg (pointer, value1, value2, #rest options,
            #key metadata :: <list> = #(), #all-keys)
    => apply(make, <llvm-cmpxchg-instruction>,
             operands: vector(llvm-builder-value(builder, pointer),
                              llvm-builder-value(builder, value1),
                              llvm-builder-value(builder, value2)),
             metadata: builder-metadata(builder, metadata),
             options);

  atomicrmw xchg;
  atomicrmw add;
  atomicrmw sub;
  atomicrmw and;
  atomicrmw nand;
  atomicrmw or;
  atomicrmw xor;
  atomicrmw max;
  atomicrmw min;
  atomicrmw umax;
  atomicrmw umin;

  op fence ( #rest options, #key metadata :: <list> = #(), #all-keys)
    => apply(make, <llvm-fence-instruction>,
             metadata: builder-metadata(builder, metadata),
             options);

  op insertvalue (aggregate, value, #rest indices)
    => make(<llvm-insert-value-instruction>,
            operands: vector(llvm-builder-value(builder, aggregate),
                             llvm-builder-value(builder, value)),
            indices: indices,
            metadata: builder-metadata(builder, #()));

  op extractvalue (aggregate, #rest indices)
    => make(<llvm-extract-value-instruction>,
            operands: vector(llvm-builder-value(builder, aggregate)),
            indices: indices,
            metadata: builder-metadata(builder, #()));

  op ret (#rest operands)
    => make(<llvm-return-instruction>,
            operands: map(curry(llvm-builder-value, builder), operands),
            metadata: builder-metadata(builder, #()));

  op br (#rest operands)
    => make(<llvm-branch-instruction>,
            operands: map(curry(llvm-builder-value, builder), operands),
            metadata: builder-metadata(builder, #()));

  op switch (value, default, jump-table :: <sequence>)
    => make(<llvm-switch-instruction>,
            operands: map(curry(llvm-builder-value, builder),
                          concatenate(vector(value, default), jump-table)),
            metadata: builder-metadata(builder, #()));

  op invoke (to, unwind, fnptrval, args :: <sequence>,
             #rest options, #key metadata :: <list> = #(), #all-keys)
    => let args = map(curry(llvm-builder-value, builder), args);
       let return-type = do-constrain-call-type(fnptrval, args);
       if (return-type)
         apply(make, <llvm-invoke-instruction>,
               operands: concatenate(map(curry(llvm-builder-value, builder),
                                         vector(to, unwind, fnptrval)),
                                     args),
               type: return-type,
               metadata: builder-metadata(builder, metadata),
               options)
       else
         apply(make, <llvm-invoke-instruction>,
               operands: map(curry(llvm-builder-value, builder),
                             concatenate(vector(to, unwind, fnptrval), args)),
               metadata: builder-metadata(builder, metadata),
               options)
       end if;

  op resume (value, #key metadata :: <list> = #())
    => make(<llvm-resume-instruction>,
            operands: vector(llvm-builder-value(builder, value)),
            metadata: builder-metadata(builder, metadata));

  op unreachable (#key metadata :: <list> = #())
    => make(<llvm-unreachable-instruction>,
            metadata: builder-metadata(builder, metadata));
end instruction-set;

define function do-constrain-call-type
    (fnptrval :: <llvm-value>, args :: <sequence>)
 => (type :: false-or(<llvm-type>));
  let fnptrtype = type-forward(fnptrval.llvm-value-type);
  if (instance?(fnptrtype, <llvm-pointer-type>))
    let pointee = type-forward(fnptrtype.llvm-pointer-type-pointee);
    if (instance?(pointee, <llvm-function-type>))
      let parameter-types = pointee.llvm-function-type-parameter-types;
      if (pointee.llvm-function-type-varargs?)
        if (args.size < parameter-types.size)
          error("Calling varargs function with %d args, expected at least %d",
                args.size, parameter-types.size);
        end if;
      elseif (args.size ~= parameter-types.size)
        error("Calling function with %d args, expected %d",
              args.size, parameter-types.size);
      end if;
      for (arg-type in parameter-types, arg in args)
        llvm-constrain-type(llvm-value-type(arg), arg-type);
      end for;
      type-forward(pointee.llvm-function-type-return-type)
    end if
  end if
end function;

define inline function ins--tail-call
    (builder :: <llvm-builder>, fnptrval :: <llvm-value>, args :: <sequence>,
     #rest options)
 => (instruction :: <llvm-instruction>);
  apply(ins--call, builder, fnptrval, args, tail-call?: #t, options)
end function;

define method llvm-builder-declare-intrinsic
    (builder :: <llvm-builder>, name :: <string>, args :: <sequence>)
 => (function :: <llvm-function>);
  let function :: <llvm-function> = $llvm-intrinsic-makers[name](args);
  llvm-builder-declare-global(builder, function.llvm-global-name, function);
end method;

define inline function ins--call-intrinsic
    (builder :: <llvm-builder>, name :: <string>, args :: <sequence>,
     #rest options)
 => (instruction :: <llvm-instruction>);
  let args = map(curry(llvm-builder-value, builder), args);
  let function :: <llvm-function>
    = llvm-builder-declare-intrinsic(builder, name, args);
  apply(ins--call, builder, function, args,
        attribute-list: function.llvm-function-attribute-list,
        options)
end function;

// Spread operands
define inline function ins--phi*
    (builder :: <llvm-builder>, #rest operands)
 => (instruction :: <llvm-instruction>);
  ins--phi(builder, operands)
end function;

define inline function ins--switch*
    (builder :: <llvm-builder>, value, default, #rest jump-table)
 => (instruction :: <llvm-instruction>);
  ins--switch(builder, value, default, jump-table)
end function;

// Convenience macros

define macro ins--if
  { ins--if(?builder:expression, ?test:expression) ?:body ?elses end }
    => { do-ins--if(?builder, ?test, method() ?body end, ?elses) }
  elses:
  { } => { #f }
  { ins--else ?:body } => { method() ?body end }
end macro;

define function do-ins--if
    (builder :: <llvm-builder>, test,
     iftrue-thunk :: <function>, iffalse-thunk :: false-or(<function>))
 => (value :: <llvm-value>)
  let test-bb = builder.llvm-builder-basic-block;
  let iftrue-bb = make(<llvm-basic-block>);
  let iffalse-bb = if (iffalse-thunk) make(<llvm-basic-block>) end;
  let common-bb = make(<llvm-basic-block>);

  // Branch on test results
  if (iffalse-bb)
    ins--br(builder, test, iftrue-bb, iffalse-bb);
  else
    ins--br(builder, test, iftrue-bb, common-bb);
  end if;

  // Condition true block
  ins--block(builder, iftrue-bb);
  let iftrue-value = iftrue-thunk();
  let iftrue-exit-bb = builder.llvm-builder-basic-block;
  if (iftrue-exit-bb)
    ins--br(builder, common-bb);
  end if;

  // Condition false block
  let (iffalse-value, iffalse-exit-bb)
    = if (iffalse-bb)
        ins--block(builder, iffalse-bb);
        let value = iffalse-thunk();
        let exit-bb = builder.llvm-builder-basic-block;
        if (exit-bb)
          ins--br(builder, common-bb);
        end if;
        values(value, exit-bb)
      else
        values(make(<llvm-undef-constant>), test-bb)
      end if;

  // Merge point
  if (iftrue-exit-bb | iffalse-exit-bb)
    ins--block(builder, common-bb);
    if (iftrue-exit-bb & iffalse-exit-bb)
      llvm-constrain-type(llvm-value-type(iftrue-value),
                          llvm-value-type(iffalse-value));
      if (llvm-void-type?(llvm-value-type(iftrue-value)))
        make(<llvm-undef-constant>)
      else
        ins--phi*(builder, iftrue-value, iftrue-exit-bb,
                  iffalse-value, iffalse-exit-bb)
      end if;
    elseif (iftrue-exit-bb)
      iftrue-value
    else
      iffalse-value
    end if
  else
    make(<llvm-undef-constant>)
  end if
end function;

define macro ins--iterate
  { ins--iterate ?:name (?builder:expression, ?bindings:*) ?:body end }
    => { ins--iterate-aux ?name (?builder,
                                 pre: [ let builder = ?builder ],
                                 ?bindings)
           ?body
       end }
bindings:
  { } => { }
  { ?:variable = ?:expression, ... }
    => { var: ?variable,
         pre: [ let ?variable ## "-phi-operands"
                  = make(<stretchy-object-vector>) ],
         add: [ do-add-iterate-phi-operand
                  (builder, ?variable ## "-phi-operands", ?variable)],
         phi: [ let ?variable
                  = make(<llvm-phi-node>,
                         operands: ?variable ## "-phi-operands",
                         metadata: builder-metadata(builder, #()));
                builder-insert(builder, ?variable) ],
         init: ?expression, ... }
end macro;

define macro ins--iterate-aux
  { ins--iterate-aux ?:name (?builder:expression,
                             #key ??var:variable,
                                  ??pre:*,
                                  ??add:*,
                                  ??phi:*,
                                  ??init:expression)
      ?:body
    end }
    => { let builder = ?builder;
         let loop-head-bb :: <llvm-basic-block> = make(<llvm-basic-block>);
         ??pre ; ...;
         local method ?name (??var, ...)
                 ??add ; ... ;
                 ins--br(builder, loop-head-bb);
               end;
         ?name(??init, ...);
         ins--block(builder, loop-head-bb);
         ??phi ; ...;
         ?body }

  // Strip off grouping delimiters
  pre:
    { [ ?tokens:* ] } => { ?tokens }
  add:
    { [ ?tokens:* ] } => { ?tokens }
  phi:
    { [ ?tokens:* ] } => { ?tokens }
end macro;

define function do-add-iterate-phi-operand
    (builder :: <llvm-builder>, operands :: <stretchy-object-vector>, operand)
 => ()
  let operand = llvm-builder-value(builder, operand);
  add!(operands, operand);
  add!(operands, builder.llvm-builder-basic-block);
  llvm-constrain-type(llvm-value-type(operands[0]),
                      llvm-value-type(operand));
end function;
