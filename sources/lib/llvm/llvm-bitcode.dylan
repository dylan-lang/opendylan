Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Record definitions

define bitcode-block $MODULE_BLOCK = 8 
  record VERSION     = 1;    // VERSION:     [version#]
  record TRIPLE      = 2;    // TRIPLE:      [strchr x N]
  record DATALAYOUT  = 3;    // DATALAYOUT:  [strchr x N]
  record ASM         = 4;    // ASM:         [strchr x N]
  record SECTIONNAME = 5;    // SECTIONNAME: [strchr x N]
  record DEPLIB      = 6;    // DEPLIB:      [strchr x N]

  // GLOBALVAR: [pointer type, isconst, initid,
  //             linkage, alignment, section, visibility, threadlocal]
  record GLOBALVAR   = 7;

  // FUNCTION:  [type, callingconv, isproto, linkage, paramattrs, alignment,
  //             section, visibility]
  record FUNCTION    = 8;

  // ALIAS: [alias type, aliasee val#, linkage]
  record ALIAS       = 9;

  // PURGEVALS: [numvals]
  record PURGEVALS   = 10;

  // GCNAME: [strchr x N]
  record GCNAME      = 11;
end bitcode-block;

// Subblocks of MODULE_BLOCK

define bitcode-block $PARAMATTR_BLOCK = 9
  // ENTRY: [paramidx0, attr0, paramidx1, attr1...
  record ENTRY = 1;
end bitcode-block;

define bitcode-block $TYPE_BLOCK = 10
  record NUMENTRY =  1;   // NUMENTRY: [numentries]

  // Type Codes
  record VOID     =  2;   // VOID
  record FLOAT    =  3;   // FLOAT
  record DOUBLE   =  4;   // DOUBLE
  record LABEL    =  5;   // LABEL
  record OPAQUE   =  6;   // OPAQUE
  record INTEGER  =  7;   // INTEGER: [width]
  record POINTER  =  8;   // POINTER: [pointee type, address space]
  record FUNCTION =  9;   // FUNCTION: [vararg, retty, paramty x N]
  record STRUCT   = 10;   // STRUCT: [ispacked, eltty x N]
  record ARRAY    = 11;   // ARRAY: [numelts, eltty]
  record VECTOR   = 12;   // VECTOR: [numelts, eltty]
  record X86_FP80 = 13;   // X86 LONG DOUBLE
  record FP128    = 14;   // LONG DOUBLE (112 bit mantissa)
  record PPC_FP128 = 15;  // PPC LONG DOUBLE (2 doubles)
  record METADATA = 16;   // METADATA
  record UNION    = 17;   // UNION: [eltty x N]
end bitcode-block;

define bitcode-block $CONSTANTS_BLOCK = 11
  record SETTYPE       =  1;  // SETTYPE:       [typeid]
  record NULL          =  2;  // NULL
  record UNDEF         =  3;  // UNDEF
  record INTEGER       =  4;  // INTEGER:       [intval]
  record WIDE_INTEGER  =  5;  // WIDE_INTEGER:  [n x intval]
  record FLOAT         =  6;  // FLOAT:         [fpval]
  record AGGREGATE     =  7;  // AGGREGATE:     [n x value number]
  record STRING        =  8;  // STRING:        [values]
  record CSTRING       =  9;  // CSTRING:       [values]
  record CE_BINOP      = 10;  // CE_BINOP:      [opcode, opval, opval]
  record CE_CAST       = 11;  // CE_CAST:       [opcode, opty, opval]
  record CE_GEP        = 12;  // CE_GEP:        [n x operands]
  record CE_SELECT     = 13;  // CE_SELECT:     [opval, opval, opval]
  record CE_EXTRACTELT = 14;  // CE_EXTRACTELT: [opty, opval, opval]
  record CE_INSERTELT  = 15;  // CE_INSERTELT:  [opval, opval, opval]
  record CE_SHUFFLEVEC = 16;  // CE_SHUFFLEVEC: [opval, opval, opval]
  record CE_CMP        = 17;  // CE_CMP:        [opty, opval, opval, pred]
  record INLINEASM     = 18;  // INLINEASM:     [sideeffect,asmstr,conststr]
  record CE_SHUFVEC_EX = 19;  // SHUFVEC_EX:    [opty, opval, opval, opval]
  record CE_INBOUNDS_GEP = 20; // INBOUNDS_GEP:  [n x operands]
  record BLOCKADDRESS  = 21;  // BLOCKADDRESS:  [fnty, fnval, bb#]
end bitcode-block;

define bitcode-block $FUNCTION_BLOCK = 12
  record DECLAREBLOCKS    =  1; // DECLAREBLOCKS: [n]

  record INST_BINOP       =  2; // BINOP:      [opcode, ty, opval, opval]
  record INST_CAST        =  3; // CAST:       [opcode, ty, opty, opval]
  record INST_GEP         =  4; // GEP:        [n x operands]
  record INST_SELECT      =  5; // SELECT:     [ty, opval, opval, opval]
  record INST_EXTRACTELT  =  6; // EXTRACTELT: [opty, opval, opval]
  record INST_INSERTELT   =  7; // INSERTELT:  [ty, opval, opval, opval]
  record INST_SHUFFLEVEC  =  8; // SHUFFLEVEC: [ty, opval, opval, opval]
  record INST_CMP         =  9; // CMP:        [opty, opval, opval, pred]

  record INST_RET         = 10; // RET:        [opty,opval<both optional>]
  record INST_BR          = 11; // BR:         [bb#, bb#, cond] or [bb#]
  record INST_SWITCH      = 12; // SWITCH:     [opty, opval, n, n x ops]
  record INST_INVOKE      = 13; // INVOKE:     [attr, fnty, op0,op1, ...]
  record INST_UNWIND      = 14; // UNWIND
  record INST_UNREACHABLE = 15; // UNREACHABLE

  record INST_PHI         = 16; // PHI:        [ty, val0,bb0, ...]
  record INST_MALLOC      = 17; // MALLOC:     [instty, op, align]
  record INST_FREE        = 18; // FREE:       [opty, op]
  record INST_ALLOCA      = 19; // ALLOCA:     [instty, op, align]
  record INST_LOAD        = 20; // LOAD:       [opty, op, align, vol]
  record INST_STORE       = 21; // STORE:      [valty,val,ptr, align, vol]
  record INST_CALL        = 22; // CALL:       [attr, fnty, fnid, args...]
  record INST_VAARG       = 23; // VAARG:      [valistty, valist, instty]
  record INST_STORE2      = 24; // STORE:      [ptrty,ptr,val, align, vol]
  record INST_GETRESULT   = 25; // GETRESULT:  [ty, opval, n]
  record INST_EXTRACTVAL  = 26; // EXTRACTVAL: [n x operands]
  record INST_INSERTVAL   = 27; // INSERTVAL:  [n x operands]
  record INST_CMP2        = 28; // CMP2:       [opty, opval, opval, pred]
  record INST_VSELECT     = 29; // VSELECT:    [ty,opval,opval,predty,pred]
  record INST_INBOUNDS_GEP = 30; // INBOUNDS_GEP: [n x operands]
  record INST_INDIRECTBR  = 31;  // INDIRECTBR: [opty, op0, op1, ...]
end bitcode-block;

define bitcode-block $TYPE_SYMTAB_BLOCK = 13
  record ENTRY = 1;     // ENTRY: [typeid, namechar x N]
end bitcode-block;

define bitcode-block $VALUE_SYMTAB_BLOCK = 14
  record ENTRY   = 1;   // VST_ENTRY: [valid, namechar x N]
  record BBENTRY = 2;   // VST_BBENTRY: [bbid, namechar x N]
end bitcode-block;

define bitcode-block $METADATA_BLOCK = 15
  record STRING        = 1;   // MDSTRING:      [values]
  record NODE          = 2;   // MDNODE:        [n x (type num, value num)]
  record NAME          = 3;   // STRING:        [values]
  record NAMED_NODE    = 4;   // NAMEDMDNODE:   [n x mdnodes]
  record KIND          = 5;   // [n x [id, name]]
  record ATTACHMENT    = 6;   // [m x [value, [n x [id, mdnode]]]
end bitcode-block;

define bitcode-block $METADATA_ATTACHMENT = 16
end bitcode-block;


/// Type output

define class <sequence-table> (<table>)
end class;

define sealed method table-protocol
    (table :: <sequence-table>)
 => (test :: <function>, hash :: <function>);
  values(method (x :: <sequence>, y :: <sequence>) x = y end, 
         method
             (key :: <sequence>, hash-state)
          => (hash :: <integer>, hash-state);
           let hash :: <integer> = 0;
           for (item in key)
             let (item-hash :: <integer>, item-hash-state)
               = object-hash(item, hash-state);
             hash := merge-hash-ids(hash, item-hash, ordered: #t);
             hash-state := item-hash-state;
           end for;
           values (hash, hash-state)
         end);
end method table-protocol;

define function refine-partitions
    (partitions :: <stretchy-vector>,
     offset :: <integer>,
     partition-table :: <object-table>,
     reference-partitions-function :: <function>)
 => ()
  iterate loop (i :: <integer> = 0)
    let stable? = #t;
    if (i < partitions.size)
      let instances :: <list> = partitions[i];
      // Try to split partitions that contain more than one instance
      unless (empty?(instances.tail))
        let split-instances = make(<sequence-table>);
        for (instance in instances)
          let reference-partitions = reference-partitions-function(instance);
          split-instances[reference-partitions]
            := add(element(split-instances, reference-partitions,
                           default: #()),
                   instance);
        end for;

        // Assign new partitions when necessary
        for (new-instances in split-instances, first? = #t then #f)
          if (first?)
            partitions[i] := new-instances;
          else
            let partition-index = partitions.size + offset;
            add!(partitions, new-instances);
            do (method (instance)
                  partition-table[instance] := partition-index;
                end,
                new-instances);
            stable? := #f;
          end if;
        end for;
      end unless;
      loop(i + 1);
    elseif (~stable?)
      loop(0);
    end if;
  end iterate;
end function;  

define function enumerate-types-constants
    (m :: <llvm-module>)
 => (type-partition-table :: <object-table>,
     type-partition-exemplars :: <vector>,
     value-partition-table :: <object-table>,
     first-constant-index :: <integer>,
     constant-partition-exemplars :: <vector>);
  // Table mapping type instances to index (partition) numbers
  let type-partition-table = make(<object-table>);
  
  // Vector of lists of the type instances in a partition
  let partition-types = make(<stretchy-object-vector>);
  
  // Table mapping value instances to index (partition) numbers
  let value-partition-table = make(<object-table>);
  
  // Vector of lists of the constant value instances in a partition
  let partition-constants = make(<stretchy-object-vector>);

  // Assign value indices to global variables
  let first-function-index :: <integer>
    = for (global :: <llvm-global-variable> in m.llvm-module-globals,
           index :: <integer> from 0)
        value-partition-table[global] := index;
      finally
        index
      end for;

  // Assign value indices to global functions
  let first-alias-index :: <integer>
    = for (function :: <llvm-function> in m.llvm-module-functions,
           index :: <integer> from first-function-index)
        value-partition-table[function] := index;
      finally
        index
      end for;

  // Assign value indices to global aliases
  let first-constant-index :: <integer>
    = for (alias :: <llvm-global-alias> in m.llvm-module-aliases,
           index :: <integer> from first-alias-index)
        value-partition-table[alias] := index;
      finally
        index
      end for;

  let initial-type-partition-table = make(<sequence-table>);
  local
    method initial-traverse-type (type :: <llvm-type>) => ();
      let type = type-forward(type);
      unless (element(type-partition-table, type, default: #f))
        // Determine which partition index this type initially belongs
        // to, assigning a new index if necessary
        let (partition-key, splittable?) = type-partition-key(type);
        let partition-index
          = element(initial-type-partition-table, partition-key, default: #f)
          | (initial-type-partition-table[partition-key]
               := initial-type-partition-table.size);

        // Record the partition assignment
        type-partition-table[type] := partition-index;

        // Record this type instance if the partition is subject to
        // splitting, or if it is the first one seen in this partition
        let partition-other-types
          = element(partition-types, partition-index, default: #());
        if (splittable? | empty?(partition-other-types))
          partition-types[partition-index]
            := add(partition-other-types, type);
        end if;

        // Traverse referenced types
        if (splittable?)
          do(initial-traverse-type, type-referenced-types(type));
        end if;
      end unless;
    end method;

  let initial-constant-partition-table = make(<sequence-table>);
  local
    method initial-traverse-value (value :: <llvm-value>) => ();
      let value = value-forward(value);

      unless (element(value-partition-table, value, default: #f))
        // Traverse referenced types
        do(initial-traverse-type, value-referenced-types(value));

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let partition-key = value-partition-key(value);
        let partition-index
          = element(initial-constant-partition-table, partition-key,
                    default: #f)
          | (initial-constant-partition-table[partition-key]
               := initial-constant-partition-table.size + first-constant-index);
        
        // Record the partition assignment
        value-partition-table[value] := partition-index;

        // Record this constant value instance
        let partition-other-values
          = element(partition-constants,
                    partition-index - first-constant-index,
                    default: #());
        partition-constants[partition-index - first-constant-index]
          := add(partition-other-values, value);

        // Traverse referenced values
        do(initial-traverse-value, value-referenced-values(value));
      end unless;
    end method;

  // Compute the initial partitions:
  
  // Traverse each named type
  do(initial-traverse-type, m.llvm-type-table);

  // Traverse globals
  for (global :: <llvm-global-variable> in m.llvm-module-globals)
    initial-traverse-type(llvm-value-type(global));
    if (global.llvm-global-variable-initializer)
      initial-traverse-value(global.llvm-global-variable-initializer)
    end if;
  end for;

  // Traverse functions
  for (function :: <llvm-function> in m.llvm-module-functions)
    initial-traverse-type(llvm-value-type(function));
  end for;

  // Traverse aliases
  for (alias :: <llvm-global-alias> in m.llvm-module-aliases)
    initial-traverse-type(llvm-value-type(alias));
    initial-traverse-value(alias.llvm-global-alias-aliasee);
  end for;
  
  // Refine type partitions until they are stable
  refine-partitions(partition-types, 0,
                    type-partition-table,
                    method (type :: <llvm-type>)
                      map(method (referenced-type :: <llvm-type>)
                            type-partition-table[type-forward(referenced-type)]
                          end,
                          type-referenced-types(type))
                    end);

  // Split constant value partitions based on types
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    method (value :: <llvm-constant-value>)
                      let type = llvm-value-type(value);
                      vector(type-partition-table[type-forward(type)])
                    end);

  // Refine constant value partitions until they are stable
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    method (value :: <llvm-constant-value>)
                      map(method (referenced-value :: <llvm-constant-value>)
                            value-partition-table
                              [value-forward(referenced-value)]
                          end,
                          value-referenced-values(value))
                    end);

  values(type-partition-table, map(first, partition-types),
         value-partition-table,
         first-constant-index, map(first, partition-constants))
end function;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-primitive-type>)
 => ();
  write-record(stream, type.llvm-primitive-type-kind);
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-integer-type>)
 => ();
  write-record(stream, #"INTEGER", type.llvm-integer-type-width);
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-pointer-type>)
 => ();
  let pointee = type-forward(type.llvm-pointer-type-pointee);
  write-record(stream, #"POINTER",
               type-partition-table[pointee],
               type.llvm-pointer-type-address-space);
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-function-type>)
 => ();
  let return-type = type-forward(type.llvm-function-type-return-type);
  apply(write-record, stream, #"FUNCTION",
        if (type.llvm-function-type-varargs?) 1 else 0 end, // vararg
        0,                                                  // ignored
        type-partition-table[return-type],                  // retty
        map(method (type) type-partition-table[type-forward(type)] end,
            type.llvm-function-type-parameter-types))
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-struct-type>)
 => ();
  apply(write-record, stream, #"STRUCT",
        if (type.llvm-struct-type-packed?) 1 else 0 end,
        map(method (type) type-partition-table[type-forward(type)] end,
            type.llvm-struct-type-elements))
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-union-type>)
 => ();
  apply(write-record, stream, #"UNION",
        map(method (type) type-partition-table[type-forward(type)] end,
            type.llvm-union-type-elements))
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-array-type>)
 => ();
  let element-type = type-forward(type.llvm-array-type-element-type);
  write-record(stream, #"ARRAY",
               type.llvm-array-type-size,
               type-partition-table[element-type]);
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-vector-type>)
 => ();
  let element-type = type-forward(type.llvm-vector-type-element-type);
  write-record(stream, #"VECTOR",
               type.llvm-vector-type-size,
               type-partition-table[element-type]);
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-opaque-type>)
 => ();
  write-record(stream, #"OPAQUE");
end method;


/// Constant output

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-null-constant>)
 => ();
  write-record(stream, #"NULL");
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-undef-constant>)
 => ();
  write-record(stream, #"UNDEF");
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-integer-constant>)
 => ();
  let integer = value.llvm-integer-constant-integer;
  write-record(stream, #"INTEGER", as-signed-vbr(integer));
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-float-constant>)
 => ();
  let type = type-forward(llvm-value-type(value));
  select (type.llvm-primitive-type-kind)
    #"FLOAT" =>
      let single-float = as(<single-float>, value.llvm-float-constant-float);
      write-record(stream, #"FLOAT", decode-single-float(single-float));
    
    #"DOUBLE" =>
      let double-float
        = as(<double-float>, value.llvm-float-constant-float);
      let (low :: <machine-word>, high :: <machine-word>)
        = decode-double-float(double-float);
      write-record(stream, #"FLOAT",
                   make(<double-machine-word>, low: low, high: high));
      
    #"X86_FP80", #"FP128", #"PPC_FP128" =>
      error("Can't write %s floating point", type.llvm-primitive-type-kind);
  end select;
end method;

// An aggregate is a string if it is an array of i8 integer constants
define function aggregate-string?
    (value :: <llvm-aggregate-constant>)
 => (string? :: <boolean>)
  let type = type-forward(llvm-value-type(value));
  if (instance?(type, <llvm-array-type>))
    let element-type = type-forward(type.llvm-array-type-element-type);
    instance?(element-type, <llvm-integer-type>)
      & element-type.llvm-integer-type-width == 8
      & every?(rcurry(instance?, <llvm-integer-constant>),
               value.llvm-aggregate-constant-values)
  else
    #f
  end if;
end function;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-aggregate-constant>)
 => ();
  if (aggregate-string?(value))
    let contents = map(llvm-integer-constant-integer,
                       value.llvm-aggregate-constant-values);
    if (zero?(contents.last))
      write-record(stream, #"CSTRING",
                   copy-sequence(contents, end: contents.size - 1));
    else
      write-record(stream, #"STRING", contents);
    end if;
  else
    write-record(stream, #"AGGREGATE",
                 map(method (value :: <llvm-constant-value>)
                       value-partition-table[value-forward(value)]
                     end,
                     value.llvm-aggregate-constant-values))
  end if;
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-asm-constant>)
 => ();
    error("write-constant-record INLINEASM");
//   llvm-asm-constant-asm-string
//   llvm-asm-constant-constraint
//   llvm-asm-constant-side-effect?
//   llvm-asm-constant-align-stack?
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-binop-constant>)
 => ();
  let opcode
    = select (value.llvm-binop-constant-operator)
        #"ADD", #"FADD"  => 0;
        #"SUB", #"FSUB"  => 1;
        #"MUL", #"FMUL"  => 2;
        #"UDIV"          => 3;
        #"SDIV", #"FDIV" => 4;
        #"UREM"          => 5;
        #"SREM", #"FREM" => 6;
        #"SHL"           => 7;
        #"LSHR"          => 8;
        #"ASHR"          => 9;
        #"AND"           => 10;
        #"OR"            => 11;
        #"XOR"           => 12;
      end select;
  write-record(stream, #"CE_BINOP", opcode,
               map(method (value :: <llvm-constant-value>)
                     value-partition-table[value-forward(value)]
                   end,
                   value.llvm-expression-constant-operands))
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-cast-constant>)
 => ();
  let opcode
    = select (value.llvm-cast-constant-operator)
        #"TRUNC" => 0;
        #"ZEXT"  => 1;
        #"SEXT"  => 2;
        #"FPTOUI" => 3;
        #"FPTOSI" => 4;
        #"UITOFP" => 5;
        #"SITOFP" => 6;
        #"FPTRUNC" => 7;
        #"FPEXT" => 8;
        #"PTRTOINT" => 9;
        #"INTTOPTR" => 10;
        #"BITCAST" => 11;
      end;
  let opval = value.llvm-expression-constant-operands[0];
  write-record(stream, #"CE_CAST", opcode,
               type-partition-table[type-forward(llvm-value-type(value))],
               value-partition-table[value-forward(opval)]);
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-gep-constant>)
 => ();
  write-record(stream,
               if (value.llvm-gep-constant-in-bounds?)
                 #"CE_INBOUNDS_GEP"
               else
                 #"CE_GEP"
               end,
               map(method (value :: <llvm-constant-value>)
                     value-partition-table[value-forward(value)]
                   end,
                   value.llvm-expression-constant-operands))
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     value :: <llvm-cmp-constant>)
 => ();
  let op0val = value.llvm-expression-constant-operands[0];
  let op1val = value.llvm-expression-constant-operands[1];
  write-record(stream, #"CE_CMP",
               type-partition-table[type-forward(llvm-value-type(op0val))],
               value-partition-table[value-forward(op0val)],
               value-partition-table[value-forward(op1val)],
               encode-predicate(value));
end method;

define method encode-predicate
    (value :: <llvm-icmp-constant>)
 => (encoding :: <integer>);
  select (value.llvm-cmp-constant-predicate)
    #"EQ"  => 32;
    #"NE"  => 33;
    #"UGT" => 34;
    #"UGE" => 35;
    #"ULT" => 36;
    #"ULE" => 37;
    #"SGT" => 38;
    #"SGE" => 39;
    #"SLT" => 40;
    #"SLE" => 41;
  end
end method;

define method encode-predicate
    (value :: <llvm-fcmp-constant>)
 => (encoding :: <integer>);
  select (value.llvm-cmp-constant-predicate)
    #"FALSE" => 0;
    #"OEQ"   => 1;
    #"OGT"   => 2;
    #"OGE"   => 3;
    #"OLT"   => 4;
    #"OLE"   => 5;
    #"ONE"   => 6;
    #"ORD"   => 7;
    #"UNO"   => 8;
    #"UEQ"   => 9;
    #"UGT"   => 10;
    #"UGE"   => 11;
    #"ULT"   => 12;
    #"ULE"   => 13;
    #"UNE"   => 14;
    #"TRUE"  => 15;
  end
end;


/// Module output

define function write-module
    (stream :: <bitcode-stream>, m :: <llvm-module>)
 => ();
  let (type-partition-table :: <object-table>,
       type-partition-exemplars :: <vector>,
       value-partition-table :: <object-table>,
       first-constant-index :: <integer>,
       constant-partition-exemplars :: <vector>)
      = enumerate-types-constants(m);

  with-block-output (stream, $MODULE_BLOCK, 3)
    // Write the blockinfo

    // Write the parameter attribute table
    begin
      // FIXME
    end;

    // Write the type table
    unless (empty?(type-partition-exemplars))
      with-block-output (stream, $TYPE_BLOCK, 3)
        write-record(stream, #"NUMENTRY", type-partition-exemplars.size);

        for (type in type-partition-exemplars)
          write-type-record(stream, type-partition-table, type);
        end for;
      end with-block-output;
    end unless;

    // Write the module info:
    begin
      // Dependent libraries
      for (deplib in m.llvm-module-dependent-libraries)
        write-record(stream, #"DEPLIB", deplib);
      end for;

      // Target triple
      unless (empty?(m.llvm-module-target-triple))
        write-record(stream, #"TRIPLE", m.llvm-module-target-triple);
      end unless;

      // Data layout
      unless (empty?(m.llvm-module-data-layout))
        write-record(stream, #"DATALAYOUT", m.llvm-module-data-layout);
      end unless;

      // Module-level assembly
      unless (empty?(m.llvm-module-asm))
        write-record(stream, #"ASM", m.llvm-module-asm);
      end unless;

      // Sections
      let section-index-table :: <string-table> = make(<string-table>);
      local
        method do-section (section-name :: <string>) => ()
          if (unfound?(element(section-index-table, section-name,
                               default: $unfound)))
            section-index-table[section-name] := section-index-table.size + 1;
            write-record(stream, #"SECTIONNAME", section-name);
          end if;
        end method;
      let all-globals
        = concatenate(m.llvm-module-globals, m.llvm-module-functions);
      for (global :: <llvm-global-value> in all-globals)
        if (global.llvm-global-section)
          do-section(global.llvm-global-section);
        end if;
      end for;

      // GC
      let gc-index-table :: <string-table> = make(<string-table>);
      local
        method do-gc (gc-name :: <string>) => ()
          if (unfound?(element(gc-index-table, gc-name, default: $unfound)))
            gc-index-table[gc-name] := gc-index-table.size + 1;
            write-record(stream, #"GCNAME", gc-name);
          end if;
        end method;
      for (global :: <llvm-global-value> in m.llvm-module-functions)
        if (global.llvm-function-garbage-collector)
          do-gc(global.llvm-function-garbage-collector);
        end if;
      end for;

      // Global variables
      for (global :: <llvm-global-variable> in m.llvm-module-globals)
        let global-type = type-forward(llvm-value-type(global));
        write-record(stream, #"GLOBALVAR",
                     type-partition-table[global-type],
                     if (global.llvm-global-variable-constant?) 1 else 0 end,
                     if (global.llvm-global-variable-initializer)
                       let value
                         = global.llvm-global-variable-initializer;
                       value-partition-table[value-forward(value)] + 1
                     else
                       0
                     end,
                     linkage-encoding(global.llvm-global-linkage-kind),
                     alignment-encoding(global.llvm-global-alignment),
                     if (global.llvm-global-section)
                       section-index-table[global.llvm-global-section]
                     else
                       0
                     end,
                     visibility-encoding(global.llvm-global-visibility-kind),
                     if (global.llvm-global-variable-thread-local?)
                       1
                     else
                       0
                     end);
      end for;

      // Functions
      for (function :: <llvm-function> in m.llvm-module-functions)
        write-record(stream, #"FUNCTION",
                     type-partition-table[llvm-value-type(function)],
                     function.llvm-function-calling-convention,
                     1,         // FIXME isproto
                     linkage-encoding(function.llvm-global-linkage-kind),
                     0,         // FIXME paramattr
                     alignment-encoding(function.llvm-global-alignment),
                     if (function.llvm-global-section)
                       section-index-table[function.llvm-global-section]
                     else
                       0
                     end,
                     visibility-encoding(function.llvm-global-visibility-kind),
                     if (function.llvm-function-garbage-collector)
                       gc-index-table[function.llvm-function-garbage-collector]
                     else
                       0
                     end if);
      end for;

      // Aliases
      for (alias :: <llvm-global-alias> in m.llvm-module-aliases)
        let alias-type = type-forward(llvm-value-type(alias));
        let aliasee = value-forward(alias.llvm-global-alias-aliasee);
        write-record(stream, #"ALIAS",
                     type-partition-table[alias-type],
                     value-partition-table[aliasee],
                     linkage-encoding(alias.llvm-global-linkage-kind),
                     visibility-encoding(alias.llvm-global-visibility-kind));
      end for;
    end;

    // Write constants
    unless (empty?(constant-partition-exemplars))
      with-block-output (stream, $CONSTANTS_BLOCK, 3)
        let current-type-partition = #f;
        for (constant in constant-partition-exemplars)
          let type-partition
            = type-partition-table[type-forward(llvm-value-type(constant))];
          if (type-partition ~= current-type-partition)
            write-record(stream, #"SETTYPE", type-partition);
            current-type-partition := type-partition;
          end if;
          write-constant-record(stream,
                                type-partition-table,
                                value-partition-table,
                                constant);
        end for;
      end with-block-output;
    end unless;

    // Write metadata

    // Write function bodies

    // Write metadata (store?)

    // Write the type symbol table
    unless (empty?(m.llvm-type-table))
      with-block-output (stream, $TYPE_SYMTAB_BLOCK, 3)
        for (type keyed-by name in m.llvm-type-table)
          write-record(stream, #"ENTRY",
                       type-partition-table[type-forward(type)],
                       name);
        end for;
      end with-block-output;
    end unless;

    // Write the value symbol table
    unless (empty?(m.llvm-global-table))
      with-block-output (stream, $VALUE_SYMTAB_BLOCK, 3)
        for (global keyed-by name in m.llvm-global-table)
          write-record(stream, #"ENTRY",
                       value-partition-table[value-forward(global)],
                       name);
        end for;
      end with-block-output;
    end unless;

  end with-block-output;
end function;

define function linkage-encoding
    (linkage :: <llvm-linkage-kind>) => (encoding :: <integer>);
  select (linkage)
    #"external"             => 0;
    #"weak"                 => 1;
    #"appending"            => 2;
    #"internal"             => 3;
    #"linkonce"             => 4;
    #"dllimport"            => 5;
    #"dllexport"            => 6;
    #"extern-weak"          => 7;
    #"common"               => 8;
    #"private"              => 9;
    #"weak-odr"             => 10;
    #"linkonce-odr"         => 11;
    #"available-externally" => 12;
    #"linker-private"       => 13;
  end select
end function;

define function visibility-encoding
    (visibility :: <llvm-visibility-kind>) => (encoding :: <integer>);
  select (visibility)
    #"default"   => 0;
    #"hidden"    => 1;
    #"protected" => 2;
  end select
end function;

define function alignment-encoding
    (alignment :: false-or(<integer>)) => (encoding :: <integer>);
  if (alignment)
    integer-length(alignment)
  else
    0
  end if;
end function;

define function llvm-write-bitcode
    (stream :: <bitcode-stream>, module :: <llvm-module>)
 => ();
  // Write out the file header
  write-fixed(stream, 8, as(<integer>, 'B'));
  write-fixed(stream, 8, as(<integer>, 'C'));
  write-fixed(stream, 4, #x0);
  write-fixed(stream, 4, #xC);
  write-fixed(stream, 4, #xE);
  write-fixed(stream, 4, #xD);

  write-module(stream, module);
end function;

define function llvm-save-bitcode-file
    (module :: <llvm-module>, locator :: <pathname>)
 => ();
  let inner-stream = #f;
  let stream = #f;
  block ()
    inner-stream
      := make(<multi-buffered-stream>,
              element-type: <byte>,
              direction: #"output",
              if-exists: #"truncate",
              locator: locator);
    stream
      := make(<bitcode-stream>, inner-stream: inner-stream);
    llvm-write-bitcode(stream, module);
  cleanup
    if (stream)
      close(stream);
    elseif (inner-stream)
      close(inner-stream);
    end if;
  end block;
end function;
