Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
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
  //             linkage, alignment, section, visibility, threadlocal,
  //             unnamed_addr]
  record GLOBALVAR   = 7;

  // FUNCTION:  [type, callingconv, isproto, linkage, paramattrs, alignment,
  //             section, visibility, gc, unnamed_addr]
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
  record INST_UNREACHABLE = 15; // UNREACHABLE

  record INST_PHI         = 16; // PHI:        [ty, val0,bb0, ...]
  record INST_MALLOC      = 17; // MALLOC:     [instty, op, align]
  record INST_FREE        = 18; // FREE:       [opty, op]
  record INST_ALLOCA      = 19; // ALLOCA:     [instty, op, align]
  record INST_LOAD        = 20; // LOAD:       [opty, op, align, vol]
  record INST_VAARG       = 23; // VAARG:      [valistty, valist, instty]
  record INST_STORE       = 24; // STORE:      [ptrty,ptr,val, align, vol]
  record INST_EXTRACTVAL  = 26; // EXTRACTVAL: [n x operands]
  record INST_INSERTVAL   = 27; // INSERTVAL:  [n x operands]
  record INST_CMP2        = 28; // CMP2:       [opty, opval, opval, pred]
  record INST_VSELECT     = 29; // VSELECT:    [ty,opval,opval,predty,pred]
  record INST_INBOUNDS_GEP = 30; // INBOUNDS_GEP: [n x operands]
  record INST_INDIRECTBR  = 31; // INDIRECTBR: [opty, op0, op1, ...]

  record DEBUG_LOC_AGAIN  = 33; // DEBUG_LOC_AGAIN
  record INST_CALL        = 34; // CALL:       [attr, fnty, fnid, args...]
  record DEBUG_LOC        = 35; // DEBUG_LOC:  [Line,Col,ScopeVal, IAVal]
  record INST_FENCE       = 36; // FENCE:      [ordering, synchscope]
  record INST_CMPXCHG     = 37; // CMPXCHG:    [ptrty,ptr,cmp,new, align, vol, ordering, synchscope]
  record INST_ATOMICRMW   = 38; // ATOMICRMW:  [ptrty,ptr,val, operation, align, vol, ordering, synchscope]
  record INST_RESUME      = 39; // RESUME:     [opval]
  record INST_LANDINGPAD  = 40; // LANDINGPAD: [ty,val,val,num,id0,val0...]
  record INST_LOADATOMIC  = 41; // LOAD:       [opty, op, align, vol, ordering, synchscope]
  record INST_STOREATOMIC = 42; // STORE:      [ptrty,ptr,val, align, vol, ordering, synchscope]
end bitcode-block;

define bitcode-block $VALUE_SYMTAB_BLOCK = 14
  record ENTRY   = 1;   // VST_ENTRY: [valid, namechar x N]
  record BBENTRY = 2;   // VST_BBENTRY: [bbid, namechar x N]
end bitcode-block;

define bitcode-block $METADATA_BLOCK = 15
  record STRING        = 1;   // MDSTRING:      [values]
  record NAME          = 4;   // STRING:        [values]
  record KIND          = 6;   // [n x [id, name]]
  record NODE          = 8;   // NODE:         [n x (type num, value num)]
  record FN_NODE       = 9;   // FN_NODE:      [n x (type num, value num)]
  record NAMED_NODE    = 10;  // NAMED_NODE:   [n x mdnodes]
end bitcode-block;

define bitcode-block $METADATA_ATTACHMENT = 16
  record ATTACHMENT    = 11;  // [m x [value, [n x [id, mdnode]]]
end bitcode-block;

define bitcode-block $TYPE_BLOCK = 17
  record NUMENTRY    =  1;   // NUMENTRY: [numentries]

  // Type Codes
  record VOID        =  2;   // VOID
  record FLOAT       =  3;   // FLOAT
  record DOUBLE      =  4;   // DOUBLE
  record LABEL       =  5;   // LABEL
  record OPAQUE      =  6;   // OPAQUE [ispacked]
  record INTEGER     =  7;   // INTEGER: [width]
  record POINTER     =  8;   // POINTER: [pointee type, address space]
  record FUNCTION    =  9;   // FUNCTION: [vararg, retty, paramty x N]
  record ARRAY       = 11;   // ARRAY: [numelts, eltty]
  record VECTOR      = 12;   // VECTOR: [numelts, eltty]
  record X86_FP80    = 13;   // X86 LONG DOUBLE
  record FP128       = 14;   // LONG DOUBLE (112 bit mantissa)
  record PPC_FP128   = 15;   // PPC LONG DOUBLE (2 doubles)
  record METADATA    = 16;   // METADATA
  record X86_MMX     = 17;   // X86 MMX

  record STRUCT_ANON = 18;   // STRUCT_ANON: [ispacked, eltty x N]
  record STRUCT_NAME = 19;   // STRUCT_NAME: [strchr x N]
  record STRUCT_NAMED = 20;  // STRUCT_NAMED: [ispacked, eltty x N]
end bitcode-block;



/// Value and type enumeration

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

// FIXME this is only necessary due to <double-machine-word>
define class <encoding-sequence-table> (<table>)
end class;

define sealed method table-protocol
    (table :: <encoding-sequence-table>)
 => (test :: <function>, hash :: <function>);
  values(method (x :: <sequence>, y :: <sequence>) x = y end, 
         method
             (key :: <sequence>, hash-state)
          => (hash :: <integer>, hash-state);
           let hash :: <integer> = 0;
           for (item in key)
             let (item-hash :: <integer>, item-hash-state)
               = encoding-item-hash(item, hash-state);
             hash := merge-hash-ids(hash, item-hash, ordered: #t);
             hash-state := item-hash-state;
           end for;
           values (hash, hash-state)
         end);
end method table-protocol;

define method encoding-item-hash
    (item :: <object>, hash-state)
 => (id :: <integer>, state)
  object-hash(item, hash-state)
end method encoding-item-hash;

define method encoding-item-hash
    (item :: <double-machine-word>, hash-state)
 => (id :: <integer>, state)
  let (id-low :: <integer>, state-low)
    = object-hash(double-machine-word-low(item), hash-state);
  let (id-high :: <integer>, state-high)
    = object-hash(double-machine-word-high(item), state-low);
  values(merge-hash-ids(id-low, id-high, ordered: #t), state-high)
end method encoding-item-hash;

define function initial-traverse-type
    (type-partition-table :: <object-table>,
     partition-types :: <stretchy-object-vector>,
     initial-type-partition-table :: <sequence-table>,
     type :: <llvm-type>)
 => ();
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
    
    // Record this type instance
    partition-types[partition-index]
      := add(element(partition-types, partition-index, default: #()), type);
    
    // Traverse referenced types
    if (splittable?)
      for (referenced-type in type-referenced-types(type))
        initial-traverse-type(type-partition-table,
                              partition-types,
                              initial-type-partition-table,
                              referenced-type)
      end for;
    end if;
  end unless;
end function;

define function refine-partitions
    (partitions :: <stretchy-vector>,
     offset :: <integer>,
     partition-table :: <mutable-explicit-key-collection>,
     reference-partitions-function :: <function>)
 => ()
  iterate loop (i :: <integer> = 0,
                initial-partition-count :: <integer> = partitions.size)
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
          end if;
        end for;
      end unless;
      loop(i + 1, initial-partition-count);
    elseif (initial-partition-count ~= partitions.size)
      loop(0, partitions.size);
    end if;
  end iterate;
end function;

// Sort partitions to ensure that the appearance of a partition
// precedes any partitions that reference it, with the possible
// exception of "delayable" partitions, which can be used to break
// cycles when necessary.
define function topological-sort-partitions
    (partitions :: <vector>,
     offset :: <integer>,
     partition-table :: <mutable-explicit-key-collection>,
     reference-partitions-function :: <function>,
     #key delay-partition-function :: false-or(<function>) = #f)
 => (new-partitions :: <vector>);
  // Sets of available partitions (offset)
  let ready-partitions = make(<bit-set>);
  let delayable-partitions = make(<bit-set>);

  // Invert the reference relation and identify initially ready and
  // delayable partitions
  let referencing-partition-sets
    = make(<simple-object-vector>, size: partitions.size);
  for (i from 0 below partitions.size)
    referencing-partition-sets[i] := make(<bit-set>);
    set-add!(ready-partitions, i);
  end for;

  let referenced-partitions
    = make(<simple-object-vector>, size: partitions.size);
  for (index from offset below partitions.size + offset)
    let instances = partitions[index - offset];
    referenced-partitions[index - offset]
      := reference-partitions-function(instances.first);
    for (referenced-index in referenced-partitions[index - offset])
      if (referenced-index >= offset)
        // Referenced partitions aren't ready yet
        set-add!(referencing-partition-sets[referenced-index - offset],
                 index - offset);
        set-remove!(ready-partitions, referenced-index - offset);
      end if;
    end for;

    if (delay-partition-function & delay-partition-function(instances.first))
      set-add!(delayable-partitions, index - offset);
    end if;
  end for;

  // Get the next available partition index
  local
    method pick-index (set :: <bit-set>) => (index :: false-or(<integer>));
      let (initial-state :: <object>, limit :: <object>,
           next-state :: <function>, finished-state? :: <function>,
           current-key :: <function>) = forward-iteration-protocol(set);
      if (finished-state?(set, initial-state, limit))
        #f
      else
        current-key(set, initial-state) + offset
      end if
    end method;

  // Assign new partitions based on the reference constraints
  let new-partitions
    = make(<stretchy-object-vector>, size: partitions.size);
  for (new-index from partitions.size + offset - 1 to offset by -1)
    // Get the next ready or delayable partition
    let index
      = pick-index(ready-partitions)
      | pick-index(delayable-partitions)
      | error("unbreakable cycle, no topological sort possible");

    referencing-partition-sets[index - offset] := #f;
    set-remove!(ready-partitions, index - offset);
    set-remove!(delayable-partitions, index - offset);

    // Remove restrictions and note newly ready partitions
    for (referenced-index in referenced-partitions[index - offset])
      if (referenced-index >= offset)
        let referencing
          = referencing-partition-sets[referenced-index - offset];
        if (referencing)
          set-remove!(referencing, index - offset);
          if (empty?(referencing))
            referencing-partition-sets[referenced-index - offset] := #f;
            set-add!(ready-partitions, referenced-index - offset);
          end if;
        end if;
      end if;
    end for;

    // Assign new partition indices
    new-partitions[new-index - offset] := partitions[index - offset];
    for (instance in partitions[index - offset])
      partition-table[instance] := new-index;
    end for;
  end for;

  new-partitions
end function;

define function enumerate-types-constants-metadata-attributes
    (m :: <llvm-module>)
 => (type-partition-table :: <object-table>,
     type-partition-exemplars :: <vector>,
     value-partition-table :: <object-table>,
     first-constant-index :: <integer>,
     constant-partition-exemplars :: <vector>,
     metadata-partition-exemplars :: <vector>,
     attributes-index-table :: <encoding-sequence-table>,
     attributes-exemplars :: <vector>);
  // Table mapping type instances to index (partition) numbers
  let type-partition-table = make(<object-table>);
  
  // Vector of lists of the type instances in a partition
  let partition-types = make(<stretchy-object-vector>);
  
  // Table mapping value instances to index (partition) numbers
  let value-partition-table = make(<object-table>);

  // Vector of lists of the constant value instances in a partition
  let partition-constants = make(<stretchy-object-vector>);

  // Vector of lists of the metadata value instances in a partition
  let partition-metadata = make(<stretchy-object-vector>);

  // Table mapping attribute encoding vectors to index numbers
  let attributes-index-table = make(<encoding-sequence-table>);
  attributes-index-table[#[]] := 0;

  // Vector of distinct encoded attribute sequences
  let attributes-exemplars = make(<stretchy-object-vector>);
  local
    method do-attribute-list (attribute-list :: <llvm-attribute-list>)
      let encoding = encode-attribute-list(attribute-list);
      element(attributes-index-table, encoding, default: #f)
        | begin
            add!(attributes-exemplars, encoding);
            attributes-index-table[encoding] := attributes-exemplars.size;
          end;
    end method;
  
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
  let initial-value-partition-table = make(<sequence-table>);
  local
    method initial-traverse-value (value :: <llvm-value>) => ();
      let value = value-forward(value);

      unless (element(value-partition-table, value, default: #f))
        // Sentinel value to prevent infinite loops
        value-partition-table[value] := #t;

        // Traverse referenced types
        for (referenced-type in value-referenced-types(value))
          initial-traverse-type(type-partition-table,
                                partition-types,
                                initial-type-partition-table,
                                referenced-type)
        end for;

        // Traverse referenced values
        do(initial-traverse-value, value-referenced-values(value));

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let (partitions, offset)
          = if (instance?(value, <llvm-metadata-value>))
              values(partition-metadata, 0)
            else
              values(partition-constants, first-constant-index)
            end;
        let partition-key = value-partition-key(value);
        let partition-index
          = element(initial-value-partition-table, partition-key, default: #f)
          | (initial-value-partition-table[partition-key]
               := partitions.size + offset);

        // Record the partition assignment
        value-partition-table[value] := partition-index;

        // Record this constant value instance
        let partition-other-values
          = element(partitions, partition-index - offset, default: #());
        partitions[partition-index - offset]
          := add(partition-other-values, value);
      end unless;
    end method,
    
    method initial-traverse-operand-value (value :: <llvm-value>) => ();
      let value = value-forward(value);

      if (instance?(value, <llvm-metadata-value>)
            & ~value.llvm-metadata-function-local?)
        initial-traverse-value(value)
      elseif (~element(value-partition-table, value, default: #f))
        // Traverse referenced types
        for (referenced-type in value-referenced-types(value))
          initial-traverse-type(type-partition-table,
                                partition-types,
                                initial-type-partition-table,
                                referenced-type)
        end for;

        // Traverse referenced values
        do(initial-traverse-operand-value, value-referenced-values(value));
      end if;
    end method;

  // Compute the initial partitions:
  
  // Traverse each named type
  for (referenced-type in m.llvm-type-table)
    initial-traverse-type(type-partition-table,
                          partition-types,
                          initial-type-partition-table,
                          referenced-type)
  end for;

  // Traverse globals
  for (global :: <llvm-global-variable> in m.llvm-module-globals)
    initial-traverse-type(type-partition-table,
                          partition-types,
                          initial-type-partition-table,
                          llvm-value-type(global));
    if (global.llvm-global-variable-initializer)
      initial-traverse-value(global.llvm-global-variable-initializer)
    end if;
  end for;

  // Traverse functions
  for (function :: <llvm-function> in m.llvm-module-functions)
    initial-traverse-type(type-partition-table,
                          partition-types,
                          initial-type-partition-table,
                          llvm-value-type(function));
    do-attribute-list(function.llvm-function-attribute-list);

    // Traverse argument types
    for (argument in function.llvm-function-value-table)
      initial-traverse-type(type-partition-table,
                            partition-types,
                            initial-type-partition-table,
                            argument.llvm-value-type)
    end for;

    // Traverse instructions
    for (basic-block in function.llvm-function-basic-blocks)
      for (instruction in basic-block.llvm-basic-block-instructions)
        // Traverse instruction operands
        do(initial-traverse-operand-value,
           instruction.llvm-instruction-operands);

        // Traverse instruction attached metadata
        for (named :: <llvm-named-metadata>
               in instruction.llvm-instruction-metadata)
          do(initial-traverse-operand-value,
             named.llvm-named-metadata-operands);
        end for;

        // Traverse instruction return type
        initial-traverse-type(type-partition-table,
                              partition-types,
                              initial-type-partition-table,
                              instruction.llvm-value-type);

        // Traverse attribute lists
        if (instance?(instruction, <llvm-call-instruction>))
          do-attribute-list(instruction.llvm-call-instruction-attribute-list);
        elseif (instance?(instruction, <llvm-invoke-instruction>))
          do-attribute-list(instruction.llvm-invoke-instruction-attribute-list);
        end if;
      end for;
    end for;
  end for;

  // Traverse aliases
  for (alias :: <llvm-global-alias> in m.llvm-module-aliases)
    initial-traverse-type(type-partition-table,
                          partition-types,
                          initial-type-partition-table,
                          llvm-value-type(alias));
    initial-traverse-value(alias.llvm-global-alias-aliasee);
  end for;

  // Traverse module named metadata
  for (named :: <llvm-named-metadata> in m.llvm-module-named-metadata)
    do(initial-traverse-value, named.llvm-named-metadata-operands);
  end for;

  // Refine type partitions until they are stable
  local
    method type-referenced-partitions(type :: <llvm-type>)
      map(method (referenced-type :: <llvm-type>)
            type-partition-table[type-forward(referenced-type)]
          end,
          type-referenced-types(type))
    end;
  refine-partitions(partition-types, 0,
                    type-partition-table,
                    type-referenced-partitions);

  // Split constant value partitions based on types
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    method (value :: <llvm-constant-value>)
                      let type = llvm-value-type(value);
                      vector(type-partition-table[type-forward(type)])
                    end);

  // Refine constant and metadata value partitions until they are stable
  local
    method value-referenced-partitions (value :: <llvm-value>)
      map(method (referenced-value :: <llvm-value>)
            value-partition-table[value-forward(referenced-value)]
          end,
          value-referenced-values(value))
    end;
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    value-referenced-partitions);
  refine-partitions(partition-metadata, 0,
                    value-partition-table,
                    value-referenced-partitions);

  // Topologically sort types
  local
    method type-partition-delay (type :: <llvm-type>)
      instance?(type, <llvm-struct-type>)
        & type.llvm-struct-type-name
    end method;
  let partition-types
    = topological-sort-partitions(partition-types, 0,
                                  type-partition-table,
                                  type-referenced-partitions,
                                  delay-partition-function:
                                    type-partition-delay);

  // Topologically sort constant values
  let partition-constants
    = topological-sort-partitions(partition-constants, first-constant-index,
                                  value-partition-table,
                                  value-referenced-partitions);

  values(type-partition-table, map(first, partition-types),
         value-partition-table,
         first-constant-index, map(first, partition-constants),
         map(first, partition-metadata),
         attributes-index-table, attributes-exemplars)
end function;

define class <hierarchical-object-table> (<mutable-explicit-key-collection>)
  constant slot %hot-parent-table :: <explicit-key-collection>,
    required-init-keyword: parent-table:;
  constant slot %hot-object-table :: <object-table>
    = make(<object-table>);
end class;

define method element
    (table :: <hierarchical-object-table>, key, #key default = $unsupplied)
 => (value);
  let value = element(table.%hot-object-table, key, default: $unfound);
  if (found?(value))
    value
  elseif (supplied?(default))
    element(table.%hot-parent-table, key, default: default)
  else
    element(table.%hot-parent-table, key)
  end if
end method;

define method element-setter
    (value, table :: <hierarchical-object-table>, key)
 => (value);
  element-setter(value, table.%hot-object-table, key)
end method;

define function enumerate-function-values
    (function :: <llvm-function>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     first-function-local-constant-index :: <integer>,
     first-function-local-metadata-index :: <integer>)
 => (value-partition-table :: <explicit-key-collection>,
     first-constant-index :: <integer>,
     constant-partition-exemplars :: <vector>,
     metadata-partition-exemplars :: <vector>);
  // Make a local copy of the value table
  let value-partition-table :: <hierarchical-object-table>
    = make(<hierarchical-object-table>, parent-table: value-partition-table);

  // Assign basic block numbers to basic blocks
  for (basic-block in function.llvm-function-basic-blocks,
       basic-block-number :: <integer> from 0)
    value-partition-table[basic-block] := basic-block-number;
  end for;

  // Assign indices to the function arguments
  let first-constant-index
    = for (argument in function.llvm-function-arguments,
           index from first-function-local-constant-index)
        value-partition-table[argument] := index;
      finally
        index
      end for;
 
  // Vector of lists of the constant value instances in a partition
  let partition-constants = make(<stretchy-object-vector>);

  // Vector of lists of the metadata value instances in a partition
  let partition-metadata = make(<stretchy-object-vector>);

  let initial-value-partition-table = make(<sequence-table>);
  local
    method initial-traverse-value (value :: <llvm-value>) => ();
      let value = value-forward(value);

      unless (element(value-partition-table, value, default: #f))
        // Sentinel value to prevent infinite loops
        value-partition-table[value] := #t;

        // Traverse referenced values
        for (referenced :: <llvm-value> in value-referenced-values(value))
          if (instance?(referenced, <llvm-constant-value>)
                | instance?(referenced, <llvm-metadata-value>))
            initial-traverse-value(referenced);
          end if;
        end for;

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let (partitions, offset)
          = if (instance?(value, <llvm-metadata-value>))
              values(partition-metadata, first-function-local-metadata-index)
            else
              values(partition-constants, first-constant-index)
            end;
        let partition-key = value-partition-key(value);
        let partition-index
          = element(initial-value-partition-table, partition-key, default: #f)
          | (initial-value-partition-table[partition-key]
               := partitions.size + offset);

        // Record the partition assignment
        value-partition-table[value] := partition-index;

        // Record this constant value instance
        let partition-other-values
          = element(partitions, partition-index - offset, default: #());
        partitions[partition-index - offset]
          := add(partition-other-values, value);
      end unless;
    end method;

  // Traverse constant instruction operands and metadata
  for (basic-block in function.llvm-function-basic-blocks)
    for (inst in basic-block.llvm-basic-block-instructions)
      for (operand :: <llvm-value> in inst.llvm-instruction-operands)
        let operand = value-forward(operand);
        if (instance?(operand, <llvm-constant-value>)
              | instance?(operand, <llvm-metadata-value>))
          initial-traverse-value(operand);
        end if;
      end for;
      for (named :: <llvm-named-metadata> in inst.llvm-instruction-metadata)
        do(initial-traverse-value, named.llvm-named-metadata-operands);
      end for;
    end for;
  end for;

  // Split constant value partitions based on types
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    method (value :: <llvm-constant-value>)
                      let type = llvm-value-type(value);
                      vector(type-partition-table[type-forward(type)])
                    end);

  // Refine constant value partitions until they are stable
  local
    method value-referenced-partitions (value :: <llvm-value>)
      map(method (referenced-value :: <llvm-value>)
            value-partition-table[value-forward(referenced-value)]
          end,
          value-referenced-values(value))
    end;
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    value-referenced-partitions);

  // Topologically sort constant values
  let partition-constants
    = topological-sort-partitions(partition-constants, first-constant-index,
                                  value-partition-table,
                                  value-referenced-partitions);

  // Enumerate the instructions themselves
  let instruction-index :: <integer>
    = first-constant-index + partition-constants.size;
  for (basic-block in function.llvm-function-basic-blocks)
    for (instruction in basic-block.llvm-basic-block-instructions)
      unless (llvm-void-type?(llvm-value-type(instruction)))
        value-partition-table[instruction] := instruction-index;
        instruction-index := instruction-index + 1;
      end unless;
    end for;
  end for;

  // Refine metadata value partitions until they are stable
  refine-partitions(partition-metadata, first-function-local-metadata-index,
                    value-partition-table,
                    value-referenced-partitions);

  values(value-partition-table,
         first-constant-index,
         map(first, partition-constants),
         map(first, partition-metadata))
end function;

define function encode-attributes
    (attributes :: <llvm-attributes>) => (encoding)
  let encoding = %logand(attributes, #xFFFF);

  let alignment = as(<integer>, %logand(%shift-right(attributes, 16), 31));
  let encoding-low
    = if (zero?(alignment))
        encoding
      else
        %logior(encoding, %shift-left(#x10000, alignment - 1))
      end if;

  let encoding-high = %shift-right(attributes, 21);
  if (zero?(encoding-high))
    encoding-low
  elseif ($machine-word-size = 32)
    make(<double-machine-word>, low: encoding-low, high: encoding-high)
  else
    error("encode-attributes $machine-word-size = %d", $machine-word-size);
  end if
end function;

define function encode-attribute-list
    (attribute-list :: <llvm-attribute-list>) => (encoding :: <sequence>)
  let encoding = make(<stretchy-object-vector>);

  let return-attributes = attribute-list.llvm-attribute-list-return-attributes;
  unless (zero?(return-attributes))
    add!(encoding, 0);
    add!(encoding, encode-attributes(return-attributes));
  end unless;

  for (parameter-attributes
         in attribute-list.llvm-attribute-list-parameter-attributes,
       index from 1)
    unless (zero?(parameter-attributes))
      add!(encoding, index);
      add!(encoding, encode-attributes(parameter-attributes));
    end unless;
  end for;

  let function-attributes
    = attribute-list.llvm-attribute-list-function-attributes;
  unless (zero?(function-attributes))
    add!(encoding, $maximum-unsigned-machine-word);
    add!(encoding, encode-attributes(function-attributes));
  end unless;

  encoding
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
  write-record(stream, #"FUNCTION",
               if (type.llvm-function-type-varargs?) 1 else 0 end, // vararg
               0,                                                  // ignored
               type-partition-table[return-type],                  // retty
               map(method (type) type-partition-table[type-forward(type)] end,
                   type.llvm-function-type-parameter-types));
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-struct-type>)
 => ();
  let element-indices
    = map(method (type) type-partition-table[type-forward(type)] end,
          type.llvm-struct-type-elements);
  if (type.llvm-struct-type-name)
    // Identified struct type: write as STRUCT_NAME followed by STRUCT_NAMED
    if (instance?(type.llvm-struct-type-name, <string>))
      write-record(stream, #"STRUCT_NAME", type.llvm-struct-type-name);
    end if;
    write-record(stream, #"STRUCT_NAMED",
                 if (type.llvm-struct-type-packed?) 1 else 0 end,
                 element-indices);
  else
    // Literal struct type: write as STRUCT_ANON
    write-record(stream, #"STRUCT_ANON",
                 if (type.llvm-struct-type-packed?) 1 else 0 end,
                 element-indices);
  end if;
end method;

define method write-type-record
    (stream :: <bitcode-stream>, type-partition-table :: <object-table>,
     type :: <llvm-union-type>)
 => ();
  write-record(stream, #"UNION",
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
  if (type.llvm-opaque-type-name)
    if (instance?(type.llvm-opaque-type-name, <string>))
      write-record(stream, #"STRUCT_NAME", type.llvm-opaque-type-name);
    end if;
    write-record(stream, #"OPAQUE", 0);
  else
    error("Reference to non-identified opaque type");
  end if;
end method;


/// Constant output

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-null-constant>)
 => ();
  write-record(stream, #"NULL");
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-undef-constant>)
 => ();
  write-record(stream, #"UNDEF");
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-integer-constant>)
 => ();
  let integer = value.llvm-integer-constant-integer;
  write-record(stream, #"INTEGER", as-signed-vbr(integer));
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
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
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-aggregate-constant>)
 => ();
  if (empty?(value.llvm-aggregate-constant-values))
    write-record(stream, #"NULL");
  elseif (aggregate-string?(value))
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
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-asm-constant>)
 => ();
    error("write-constant-record INLINEASM");
//   llvm-asm-constant-asm-string
//   llvm-asm-constant-constraint
//   llvm-asm-constant-side-effect?
//   llvm-asm-constant-align-stack?
end method;

define function binop-operator-encoding
    (operator :: <llvm-binary-operator>)
 => (encoding :: <integer>);
  select (operator)
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
  end select
end function;

define function binop-flags-encoding
    (value :: <llvm-binary-operator-flags-mixin>)
 => (encoding :: <integer>);
  logior(if (value.llvm-binop-no-unsigned-wrap?) ash(1, 0) else 0 end,
         if (value.llvm-binop-no-signed-wrap?)   ash(1, 1) else 0 end,
         if (value.llvm-binop-exact?)            ash(1, 0) else 0 end)
end function;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-binop-constant>)
 => ();
  let operands
    = map-as(<stretchy-object-vector>,
             method (value :: <llvm-constant-value>)
               value-partition-table[value-forward(value)]
             end,
             value.llvm-expression-constant-operands);
  let flags = binop-flags-encoding(value);
  unless (zero?(flags))
    add!(operands, flags);
  end unless;
  write-record(stream, #"CE_BINOP",
               binop-operator-encoding(value.llvm-binop-constant-operator),
               operands)
end method;

define function cast-operator-encoding
    (operator :: <llvm-cast-operator>)
 => (encoding :: <integer>);
  select (operator)
    #"TRUNC"    => 0;
    #"ZEXT"     => 1;
    #"SEXT"     => 2;
    #"FPTOUI"   => 3;
    #"FPTOSI"   => 4;
    #"UITOFP"   => 5;
    #"SITOFP"   => 6;
    #"FPTRUNC"  => 7;
    #"FPEXT"    => 8;
    #"PTRTOINT" => 9;
    #"INTTOPTR" => 10;
    #"BITCAST"  => 11;
  end
end function;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-cast-constant>)
 => ();
  let opval = value-forward(value.llvm-expression-constant-operands[0]);
  write-record(stream, #"CE_CAST",
               cast-operator-encoding(value.llvm-cast-constant-operator),
               type-partition-table[type-forward(llvm-value-type(opval))],
               value-partition-table[opval]);
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-gep-constant>)
 => ();
  let operands = make(<stretchy-object-vector>);
  for (operand :: <llvm-constant-value>
         in value.llvm-expression-constant-operands)
    let operand = value-forward(operand);
    add!(operands,
         type-partition-table[type-forward(llvm-value-type(operand))]);
    add!(operands,
         value-partition-table[operand]);
  end for;
  write-record(stream,
               if (value.llvm-gep-constant-in-bounds?)
                 #"CE_INBOUNDS_GEP"
               else
                 #"CE_GEP"
               end,
               operands)
end method;

define method write-constant-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-cmp-constant>)
 => ();
  let op0val = value-forward(value.llvm-expression-constant-operands[0]);
  let op1val = value-forward(value.llvm-expression-constant-operands[1]);
  write-record(stream, #"CE_CMP",
               type-partition-table[type-forward(llvm-value-type(op0val))],
               value-partition-table[op0val],
               value-partition-table[op1val],
               encode-predicate(value));
end method;

define method encode-predicate
    (value :: <llvm-icmp-mixin>)
 => (encoding :: <integer>);
  select (value.llvm-cmp-predicate)
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
    (value :: <llvm-fcmp-mixin>)
 => (encoding :: <integer>);
  select (value.llvm-cmp-predicate)
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


/// Constant table output

define function write-constant-table
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <mutable-explicit-key-collection>,
     constant-partition-exemplars :: <sequence>)
 => ();
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
end function;


/// Metadata output

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-metadata-string>)
 => ();
  write-record(stream, #"STRING",
               map-as(<vector>,
                      curry(as, <integer>), value.llvm-metadata-string));
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-metadata-node>)
 => ();
  let operands = make(<stretchy-object-vector>);
  for (operand in value.llvm-metadata-node-values)
    if (operand)
      let operand = value-forward(operand);
      add!(operands,
           type-partition-table[type-forward(llvm-value-type(operand))]);
      add!(operands, value-partition-table[operand]);
    else
      add!(operands, type-partition-table[$llvm-void-type]);
      add!(operands, 0);
    end if;
  end for;
  write-record(stream,
               if (value.llvm-metadata-function-local?)
                 #"FN_NODE"
               else
                 #"NODE"
               end, 
               operands);
end method;


/// Metadata table output

define function write-metadata-table
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <mutable-explicit-key-collection>,
     metadata-partition-exemplars :: <sequence>,
     named-metadata :: <sequence>)
 => ();
  unless (empty?(metadata-partition-exemplars))
    with-block-output (stream, $METADATA_BLOCK, 4)
      // Write metadata records
      for (metadata in metadata-partition-exemplars)
        write-metadata-record(stream,
                              type-partition-table,
                              value-partition-table,
                              metadata);
      end for;

      // Write records for each module-level named metadata
      for (named :: <llvm-named-metadata> in named-metadata)
        write-record(stream, #"NAME",
                     map-as(<vector>, curry(as, <integer>),
                            named.llvm-named-metadata-name));
        write-record(stream, #"NAMED_NODE",
                     map(method (operand)
                           value-partition-table[value-forward(operand)]
                         end,
                         named.llvm-named-metadata-operands));
      end for;
    end with-block-output;
  end unless;
end function;


/// Instruction output

define function add-value
    (operands :: <stretchy-vector>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-value>)
 => ();
  let index = value-partition-table[value-forward(value)];
  add!(operands, index);
end function;

define function add-value-type
    (operands :: <stretchy-vector>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-value>)
 => ();
  let value = value-forward(value);
  let index = value-partition-table[value];
  add!(operands, index);
  if (index >= instruction-index)
    add!(operands, type-partition-table[type-forward(llvm-value-type(value))]);
  end if;
end function;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-binop-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add!(operands,
       binop-operator-encoding(value.llvm-binop-instruction-operator));

  let flags = binop-flags-encoding(value);
  unless (zero?(flags))
    add!(operands, flags);
  end unless;

  write-record(stream, #"INST_BINOP", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-cast-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add!(operands, type-partition-table[type-forward(llvm-value-type(value))]);
  add!(operands, cast-operator-encoding(value.llvm-cast-instruction-operator));
  write-record(stream, #"INST_CAST", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-gep-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  for (operand in value.llvm-instruction-operands)
    add-value-type(operands, instruction-index,
                   type-partition-table, value-partition-table,
                   operand);
  end for;
  write-record(stream,
               if (value.llvm-gep-instruction-in-bounds?)
                 #"INST_INBOUNDS_GEP"
               else
                 #"INST_GEP"
               end if,
               operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-select-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[2]);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  write-record(stream, #"INST_VSELECT", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-extractelement-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  write-record(stream, #"INST_EXTRACTELT", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-insertelement-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[2]);
  write-record(stream, #"INST_INSERTELT", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-shufflevector-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[2]);
  write-record(stream, #"INST_SHUFFLEVEC", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-cmp-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add!(operands, encode-predicate(value));
  write-record(stream, #"INST_CMP", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-return-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  for (operand in value.llvm-instruction-operands)
    add-value-type(operands, instruction-index,
                   type-partition-table, value-partition-table,
                   operand);
  end for;
  write-record(stream, #"INST_RET", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-branch-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  if (value.llvm-instruction-operands.size = 1)
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[0]);
  else
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[1]);
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[2]);
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[0]);
  end if;
  write-record(stream, #"INST_BR", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-switch-instruction>)
 => ();
  let operand0 = value-forward(value.llvm-instruction-operands[0]);
  write-record(stream, #"INST_SWITCH",
               type-partition-table[type-forward(llvm-value-type(operand0))],
               map(method (value :: <llvm-value>)
                     value-partition-table[value-forward(value)]
                   end,
                   value.llvm-instruction-operands));
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-invoke-instruction>)
 => ();
  let attribute-list-encoding
    = encode-attribute-list(value.llvm-invoke-instruction-attribute-list);
  let operands = make(<stretchy-object-vector>);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[2]);

  // Fixed parameters
  let callee = value-forward(value.llvm-instruction-operands[2]);
  let function-pointer-type
    = type-forward(llvm-value-type(callee));
  let function-type
    = type-forward(function-pointer-type.llvm-pointer-type-pointee);
  let fixed-parameter-count
    = function-type.llvm-function-type-parameter-types.size;
  for (i from 3 below fixed-parameter-count + 3)
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[i]);
  end for;

  // varargs parameters
  if (function-type.llvm-function-type-varargs?)
    for (i from fixed-parameter-count + 3
           below value.llvm-instruction-operands.size)
      add-value-type(operands, instruction-index,
                     type-partition-table, value-partition-table,
                     value.llvm-instruction-operands[i]);
    end for;
  end if;
  
  write-record(stream, #"INST_INVOKE",
               attributes-index-table[attribute-list-encoding],
               value.llvm-invoke-instruction-calling-convention,
               operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-unreachable-instruction>)
 => ();
  write-record(stream, #"INST_UNREACHABLE");
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-indirect-branch-instruction>)
 => ();
    error("write-instruction-record indirectbr");
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-phi-node>)
 => ();
  write-record(stream, #"INST_PHI",
               type-partition-table[type-forward(llvm-value-type(value))],
               map(method (value :: <llvm-value>)
                     value-partition-table[value-forward(value)]
                   end,
                   value.llvm-instruction-operands));
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-resume-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  write-record(stream, #"INST_RESUME", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-landingpad-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add!(operands, type-partition-table[type-forward(llvm-value-type(value))]);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add!(operands, if (value.llvm-landingpad-instruction-cleanup?) 1 else 0 end);
  add!(operands, value.llvm-instruction-operands.size - 1);
  for (i from 1 below value.llvm-instruction-operands.size)
    let operand = value.llvm-instruction-operands[i];
    if (instance?(type-forward(llvm-value-type(operand)), <llvm-array-type>))
      add!(operands, 1);        // Filter
    else
      add!(operands, 0);        // Catch
    end if;
    add-value-type(operands, instruction-index,
                   type-partition-table, value-partition-table,
                   operand);
  end for;
  write-record(stream, #"INST_LANDINGPAD", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-alloca-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[0]);
  add!(operands, alignment-encoding(value.llvm-alloca-instruction-alignment));
  write-record(stream, #"INST_ALLOCA",
               type-partition-table
                 [type-forward(llvm-value-type(value))],
               type-partition-table
                 [type-forward(llvm-value-type(value.llvm-instruction-operands[0]))],
               operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-load-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add!(operands, alignment-encoding(value.llvm-memory-instruction-alignment));
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  write-record(stream, #"INST_LOAD", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-store-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[0]);
  add!(operands, alignment-encoding(value.llvm-memory-instruction-alignment));
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  write-record(stream, #"INST_STORE", operands);
end method;

define function atomic-ordering-encoding
    (ordering :: <llvm-atomic-ordering>)
 => (encoding :: <integer>);
  select (ordering)
    #"not-atomic"              => 0;
    #"unordered"               => 1;
    #"monotonic"               => 2;
    #"acquire"                 => 3;
    #"release"                 => 4;
    #"acquire-release"         => 5;
    #"sequentially-consistent" => 6;
  end
end function;

define function atomic-scope-encoding
    (scope :: <llvm-synchronization-scope>)
 => (encoding :: <integer>);
  select (scope)
    #"single-thread" => 0;
    #"cross-thread"  => 1;
  end
end function;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-atomic-load-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add!(operands, alignment-encoding(value.llvm-memory-instruction-alignment));
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  add!(operands,
       atomic-ordering-encoding(value.llvm-atomic-instruction-ordering));
  add!(operands, atomic-scope-encoding(value.llvm-atomic-instruction-scope));
  write-record(stream, #"INST_LOADATOMIC", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-atomic-store-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[0]);
  add!(operands, alignment-encoding(value.llvm-memory-instruction-alignment));
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  add!(operands, atomic-ordering-encoding(value.llvm-atomic-instruction-ordering));
  add!(operands, atomic-scope-encoding(value.llvm-atomic-instruction-scope));
  write-record(stream, #"INST_STOREATOMIC", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-fence-instruction>)
 => ();
  write-record(stream, #"INST_FENCE",
               atomic-ordering-encoding(value.llvm-atomic-instruction-ordering),
               atomic-scope-encoding(value.llvm-atomic-instruction-scope));
end method;

define function atomicrmw-operation-encoding
    (operation :: <llvm-atomicrmw-operation>)
 => (encoding :: <integer>);
  select (operation)
    #"xchg" => 0;
    #"add"  => 1;
    #"sub"  => 2;
    #"and"  => 3;
    #"nand" => 4;
    #"or"   => 5;
    #"xor"  => 6;
    #"max"  => 7;
    #"min"  => 8;
    #"umax" => 9;
    #"umin" => 10;
  end
end;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-atomicrmw-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add!(operands, atomicrmw-operation-encoding(value.llvm-atomicrmw-instruction-operation));
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  add!(operands, atomic-ordering-encoding(value.llvm-atomic-instruction-ordering));
  add!(operands, atomic-scope-encoding(value.llvm-atomic-instruction-scope));
  write-record(stream, #"INST_ATOMICRMW", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-cmpxchg-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value(operands, value-partition-table,
            value.llvm-instruction-operands[2]);
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  add!(operands, atomic-ordering-encoding(value.llvm-atomic-instruction-ordering));
  add!(operands, atomic-scope-encoding(value.llvm-atomic-instruction-scope));
  write-record(stream, #"INST_CMPXCHG", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-call-instruction>)
 => ();
  let attribute-list-encoding
    = encode-attribute-list(value.llvm-call-instruction-attribute-list);
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);

  // Fixed parameters
  let callee = value-forward(value.llvm-instruction-operands[0]);
  let function-pointer-type
    = type-forward(llvm-value-type(callee));
  let function-type
    = type-forward(function-pointer-type.llvm-pointer-type-pointee);
  let fixed-parameter-count
    = function-type.llvm-function-type-parameter-types.size;
  for (i from 1 below fixed-parameter-count + 1)
    add-value(operands, value-partition-table,
              value.llvm-instruction-operands[i]);
  end for;

  // varargs parameters
  if (function-type.llvm-function-type-varargs?)
    for (i from fixed-parameter-count + 1
           below value.llvm-instruction-operands.size)
      add-value-type(operands, instruction-index,
                     type-partition-table, value-partition-table,
                     value.llvm-instruction-operands[i]);
    end for;
  end if;

  write-record(stream, #"INST_CALL",
               attributes-index-table[attribute-list-encoding],
               logior(ash(value.llvm-call-instruction-calling-convention, 1),
                      if(value.llvm-call-instruction-tail-call?) 1 else 0 end),
               operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-va-arg-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  let valist = value-forward(value.llvm-instruction-operands[0]);
  add!(operands, type-partition-table[type-forward(llvm-value-type(valist))]);
  add-value(operands, value-partition-table, valist);
  add!(operands, type-partition-table[type-forward(llvm-value-type(value))]);
  write-record(stream, #"INST_VAARG", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-extract-value-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  concatenate!(operands, value.llvm-aggregate-instruction-indices);
  write-record(stream, #"INST_EXTRACTVAL", operands);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-insert-value-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[0]);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 value.llvm-instruction-operands[1]);
  concatenate!(operands, value.llvm-aggregate-instruction-indices);
  write-record(stream, #"INST_INSERTVAL", operands);
end method;


/// Function output

define function write-function
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <object-table>,
     first-function-local-constant-index :: <integer>,
     first-function-local-metadata-index :: <integer>,
     attributes-index-table :: <encoding-sequence-table>,
     metadata-kind-table :: <string-table>,
     function :: <llvm-function>)
 => ();
  unless (empty?(function.llvm-function-basic-blocks))
    let (value-partition-table :: <mutable-explicit-key-collection>,
         first-constant-index :: <integer>,
         constant-partition-exemplars :: <vector>,
         metadata-partition-exemplars :: <vector>)
      = enumerate-function-values(function,
                                  type-partition-table,
                                  value-partition-table,
                                  first-function-local-constant-index,
                                  first-function-local-metadata-index);

    with-block-output (stream, $FUNCTION_BLOCK, 3)
      // Tell the reader how many basic blocks there will be
      write-record(stream, #"DECLAREBLOCKS",
                   size(function.llvm-function-basic-blocks));

      // Write the function constant table
      write-constant-table(stream,
                           type-partition-table,
                           value-partition-table,
                           constant-partition-exemplars);

      // Write the function metadata table
      write-metadata-table(stream,
                           type-partition-table,
                           value-partition-table,
                           metadata-partition-exemplars,
                           #[]);

      let attached-instruction-indices :: <object-table>
        = make(<object-table>);

      // Write the instructions
      let instruction-index :: <integer>
        = first-constant-index + constant-partition-exemplars.size;
      let attachment-instruction-index :: <integer> = 0;
      for (basic-block in function.llvm-function-basic-blocks)
        for (instruction in basic-block.llvm-basic-block-instructions)
          write-instruction-record(stream, instruction-index,
                                   type-partition-table,
                                   value-partition-table,
                                   attributes-index-table,
                                   instruction);

          // All non-void instructions are assigned value indices
          unless (llvm-void-type?(llvm-value-type(instruction)))
            instruction-index := instruction-index + 1;
          end unless;

          // All instructions are assigned a separate index for
          // identifying metadata attachments
          unless (empty?(instruction.llvm-instruction-metadata))
            attached-instruction-indices[instruction]
              := attachment-instruction-index;
          end unless;
          attachment-instruction-index := attachment-instruction-index + 1;
        end for;
      end for;

      // Write the metadata attachment table
      unless (empty?(attached-instruction-indices))
        with-block-output (stream, $METADATA_ATTACHMENT, 3)
          for (index :: <integer> keyed-by inst :: <llvm-instruction-value>
                 in attached-instruction-indices)
            let operands = make(<stretchy-object-vector>);
            for (named :: <llvm-named-metadata>
                   in inst.llvm-instruction-metadata)
              let name = named.llvm-named-metadata-name;
              let kind
                = element(metadata-kind-table, name, default: #f)
                | (metadata-kind-table[name] := metadata-kind-table.size);
              let value = named.llvm-named-metadata-operands[0];
              add!(operands, kind);
              add!(operands, value-partition-table[value-forward(value)]);
            end for;
            write-record(stream, #"ATTACHMENT", index, operands);
          end for;
        end;
      end unless;

      // Write the function value symbol table
      unless (empty?(function.llvm-function-value-table))
        with-block-output (stream, $VALUE_SYMTAB_BLOCK, 3)
          for (value keyed-by name in function.llvm-function-value-table)
            if (instance?(value, <llvm-basic-block>))
              write-record(stream, #"BBENTRY",
                           value-partition-table[value-forward(value)],
                           name);
            else
              write-record(stream, #"ENTRY",
                           value-partition-table[value-forward(value)],
                           name);
            end if;
          end for;
        end with-block-output;
      end unless;
    end with-block-output;
  end unless;
end function;


/// Module output

define function write-module
    (stream :: <bitcode-stream>, m :: <llvm-module>)
 => ();
  let (type-partition-table :: <object-table>,
       type-partition-exemplars :: <vector>,
       value-partition-table :: <object-table>,
       first-constant-index :: <integer>,
       constant-partition-exemplars :: <vector>,
       metadata-partition-exemplars :: <vector>,
       attributes-index-table :: <encoding-sequence-table>,
       attributes-exemplars :: <vector>)
      = enumerate-types-constants-metadata-attributes(m);

  with-block-output (stream, $MODULE_BLOCK, 3)
    // Write the blockinfo

    // Write the parameter attribute table
    unless (empty?(attributes-exemplars))
      with-block-output (stream, $PARAMATTR_BLOCK, 3)
        for (encoding in attributes-exemplars)
          write-record(stream, #"ENTRY", encoding);
        end for;
      end with-block-output;
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
                     end,
                     if (global.llvm-global-unnamed-address?)
                       1
                     else
                       0
                     end);
      end for;

      // Functions
      for (function :: <llvm-function> in m.llvm-module-functions)
        let attribute-list-encoding
          = encode-attribute-list(function.llvm-function-attribute-list);
        write-record(stream, #"FUNCTION",
                     type-partition-table[llvm-value-type(function)],
                     function.llvm-function-calling-convention,
                     if (empty?(function.llvm-function-basic-blocks))
                       1
                     else
                       0
                     end,
                     linkage-encoding(function.llvm-global-linkage-kind),
                     attributes-index-table[attribute-list-encoding],
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
                     end if,
                     if (function.llvm-global-unnamed-address?)
                       1
                     else
                       0
                     end);
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
    write-constant-table(stream,
                         type-partition-table,
                         value-partition-table,
                         constant-partition-exemplars);

    // Write metadata
    write-metadata-table(stream,
                         type-partition-table,
                         value-partition-table,
                         metadata-partition-exemplars,
                         m.llvm-module-named-metadata);

    let metadata-kind-table :: <string-table> = make(<string-table>);

    // Write function bodies
    let first-function-local-constant-index :: <integer>
      = first-constant-index + constant-partition-exemplars.size;
    let first-function-local-metadata-index :: <integer>
      = metadata-partition-exemplars.size;
    for (function :: <llvm-function> in m.llvm-module-functions)
      write-function(stream,
                     type-partition-table,
                     value-partition-table,
                     first-function-local-constant-index,
                     first-function-local-metadata-index,
                     attributes-index-table,
                     metadata-kind-table,
                     function);
    end for;

    // Write metadata kinds
    unless (empty?(metadata-kind-table))
      with-block-output (stream, $METADATA_BLOCK, 4)
        for (index :: <integer> keyed-by name :: <string>
               in metadata-kind-table)
          write-record(stream, #"KIND", index, name);
        end for;
      end;
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
    #"linker-private-weak"  => 14;
    #"linker-private-weak-def-auto" => 15;
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
