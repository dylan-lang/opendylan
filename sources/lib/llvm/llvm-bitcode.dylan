Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
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

  // SOURCE_FILENAME: [namechar x N]
  record SOURCE_FILENAME = 16;
end bitcode-block;

define constant $llvm-bitcode-module-version = 2;

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
  record INST_GEP_OLD     =  4; // GEP:        [n x operands]
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
  record INST_ALLOCA      = 19; // ALLOCA:     [instty, op, align]
  record INST_LOAD        = 20; // LOAD:       [opty, op, align, vol]
  record INST_VAARG       = 23; // VAARG:      [valistty, valist, instty]
  record INST_STORE_OLD   = 24; // STORE:      [ptrty,ptr,val, align, vol]
  record INST_EXTRACTVAL  = 26; // EXTRACTVAL: [n x operands]
  record INST_INSERTVAL   = 27; // INSERTVAL:  [n x operands]
  record INST_CMP2        = 28; // CMP2:       [opty, opval, opval, pred]
  record INST_VSELECT     = 29; // VSELECT:    [ty,opval,opval,predty,pred]
  record INST_INBOUNDS_GEP_OLD = 30; // INBOUNDS_GEP: [n x operands]
  record INST_INDIRECTBR  = 31; // INDIRECTBR: [opty, op0, op1, ...]

  record DEBUG_LOC_AGAIN  = 33; // DEBUG_LOC_AGAIN
  record INST_CALL        = 34; // CALL:       [attr, fnty, fnid, args...]
  record DEBUG_LOC        = 35; // DEBUG_LOC:  [Line,Col,ScopeVal, IAVal]
  record INST_FENCE       = 36; // FENCE:      [ordering, synchscope]
  record INST_CMPXCHG_OLD = 37; // CMPXCHG:    [ptrty, ptr, cmp, new, vol, successordering, synchscope, failureordering?, isweak?]
  record INST_ATOMICRMW   = 38; // ATOMICRMW:  [ptrty,ptr,val, operation, align, vol, ordering, synchscope]
  record INST_RESUME      = 39; // RESUME:     [opval]
  record INST_LANDINGPAD_OLD = 40; // LANDINGPAD: [ty,val,val,num,id0,val0...]
  record INST_LOADATOMIC  = 41; // LOAD:       [opty, op, align, vol, ordering, synchscope]
  record INST_STOREATOMIC_OLD = 42; // STORE:      [ptrty,ptr,val, align, vol, ordering, synchscope]
  record INST_GEP         = 43; // GEP:  [inbounds, n x operands]
  record INST_STORE       = 44; // STORE: [ptrty,ptr,valty,val, align, vol]
  record INST_STOREATOMIC = 45; // STORE: [ptrty,ptr,val, align, vol
  record INST_CMPXCHG     = 46; // CMPXCHG: [ptrty,ptr,valty,cmp,new, align, vol,ordering,synchscope]
  record INST_LANDINGPAD  = 47; // LANDINGPAD: [ty,val,num,id0,val0...]
end bitcode-block;

define bitcode-block $IDENTIFICATION_BLOCK = 13
  record STRING = 1;            // IDENTIFICATION: [strchr x N]
  record EPOCH = 2;             // EPOCH: [epoch#]
end bitcode-block;

define constant $llvm-bitcode-identification-string
  = "Open Dylan bitcode writer for LLVM 7";
define constant $llvm-bitcode-epoch = 0;

define bitcode-block $VALUE_SYMTAB_BLOCK = 14
  record ENTRY   = 1;   // VST_ENTRY: [valid, namechar x N]
  record BBENTRY = 2;   // VST_BBENTRY: [bbid, namechar x N]
end bitcode-block;

define bitcode-block $METADATA_BLOCK = 15
  record STRING        = 1;   // MDSTRING:      [values]
  record VALUE         = 2;   // VALUE:         [type num, value num]
  record NODE          = 3;   // NODE:          [n x md num]
  record NAME          = 4;   // STRING:        [values]
  record DISTINCT_NODE = 5;   // DISTINCT_NODE: [n x md num]
  record LOCATION      = 7;   // [distinct, line, col, scope, inlined-at?]
  //record NODE        = 8;   // NODE:         [n x (type num, value num)]
  //record FN_NODE     = 9;   // FN_NODE:      [n x (type num, value num)]
  record NAMED_NODE    = 10;  // NAMED_NODE:   [n x mdnodes]
  record GENERIC_DEBUG = 12;  // [distinct, tag, vers, header, n x md num]
  record SUBRANGE      = 13;  // [distinct, count, lo]
  record ENUMERATOR    = 14;  // [distinct, value, name]
  record BASIC_TYPE    = 15;  // [distinct, tag, name, size, align, enc]
  record FILE          = 16;  // [distinct, filename, directory, checksumkind, checksum]
  record DERIVED_TYPE  = 17;  // [distinct, ...]
  record COMPOSITE_TYPE = 18;  // [distinct, ...]
  record SUBROUTINE_TYPE =19;  // [distinct, flags, types]
  record COMPILE_UNIT  = 20;  // [distinct, ...]
  record SUBPROGRAM    = 21;  // [distinct, ...]
  record LEXICAL_BLOCK = 22;  // [distinct, scope, file, line, column]
  record LEXICAL_BLOCK_FILE = 23;//[distinct, scope, file, discriminator]
  record NAMESPACE     = 24;  // [distinct, scope, file, name, line]
  record TEMPLATE_TYPE = 25;  // [distinct, scope, name, type, ...]
  record TEMPLATE_VALUE = 26;  // [distinct, scope, name, type, value, ...]
  record GLOBAL_VAR    = 27;  // [distinct, ...]
  record LOCAL_VAR     = 28;  // [distinct, ...]
  record EXPRESSION    = 29;  // [distinct, n x element]
  record OBJC_PROPERTY = 30;  // [distinct, name, file, line, ...]
  record IMPORTED_ENTITY = 31;  // [distinct, tag, scope, entity, line, name]
  record MODULE        = 32;  // [distinct, scope, name, ...]
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
  record FUNCTION_OLD =  9;   // FUNCTION: [vararg, retty, paramty x N]
  record HALF        = 10;   // HALF
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
  record FUNCTION    = 21;   // FUNCTION: [vararg, retty, paramty x N]
  record TOKEN       = 22;   // TOKEN
end bitcode-block;

define bitcode-block $METADATA_KIND_BLOCK = 22
  record KIND          = 6;     // [n x [id, name]]
end bitcode-block;

define bitcode-block $STRTAB_BLOCK = 23
  record BLOB = 1;
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

  // Vector of lists of the metadata instances in a partition
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

        // Traverse referenced metadata
        do(initial-traverse-metadata, value-referenced-metadata(value));

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let partition-key = value-partition-key(value);
        let partition-index
          = element(initial-value-partition-table, partition-key, default: #f)
          | (initial-value-partition-table[partition-key]
               := partition-constants.size + first-constant-index);

        // Record the partition assignment
        value-partition-table[value] := partition-index;

        // Record this constant value instance
        let partition-other-values
          = element(partition-constants,
                    partition-index - first-constant-index,
                    default: #());
        partition-constants[partition-index - first-constant-index]
          := add(partition-other-values, value);
      end unless;
    end method,

    method initial-traverse-operand-value (value :: <llvm-value>) => ();
      let value = value-forward(value);
      if (~element(value-partition-table, value, default: #f))
        // Traverse referenced types
        for (referenced-type in value-referenced-types(value))
          initial-traverse-type(type-partition-table,
                                partition-types,
                                initial-type-partition-table,
                                referenced-type)
        end for;

        // Traverse referenced values
        do(initial-traverse-operand-value, value-referenced-values(value));

        // Traverse referenced metadata
        //do(initial-traverse-metadata, value-referenced-metadata(value));
      end if;
    end method,

    method initial-traverse-metadata (metadata :: <llvm-metadata>) => ();
      let metadata = llvm-metadata-forward(metadata);
      unless (element(value-partition-table, metadata, default: #f))
        // Sentinel value to prevent infinite loops
        value-partition-table[metadata] := #t;

        // Traverse referenced metadata
        do(initial-traverse-metadata, metadata-referenced-metadata(metadata));

        // Traverse referenced values
        do(initial-traverse-value, metadata-referenced-values(metadata));

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let partition-key = metadata-partition-key(metadata);
        let partition-index
          = element(value-partition-table, partition-key,
                    default: #f)
          | (value-partition-table[partition-key]
               := partition-metadata.size);

        // Record the partition assignment
        value-partition-table[metadata] := partition-index;

        // Record this metadata reference
        let partition-other-metadata
          = element(partition-metadata, partition-index, default: #());
        partition-metadata[partition-index]
          := add(partition-other-metadata, metadata);
      end unless;
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
        for (attachment :: <llvm-metadata-attachment>
               in instruction.llvm-instruction-metadata)
          initial-traverse-metadata
            (attachment.llvm-metadata-attachment-metadata);
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
    do(initial-traverse-metadata, named.llvm-named-metadata-operands);
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
    end,
    method metadata-referenced-partitions (metadata :: <llvm-metadata>)
      map(method (referenced-metadata :: <llvm-metadata>)
            value-partition-table[llvm-metadata-forward(referenced-metadata)]
          end,
          metadata-referenced-metadata(metadata))
    end;
  refine-partitions(partition-constants, first-constant-index,
                    value-partition-table,
                    value-referenced-partitions);
  refine-partitions(partition-metadata, 0,
                    value-partition-table,
                    metadata-referenced-partitions);

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
     function-attachments :: <sequence>,
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
          if (instance?(referenced, <llvm-constant-value>))
            initial-traverse-value(referenced);
          elseif (instance?(referenced, <llvm-metadata-value>))
            initial-traverse-metadata(referenced.llvm-metadata-value-metadata);
          end if;
        end for;

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let partition-key = value-partition-key(value);
        let partition-index
          = element(initial-value-partition-table, partition-key, default: #f)
          | (initial-value-partition-table[partition-key]
               := partition-constants.size + first-constant-index);

        // Record the partition assignment
        value-partition-table[value] := partition-index;

        // Record this constant value instance
        let index = partition-index - first-constant-index;
        let partition-other-values
          = element(partition-constants, index, default: #());
        partition-constants[index] := add(partition-other-values, value);
      end unless;
    end method,

    method initial-traverse-metadata (metadata :: <llvm-metadata>) => ();
      let metadata = llvm-metadata-forward(metadata);
      unless (element(value-partition-table, metadata, default: #f))
        // Sentinel value to prevent infinite loops
        value-partition-table[metadata] := #t;

        // Traverse referenced metadata
        do(initial-traverse-metadata, metadata-referenced-metadata(metadata));

        // Traverse referenced constant values
        for (v in metadata-referenced-values(metadata))
          if (instance?(v, <llvm-constant-value>))
            initial-traverse-value(v);
          elseif (instance?(v, <llvm-metadata-value>))
            initial-traverse-metadata(v.llvm-metadata-value-metadata);
          end if;
        end for;

        // Determine which partition index this value initially belongs
        // to, assigning a new index if necessary
        let partition-key = metadata-partition-key(metadata);
        let partition-index
          = element(value-partition-table, partition-key,
                    default: #f)
          | (value-partition-table[partition-key]
               := partition-metadata.size + first-function-local-metadata-index);

        // Record the partition assignment
        value-partition-table[metadata] := partition-index;

        // Record this metadata reference
        let index = partition-index - first-function-local-metadata-index;
        let partition-other-metadata
          = element(partition-metadata, index, default: #());
        partition-metadata[index] := add(partition-other-metadata, metadata);
      end unless;
    end method;

  // Traverse global attached metadata
  for (a :: <llvm-metadata-attachment> in function-attachments)
    initial-traverse-metadata(a.llvm-metadata-attachment-metadata);
  end for;

  // Traverse constant instruction operands and metadata
  for (basic-block in function.llvm-function-basic-blocks)
    for (inst in basic-block.llvm-basic-block-instructions)
      for (operand :: <llvm-value> in inst.llvm-instruction-operands)
        let operand = value-forward(operand);
        if (instance?(operand, <llvm-constant-value>))
          initial-traverse-value(operand);
        elseif (instance?(operand, <llvm-metadata-value>))
          initial-traverse-metadata(operand.llvm-metadata-value-metadata);
        end if;
      end for;
      for (attachment :: <llvm-metadata-attachment>
             in inst.llvm-instruction-metadata)
        initial-traverse-metadata(attachment.llvm-metadata-attachment-metadata);
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
  elseif ($machine-word-size = 64)
    %logior(encoding-low, %shift-left(encoding-high, 32))
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
  write-abbrev-record
    (stream, #"function",
     if (type.llvm-function-type-varargs?) 1 else 0 end,
     type-partition-table[return-type],
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
      write-abbrev-record(stream, #"struct-name", type.llvm-struct-type-name);
    end if;
    write-abbrev-record(stream, #"struct-named",
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
  let width = type-forward(value.llvm-value-type).llvm-integer-type-width;
  let signed-integer
    = if (generic-logbit?(width - 1, integer))
        generic-logior(integer, generic-ash(-1, width))
      else
        integer
      end if;
  write-record(stream, #"INTEGER", as-signed-vbr(signed-integer));
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
                   if ($machine-word-size = 32)
                     make(<double-machine-word>, low: low, high: high)
                   elseif ($machine-word-size = 64)
                     %logior(low, %shift-left(high, 32))
                   else
                     error("float-constant $machine-word-size = %d",
                           $machine-word-size);
                   end if);

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
      write-abbrev-record(stream, #"cstring",
                          copy-sequence(contents, end: contents.size - 1));
    else
      write-abbrev-record(stream, #"string", contents);
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
     constant-partition-exemplars :: <sequence>,
     #key global? = #f)
 => ();
  unless (empty?(constant-partition-exemplars))
    with-block-output (stream, $CONSTANTS_BLOCK, 3)
      // Abbrevs
      if (global?)
        write-abbrev-definition(stream, #"cstring",
                                stream-record-id(stream, #"CSTRING"),
                                op-array(), op-fixed(8));
        write-abbrev-definition(stream, #"string",
                                stream-record-id(stream, #"STRING"),
                                op-array(), op-fixed(8));
      end if;

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
     metadata :: <llvm-metadata-node>)
 => ();
  let operands = make(<stretchy-object-vector>);
  for (operand in metadata.llvm-metadata-node-values)
    if (operand)
      let operand = llvm-metadata-forward(operand);
      add!(operands, value-partition-table[operand] + 1);
    else
      add!(operands, 0);
    end if;
  end for;
  write-record(stream,
               if (metadata.llvm-metadata-distinct?)
                 #"DISTINCT_NODE"
               else
                 #"NODE"
               end,
               operands);
end method;

define method encode-emission-kind
    (kind :: <llvm-debug-emission-kind>)
 => (encoding :: <integer>)
  select (kind)
    #"no-debug" => 0;
    #"full-debug" => 1;
    #"line-tables-only" => 2;
    #"debug-directives-only" => 3;
  end select
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DICompileUnit-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  assert(metadata.llvm-metadata-distinct?);
  write-record(stream, #"COMPILE_UNIT",
               1,               // distinct
               metadata.llvm-DICompileUnit-metadata-language,
               moni(metadata.llvm-DICompileUnit-metadata-file),
               moni(metadata.llvm-DICompileUnit-metadata-producer),
               if (metadata.llvm-DICompileUnit-metadata-optimized?) 1 else 0 end,
               moni(metadata.llvm-DICompileUnit-metadata-flags),
               metadata.llvm-DICompileUnit-metadata-runtime-version,
               0,               // splitDebugFilename
               encode-emission-kind
                 (metadata.llvm-DICompileUnit-metadata-emission-kind),
               moni(metadata.llvm-DICompileUnit-metadata-enums),
               moni(metadata.llvm-DICompileUnit-retained-types),
               0,               // subprograms
               0,               // FIXME globals
               0,               // FIXME imports
               0,               // FIXME dwoId
               0,               // FIXME macros
               if (metadata.llvm-DICompileUnit-metadata-split-debug-inlining?) 1 else 0 end,
               0,               // FIXME debugInfoForProfiling
               0);              // FIXME gnuPubnames
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DIFile-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"FILE",
               if (metadata.llvm-metadata-distinct?) 1 else 0 end,
               moni(metadata.llvm-DIFile-metadata-filename),
               moni(metadata.llvm-DIFile-metadata-directory),
               0,               // checksumkind
               0);              // checksum
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DIBasicType-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"BASIC_TYPE",
               if (metadata.llvm-metadata-distinct?) 1 else 0 end,
               metadata.llvm-DIBasicType-metadata-tag,
               moni(metadata.llvm-DIBasicType-metadata-name),
               metadata.llvm-DIBasicType-metadata-size,
               0,               // align
               metadata.llvm-DIBasicType-metadata-encoding);
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DICompositeType-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"COMPOSITE_TYPE",
               // logior of distinct flag with not-used-in-old-typeref flag
               if (metadata.llvm-metadata-distinct?) 3 else 2 end,
               metadata.llvm-DICompositeType-metadata-tag,
               moni(metadata.llvm-DICompositeType-metadata-name),
               moni(metadata.llvm-DICompositeType-metadata-file),
               metadata.llvm-DICompositeType-metadata-line,
               moni(metadata.llvm-DICompositeType-metadata-scope),
               moni(metadata.llvm-DICompositeType-metadata-base-type),
               metadata.llvm-DICompositeType-metadata-size,
               0,               // align
               0,               // offset
               0,               // flags
               moni(metadata.llvm-DICompositeType-metadata-elements),
               0,               // runtimeLang
               0,               // vtableHolder
               0,               // templateParams
               0,               // identifier
               0);              // discriminator
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DIDerivedType-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"DERIVED_TYPE",
               if (metadata.llvm-metadata-distinct?) 1 else 0 end,
               metadata.llvm-DIDerivedType-metadata-tag,
               moni(metadata.llvm-DIDerivedType-metadata-name),
               moni(metadata.llvm-DIDerivedType-metadata-file),
               metadata.llvm-DIDerivedType-metadata-line,
               moni(metadata.llvm-DIDerivedType-metadata-scope),
               moni(metadata.llvm-DIDerivedType-metadata-base-type),
               metadata.llvm-DIDerivedType-metadata-size,
               metadata.llvm-DIDerivedType-metadata-align,
               metadata.llvm-DIDerivedType-metadata-offset,
               metadata.llvm-DIDerivedType-metadata-flags,
               moni(metadata.llvm-DIDerivedType-metadata-extra-data),
               0);              // dwarfAddressSpace
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DILexicalBlock-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"LEXICAL_BLOCK",
               if (metadata.llvm-metadata-distinct?) 1 else 0 end,
               moni(metadata.llvm-DILexicalBlock-metadata-scope),
               moni(metadata.llvm-DILexicalBlock-metadata-file),
               metadata.llvm-DILexicalBlock-metadata-line,
               metadata.llvm-DILexicalBlock-metadata-column);
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DILocalVariable-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"LOCAL_VAR",
               // logior of distinct flag and has-alignment-flag
               if (metadata.llvm-metadata-distinct?) 3 else 2 end,
               moni(metadata.llvm-DILocalVariable-metadata-scope),
               moni(metadata.llvm-DILocalVariable-metadata-name),
               moni(metadata.llvm-DILocalVariable-metadata-file),
               metadata.llvm-DILocalVariable-metadata-line,
               moni(metadata.llvm-DILocalVariable-metadata-type),
               metadata.llvm-DILocalVariable-metadata-arg,
               metadata.llvm-DILocalVariable-metadata-flags,
               metadata.llvm-DILocalVariable-metadata-align);
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DIExpression-metadata>)
  => ();
  write-record(stream, #"EXPRESSION",
               // logior of distinct flag and version 3
               if (metadata.llvm-metadata-distinct?) 7 else 6 end);
  // FIXME operands
end method;


define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DILocation-metadata>)
  => ();
  write-record(stream, #"LOCATION",
               if (metadata.llvm-metadata-distinct?) 1 else 0 end,
               metadata.llvm-DILocation-metadata-line,
               metadata.llvm-DILocation-metadata-column,
               value-partition-table
                 [llvm-metadata-forward(metadata.llvm-DILocation-metadata-scope)],
               0);              // inlinedAt
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DISubprogram-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"SUBPROGRAM",
               // logior of distinct flag and has-unit-flag
               if (metadata.llvm-metadata-distinct?) 3 else 2 end,
               moni(metadata.llvm-DISubprogram-metadata-scope),
               moni(metadata.llvm-DISubprogram-metadata-name),
               moni(metadata.llvm-DISubprogram-metadata-linkage-name),
               moni(metadata.llvm-DISubprogram-metadata-file),
               metadata.llvm-DISubprogram-metadata-line,
               moni(metadata.llvm-DISubprogram-metadata-type),
               if (metadata.llvm-DISubprogram-metadata-local?) 1 else 0 end,
               if (metadata.llvm-DISubprogram-metadata-definition?) 1 else 0 end,
               metadata.llvm-DISubprogram-metadata-scope-line,
               0,               // containingType
               0,               // virtuality
               0,               // virtualIndex
               metadata.llvm-DISubprogram-metadata-flags,
               if (metadata.llvm-DISubprogram-metadata-optimized?) 1 else 0 end,
               moni(metadata.llvm-DISubprogram-metadata-unit),
               0,               // templateParams
               0,               // declaration
               moni(metadata.llvm-DISubprogram-metadata-retained-nodes),
               0,               // thisAdjustment
               0);              // thrownTypes
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DISubrange-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"SUBRANGE",
               // logior of distinct flag with version=1
               if (metadata.llvm-metadata-distinct?) 3 else 2 end,
               moni(metadata.llvm-DISubrange-metadata-count),
               as-signed-vbr(metadata.llvm-DISubrange-metadata-lower-bound));
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-DISubroutineType-metadata>)
  => ();
  local
    method moni                 // metadata or null index
         (m :: false-or(<llvm-metadata>))
      => (id :: <integer>);
      if (m) value-partition-table[llvm-metadata-forward(m)] + 1 else 0 end if
    end method;
  write-record(stream, #"SUBROUTINE_TYPE",
               // logior of distinct flag with has-no-old-typerefs flag
               if (metadata.llvm-metadata-distinct?) 3 else 2 end,
               metadata.llvm-DISubroutineType-metadata-flags,
               moni(metadata.llvm-DISubroutineType-metadata-types),
               0);              // CC
end method;

define method write-metadata-record
    (stream :: <bitcode-stream>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     metadata :: <llvm-value-metadata>)
  => ();
  let value = value-forward(metadata.llvm-metadata-value);
  let type = type-forward(value.llvm-value-type);
  write-record(stream, #"VALUE",
               type-partition-table[type],
               value-partition-table[value]);
end method;


/// Metadata table output

define function write-metadata-kind-table
    (stream :: <bitcode-stream>,
     metadata-kind-table :: <mutable-explicit-key-collection>)
 => ();
  with-block-output (stream, $METADATA_KIND_BLOCK, 3)
    for (kind :: <integer> keyed-by name :: <string> in metadata-kind-table)
      write-record(stream, #"KIND", kind,
                   map-as(<vector>, curry(as, <integer>), name));
    end for;
  end with-block-output;
end function;

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
                           value-partition-table[llvm-metadata-forward(operand)]
                         end,
                         named.llvm-named-metadata-operands));
      end for;
    end with-block-output;
  end unless;
end function;


/// Instruction output

define method add-value
    (operands :: <stretchy-vector>,
     instruction-index :: <integer>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-metadata-value>)
  => ();
  let metadata = value.llvm-metadata-value-metadata;
  let index = value-partition-table[llvm-metadata-forward(metadata)];
  add!(operands, instruction-index - index);
end method;

define method add-value
    (operands :: <stretchy-vector>,
     instruction-index :: <integer>,
     value-partition-table :: <explicit-key-collection>,
     value :: <llvm-value>)
 => ();
  let index = value-partition-table[value-forward(value)];
  add!(operands, instruction-index - index);
end method;

define function add-type
    (operands :: <stretchy-vector>,
     type-partition-table :: <explicit-key-collection>,
     type :: <llvm-type>)
 => ();
  add!(operands, type-partition-table[type-forward(type)]);
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
  add!(operands, instruction-index - index);
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
  add-value(operands, instruction-index, value-partition-table,
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
  add!(operands,
       if (value.llvm-gep-instruction-in-bounds?) 1 else 0 end);
  add!(operands,
       type-partition-table[type-forward(value.llvm-gep-source-type)]);
  for (operand in value.llvm-instruction-operands)
    add-value-type(operands, instruction-index,
                   type-partition-table, value-partition-table,
                   operand);
  end for;
  write-record(stream, #"INST_GEP", operands);
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
  add-value(operands, instruction-index, value-partition-table,
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
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value(operands, instruction-index, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table,
            value.llvm-instruction-operands[1]);
  add!(operands, encode-predicate(value));
  write-record(stream, #"INST_CMP2", operands);
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
    add!(operands,
         value-partition-table[value-forward
                                 (value.llvm-instruction-operands[0])]);
  else
    add!(operands,
         value-partition-table[value-forward
                                 (value.llvm-instruction-operands[1])]);
    add!(operands,
         value-partition-table[value-forward
                                 (value.llvm-instruction-operands[2])]);
    add-value(operands, instruction-index, value-partition-table,
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
  let operands = value.llvm-instruction-operands;
  let operand0 = value-forward(operands[0]);
  let record = make(<stretchy-object-vector>);
  for (i from 1 below operands.size)
    add!(record, value-partition-table[value-forward(operands[i])]);
  end for;
  write-record(stream, #"INST_SWITCH",
               type-partition-table[type-forward(llvm-value-type(operand0))],
               instruction-index - value-partition-table[operand0],
               record);
end method;

define method write-instruction-record
    (stream :: <bitcode-stream>,
     instruction-index :: <integer>,
     type-partition-table :: <object-table>,
     value-partition-table :: <explicit-key-collection>,
     attributes-index-table :: <encoding-sequence-table>,
     value :: <llvm-invoke-instruction>)
 => ();
  let operands = make(<stretchy-object-vector>);

  let attribute-list-encoding
    = encode-attribute-list(value.llvm-invoke-instruction-attribute-list);
  add!(operands,
       attributes-index-table[attribute-list-encoding]);
  add!(operands,
       logior(value.llvm-invoke-instruction-calling-convention, ash(1, 13)));

  add!(operands,
       value-partition-table[value-forward(value.llvm-instruction-operands[0])]);
  add!(operands,
       value-partition-table[value-forward(value.llvm-instruction-operands[1])]);

  let callee = value-forward(value.llvm-instruction-operands[2]);
  let function-pointer-type
    = type-forward(llvm-value-type(callee));
  let function-type
    = type-forward(function-pointer-type.llvm-pointer-type-pointee);
  add!(operands, type-partition-table[function-type]);

  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
                 callee);

  // Fixed parameters
  let fixed-parameter-count
    = function-type.llvm-function-type-parameter-types.size;
  for (i from 3 below fixed-parameter-count + 3)
    add-value(operands, instruction-index, value-partition-table,
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

  write-record(stream, #"INST_INVOKE", operands);
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
  let operands = make(<stretchy-object-vector>);
  for (i from 0 below value.llvm-instruction-operands.size by 2)
    let operand = value.llvm-instruction-operands[i];
    let operand-index = value-partition-table[value-forward(operand)];
    let label = value.llvm-instruction-operands[i + 1];
    add!(operands, as-signed-vbr(instruction-index - operand-index));
    add!(operands, value-partition-table[value-forward(label)]);
  end for;
  write-record(stream, #"INST_PHI",
               type-partition-table[type-forward(llvm-value-type(value))],
               operands);
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
  add!(operands, if (value.llvm-landingpad-instruction-cleanup?) 1 else 0 end);
  add!(operands, value.llvm-instruction-operands.size);
  for (operand in value.llvm-instruction-operands)
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
  write-record(stream, #"INST_ALLOCA",
               type-partition-table
                 [type-forward(value.llvm-alloca-allocated-type)],
               type-partition-table
                 [type-forward(llvm-value-type(value.llvm-instruction-operands[0]))],
               value-partition-table[value.llvm-instruction-operands[0]],
               logior(alignment-encoding
                        (value.llvm-alloca-instruction-alignment),
                      64 /* explicit type flag */));
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
  add-type(operands, type-partition-table, value.llvm-value-type);
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
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
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
  add-value-type(operands, instruction-index,
                 type-partition-table, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table,
            value.llvm-instruction-operands[1]);
  add-value(operands, instruction-index, value-partition-table,
            value.llvm-instruction-operands[2]);
  add!(operands, if (value.llvm-instruction-volatile?) 1 else 0 end);
  add!(operands, atomic-ordering-encoding(value.llvm-atomic-instruction-ordering));
  add!(operands, atomic-scope-encoding(value.llvm-atomic-instruction-scope));
  add!(operands, atomic-ordering-encoding(value.llvm-cmpxchg-instruction-failure-ordering));
  add!(operands, if (value.llvm-cmpxchg-instruction-weak?) 1 else 0 end);
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
    add-value(operands, instruction-index, value-partition-table,
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
  add-value(operands, instruction-index, value-partition-table, valist);
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
     function :: <llvm-function>,
     function-attachments :: <sequence>)
 => ();
  unless (empty?(function.llvm-function-basic-blocks))
    let (value-partition-table :: <mutable-explicit-key-collection>,
         first-constant-index :: <integer>,
         constant-partition-exemplars :: <vector>,
         metadata-partition-exemplars :: <vector>)
      = enumerate-function-values(function,
                                  function-attachments,
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
      unless (empty?(attached-instruction-indices)
                & empty?(function-attachments))
        with-block-output (stream, $METADATA_ATTACHMENT, 3)
          unless (empty?(function-attachments))
            let operands = make(<stretchy-object-vector>);
            for (attachment :: <llvm-metadata-attachment>
                   in function-attachments)
              let metadata = attachment.llvm-metadata-attachment-metadata;
              add!(operands, attachment.llvm-metadata-attachment-kind);
              add!(operands,
                   value-partition-table[llvm-metadata-forward(metadata)]);
            end for;
            write-record(stream, #"ATTACHMENT", operands);
          end unless;

          for (index :: <integer> keyed-by inst :: <llvm-instruction-value>
                 in attached-instruction-indices)
            let operands = make(<stretchy-object-vector>);
            for (attachment :: <llvm-metadata-attachment>
                   in inst.llvm-instruction-metadata)
              let metadata = attachment.llvm-metadata-attachment-metadata;
              add!(operands, attachment.llvm-metadata-attachment-kind);
              add!(operands,
                   value-partition-table[llvm-metadata-forward(metadata)]);
            end for;
            write-record(stream, #"ATTACHMENT", index, operands);
          end for;
        end;
      end unless;

      // Write the function value symbol table
      unless (empty?(function.llvm-function-value-table))
        with-block-output (stream, $VALUE_SYMTAB_BLOCK, 3)
          for (value keyed-by name in function.llvm-function-value-table)
            let id
              = if (instance?(value, <llvm-basic-block>))
                  stream-record-id(stream, #"BBENTRY")
                else
                  stream-record-id(stream, #"ENTRY")
                end if;
            let abbrev
              = if (every?(rcurry(member?, $char6-charset), name))
                  #"entry-6"
                else
                  #"entry-8"
                end if;
            write-abbrev-record(stream, abbrev, id,
                                value-partition-table[value-forward(value)],
                                name);
          end for;
        end with-block-output;
      end unless;
    end with-block-output;
  end unless;
end function;


/// String table

define class <string-table-builder> (<object>)
  slot builder-table-storage :: <byte-vector> = make(<byte-vector>, size: 1024);
  slot builder-table-size :: <integer> = 0;
end class;

define function add-string
    (builder :: <string-table-builder>, string :: <byte-string>)
 => (offset :: <integer>);
  let offset = builder.builder-table-size;
  let new-size = builder.builder-table-size + string.size;
  if (new-size > builder.builder-table-storage.size)
    let old-storage = builder.builder-table-storage;
    builder.builder-table-storage
      := make(<byte-vector>, size: truncate/(new-size * 3, 2));
    copy-bytes(builder.builder-table-storage, 0,
               old-storage, 0, builder.builder-table-size);
  end if;
  copy-bytes(builder.builder-table-storage, offset, string, 0, string.size);
  builder.builder-table-size := new-size;
  offset
end function;


/// Module output

define function write-module
    (stream :: <bitcode-stream>, m :: <llvm-module>)
 => ();
  // Write the identification block
  with-block-output (stream, $IDENTIFICATION_BLOCK, 5)
    write-record(stream, #"STRING", $llvm-bitcode-identification-string);
    write-record(stream, #"EPOCH", $llvm-bitcode-epoch);
  end with-block-output;

  let strtab-builder = make(<string-table-builder>);

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
    // Write the module info:
    begin
      // Version
      write-record(stream, #"VERSION", $llvm-bitcode-module-version);

      // Write the blockinfo
      with-block-output (stream, $BLOCKINFO_BLOCK, 2)
        // Value symbol table abbreviations
        write-record(stream, #"SETBID", $VALUE_SYMTAB_BLOCK.block-id);
        write-blockinfo-abbrev-definition
          (stream, $VALUE_SYMTAB_BLOCK, #"entry-8",
           op-fixed(2),
           op-vbr(8),
           op-array(), op-fixed(8));
        write-blockinfo-abbrev-definition
          (stream, $VALUE_SYMTAB_BLOCK, #"entry-6",
           op-fixed(2),
           op-vbr(8),
           op-array(), op-char6());
      end with-block-output;

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

          let type-bits = integer-length(type-partition-exemplars.size);
          write-abbrev-definition(stream, #"function",
                                  stream-record-id(stream, #"FUNCTION"),
                                  op-fixed(1), // varargs?
                                  op-fixed(type-bits), // return-type
                                  op-array(), op-fixed(type-bits));
          write-abbrev-definition(stream, #"struct-name",
                                  stream-record-id(stream, #"STRUCT_NAME"),
                                  op-array(), op-char6());
          write-abbrev-definition(stream, #"struct-named",
                                  stream-record-id(stream, #"STRUCT_NAMED"),
                                  op-fixed(1),
                                  op-array(), op-fixed(type-bits));

          for (type in type-partition-exemplars)
            write-type-record(stream, type-partition-table, type);
          end for;
        end with-block-output;
      end unless;

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

      // Source filename
      unless (empty?(m.llvm-module-source-filename))
        write-record(stream, #"SOURCE_FILENAME", m.llvm-module-source-filename);
      end unless;

      // Global variables
      for (global :: <llvm-global-variable> in m.llvm-module-globals)
        let global-type = type-forward(llvm-value-type(global));
        let name = global.llvm-global-name;
        write-record(stream, #"GLOBALVAR",
                     add-string(strtab-builder, name), name.size,
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
                     thread-local-encoding(global.llvm-global-variable-thread-local),
                     unnamed-address-encoding(global.llvm-global-unnamed-address)
                     // externally_initialized
                     // dllstorageclass
                     // comdat
                     // attributes
                     // preemption specifier
                     );
      end for;

      // Functions
      for (function :: <llvm-function> in m.llvm-module-functions)
        let name = function.llvm-global-name;
        let attribute-list-encoding
          = encode-attribute-list(function.llvm-function-attribute-list);
    // FUNCTION:  [strtab offset, strtab size, type, callingconv, isproto,
    //             linkage, paramattrs, alignment, section, visibility, gc,
    //             unnamed_addr, prologuedata, dllstorageclass, comdat,
    //             prefixdata, personalityfn, DSO_Local]
        write-record(stream, #"FUNCTION",
                     add-string(strtab-builder, name), name.size,
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
                     unnamed-address-encoding(function.llvm-global-unnamed-address),
                     0,      // prologuedata
                     0,      // dllstorageclass
                     0,      // comdat
                     0,      // prefixdata
                     if (function.llvm-function-personality)
                       value-partition-table
                         [value-forward(function.llvm-function-personality)] + 1
                     else
                       0
                     end,       // personalityfn
                     0);        // DSO_Local
      end for;

      // Aliases
      for (alias :: <llvm-global-alias> in m.llvm-module-aliases)
        let name = alias.llvm-global-name;
        let alias-type = type-forward(llvm-value-type(alias));
        let aliasee = value-forward(alias.llvm-global-alias-aliasee);
        write-record(stream, #"ALIAS",
                     add-string(strtab-builder, name), name.size,
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
                         constant-partition-exemplars,
                         global?: #t);

    // Write metadata kinds
    write-metadata-kind-table(stream, m.%metadata-kind-table);

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
      let function-attachments
        = element(m.llvm-global-metadata-attachment-table, function,
                  default: #());
      write-function(stream,
                     type-partition-table,
                     value-partition-table,
                     first-function-local-constant-index,
                     first-function-local-metadata-index,
                     attributes-index-table,
                     metadata-kind-table,
                     function,
                     function-attachments);
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
  end with-block-output;

  with-block-output (stream, $STRTAB_BLOCK, 3)
    write-abbrev-definition(stream, #"blob",
                            stream-record-id(stream, #"BLOB"),
                            op-blob());
    write-abbrev-record(stream, #"blob",
                        copy-sequence(strtab-builder.builder-table-storage,
                                      end: strtab-builder.builder-table-size));
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
  if (alignment & ~zero?(alignment))
    integer-length(alignment)
  else
    0
  end if
end function;

define function thread-local-encoding
    (thread-local-kind :: <llvm-global-thread-local-kind>)
 => (encoding :: <integer>);
  select (thread-local-kind)
    #f              => 0;
    #t              => 1;
    #"localdynamic" => 2;
    #"initialexec"  => 3;
    #"localexec"    => 4;
  end select
end function;

define function unnamed-address-encoding
    (unnamed :: <llvm-unnamed-address-kind>)
 => (encoding :: <integer>)
  select (unnamed)
    #f                     => 0;
    #"global-unnamed-addr" => 1;
    #"local-unnamed-addr"  => 2;
  end select;
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
