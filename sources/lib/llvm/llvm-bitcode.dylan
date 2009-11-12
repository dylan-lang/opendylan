Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
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
  record PPC_FP128 = 15;   // PPC LONG DOUBLE (2 doubles)

  record METADATA = 16;   // METADATA
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
  record ENTRY   = 1;  // VST_ENTRY: [valid, namechar x N]
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

define function enumerate-types
    (m :: <llvm-module>)
 => (type-partition-table :: <object-table>,
     type-partition-exemplars :: <vector>);
  let type-partition-table = make(<object-table>);
  let partition-types = make(<stretchy-object-vector>);

  let initial-partition-table = make(<sequence-table>);
  local
    method initial-traverse-type (type :: <llvm-type>) => ();
      let type = type-forward(type);
      unless (element(type-partition-table, type, default: #f))
        let (partition-key, splittable?) = type-partition-key(type);
        let partition-index
          = element(initial-partition-table, partition-key, default: #f)
          | (initial-partition-table[partition-key]
               := initial-partition-table.size);
        
        type-partition-table[type] := partition-index;

        let partition-other-types
          = element(partition-types, partition-index, default: #());
        if (splittable? | empty?(partition-other-types))
          partition-types[partition-index]
            := add(partition-other-types, type);
        end if;

        if (splittable?)
          do(initial-traverse-type, type-referenced-types(type));
        end if;
      end unless;
    end method;

  // Traverse each mentioned type to compute the initial partitions
  do(initial-traverse-type, m.llvm-type-table);

  // Refine partitions until they are stable
  iterate loop (i :: <integer> = 0)
    let stable? = #t;
    if (i < partition-types.size)
      let types :: <list> = partition-types[i];
      // Try to split partitions that contain more than one type
      unless (empty?(types.tail))
        let split-types = make(<sequence-table>);
        for (type in types)
          let reference-partitions
            = map(method (type) type-partition-table[type-forward(type)] end,
                  type-referenced-types(type));
          split-types[reference-partitions]
            := add(element(split-types, reference-partitions, default: #()),
                   type);
        end for;

        // Assign new partitions when necessary
        for (new-types in split-types, first? = #t then #f)
          if (first?)
            partition-types[i] := new-types;
          else
            let partition-index = partition-types.size;
            add!(partition-types, new-types);
            do (method (type)
                  type-partition-table[type] := partition-index;
                end,
                new-types);
            stable? := #f;
          end if;
        end for;
      end unless;
      loop(i + 1);
    elseif (~stable?)
      loop(0);
    end if;
  end iterate;

  //
  values(type-partition-table, map(first, partition-types))
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


/// Module output

define function write-module
    (stream :: <bitcode-stream>, m :: <llvm-module>)
 => ();
  with-block-output (stream, $MODULE_BLOCK, 3)
    // Write the blockinfo

    // Write the parameter attribute table

    // Write the type table
    let (type-partition-table :: <object-table>,
         type-partition-exemplars :: <vector>) = enumerate-types(m);
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
    end;
      

    // Write constants

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

  end with-block-output;
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
