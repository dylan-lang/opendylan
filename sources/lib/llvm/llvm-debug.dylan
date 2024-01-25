Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $llvm-debug-metadata-version = 3;

define constant $DW-TAG-invalid :: <integer> = -1;

define constant $DW-TAG-anchor :: <integer> = 0;
define constant $DW-TAG-auto-variable :: <integer> = 256;
define constant $DW-TAG-arg-variable :: <integer> = 257;
define constant $DW-TAG-return-variable :: <integer> = 258;
define constant $DW-TAG-vector-type :: <integer> = 259;

define constant $DW-TAG-user-base :: <integer> = 4096;

define constant $DW-TAG-array-type :: <integer> = 1;
define constant $DW-TAG-class-type :: <integer> = 2;
define constant $DW-TAG-entry-point :: <integer> = 3;
define constant $DW-TAG-enumeration-type :: <integer> = 4;
define constant $DW-TAG-formal-parameter :: <integer> = 5;
define constant $DW-TAG-imported-declaration :: <integer> = 8;
define constant $DW-TAG-label :: <integer> = 10;
define constant $DW-TAG-lexical-block :: <integer> = 11;
define constant $DW-TAG-member :: <integer> = 13;
define constant $DW-TAG-pointer-type :: <integer> = 15;
define constant $DW-TAG-reference-type :: <integer> = 16;
define constant $DW-TAG-compile-unit :: <integer> = 17;
define constant $DW-TAG-string-type :: <integer> = 18;
define constant $DW-TAG-structure-type :: <integer> = 19;
define constant $DW-TAG-subroutine-type :: <integer> = 21;
define constant $DW-TAG-typedef :: <integer> = 22;
define constant $DW-TAG-union-type :: <integer> = 23;
define constant $DW-TAG-unspecified-parameters :: <integer> = 24;
define constant $DW-TAG-variant :: <integer> = 25;
define constant $DW-TAG-common-block :: <integer> = 26;
define constant $DW-TAG-common-inclusion :: <integer> = 27;
define constant $DW-TAG-inheritance :: <integer> = 28;
define constant $DW-TAG-inlined-subroutine :: <integer> = 29;
define constant $DW-TAG-module :: <integer> = 30;
define constant $DW-TAG-ptr-to-member-type :: <integer> = 31;
define constant $DW-TAG-set-type :: <integer> = 32;
define constant $DW-TAG-subrange-type :: <integer> = 33;
define constant $DW-TAG-with-stmt :: <integer> = 34;
define constant $DW-TAG-access-declaration :: <integer> = 35;
define constant $DW-TAG-base-type :: <integer> = 36;
define constant $DW-TAG-catch-block :: <integer> = 37;
define constant $DW-TAG-const-type :: <integer> = 38;
define constant $DW-TAG-constant :: <integer> = 39;
define constant $DW-TAG-enumerator :: <integer> = 40;
define constant $DW-TAG-file-type :: <integer> = 41;
define constant $DW-TAG-friend :: <integer> = 42;
define constant $DW-TAG-namelist :: <integer> = 43;
define constant $DW-TAG-namelist-item :: <integer> = 44;
define constant $DW-TAG-packed-type :: <integer> = 45;
define constant $DW-TAG-subprogram :: <integer> = 46;
define constant $DW-TAG-template-type-parameter :: <integer> = 47;
define constant $DW-TAG-template-value-parameter :: <integer> = 48;
define constant $DW-TAG-thrown-type :: <integer> = 49;
define constant $DW-TAG-try-block :: <integer> = 50;
define constant $DW-TAG-variant-part :: <integer> = 51;
define constant $DW-TAG-variable :: <integer> = 52;
define constant $DW-TAG-volatile-type :: <integer> = 53;
define constant $DW-TAG-dwarf-procedure :: <integer> = 54;
define constant $DW-TAG-restrict-type :: <integer> = 55;
define constant $DW-TAG-interface-type :: <integer> = 56;
define constant $DW-TAG-namespace :: <integer> = 57;
define constant $DW-TAG-imported-module :: <integer> = 58;
define constant $DW-TAG-unspecified-type :: <integer> = 59;
define constant $DW-TAG-partial-unit :: <integer> = 60;
define constant $DW-TAG-imported-unit :: <integer> = 61;
define constant $DW-TAG-condition :: <integer> = 63;
define constant $DW-TAG-shared-type :: <integer> = 64;
define constant $DW-TAG-lo-user :: <integer> = 16512;
define constant $DW-TAG-hi-user :: <integer> = 65535;

define constant $DW-ATE-address :: <integer> = 1;
define constant $DW-ATE-boolean :: <integer> = 2;
define constant $DW-ATE-complex-float :: <integer> = 3;
define constant $DW-ATE-float :: <integer> = 4;
define constant $DW-ATE-signed :: <integer> = 5;
define constant $DW-ATE-signed-char :: <integer> = 6;
define constant $DW-ATE-unsigned :: <integer> = 7;
define constant $DW-ATE-unsigned-char :: <integer> = 8;
define constant $DW-ATE-imaginary-float :: <integer> = 9;
define constant $DW-ATE-packed-decimal :: <integer> = 10;
define constant $DW-ATE-numeric-string :: <integer> = 11;
define constant $DW-ATE-edited :: <integer> = 12;
define constant $DW-ATE-signed-fixed :: <integer> = 13;
define constant $DW-ATE-unsigned-fixed :: <integer> = 14;
define constant $DW-ATE-decimal-float :: <integer> = 15;
define constant $DW-ATE-lo-user :: <integer> = 128;
define constant $DW-ATE-hi-user :: <integer> = 255;

define constant $DW-LANG-C89 :: <integer> = 1;
define constant $DW-LANG-C :: <integer> = 2;
define constant $DW-LANG-Ada83 :: <integer> = 3;
define constant $DW-LANG-C-plus-plus :: <integer> = 4;
define constant $DW-LANG-Cobol74 :: <integer> = 5;
define constant $DW-LANG-Cobol85 :: <integer> = 6;
define constant $DW-LANG-Fortran77 :: <integer> = 7;
define constant $DW-LANG-Fortran90 :: <integer> = 8;
define constant $DW-LANG-Pascal83 :: <integer> = 9;
define constant $DW-LANG-Modula2 :: <integer> = 10;
define constant $DW-LANG-Java :: <integer> = 11;
define constant $DW-LANG-C99 :: <integer> = 12;
define constant $DW-LANG-Ada95 :: <integer> = 13;
define constant $DW-LANG-Fortran95 :: <integer> = 14;
define constant $DW-LANG-PLI :: <integer> = 15;
define constant $DW-LANG-ObjC :: <integer> = 16;
define constant $DW-LANG-ObjC-plus-plus :: <integer> = 17;
define constant $DW-LANG-UPC :: <integer> = 18;
define constant $DW-LANG-D :: <integer> = 19;
define constant $DW-LANG-Python :: <integer> = 20;
define constant $DW-LANG-OpenCL :: <integer> = 21;
define constant $DW-LANG-Go :: <integer> = 22;
define constant $DW-LANG-Modula3 :: <integer> = 23;
define constant $DW-LANG-Haskell :: <integer> = 24;
define constant $DW-LANG-C-plus-plus-03 :: <integer> = 25;
define constant $DW-LANG-C-plus-plus-11 :: <integer> = 26;
define constant $DW-LANG-OCaml :: <integer> = 27;
define constant $DW-LANG-Rust :: <integer> = 28;
define constant $DW-LANG-C11 :: <integer> = 29;
define constant $DW-LANG-Swift :: <integer> = 30;
define constant $DW-LANG-Julia :: <integer> = 31;
define constant $DW-LANG-Dylan :: <integer> = 32;
define constant $DW-LANG-C-plus-plus-14 :: <integer> = 33;
define constant $DW-LANG-lo-user :: <integer> = 32768;
define constant $DW-LANG-Mips-Assembler = 32769;
define constant $DW-LANG-hi-user :: <integer> = 65535;

// From llvm/DebugInfo.h
define constant $flag-artificial :: <integer> = ash(1, 6);


/// Debug information constructor functions

define inline function i32
    (value :: <abstract-integer>) => (llvm-value :: <llvm-integer-constant>)
  make(<llvm-integer-constant>, type: $llvm-i32-type, integer: value)
end function;

define function llvm-make-canonical-metadata-string
    (str :: false-or(<string>))
 => (metadata :: false-or(<llvm-metadata-string>));
  str
    & ~empty?(str)
    & make(<llvm-metadata-string>, string: str)
end function;

define function llvm-make-dbg-compile-unit
    (lang :: <integer>,
     file :: <llvm-DIFile-metadata>,
     producer :: <string>,
     #key optimized? :: <boolean> = #f,
          flags :: <string> = "",
          runtime-version :: <integer> = 0,
          enum-types :: <sequence> = #[],
          retained-types :: <sequence> = #[],
          imported-entities :: <sequence> = #[],
          module :: false-or(<llvm-module>) = #f,
          kind :: <llvm-debug-emission-kind> = #"full-debug")
 => (compile-unit :: <llvm-metadata>);
  let enums-node
    = make(<llvm-metadata-node>, node-values: enum-types);
  let retained-types-node
    = make(<llvm-metadata-node>, node-values: retained-types);
  let imported-entities-node
    = make(<llvm-metadata-node>, node-values: imported-entities);
  let compile-unit
    = make(<llvm-DICompileUnit-metadata>,
           distinct?: #t,
           language: lang,
           file: file,
           producer: llvm-make-canonical-metadata-string(producer),
           isOptimized: optimized?,
           runtimeVersion: runtime-version,
           emissionKind: kind,
           enums: enums-node,
           retainedTypes: retained-types-node);
  if (module)
    add-to-named-metadata(module, "llvm.dbg.cu", compile-unit);

    llvm-module-add-flag(module, #"warning", "Debug Info Version",
                         i32($llvm-debug-metadata-version));

    llvm-module-add-flag(module, #"warning", "Dwarf Version", i32(4));
  end if;
  compile-unit
end function;

define function llvm-make-dbg-file
    (file :: <pathname>,
     directory :: <pathname>)
 => (dbg-file :: <llvm-metadata>)
  make(<llvm-DIFile-metadata>,
       filename:
         llvm-make-canonical-metadata-string(as(<string>, file)),
       directory:
         llvm-make-canonical-metadata-string(as(<string>, directory)))
end function;

define function llvm-make-dbg-function-type
    (dbg-file :: <llvm-metadata>,
     return-type :: false-or(<llvm-metadata>),
     parameter-types :: <sequence>)
 => (dbg-function-type :: <llvm-metadata>);
  let params
    = add(as(<list>, parameter-types), return-type);
  let types
    = make(<llvm-metadata-node>, node-values: params);
  make(<llvm-DISubroutineType-metadata>,
       types: types)
end function;

define function llvm-make-dbg-function
    (scope :: false-or(<llvm-metadata>),
     name :: <string>,
     linkage-name :: <string>,
     compile-unit :: <llvm-metadata>,
     dbg-file :: <llvm-metadata>,
     line-number :: false-or(<integer>),
     dbg-function-type :: <llvm-metadata>,
     #key local? :: <boolean> = #f,
          definition? :: <boolean> = #f,
          optimized? :: <boolean> = #f,
          decl :: false-or(<llvm-metadata>) = #f,
          scope-line-number :: false-or(<integer>) = line-number,
          module :: false-or(<llvm-module>) = #f,
          function :: false-or(<llvm-function>) = #f)
  => (dbg-function :: <llvm-metadata>);
  let dbg-function
    = make(<llvm-DISubprogram-metadata>,
           distinct?: #t,
           scope: scope,
           name: llvm-make-canonical-metadata-string(name),
           linkageName: llvm-make-canonical-metadata-string(linkage-name),
           file: dbg-file,
           line: line-number,
           type: dbg-function-type,
           isLocal: local?,
           isDefinition: definition?,
           scopeLine: scope-line-number,
           isOptimized: optimized?,
           unit: compile-unit);
  if (module & function)
    let attachments
      = element(module.llvm-global-metadata-attachment-table, function,
                default: #());
    module.llvm-global-metadata-attachment-table[function]
      := add(attachments,
             make(<llvm-metadata-attachment>, kind: $llvm-metadata-kind-dbg,
                  metadata: dbg-function));
  end if;
  dbg-function
end function;

define function add-to-named-metadata
    (module :: <llvm-module>,
     name :: <string>,
     metadata-value :: <llvm-metadata>)
 => ();
  let named
    = element(module.%named-metadata-table, name, default: #f)
    | begin
        let md
          = make(<llvm-named-metadata>,
                 name: name, operands: make(<stretchy-object-vector>));
        add!(module.llvm-module-named-metadata, md);
        module.%named-metadata-table[name] := md
      end;
  add!(named.llvm-named-metadata-operands, metadata-value);
end function;

define function llvm-make-dbg-lexical-block
    (scope :: <llvm-metadata>,
     dbg-file :: <llvm-metadata>,
     line-number :: false-or(<integer>),
     column-number :: <integer>)
 => (dbg-lexical-block :: <llvm-metadata>);
  make(<llvm-DILexicalBlock-metadata>,
       distinct?: #t,
       scope: scope,
       file: dbg-file,
       line: line-number,
       column: column-number)
end function;

define function llvm-make-dbg-local-variable
    (kind :: one-of(#"auto", #"argument", #"return"),
     scope :: false-or(<llvm-metadata>), name :: <string>,
     dbg-file :: <llvm-metadata>, line-number :: false-or(<integer>),
     type :: <llvm-metadata>,
     #key arg :: <integer> = 0,
          artificial? = #f)
 => (dbg-local-variable :: <llvm-metadata>);
  make(<llvm-DILocalVariable-metadata>,
       scope: scope,
       name: llvm-make-canonical-metadata-string(name),
       arg: arg,
       file: dbg-file,
       line: line-number,
       type: type,
       flags: if (artificial?) $flag-artificial else 0 end)
end function;

define function llvm-make-dbg-basic-type
    (kind :: one-of(#"address", #"boolean", #"float",
                    #"signed", #"signed-char", #"unsigned", #"unsigned-char"),
     scope :: false-or(<llvm-metadata>), name :: <string>,
     type-size :: <integer>, type-alignment :: <integer>,
     type-offset :: <integer>)
 => (dbg-basic-type :: <llvm-metadata>);
  let type-encoding
    = select (kind)
        #"address"       => $DW-ATE-address;
        #"boolean"       => $DW-ATE-boolean;
        #"float"         => $DW-ATE-float;
        #"signed"        => $DW-ATE-signed;
        #"signed-char"   => $DW-ATE-signed-char;
        #"unsigned"      => $DW-ATE-unsigned;
        #"unsigned-char" => $DW-ATE-unsigned-char;
      end select;
  make(<llvm-DIBasicType-metadata>,
       name: llvm-make-canonical-metadata-string(name),
       size: type-size,
       encoding: type-encoding)
end function;

define function llvm-make-dbg-derived-type
    (kind :: one-of(#"parameter", #"member", #"pointer", #"reference",
                    #"typedef", #"const", #"volatile", #"restrict"),
     scope :: false-or(<llvm-metadata>), name :: <string>,
     dbg-file :: false-or(<llvm-metadata>),
     line-number :: false-or(<integer>),
     type-size :: <integer>, type-alignment :: <integer>,
     type-offset :: <integer>,
     derived-from :: false-or(<llvm-metadata>))
 => (dbg-derived-type :: <llvm-metadata>);
  let tag
    = select (kind)
        #"parameter" => $DW-TAG-formal-parameter;
        #"member"    => $DW-TAG-member;
        #"pointer"   => $DW-TAG-pointer-type;
        #"reference" => $DW-TAG-reference-type;
        #"typedef"   => $DW-TAG-typedef;
        #"const"     => $DW-TAG-const-type;
        #"volatile"  => $DW-TAG-volatile-type;
        #"restrict"  => $DW-TAG-restrict-type;
      end select;
  make(<llvm-DIDerivedType-metadata>,
       tag: tag,
       name: llvm-make-canonical-metadata-string(name),
       line: line-number | 0,
       scope: scope,
       baseType: derived-from,
       size: type-size,
       align: type-alignment,
       offset: type-offset,
       flags: 0,
       extra-data: #f)
end function;

define function llvm-make-dbg-composite-type
    (kind :: one-of(#"array", #"enum", #"struct", #"union", #"vector",
                    #"function", #"inheritance"),
     scope :: false-or(<llvm-metadata>), name :: <string>,
     dbg-file :: false-or(<llvm-metadata>),
     line-number :: false-or(<integer>),
     type-size :: <integer>, type-alignment :: <integer>,
     elements :: <sequence>,
     derived-from :: false-or(<llvm-metadata>))
 => (dbg-composite-type :: <llvm-metadata>);
  let tag
    = select (kind)
        #"array"       => $DW-TAG-array-type;
        #"enum"        => $DW-TAG-enumeration-type;
        #"struct"      => $DW-TAG-structure-type;
        #"union"       => $DW-TAG-union-type;
        #"vector"      => $DW-TAG-vector-type;
        #"function"    => $DW-TAG-subroutine-type;
        #"inheritance" => $DW-TAG-inheritance;
      end select;
  make(<llvm-DICompositeType-metadata>,
       tag: tag,
       name: llvm-make-canonical-metadata-string(name),
       file: dbg-file,
       line: line-number | 0,
       scope: scope,
       base-type: #f,
       size: type-size,
       align: type-alignment,
       elements: make(<llvm-metadata-node>, node-values: elements))
end function;

define function llvm-make-dbg-value-metadata
    (value :: <llvm-value>)
 => (dbg-local-value :: <llvm-value>);
  make(<llvm-metadata-value>,
       metadata: make(<llvm-value-metadata>, value: value))
end function;
