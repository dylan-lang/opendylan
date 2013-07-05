Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $llvm-debug-version = ash(12, 16);
//define constant $llvm-debug-version-mask = ash(#xFFFF, 16);

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
define constant $DW-LANG-lo-user :: <integer> = 32768;
define constant $DW-LANG-Dylan :: <integer> = #xD1D1;
define constant $DW-LANG-hi-user :: <integer> = 65535;


/// Debug information constructor functions

define inline function i32
    (value :: <abstract-integer>) => (llvm-value :: <llvm-integer-constant>)
  make(<llvm-integer-constant>, type: $llvm-i32-type, integer: value)
end function;

define inline function i64
    (value :: <abstract-integer>) => (llvm-value :: <llvm-integer-constant>)
  make(<llvm-integer-constant>, type: $llvm-i64-type, integer: value)
end function;

define function llvm-make-dbg-compile-unit
    (lang :: <integer>,
     file :: <pathname>,
     directory :: <pathname>,
     producer :: <string>,
     #key optimized? = #f,
          flags :: <string> = "",
          runtime-version :: <integer> = 0,
          enum-types :: <sequence> = #[],
          retained-types :: <sequence> = #[],
          functions :: <sequence> = #[],
          globals :: <sequence> = #[],
          imported-entities :: <sequence> = #[],
          module :: false-or(<llvm-module>) = #f,
	  split-debug-path :: false-or(<pathname>) = #f)
 => (compile-unit :: <llvm-metadata-value>);
  let enum-node
    = make(<llvm-metadata-node>, node-values: enum-types, function-local?: #f);
  let retained-node
    = make(<llvm-metadata-node>,
           node-values: retained-types, function-local?: #f);
  let functions-node
    = make(<llvm-metadata-node>, node-values: functions, function-local?: #f);
  let globals-node
    = make(<llvm-metadata-node>,
           node-values: globals, function-local?: #f);
  let imported-entities-node
    = make(<llvm-metadata-node>,
	   node-values: imported-entities, function-local?: #f);

  let compile-unit
    = make(<llvm-metadata-node>,
           function-local?: #f,
           node-values: vector(i32($llvm-debug-version + $DW-TAG-compile-unit),
			       llvm-make-dbg-file-directory(file, directory),
                               i32(lang),
                               make(<llvm-metadata-string>, string: as(<string>, producer)),
                               if (optimized?) $llvm-true else $llvm-false end,
                               make(<llvm-metadata-string>, string: flags),
                               i32(runtime-version),
                               enum-node,
                               retained-node,
                               functions-node,
                               globals-node,
			       imported-entities-node,
			       make(<llvm-metadata-string>,
                                    string: as(<string>,
					       split-debug-path | ""))));
  if (module)
    add-to-named-metadata(module, "llvm.dbg.cu", compile-unit);
  end if;
  compile-unit
end function;

define function llvm-make-dbg-file-directory
    (file :: <pathname>,
     directory :: <pathname>)
 => (file-directory-pair :: <llvm-metadata-value>)
  make(<llvm-metadata-node>, function-local?: #f,
       node-values: vector(make(<llvm-metadata-string>,
				string: as(<string>, file)),
			   make(<llvm-metadata-string>,
				string: as(<string>, directory))))
end function;

define function llvm-make-dbg-file
    (file :: <pathname>,
     directory :: <pathname>)
 => (dbg-file :: <llvm-metadata-value>);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-file-type),
			   llvm-make-dbg-file-directory(file, directory)))
end function;

define method as-file-directory
    (value :: false-or(<llvm-metadata-value>))
 => (file-directory-pair :: false-or(<llvm-metadata-value>));
  value
end method;

define method as-file-directory
    (value :: <llvm-metadata-node>)
 => (file-directory-pair :: false-or(<llvm-metadata-value>));
  let node-values = value.llvm-metadata-node-values;
  if (instance?(node-values[0], <llvm-integer-constant>)
	& node-values[0].llvm-integer-constant-integer = $llvm-debug-version + $DW-TAG-file-type)
    node-values[1]
  else
    value
  end if
end method;

define function llvm-make-dbg-function-type
    (dbg-file :: <llvm-metadata-value>,
     return-type :: false-or(<llvm-metadata-value>),
     parameter-types :: <sequence>)
 => (dbg-function-type :: <llvm-metadata-value>);
  let i32-zero = i32(0);
  let i64-zero = i64(0);
  let params
    = add(as(<list>, parameter-types), return-type);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-subroutine-type),
			   i32-zero,
			   i32-zero,
                           make(<llvm-metadata-string>, string: ""),
                           i32-zero,
                           i64-zero,
                           i64-zero,
                           i64-zero,
                           i32-zero,
                           #f,
                           make(<llvm-metadata-node>,
                                function-local?: #f,
                                node-values: params),
                           i32-zero,
                           i32-zero))
end function;

define function llvm-make-dbg-function
    (context :: false-or(<llvm-metadata-value>),
     name :: <string>,
     linkage-name :: <string>,
     dbg-file :: <llvm-metadata-value>,
     line-number,
     dbg-function-type :: <llvm-metadata-value>,
     #key local? = #f,
          definition? = #f,
          optimized? = #f,
          function :: false-or(<llvm-function>) = #f,
          decl :: false-or(<llvm-metadata-value>) = #f,
          module :: false-or(<llvm-module>) = #f,
          scope-line-number = line-number)
 => (dbg-function :: <llvm-metadata-value>);
  let i32-zero = i32(0);
  let dbg-name = make(<llvm-metadata-string>, string: name);
  let node
    = make(<llvm-metadata-node>,
           function-local?: #f,
           node-values: vector(i32($llvm-debug-version + $DW-TAG-subprogram),
			       as-file-directory(dbg-file),
                               context,
                               dbg-name,
                               dbg-name,
                               make(<llvm-metadata-string>,
				    string: linkage-name),
                               i32(line-number),
                               dbg-function-type,
                               if (local?) $llvm-true else $llvm-false end,
                               if (definition?) $llvm-true else $llvm-false end,
                               i32-zero,
                               i32-zero,
                               #f,
                               i32-zero, // flags
                               if (optimized?) $llvm-true else $llvm-false end,
                               function,
                               #f, // template parameters
                               decl,
			       make(<llvm-metadata-node>, function-local: #f,
				    node-values: vector(i32-zero)),
			       i32(scope-line-number)));
  if (module)
    add-to-named-metadata(module, "llvm.dbg.sp", node);
  end if;
  node
end function;

define function add-to-named-metadata
    (module :: <llvm-module>,
     name :: <string>,
     metadata-value :: <llvm-metadata-value>)
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

define variable *lexical-block-unique-id* = 0;

define function llvm-make-dbg-lexical-block
    (scope :: false-or(<llvm-metadata-value>),
     dbg-file :: <llvm-metadata-value>,
     line-number,
     column-number)
 => (dbg-lexical-block :: <llvm-metadata-value>);
  *lexical-block-unique-id* := *lexical-block-unique-id* + 1;
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-lexical-block),
			   as-file-directory(dbg-file),
                           scope,
                           i32(line-number),
                           i32(column-number),
                           i32(*lexical-block-unique-id*)))
end function;

define function llvm-make-dbg-local-variable
    (kind :: one-of(#"auto", #"argument", #"return"),
     scope :: false-or(<llvm-metadata-value>), name :: <string>,
     dbg-file :: <llvm-metadata-value>, line-number,
     type :: <llvm-metadata-value>,
     #key arg :: <integer> = 0,
          module :: false-or(<llvm-module>) = #f,
          function-name :: <string> = "fn")
 => (dbg-local-variable :: <llvm-metadata-value>);
  let tag
    = select (kind)
        #"auto"     => $DW-TAG-auto-variable;
        #"argument" => $DW-TAG-arg-variable;
        #"return"   => $DW-TAG-return-variable;
      end select;
  let node
    = make(<llvm-metadata-node>,
           function-local?: #f,
           node-values: vector(i32($llvm-debug-version + tag),
                               scope,
                               make(<llvm-metadata-string>, string: name),
                               dbg-file,
                               i32(logior(line-number, ash(arg, 24))),
                               type,
                               i32(0),
                               i32(0)));
  if (module)
    add-to-named-metadata(module,
                          concatenate("llvm.dbg.lv.", function-name),
                          node);
  end if;
  node
end function;

define function llvm-make-dbg-basic-type
    (kind :: one-of(#"address", #"boolean", #"float",
                    #"signed", #"signed-char", #"unsigned", #"unsigned-char"),
     scope :: false-or(<llvm-metadata-value>), name :: <string>,
     type-size :: <integer>, type-alignment :: <integer>,
     type-offset :: <integer>)
 => (dbg-basic-type :: <llvm-metadata-value>);
  let type-encoding
    = select (kind)
        #"address"       => $DW-ATE-address;
        #"boolean"       => $DW-ATE-boolean;
        #"float"         => $DW-ATE-float;
        #"signed"        => $DW-ATE-signed;
        #"signed-char"   => $DW-ATE-signed-char;
        #"unsigned"      => $DW-ATE-signed;
        #"unsigned-char" => $DW-ATE-unsigned-char;
      end select;
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-base-type),
                           #f,
                           scope,
                           make(<llvm-metadata-string>, string: name),
                           i32(0),
                           i64(type-size),
                           i64(type-alignment),
                           i64(type-offset),
                           i32(0), // flags
                           i32(type-encoding)))
end function;

define function llvm-make-dbg-derived-type
    (kind :: one-of(#"parameter", #"member", #"pointer", #"reference",
                    #"typedef", #"const", #"volatile", #"restrict"),
     scope :: false-or(<llvm-metadata-value>), name :: <string>,
     dbg-file :: false-or(<llvm-metadata-value>), line-number,
     type-size :: <integer>, type-alignment :: <integer>,
     type-offset :: <integer>,
     derived-from :: false-or(<llvm-metadata-value>))
 => (dbg-derived-type :: <llvm-metadata-value>);
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
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + tag),
                           as-file-directory(dbg-file),
                           scope,
                           make(<llvm-metadata-string>, string: name),
                           i32(line-number | 0),
                           i64(type-size),
                           i64(type-alignment),
                           i64(type-offset),
                           i32(0), // flags
                           derived-from))
end function;

define function llvm-make-dbg-composite-type
    (kind :: one-of(#"array", #"enum", #"struct", #"union", #"vector",
                    #"function", #"inheritance"),
     scope :: false-or(<llvm-metadata-value>), name :: <string>,
     dbg-file :: false-or(<llvm-metadata-value>), line-number,
     type-size :: <integer>, type-alignment :: <integer>,
     elements :: <sequence>,
     derived-from :: false-or(<llvm-metadata-value>))
 => (dbg-composite-type :: <llvm-metadata-value>);
  let i32-zero = i32(0);
  let i64-zero = i64(0);
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

  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + tag),
                           as-file-directory(dbg-file),
                           scope,
                           make(<llvm-metadata-string>, string: name),
                           i32(line-number | 0),
                           i64(type-size),
                           i64(type-alignment),
                           i32-zero,
                           i32-zero, // flags
                           #f, // derived-from
                           make(<llvm-metadata-node>,
                                function-local?: #f,
                                node-values: elements),
                           i32-zero, // runtime version
                           #f))
end function;
