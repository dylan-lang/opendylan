Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2011 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $llvm-debug-version = ash(8, 16);
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

define function llvm-make-dbg-compile-unit
    (lang :: <integer>,
     file :: <pathname>,
     directory :: <pathname>,
     producer :: <string>,
     #key optimized? = #f,
          flags :: <string> = "",
          runtime-version :: <integer> = 0)
 => (compile-unit :: <llvm-metadata-value>);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-compile-unit),
                           i32(0),
                           i32(lang),
                           make(<llvm-metadata-string>,
                                string: as(<string>, file)),
                           make(<llvm-metadata-string>,
                                string: as(<string>, directory)),
                           make(<llvm-metadata-string>,
                                string: as(<string>, producer)),
                           $llvm-true,
                           if (optimized?) $llvm-true else $llvm-false end,
                           make(<llvm-metadata-string>, string: flags),
                           i32(runtime-version)))
end function;

define function llvm-make-dbg-file
    (compile-unit :: <llvm-metadata-value>,
     file :: <pathname>,
     directory :: <pathname>)
 => (dbg-file :: <llvm-metadata-value>);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-file-type),
                           make(<llvm-metadata-string>,
                                string: as(<string>, file)),
                           make(<llvm-metadata-string>,
                                string: as(<string>, directory)),
                           compile-unit))
end function;

define function llvm-make-dbg-function-type
    (dbg-file :: <llvm-metadata-value>,
     return-type :: false-or(<llvm-metadata-value>),
     parameter-types :: <sequence>)
 => (dbg-function-type :: <llvm-metadata-value>);
  let i32-zero
    = make(<llvm-integer-constant>, type: $llvm-i32-type, integer: 0);
  let i64-zero
    = make(<llvm-integer-constant>, type: $llvm-i64-type, integer: 0);
  let params
    = add(as(<list>, parameter-types), return-type);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-subroutine-type),
                           dbg-file,
                           make(<llvm-metadata-string>, string: ""),
                           dbg-file,
                           i32-zero,
                           i64-zero,
                           i64-zero,
                           i32-zero,
                           i32-zero,
                           #f,
                           make(<llvm-metadata-node>,
                                function-local?: #f,
                                node-values: params),
                           i32-zero,
                           #f))
end function;

define function llvm-make-dbg-function
    (context :: <llvm-metadata-value>,
     name :: <string>,
     linkage-name :: <string>,
     dbg-file :: <llvm-metadata-value>,
     line-number,
     dbg-function-type :: <llvm-metadata-value>,
     #key local? = #f,
          definition? = #f,
          optimized? = #f,
          function :: false-or(<llvm-function>) = #f)
 => (dbg-function :: <llvm-metadata-value>);
  let i32-zero
    = make(<llvm-integer-constant>, type: $llvm-i32-type, integer: 0);
  let dbg-name = make(<llvm-metadata-string>, string: name);
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-subprogram),
                           i32-zero,
                           context,
                           dbg-name,
                           dbg-name,
                           make(<llvm-metadata-string>, string: linkage-name),
                           dbg-file,
                           i32(line-number),
                           dbg-function-type,
                           if (local?) $llvm-true else $llvm-false end,
                           if (definition?) $llvm-true else $llvm-false end,
                           i32-zero,
                           i32-zero,
                           #f,
                           $llvm-false,
                           if (optimized?) $llvm-true else $llvm-false end,
                           function))
end function;

define variable *lexical-block-unique-id* = 0;

define function llvm-make-dbg-lexical-block
    (scope :: <llvm-metadata-value>,
     dbg-file :: <llvm-metadata-value>,
     line-number,
     column-number)
 => (dbg-lexical-block :: <llvm-metadata-value>);
  *lexical-block-unique-id* := *lexical-block-unique-id* + 1;
  make(<llvm-metadata-node>,
       function-local?: #f,
       node-values: vector(i32($llvm-debug-version + $DW-TAG-lexical-block),
                           scope,
                           i32(line-number),
                           i32(column-number),
                           dbg-file,
                           i32(*lexical-block-unique-id*)))
end function;
