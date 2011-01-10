module:    coff-constants
Synopsis:  The constants used in the description of a COFF file
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// TEMPORARY: big-literal is a function for making potentially oversized 
// integers, at a time when the Dylan reader is unable to do this for itself.

define function big-literal 
    (hi :: <integer>, lo :: <integer>) => (res :: <abstract-integer>)
  generic-+(generic-ash(hi, 16), lo);
end function;



// Some constants corresponding to symbol values.

define constant $sym-undefined        =    0;
define constant $sym-absolute         =    -1;
define constant $sym-debug            =    -2;


// Some constants corresponding to symbol storage classes.

define constant $sym-end-of-function  =   -1;
define constant $sym-automatic        =    1;
define constant $sym-external         =    2;
define constant $sym-static           =    3;
define constant $sym-function         =  101;
define constant $sym-file             =  103;
define constant $sym-section          =  104;

define constant $sym-type-null        =    0;
define constant $sym-type-function    = #x20;


// Some constants corresponding to COFF section mapping options

define constant $type-no-pad            = #x00000008;
define constant $cnt-code               = #x00000020;
define constant $cnt-initialized-data   = #x00000040;
define constant $cnt-uninitialized-data = #x00000080;
define constant $lnk-other              = #x00000100;
define constant $lnk-info               = #x00000200;
define constant $lnk-remove             = #x00000800;
define constant $lnk-comdat             = #x00001000;
define constant $mem-fardata            = #x00008000;
define constant $mem-purgeable          = #x00020000;
define constant $mem-16bit              = #x00020000;
define constant $mem-locked             = #x00040000;
define constant $mem-preload            = #x00080000;
define constant $align-1bytes           = #x00100000;
define constant $align-2bytes           = #x00200000;
define constant $align-4bytes           = #x00300000;
define constant $align-8bytes           = #x00400000;
define constant $align-16bytes          = #x00500000;
define constant $align-32bytes          = #x00600000;
define constant $align-64bytes          = #x00700000;
define constant $lnk-nreloc-ovfl        = #x01000000;
define constant $mem-discardable        = #x02000000;
define constant $mem-not-cached         = #x04000000;
define constant $mem-not-paged          = #x08000000;
define constant $mem-shared             = #x10000000;
define constant $mem-execute            = big-literal(#x2000, #x0000);
define constant $mem-read               = big-literal(#x4000, #x0000);
define constant $mem-write              = big-literal(#x8000, #x0000);


define constant $code-flags 
  = generic-+($mem-execute, 
              generic-+($mem-read, $align-4bytes + $cnt-code));

define constant $data-flags 
  = generic-+($mem-read, 
              generic-+($mem-write, $align-4bytes + $cnt-initialized-data));

define constant $debug-flags 
  = generic-+($mem-read, $mem-discardable + $cnt-initialized-data);



