Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <llvm-attributes> = <machine-word>;

define constant $llvm-attribute-none            = as(<machine-word>, 0);
define constant $llvm-attribute-zext            = %shift-left(1, 0);
define constant $llvm-attribute-sext            = %shift-left(1, 1);
define constant $llvm-attribute-noreturn        = %shift-left(1, 2);
define constant $llvm-attribute-inreg           = %shift-left(1, 3);
define constant $llvm-attribute-sret            = %shift-left(1, 4);
define constant $llvm-attribute-nounwind        = %shift-left(1, 5);
define constant $llvm-attribute-noalias         = %shift-left(1, 6);
define constant $llvm-attribute-byval           = %shift-left(1, 7);
define constant $llvm-attribute-nest            = %shift-left(1, 8);
define constant $llvm-attribute-readnone        = %shift-left(1, 9);
define constant $llvm-attribute-readonly        = %shift-left(1, 10);
define constant $llvm-attribute-noinline        = %shift-left(1, 11);
define constant $llvm-attribute-alwaysinline    = %shift-left(1, 12);
define constant $llvm-attribute-optsize         = %shift-left(1, 13);
define constant $llvm-attribute-ssp             = %shift-left(1, 14);
define constant $llvm-attribute-sspreq          = %shift-left(1, 15);

define function llvm-attribute-alignment
    (alignment :: <integer>) => (attribute :: <llvm-attributes>)
  %shift-left(integer-length(alignment), 16)
end function;

define constant $llvm-attribute-nocapture       = %shift-left(1, 21);
define constant $llvm-attribute-noredzone       = %shift-left(1, 22);
define constant $llvm-attribute-noimplicitfloat = %shift-left(1, 23);
define constant $llvm-attribute-naked           = %shift-left(1, 24);
define constant $llvm-attribute-inlinehint      = %shift-left(1, 25);

define function llvm-attribute-stack-alignment
    (alignment :: <integer>) => (attribute :: <llvm-attributes>)
  %shift-left(integer-length(alignment), 26)
end function;

define constant llvm-attribute-merge = %logior;

define class <llvm-attribute-list> (<object>)
  constant slot llvm-attribute-list-return-attributes :: <llvm-attributes>,
    init-value: $llvm-attribute-none, init-keyword: return-attributes:;
  constant slot llvm-attribute-list-function-attributes :: <llvm-attributes>,
    init-value: $llvm-attribute-none, init-keyword: function-attributes:;
  constant slot llvm-attribute-list-parameter-attributes :: <sequence>,
    init-value: #[], init-keyword: parameter-attributes:;
end class;

define constant $llvm-empty-attribute-list :: <llvm-attribute-list>
  = make(<llvm-attribute-list>);
