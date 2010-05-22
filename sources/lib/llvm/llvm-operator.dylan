Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <llvm-binary-operator>
  = one-of(#"ADD", #"FADD", #"SUB", #"FSUB", #"MUL", #"FMUL",
           #"UDIV", #"SDIV", #"FDIV", #"UREM", #"SREM", #"FREM",
           #"SHL", #"LSHR", #"ASHR", #"AND", #"OR", #"XOR");

define abstract class <llvm-binary-operator-flags-mixin> (<object>)
  constant slot llvm-binop-no-unsigned-wrap? :: <boolean>,
    init-value: #f, init-keyword: no-unsigned-wrap?:;
  constant slot llvm-binop-no-signed-wrap? :: <boolean>,
    init-value: #f, init-keyword: no-signed-wrap?:;
  constant slot llvm-binop-exact? :: <boolean>,
    init-value: #f, init-keyword: exact?:;
end class;  

define constant <llvm-cast-operator>
  = one-of(#"TRUNC", #"ZEXT", #"SEXT", #"FPTOUI", #"FPTOSI",
           #"UITOFP", #"SITOFP", #"FPTRUNC", #"FPEXT",
           #"PTRTOINT", #"INTTOPTR", #"BITCAST");

define constant <llvm-predicate>
  = one-of(#"EQ", #"NE",
           #"SLT", #"SGT", #"SLE", #"SGE",
           #"ULT", #"UGT", #"ULE", #"UGE",
           #"OEQ", #"ONE", #"OLT", #"OGT",
           #"OLE", #"OGE", #"ORD", #"UNO",
           #"UEQ", #"UNE",
           #"ULT", #"UGT", #"ULE", #"UGE",
           #"TRUE", #"FALSE");

define abstract class <llvm-cmp-mixin> (<object>)
  constant slot llvm-cmp-predicate :: <llvm-predicate>,
    required-init-keyword: predicate:;
end class;

define class <llvm-icmp-mixin> (<llvm-cmp-mixin>)
end class;

define class <llvm-fcmp-mixin> (<llvm-cmp-mixin>)
end class;
