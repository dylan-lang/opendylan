Module: java-vm-code-generation
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


format-out ("initing java-opcodes.dylan\n") ;

// model pushes and pops - basically may be type constraints, and want to
// know if a local is involved
define abstract sealed class <push-pop-model> (<object>)
end;

// when a type is known (argument or result of call, arith)
define abstract class <push-pop-model-typed> (<push-pop-model>)
  sealed constant slot type-constraint :: <java-type>, required-init-keyword: type-constraint: ;
end;

define class <push-pop-model-tiped> (<push-pop-model-typed>)
end;

define method print-object (mod :: <push-pop-model-typed>, s :: <stream>) => ()
  format (s, "[PPM %s]", mod.type-constraint)
end;

// for push/pop local variables (always have a type constraint!)
define abstract class <push-pop-model-local> (<push-pop-model-typed>)
end;

// these have their own local number built in
define class <push-pop-model-local-num> (<push-pop-model-local>)
  sealed constant slot local-var-num :: <integer>, required-init-keyword: local-var-num: ;
end;

// these get it from the operand
define class <push-pop-model-local-ind> (<push-pop-model-local>)
end;

define method print-object (mod :: <push-pop-model-local-num>, s :: <stream>) => ()
  format (s, "[PPM loc%s :: %s]", mod.local-var-num, mod.type-constraint)
end;

define method print-object (mod :: <push-pop-model-local-ind>, s :: <stream>) => ()
  format (s, "[PPM loc? :: %s]", mod.type-constraint)
end;



// instances of this type used for ops like dup, where the operation only relates
// stack entries, not define their type
define class <push-pop-model-metavar> (<push-pop-model>)
  slot  type-variable = #f ;
end;
define method print-object (mod :: <push-pop-model-metavar>, s :: <stream>) => ()
  format (s, "[PPMvar :: %s]", mod.type-variable)
end;

define class <push-pop-model-metavar2> (<push-pop-model-metavar>)
end;
define method print-object (mod :: <push-pop-model-metavar2>, s :: <stream>) => ()
  format (s, "[PPMvar2 :: %s]", mod.type-variable)
end;

define constant $pop-var1$ = make (<push-pop-model-metavar>) ;
define constant $pop-var2$ = make (<push-pop-model-metavar>) ;
define constant $pop-var3$ = make (<push-pop-model-metavar>) ;
define constant $pop2-var1$ = make (<push-pop-model-metavar2>) ;
//define constant $pop2-var2$ = make (<push-pop-model-metavar2>) ;

define class <push-pop-model-metavar-checked> (<push-pop-model-metavar>)
end;

define constant $pop-check$ = make (<push-pop-model-metavar-checked>) ;

// return address - like an extra type
define class <push-pop-model-address> (<push-pop-model>)
end;
define method print-object (mod :: <push-pop-model-address>, s :: <stream>) => ()
  format (s, "[PPMaddr]")
end;

// class representing when the opcode refers to a some value or type in the constant pool
// have to pick up constant pool entry from context?
define class <push-pop-model-constant> (<push-pop-model>)
end;
define method print-object (mod :: <push-pop-model-constant>, s :: <stream>) => ()
  format (s, "[PPMconst]")
end;

define class <push-pop-model-instance> (<push-pop-model-constant>)
  sealed constant slot in-new? :: <boolean> = #f, init-keyword: in-new?: ;
end;
define class <push-pop-model-field> (<push-pop-model-constant>)
end;

define class <push-pop-model-array> (<push-pop-model-instance>)
  sealed constant slot  prim? :: <boolean> = #f, init-keyword: prim?: ;
end;



// bytecode abstractions

define sealed abstract class <java-abstract-bytecode> (<object>)
  sealed constant slot  opname :: <byte-string>, required-init-keyword: opname: ;
  sealed constant slot  opcode :: <integer>,     required-init-keyword: opcode: ;
end;

define method print-object (bytecode :: <java-abstract-bytecode>, stream :: <stream>) => ()
  format (stream, "<<%s>>", bytecode.opname)
end;

define class <java-bytecode> (<java-abstract-bytecode>)
  sealed constant slot  push-count :: <integer>, required-init-keyword: push-count: ;
  sealed constant slot  pop-list  :: <list> = #(),        init-keyword: pop-list: ;
  sealed constant slot  push-list :: <list> = #(),        init-keyword: push-list: ;
end;

// for calls the pushes count depends on the method spec
define class <java-call-bytecode> (<java-abstract-bytecode>)
  sealed constant slot  pops-instance? :: <boolean>, required-init-keyword: pops-instance?: ;
end;

define function pp-typed (tipe :: <java-type>) => (pp :: <push-pop-model-tiped>)
  make (<push-pop-model-tiped>, type-constraint: tipe)
end;

define constant $push-pop-int$    = pp-typed ($java-int-type$) ;
define constant $push-pop-long$   = pp-typed ($java-long-type$) ;
define constant $push-pop-float$  = pp-typed ($java-float-type$) ;
define constant $push-pop-double$ = pp-typed ($java-double-type$) ;
define constant $push-pop-byte$   = pp-typed ($java-byte-type$) ;
define constant $push-pop-char$   = pp-typed ($java-char-type$) ;
define constant $push-pop-short$  = pp-typed ($java-short-type$) ;
define constant $push-pop-obj$    = pp-typed ($java/lang/Object$) ;
define constant $push-pop-null$   = pp-typed ($java-null-type$) ;

// need this early for the push-pop-throwable
define constant $java/lang/Throwable$ = java-lang-class ("Throwable", $java/lang/Object$) ;

define constant $push-pop-throwable$ = pp-typed ($java/lang/Throwable$) ;
define constant $push-pop-array$    =  pp-typed ($java-array-type$) ;
define constant $one-object$  = list ($push-pop-obj$) ;
define constant $one-null$    = list ($push-pop-null$) ;
define constant $one-throwable$ = list ($push-pop-throwable$) ;
define constant $one-array$   = list ($push-pop-array$) ;
define constant $one-byte$    = list ($push-pop-byte$) ;
define constant $one-char$    = list ($push-pop-char$) ;
define constant $one-short$    = list ($push-pop-short$) ;
define constant $one-int$    = list ($push-pop-int$) ;
define constant $one-long$    = list ($push-pop-long$) ;
define constant $one-float$    = list ($push-pop-float$) ;
define constant $one-double$    = list ($push-pop-double$) ;
define constant $two-ints$   = list ($push-pop-int$, $push-pop-int$) ;
define constant $two-longs$   = list ($push-pop-long$, $push-pop-long$) ;
define constant $two-floats$   = list ($push-pop-float$, $push-pop-float$) ;
define constant $two-doubles$   = list ($push-pop-double$, $push-pop-double$) ;
define constant $two-objects$   = list ($push-pop-obj$, $push-pop-obj$) ;

define constant $java-bytecodes = make (<simple-vector>, size: #x100, fill: #f) ;

define function make-jop (name :: <byte-string>,
			  code :: <integer>,
			  push-count :: <integer>,
			  #key push-list = #(),
			       pop-list = #()) => (op :: <java-bytecode>)
  let  bytecode :: <java-bytecode> =
    make (<java-bytecode>,
	  opname: name,
	  opcode: code,
	  push-count: push-count,
	  push-list: push-list,
	  pop-list: pop-list) ;
  $java-bytecodes [code] := bytecode ;
end;

define constant  j-nop         = make-jop ("nop",         0, 0) ;
define constant  j-aconst-null = make-jop ("aconst-null", 1, 1, push-list: $one-null$) ;
define constant  j-iconst-m1   = make-jop ("iconst-m1",   2, 1, push-list: $one-int$) ;
define constant  j-iconst-0    = make-jop ("iconst-0",  3, 1, push-list: $one-int$) ;
define constant  j-iconst-1    = make-jop ("iconst-1",  4, 1, push-list: $one-int$) ;
define constant  j-iconst-2    = make-jop ("iconst-2",  5, 1, push-list: $one-int$) ;
define constant  j-iconst-3    = make-jop ("iconst-3",  6, 1, push-list: $one-int$) ;
define constant  j-iconst-4    = make-jop ("iconst-4",  7, 1, push-list: $one-int$) ;
define constant  j-iconst-5    = make-jop ("iconst-5",  8, 1, push-list: $one-int$) ;
define constant  j-lconst-0    = make-jop ("lconst-0",  9, 2, push-list: $one-long$) ;
define constant  j-lconst-1    = make-jop ("lconst-1",  10, 2, push-list: $one-long$) ;
define constant  j-fconst-0    = make-jop ("fconst-0",  11, 1, push-list: $one-float$) ;
define constant  j-fconst-1    = make-jop ("fconst-1",  12, 1, push-list: $one-float$) ;
define constant  j-fconst-2    = make-jop ("fconst-2",  13, 1, push-list: $one-float$) ;
define constant  j-dconst-0    = make-jop ("dconst-0",  14, 2, push-list: $one-double$) ;
define constant  j-dconst-1    = make-jop ("dconst-1",  15, 2, push-list: $one-double$) ;
define constant  j-bipush      = make-jop ("bipush",    16, 1, push-list: $one-int$) ;
define constant  j-sipush      = make-jop ("sipush",    17, 1, push-list: $one-int$) ;


define constant $one-constant$ :: <list> = list (make (<push-pop-model-constant>)) ;
define constant $one-retaddr$  :: <list> = list (make (<push-pop-model-address>)) ;
define constant $one-instance$ :: <list> = list (make (<push-pop-model-instance>)) ;
define constant $one-field$ :: <list> = list (make (<push-pop-model-field>)) ;
define constant $field-and-instance$ :: <list> = list (head ($one-field$), head ($one-instance$)) ;
define constant $one-new-instance$ :: <list> = list (make (<push-pop-model-instance>, in-new?: #t)) ;
define constant $one-new-array$ :: <list> = list (make (<push-pop-model-array>, prim?: #f)) ;
define constant $one-new-prim-array$ :: <list> = list (make (<push-pop-model-array>, prim?: #t)) ;

define constant  j-ldc1        = make-jop ("ldc1",      18, 1, push-list: $one-constant$) ;
define constant  j-ldc2        = make-jop ("ldc2",      19, 1, push-list: $one-constant$) ;
define constant  j-ldc2w       = make-jop ("ldc2w",     20, 2, push-list: $one-constant$) ;

define constant  j-iconsts = vector (j-iconst-m1, j-iconst-0, j-iconst-1, j-iconst-2, j-iconst-3, j-iconst-4, j-iconst-5) ;



define function pp-var (i :: <integer>, tipe :: <java-type>) => (ppvar :: <push-pop-model-local-num>)
  make (<push-pop-model-local-num>, local-var-num: i, type-constraint: tipe)
end;

define function pp-var-n (tipe :: <java-type>) => (ppvar :: <push-pop-model-local-ind>)
  make (<push-pop-model-local-ind>, type-constraint: tipe)
end;

define constant  j-iload   = make-jop ("iload",   21 + j-int-code, 1, push-list: $one-int$) ;
define constant  j-lload   = make-jop ("lload",   21 + j-long-code, 2, push-list: $one-long$) ;
define constant  j-fload   = make-jop ("fload",   21 + j-float-code, 1, push-list: $one-float$) ;
define constant  j-dload   = make-jop ("dload",   21 + j-double-code, 2, push-list: $one-double$) ;
define constant  j-aload   = make-jop ("aload",   21 + j-ref-code, 1, 
                                   push-list: list (pp-var-n ($java/lang/Object$))) ;
define constant  j-iload-0 = make-jop ("iload-0", 26, 1, push-list: list (pp-var (0, $java-int-type$))) ;
define constant  j-iload-1 = make-jop ("iload-1", 27, 1, push-list: list (pp-var (1, $java-int-type$))) ;
define constant  j-iload-2 = make-jop ("iload-2", 28, 1, push-list: list (pp-var (2, $java-int-type$))) ;
define constant  j-iload-3 = make-jop ("iload-3", 29, 1, push-list: list (pp-var (3, $java-int-type$))) ;
define constant  j-lload-0 = make-jop ("lload-0", 30, 2, push-list: list (pp-var (0, $java-long-type$))) ;
define constant  j-lload-1 = make-jop ("lload-1", 31, 2, push-list: list (pp-var (1, $java-long-type$))) ;
define constant  j-lload-2 = make-jop ("lload-2", 32, 2, push-list: list (pp-var (2, $java-long-type$))) ;
define constant  j-lload-3 = make-jop ("lload-3", 33, 2, push-list: list (pp-var (3, $java-long-type$))) ;
define constant  j-fload-0 = make-jop ("fload-0", 34, 1, push-list: list (pp-var (0, $java-float-type$))) ;
define constant  j-fload-1 = make-jop ("fload-1", 35, 1, push-list: list (pp-var (1, $java-float-type$))) ;
define constant  j-fload-2 = make-jop ("fload-2", 36, 1, push-list: list (pp-var (2, $java-float-type$))) ;
define constant  j-fload-3 = make-jop ("fload-3", 37, 1, push-list: list (pp-var (3, $java-float-type$))) ;
define constant  j-dload-0 = make-jop ("dload-0", 38, 2, push-list: list (pp-var (0, $java-double-type$))) ;
define constant  j-dload-1 = make-jop ("dload-1", 39, 2, push-list: list (pp-var (1, $java-double-type$))) ;
define constant  j-dload-2 = make-jop ("dload-2", 40, 2, push-list: list (pp-var (2, $java-double-type$))) ;
define constant  j-dload-3 = make-jop ("dload-3", 41, 2, push-list: list (pp-var (3, $java-double-type$))) ;
define constant  j-aload-0 = make-jop ("aload-0", 42, 1, push-list: list (pp-var (0, $java/lang/Object$))) ;
define constant  j-aload-1 = make-jop ("aload-1", 43, 1, push-list: list (pp-var (1, $java/lang/Object$))) ;
define constant  j-aload-2 = make-jop ("aload-2", 44, 1, push-list: list (pp-var (2, $java/lang/Object$))) ;
define constant  j-aload-3 = make-jop ("aload-3", 45, 1, push-list: list (pp-var (3, $java/lang/Object$))) ;

define constant $int-arr$   :: <push-pop-model>  = pp-typed (array-type ($java-int-type$)) ;
define constant $float-arr$ :: <push-pop-model>  = pp-typed (array-type ($java-float-type$)) ;
define constant $long-arr$  :: <push-pop-model>  = pp-typed (array-type ($java-long-type$)) ;
define constant $double-arr$ :: <push-pop-model> = pp-typed (array-type ($java-double-type$)) ;
define constant $obj-arr$  :: <push-pop-model>   = pp-typed (array-type ($java/lang/Object$)) ;
define constant $byte-arr$  :: <push-pop-model>  = pp-typed (array-type ($java-byte-type$)) ;
define constant $char-arr$  :: <push-pop-model>  = pp-typed (array-type ($java-char-type$)) ;
define constant $short-arr$  :: <push-pop-model> = pp-typed (array-type ($java-short-type$)) ;

define constant  j-iaload  = make-jop ("iaload", 46 + j-int-code,   -1, 
                                   pop-list: list ($push-pop-int$, $int-arr$), push-list: $one-int$) ;
define constant  j-laload  = make-jop ("laload", 46 + j-long-code,  0, 
                                   pop-list: list ($push-pop-int$, $long-arr$), push-list: $one-long$) ;
define constant  j-faload  = make-jop ("faload", 46 + j-float-code, -1, 
                                   pop-list: list ($push-pop-int$, $float-arr$), push-list: $one-float$) ;
define constant  j-daload  = make-jop ("daload", 46 + j-double-code, 0, 
                                   pop-list: list ($push-pop-int$, $double-arr$), push-list: $one-double$) ;
define constant  j-aaload  = make-jop ("aaload", 46 + j-ref-code, -1, 
                                   pop-list: list ($push-pop-int$, $obj-arr$), push-list: $one-int$) ;
define constant  j-baload  = make-jop ("baload", 46 + j-byte-code, -1, 
                                   pop-list: list ($push-pop-int$, $byte-arr$), push-list: $one-int$) ;
define constant  j-caload  = make-jop ("caload", 46 + j-char-code, -1, 
                                   pop-list: list ($push-pop-int$, $char-arr$), push-list: $one-int$) ;
define constant  j-saload  = make-jop ("saload", 46 + j-short-code, -1,
                                   pop-list: list ($push-pop-int$, $short-arr$), push-list: $one-int$) ;


define sealed generic j-acode-for (type :: <java-type>, operation) => (res :: <java-abstract-bytecode>) ;

define method j-acode-for (type :: <java-type>, operation == #"load") => (res :: <java-abstract-bytecode>)
  $java-bytecodes [46 + j-code-for (type)]
end;

  

define constant  j-istore   = make-jop ("istore", 54 + j-int-code, -1, 
                                    pop-list: list (pp-var-n ($java-int-type$))) ;
define constant  j-lstore   = make-jop ("lstore", 54 + j-long-code, -2,
                                    pop-list: list (pp-var-n ($java-long-type$))) ;
define constant  j-fstore   = make-jop ("fstore", 54 + j-float-code, -1,
                                    pop-list: list (pp-var-n ($java-float-type$))) ;
define constant  j-dstore   = make-jop ("dstore", 54 + j-double-code, -2, pop-list: list (pp-var-n ($java-double-type$))) ;
define constant  j-astore   = make-jop ("astore", 54 + j-ref-code, -1, pop-list: list (pp-var-n ($java/lang/Object$))) ;
define constant  j-istore-0 = make-jop ("istore-0", 59, -1, pop-list: list (pp-var (0, $java-int-type$))) ;
define constant  j-istore-1 = make-jop ("istore-1", 60, -1, pop-list: list (pp-var (1, $java-int-type$))) ;
define constant  j-istore-2 = make-jop ("istore-2", 61, -1, pop-list: list (pp-var (2, $java-int-type$))) ;
define constant  j-istore-3 = make-jop ("istore-3", 62, -1, pop-list: list (pp-var (3, $java-int-type$))) ;
define constant  j-lstore-0 = make-jop ("lstore-0", 63, -2, pop-list: list (pp-var (0, $java-long-type$))) ;
define constant  j-lstore-1 = make-jop ("lstore-1", 64, -2, pop-list: list (pp-var (1, $java-long-type$))) ;
define constant  j-lstore-2 = make-jop ("lstore-2", 65, -2, pop-list: list (pp-var (2, $java-long-type$))) ;
define constant  j-lstore-3 = make-jop ("lstore-3", 66, -2, pop-list: list (pp-var (3, $java-long-type$))) ;
define constant  j-fstore-0 = make-jop ("fstore-0", 67, -1, pop-list: list (pp-var (0, $java-float-type$))) ;
define constant  j-fstore-1 = make-jop ("fstore-1", 68, -1, pop-list: list (pp-var (1, $java-float-type$))) ;
define constant  j-fstore-2 = make-jop ("fstore-2", 69, -1, pop-list: list (pp-var (2, $java-float-type$))) ;
define constant  j-fstore-3 = make-jop ("fstore-3", 70, -1, pop-list: list (pp-var (3, $java-float-type$))) ;
define constant  j-dstore-0 = make-jop ("dstore-0", 71, -2, pop-list: list (pp-var (0, $java-double-type$))) ;
define constant  j-dstore-1 = make-jop ("dstore-1", 72, -2, pop-list: list (pp-var (1, $java-double-type$))) ;
define constant  j-dstore-2 = make-jop ("dstore-2", 73, -2, pop-list: list (pp-var (2, $java-double-type$))) ;
define constant  j-dstore-3 = make-jop ("dstore-3", 74, -2, pop-list: list (pp-var (3, $java-double-type$))) ;
define constant  j-astore-0 = make-jop ("astore-0", 75, -1, pop-list: list (pp-var (0, $java/lang/Object$))) ;
define constant  j-astore-1 = make-jop ("astore-1", 76, -1, pop-list: list (pp-var (1, $java/lang/Object$))) ;
define constant  j-astore-2 = make-jop ("astore-2", 77, -1, pop-list: list (pp-var (2, $java/lang/Object$))) ;
define constant  j-astore-3 = make-jop ("astore-3", 78, -1, pop-list: list (pp-var (3, $java/lang/Object$))) ;

define constant  j-iastore  = make-jop ("iastore", 79 + j-int-code, -3, pop-list: list ($push-pop-int$, $push-pop-int$, $int-arr$)) ;
define constant  j-lastore  = make-jop ("lastore", 79 + j-long-code, -4, pop-list: list ($push-pop-long$, $push-pop-int$, $long-arr$)) ;
define constant  j-fastore  = make-jop ("fastore", 79 + j-float-code, -3, pop-list: list ($push-pop-float$, $push-pop-int$, $float-arr$)) ;
define constant  j-dastore  = make-jop ("dastore", 79 + j-double-code, -4, pop-list: list ($push-pop-double$, $push-pop-int$, $double-arr$));
define constant  j-aastore  = make-jop ("aastore", 79 + j-ref-code, -3, pop-list: list ($push-pop-obj$, $push-pop-int$, $obj-arr$)) ;
define constant  j-bastore  = make-jop ("bastore", 79 + j-byte-code, -3, pop-list: list ($push-pop-int$, $push-pop-int$, $byte-arr$)) ;
define constant  j-castore  = make-jop ("castore", 79 + j-char-code, -3, pop-list: list ($push-pop-int$, $push-pop-int$, $char-arr$)) ;
define constant  j-sastore  = make-jop ("sastore", 79 + j-short-code, -3, pop-list: list ($push-pop-int$, $push-pop-int$, $short-arr$)) ;


define method j-acode-for (type :: <java-type>, operation == #"store") => (res :: <java-abstract-bytecode>)
  $java-bytecodes [79 + j-code-for (type)]
end;


define constant  j-pop     = make-jop ("pop", 87, -1,    pop-list: list ($pop-var1$)) ;
define constant  j-pop2    = make-jop ("pop2", 88, -2,   pop-list: list ($pop2-var1$)) ;
define constant  j-dup     = make-jop ("dup",  89, 1,
				       pop-list: list ($pop-var1$),
				       push-list: list ($pop-var1$, $pop-var1$)) ;
define constant  j-dup-x1  = make-jop ("dup-x1", 90, 1,
				       pop-list: list ($pop-var1$, $pop-var2$), 
				       push-list: list ($pop-var1$, $pop-var2$, $pop-var1$)) ;
define constant  j-dup-x2  = make-jop ("dup-x2", 91, 1,
				       pop-list: list ($pop-var1$, $pop-var2$, $pop-var3$), 
				       push-list: list ($pop-var1$, $pop-var3$, $pop-var2$, $pop-var1$)) ;
define constant  j-dup2    = make-jop ("dup2",   92, 2,
				       pop-list: list ($pop2-var1$),
				       push-list: list ($pop2-var1$, $pop2-var1$)) ;
define constant  j-dup2-x1 = make-jop ("dup2-x1", 93, 2,
				       pop-list:  list ($pop2-var1$, $pop-var1$),
				       push-list: list ($pop2-var1$, $pop-var1$, $pop2-var1$)) ;
define constant  j-dup2-x2 = make-jop ("dup2-x2", 94, 2,
				       pop-list:  list ($pop2-var1$, $pop-var1$, $pop-var2$),
				       push-list: list ($pop2-var1$, $pop-var2$, $pop-var1$, $pop2-var1$)) ;
define constant  j-swap    = make-jop ("swap", 95, 0,
				       pop-list:  list ($pop-var1$, $pop-var2$),
				       push-list: list ($pop-var1$, $pop-var2$)) ;
define constant  j-iadd  = make-jop ("iadd", 96 + j-int-code, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-ladd  = make-jop ("ladd", 96 + j-long-code, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-fadd  = make-jop ("fadd", 96 + j-float-code, -1,
				     pop-list: $two-floats$, push-list: $one-float$) ;
define constant  j-dadd  = make-jop ("dadd", 96 + j-double-code, -2,
				     pop-list: $two-doubles$, push-list: $one-double$) ;
define constant  j-isub  = make-jop ("isub", 100 + j-int-code, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lsub  = make-jop ("lsub", 100 + j-long-code, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-fsub  = make-jop ("fsub", 100 + j-float-code, -1,
				     pop-list: $two-floats$, push-list: $one-float$) ;
define constant  j-dsub  = make-jop ("dsub", 100 + j-double-code, -2,
				     pop-list: $two-doubles$, push-list: $one-double$) ;
define constant  j-imul  = make-jop ("imul", 104 + j-int-code, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lmul  = make-jop ("lmul", 104 + j-long-code, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-fmul  = make-jop ("fmul", 104 + j-float-code, -1,
				     pop-list: $two-floats$, push-list: $one-float$) ;
define constant  j-dmul  = make-jop ("dmul", 104 + j-double-code, -2,
				     pop-list: $two-doubles$, push-list: $one-double$) ;
define constant  j-idiv  = make-jop ("idiv", 108 + j-int-code, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-ldiv  = make-jop ("ldiv", 108 + j-long-code, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-fdiv  = make-jop ("fdiv", 108 + j-float-code, -1,
				     pop-list: $two-floats$, push-list: $one-float$) ;
define constant  j-ddiv  = make-jop ("ddiv", 108 + j-double-code, -2,
				     pop-list: $two-doubles$, push-list: $one-double$) ;
define constant  j-imod  = make-jop ("imod", 112 + j-int-code, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lmod  = make-jop ("lmod", 112 + j-long-code, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-fmod  = make-jop ("fmod", 112 + j-float-code, -1,
				     pop-list: $two-floats$, push-list: $one-float$) ;
define constant  j-dmod  = make-jop ("dmod", 112 + j-double-code, -2,
				     pop-list: $two-doubles$, push-list: $one-double$) ;
define constant  j-ineg  = make-jop ("ineg", 116 + j-int-code, 0,
				     pop-list: $one-int$, push-list: $one-int$) ;
define constant  j-lneg  = make-jop ("lneg", 116 + j-long-code, 0,
				     pop-list: $one-long$, push-list: $one-long$) ;
define constant  j-fneg  = make-jop ("fneg", 116 + j-float-code, 0,
				     pop-list: $one-float$, push-list: $one-float$) ;
define constant  j-dneg  = make-jop ("dneg", 116 + j-double-code, 0,
				     pop-list: $one-double$, push-list: $one-double$) ;
define constant  j-ishl  = make-jop ("ishl", 120, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lshl  = make-jop ("lshl", 121, -1,
				     pop-list: list ($push-pop-int$, $push-pop-long$), push-list: $one-long$) ;
define constant  j-ishr  = make-jop ("ishr", 122, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lshr  = make-jop ("lshr", 123, -1,
				     pop-list: list ($push-pop-int$, $push-pop-long$), push-list: $one-long$) ;
define constant  j-iushr = make-jop ("iushr", 124, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lushr = make-jop ("lushr", 125, -1,
				     pop-list: list ($push-pop-int$, $push-pop-long$), push-list: $one-long$) ;
define constant  j-iand  = make-jop ("iand", 126, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-land  = make-jop ("land", 127, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-ior   = make-jop ("ior", 128, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lor   = make-jop ("lor", 129, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;
define constant  j-ixor  = make-jop ("ixor", 130, -1,
				     pop-list: $two-ints$, push-list: $one-int$) ;
define constant  j-lxor  = make-jop ("lxor", 131, -2,
				     pop-list: $two-longs$, push-list: $one-long$) ;

define constant  j-iinc  = make-jop ("iinc", 132, 0) ;  // this needs a special verification rule!

define constant  j-i2l  = make-jop ("i2l", 133, 1,
				    pop-list: $one-int$, push-list: $one-long$) ;
define constant  j-i2f  = make-jop ("i2f", 134, 0,
				    pop-list: $one-int$, push-list: $one-float$) ;
define constant  j-i2d  = make-jop ("i2d", 135, 1,
				    pop-list: $one-int$, push-list: $one-double$) ;
define constant  j-l2i  = make-jop ("l2i", 136, -1,
				    pop-list: $one-long$, push-list: $one-int$) ;
define constant  j-l2f  = make-jop ("l2f", 137, -1,
				    pop-list: $one-long$, push-list: $one-float$) ;
define constant  j-l2d  = make-jop ("l2d", 138, 0,
				    pop-list: $one-long$, push-list: $one-double$) ;
define constant  j-f2i  = make-jop ("f2i", 139, 0,
				    pop-list: $one-float$, push-list: $one-int$) ;
define constant  j-f2l  = make-jop ("f2l", 140, 1,
				    pop-list: $one-float$, push-list: $one-long$) ;
define constant  j-f2d  = make-jop ("f2d", 141, 1,
				    pop-list: $one-float$, push-list: $one-double$) ;
define constant  j-d2i  = make-jop ("d2i", 142, -1,
				    pop-list: $one-double$, push-list: $one-int$) ;
define constant  j-d2l  = make-jop ("d2l", 143, 0,
				    pop-list: $one-double$, push-list: $one-long$) ;
define constant  j-d2f  = make-jop ("d2f", 144, -1,
				    pop-list: $one-double$, push-list: $one-float$) ;
define constant  j-int2byte  = make-jop ("int2byte", 145, 0,
					 pop-list: $one-int$, push-list: $one-byte$) ;
define constant  j-int2char  = make-jop ("int2char", 146, 0,
					 pop-list: $one-int$, push-list: $one-char$) ;
define constant  j-int2short  = make-jop ("int2short", 147, 0,
					  pop-list: $one-int$, push-list: $one-short$) ;

define constant  j-lcmp  = make-jop ("lcmp", 148, -3,
				     pop-list: $two-longs$, push-list: $one-int$) ;
define constant  j-fcmpl  = make-jop ("fcmpl", 149, -1,
				      pop-list: $two-floats$, push-list: $one-int$) ;
define constant  j-fcmpg  = make-jop ("fcmpg", 150, -1,
				      pop-list: $two-floats$, push-list: $one-int$) ;
define constant  j-dcmpl  = make-jop ("dcmpl", 151, -3,
				      pop-list: $two-doubles$, push-list: $one-int$) ;
define constant  j-dcmpg  = make-jop ("dcmpg", 152, -3,
				      pop-list: $two-doubles$, push-list: $one-int$) ;

define constant  j-ifeq  = make-jop ("ifeq", 153, -1, pop-list: $one-int$) ;
define constant  j-ifne  = make-jop ("ifne", 154, -1, pop-list: $one-int$) ;
define constant  j-iflt  = make-jop ("iflt", 155, -1, pop-list: $one-int$) ;
define constant  j-ifge  = make-jop ("ifge", 156, -1, pop-list: $one-int$) ;
define constant  j-ifgt  = make-jop ("ifgt", 157, -1, pop-list: $one-int$) ;
define constant  j-ifle  = make-jop ("ifle", 158, -1, pop-list: $one-int$) ;
define constant  j-ifnull  = make-jop ("ifnull", 198, -1, pop-list: $one-object$) ;
define constant  j-ifnonnull  = make-jop ("ifnonnull", 199, -1, pop-list: $one-object$) ;

define constant  j-if-icmpeq = make-jop ("if-icmpeq", 159, -2, pop-list: $two-ints$) ;
define constant  j-if-icmpne = make-jop ("if-icmpne", 160, -2, pop-list: $two-ints$) ;
define constant  j-if-icmplt = make-jop ("if-icmplt", 161, -2, pop-list: $two-ints$) ;
define constant  j-if-icmpge = make-jop ("if-icmpge", 162, -2, pop-list: $two-ints$) ;
define constant  j-if-icmpgt = make-jop ("if-icmpgt", 163, -2, pop-list: $two-ints$) ;
define constant  j-if-icmple = make-jop ("if-icmple", 164, -2, pop-list: $two-ints$) ;
define constant  j-if-acmpeq = make-jop ("if-acmpeq", 165, -2, pop-list: $two-objects$) ;
define constant  j-if-acmpne = make-jop ("if-acmpne", 166, -2, pop-list: $two-objects$) ;

define constant  j-goto    = make-jop ("goto", 167, 0) ;
define constant  j-goto-w  = make-jop ("goto_w", 200, 0) ;
// note that jsr does push, but most uses of it pop again before return!
define constant  j-jsr    = make-jop ("jsr", 168, 1, push-list: $one-retaddr$) ;
define constant  j-jsr-w  = make-jop ("jsr-w", 201, 1,  push-list: $one-retaddr$) ;

define constant  j-ret   = make-jop ("ret", 169, 0) ;
define constant  j-tableswitch  = make-jop ("tableswitch", 170, -1, pop-list: $one-int$) ;
define constant  j-lookupswitch  = make-jop ("lookupswitch", 171, -1, pop-list: $one-int$) ;

define constant  j-ireturn  = make-jop ("ireturn", 172, -1, pop-list: $one-int$) ;
define constant  j-lreturn  = make-jop ("lreturn", 173, -2, pop-list: $one-long$) ;
define constant  j-freturn  = make-jop ("freturn", 174, -1, pop-list: $one-float$) ;
define constant  j-dreturn  = make-jop ("dreturn", 175, -2, pop-list: $one-double$) ;
define constant  j-areturn  = make-jop ("areturn", 176, -1, pop-list: $one-object$) ;
define constant  j-return   = make-jop ("return", 177, 0) ;

define constant  j-getstatic = make-jop ("getstatic", 178, 1,  push-list: $one-field$) ;
define constant  j-putstatic = make-jop ("putstatic", 179, -1, pop-list: $one-field$) ;
define constant  j-getfield  = make-jop ("getfield", 180, 0,   pop-list: $one-instance$, push-list: $one-field$) ;
define constant  j-putfield  = make-jop ("putfield", 181, -2,  pop-list: $field-and-instance$) ;

define constant  j-getstatic2 = make-jop ("getstatic", 178, 2,  push-list: $one-field$) ;
define constant  j-putstatic2 = make-jop ("putstatic", 179, -2, pop-list: $one-field$) ;
define constant  j-getfield2  = make-jop ("getfield", 180, 1,   pop-list: $one-instance$, push-list: $one-field$) ;
define constant  j-putfield2  = make-jop ("putfield", 181, -3,  pop-list: $field-and-instance$) ;

define function make-jcop (opname, opcode, pops-instance?)
  let  bytecode = make (<java-call-bytecode>,
			opname: opname,
			opcode: opcode,
			pops-instance?: pops-instance?) ;
  $java-bytecodes [opcode] := bytecode
end;

define constant  j-invokevirtual  = make-jcop ("invokevirtual", 182, #t) ;
define constant  j-invokespecial  = make-jcop ("invokespecial", 183, #t) ;
define constant  j-invokestatic   = make-jcop ("invokestatic", 184, #f) ;
define constant  j-invokeinterface = make-jcop ("invokeinterface", 185, #t) ;

define constant  j-new         = make-jop ("new", 187, 1, push-list: $one-new-instance$) ;
define constant  j-newarray    = make-jop ("newarray", 188, 0, pop-list: $one-int$, push-list: $one-new-prim-array$) ;
define constant  j-anewarray   = make-jop ("anewarray", 189, 0, pop-list: $one-int$, push-list: $one-new-array$) ;
define constant  j-arraylength = make-jop ("arraylength", 190, 0, pop-list: $one-array$, push-list: $one-int$) ;
define constant  j-athrow      = make-jop ("athrow", 191, -1, pop-list: $one-throwable$) ;
define constant  j-checkcast   = make-jop ("checkcast", 192, 0, pop-list: list ($pop-check$), push-list: list ($pop-check$)) ;

define constant  j-instanceof  = make-jop ("instanceof", 193, 0, pop-list: $one-object$, push-list: $one-int$) ;
define constant  j-monitorenter = make-jop ("monitorenter", 194, -1, pop-list: $one-object$) ;
define constant  j-monitorexit  = make-jop ("monitorexit", 195, -1, pop-list: $one-object$) ;

define constant  j-wide  = make-jop ("wide", 196, 0) ; // bit of an odd one this

define constant  j-multianewarray  = make-jcop ("multianewarray", 197, #f) ;

define constant  j-software = make-jcop ("software", 254, #f) ;
define constant  j-hardware = make-jcop ("hardware", 255, #f) ;


format-out ("inited java-opcodes.dylan\n") ;

// eof
