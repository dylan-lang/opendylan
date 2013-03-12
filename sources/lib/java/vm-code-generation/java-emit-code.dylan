Module: java-vm-code-generation
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

format-out ("initing java-emit-code.dylan\n") ;



// should be able to get rid of this
define sealed class <java-label> (<object>)
  sealed slot pc = #f ;
end;

define constant $initial-jbb-vecsize$ = 15 ;

define variable  *break-on-non-verify* :: <boolean> = #f ;


define class <java-basic-block> (<java-code>)
  sealed slot  meth :: <java-method>, init-keyword: meth: ;
  sealed slot  bytecodes :: <simple-object-vector> = make (<simple-object-vector>, size: $initial-jbb-vecsize$) ;
  sealed slot  the-label :: <java-label> = make (<java-label>) ;
  sealed slot  icount :: <integer> = 0 ;
  sealed slot  tcount :: <integer> = $initial-jbb-vecsize$ ;
  sealed slot  initial-stack-depth = #f ;
  sealed slot  stack-depth :: <integer> = 0 ;
  sealed slot  initial-stack-model = #() ;
  sealed slot  stack-model         = #() ;
  sealed slot  initial-local-var-types = #f ;
  sealed slot  local-var-types     = #f ;
  sealed slot  constants ;  // just cache the value in the <java-concrete-class> for easier debugging
end;


define variable *inside-bb-generation* :: <boolean> = #f ;

define function make-jbb (jmeth :: <java-method>) => (jbb :: <java-basic-block>)
//  if (*inside-bb-generation*)
//    error ("make-jbb called inside bb generation!")
//  end;
  *inside-bb-generation* := #t ;

  let  opc = jmeth.pc ;
  let  jbb = make (<java-basic-block>, meth: jmeth, pc: opc) ;
  jbb.the-label.pc := opc ;
  jmeth.basic-blocks := add! (jmeth.basic-blocks, jbb) ;
  jbb.constants := jmeth.java-class.concrete-implementation.constants ;
  jbb
end;


define sealed abstract class <java-abstract-frag> (<object>)
end;

define class <java-imm-frag> (<java-abstract-frag>)
  sealed slot  imm-value :: <integer>, required-init-keyword: imm-value: ;
end;

define class <java-frag> (<java-abstract-frag>)
  sealed slot  opcode :: <java-abstract-bytecode>, required-init-keyword: opcode: ;
end;

define constant $dummy-java-frag$ :: <java-frag> = make (<java-frag>, opcode: j-nop) ;

define sealed generic frag-size (frag :: <java-abstract-frag>) => (size :: <integer>) ;
//define method frag-size-setter (size :: <integer>, frag :: <java-abstract-frag>) => (size :: <integer>) size end;

define method frag-size (frag :: <java-frag>) => (size :: <integer>) 1 end ;
define method frag-size (frag :: <java-imm-frag>) => (size :: <integer>) 1 end ;

define method print-object (frag :: <java-frag>, stream :: <stream>) => ()
  format (stream, "{<java-frag %s>", frag.opcode.opname)
end;

define method print-object (frag :: <java-imm-frag>, stream :: <stream>) => ()
  format (stream, "{<java-imm-frag #%d %d>", frag.imm-value, frag.frag-size)
end;

define sealed generic output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-abstract-frag>) => (npc :: <integer>) ;

// hack for emulator??
define function bytify (i :: <integer>) => (res :: <integer>)
  i := logand (i, #xff) ;
//  if (i >= #x80)
//    i := i - #x100
//  end;
  i
end;


define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-imm-frag>) => (npc :: <integer>)
  vec [peecee] := bytify (frag.imm-value) ;
  peecee + 1
end;


define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-frag>) => (npc :: <integer>)
  vec [peecee] := bytify (frag.opcode.opcode) ;
  peecee + 1
end;

define class <java-op1-frag> (<java-frag>)
  sealed slot  op, init-keyword: op: ;
end;

define method print-object (frag :: <java-op1-frag>, stream :: <stream>) => ()
  format (stream, "{<java-op1-frag %s %d>", frag.opcode.opname, frag.op)
end;

define method frag-size (frag :: <java-op1-frag>) => (size :: <integer>) 2 end ;

//define method initialize (frag :: <java-op1-frag>, #key) => ()
//  next-method () ;
////  frag.frag-size := 2
//end;

define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-op1-frag>) => (npc :: <integer>)
  vec [peecee]     := bytify (frag.opcode.opcode) ;
  vec [peecee + 1] := bytify (frag.op) ;
  peecee + 2
end;

define class <java-op2-frag> (<java-op1-frag>)
  sealed slot constants, required-init-keyword: constants: ;  // purely for printing nicely
end;

define method print-object (frag :: <java-op2-frag>, stream :: <stream>) => ()
  let  index  = frag.op ;
  format (stream, "{<java-op2-frag %s %d", frag.opcode.opname, index) ;
  let  consts = frag.constants ;
  if (element (consts, index, default: #f))
    format (stream, " : %s", consts[index])
  end;
  format (stream, ">}")
end;

define method frag-size (frag :: <java-op2-frag>) => (size :: <integer>) 3 end ;

//define method initialize (frag :: <java-op2-frag>, #key) => ()
//  next-method () ;
////  frag.frag-size := 3
//end;

define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-op2-frag>) => (npc :: <integer>)
  vec [peecee]     := bytify (frag.opcode.opcode) ;
  vec [peecee + 1] := bytify (ash (frag.op, -8)) ;
  vec [peecee + 2] := bytify (frag.op) ;
  peecee + 3
end;

// this only used for invokeinterface
define class <java-op22-frag> (<java-op2-frag>)
  sealed slot nargs :: <integer>, required-init-keyword: nargs: ;
end;

define method print-object (frag :: <java-op22-frag>, stream :: <stream>) => ()
  let  index  = frag.op ;
  format (stream, "{<java-op22-frag %s %d %d", frag.opcode.opname, index, frag.nargs) ;
  let  consts = frag.constants ;
  if (element (consts, index, default: #f))
    format (stream, " : %s", consts[index])
  end;
  format (stream, ">}")
end;

define method frag-size (frag :: <java-op22-frag>) => (size :: <integer>) 5 end ;

define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-op22-frag>) => (npc :: <integer>)
  vec [peecee]     := bytify (frag.opcode.opcode) ;
  vec [peecee + 1] := bytify (ash (frag.op, -8)) ;
  vec [peecee + 2] := bytify (frag.op) ;
  vec [peecee + 3] := bytify (frag.nargs) ;
  vec [peecee + 4] := 0 ;
  peecee + 5
end;


define class <java-branch-frag> (<java-frag>)
  sealed slot  meth, init-keyword: meth: ;
  sealed slot  dest, init-keyword: dest: ;
end;

define method print-object (frag :: <java-branch-frag>, stream :: <stream>) => ()
  format (stream, "{<java-branch-frag %s %s -> %s>", frag.opcode.opname, frag.meth, frag.dest)
end;

define method frag-size (frag :: <java-branch-frag>) => (size :: <integer>) 3 end ;

//define method initialize (frag :: <java-branch-frag>, #key) => ()
//  next-method () ;
////  frag.frag-size := 3
//end;

define constant $relative-tag$ :: <integer> = 1000000 ;

define function branch-relative (branch-offset :: <integer>) => (off :: <integer>)
  branch-offset + $relative-tag$
end;





define open generic resolve-branch-dest (thing, meth :: <java-method>, peecee :: <integer>) => (offset :: <integer>) ;

define method resolve-branch-dest (thing :: <object>, meth :: <java-method>, peecee :: <integer>)
 => (offset :: <integer>)
  error ("bad dest for fragment %s(%s)\n", thing, thing.object-class)
end;

define method resolve-branch-dest (thing :: <function>, meth :: <java-method>, peecee :: <integer>)
 => (offset :: <integer>)
  let offset :: <integer> = thing() ;
  offset
end;

define method resolve-branch-dest (thing :: <integer>, meth :: <java-method>, peecee :: <integer>)
 => (offset :: <integer>)
  if (thing >= $relative-tag$)   // hack for relative
    (thing - $relative-tag$) - peecee
  else
    thing
  end
end;

define method output-frag (peecee :: <integer>, vec :: <byte-vector>, frag :: <java-branch-frag>)
 => (npc :: <integer>)
  vec [peecee] := frag.opcode.opcode.bytify ;
  let  offs :: <integer> = resolve-branch-dest (frag.dest, frag.meth, peecee) ;
  vec [peecee + 1] := bytify (ash (offs, -8)) ;
  vec [peecee + 2] := offs.bytify ;
  peecee + 3
end;



// actually stash the frag into a vector, maintain the true pc count.
define function add-bytecode (jbb :: <java-basic-block>, byte :: <java-abstract-frag>) => ()
  let  ic :: <integer> = jbb.icount ;
  let  tc :: <integer> = jbb.tcount ;
  let  bcodes :: <simple-object-vector> = jbb.bytecodes ;
  if (ic == tc)
    let  ntc = 2 * tc ;
    let  new :: <simple-object-vector> = make (<simple-object-vector>, size: ntc) ;
    jbb.tcount := ntc ;
    for (n :: <integer> from 0 below ic)
      new[n] := bcodes[n]
    end;
    jbb.bytecodes := bcodes := new
  end;
  bcodes [ic] := byte ;
  jbb.icount := ic + 1 ;
  jbb.pc := jbb.pc + byte.frag-size
end;

define variable *debug-jvm-instrs* = 4 ;


// maintain the model of stack depth within a BB - collect the max depth as
// well as tracking the current depth.
define function maintain-stack-depth (jbb :: <java-basic-block>, pushes :: <integer>, op :: <java-abstract-bytecode>) => ()
  if (*debug-jvm-instrs*)
    format-out ("... instruction %s changes depth from %d to %d\n",
                op, jbb.stack-depth, jbb.stack-depth + pushes)
  end;
  unless (zero? (pushes))
    let  new-depth :: <integer> = jbb.stack-depth + pushes ;
    if (new-depth > jbb.max-stack)
      jbb.max-stack := new-depth
    elseif (negative? (new-depth))
      if (*debug-jvm-instrs*)
        format-out ("############## negative JVM stack depth!! %s\n", op) ;
	java-marker-op (jbb) ;
      else
        error ("negative JVM stack depth in Java backend")
      end;
      new-depth := 0
    end;
    jbb.stack-depth := new-depth
  end
end;






define sealed generic model-a-push (jbb :: <java-basic-block>, oper :: <java-abstract-frag>, pushee :: <push-pop-model>) => (words :: <integer>) ;
define sealed generic model-a-pop (jbb :: <java-basic-block>, oper :: <java-abstract-frag>, pushee :: <push-pop-model>) => (words :: <integer>) ;


define function model-pop-discards (jbb :: <java-basic-block>, count :: <integer>) => (words :: <integer>)
  let  original-depth = jbb.stack-depth ;
  let  depth = original-depth ;
  let  model = jbb.stack-model ;
  for (n :: <integer> from 0 below count)
    unless (instance? (model, <pair>))
      error ("empty stack on pop-discarding")
    end;
    let  tipe = model.head ;
    model := model.tail ;
    depth := depth - tipe.java-type-words
  end;
  jbb.stack-model := model ;
  jbb.stack-depth := depth ;
  depth - original-depth
end;



define function model-push-type (jbb :: <java-basic-block>, tipe :: <java-type>) => (words :: <integer>)
  jbb.stack-model := pair (tipe, jbb.stack-model) ;
  let  count = tipe.java-type-words ;
  jbb.stack-depth := jbb.stack-depth + count ;
  count
end;


define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-constant>) => (words :: <integer>)
  let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
  if (instance? (oper, <java-op1-frag>))
    let  index :: <integer> = oper.op ;
    let  constant = pool [index] ;
    let  tipe = constant.constant-javatype ;
    model-push-type (jbb, tipe)
  else
    error ("not an op fragment in pushed constant?!")
  end
end;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-field>) => (words :: <integer>)
  let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
  if (instance? (oper, <java-op1-frag>))
    let  index :: <integer> = oper.op ;
    let  constant = pool [index] ;
    let  tipe = constant.constant-javatype ;
    if (*debug-jvm-instrs* == #t)
      format-out ("pushing a field, type %s\n", tipe) 
    end;
    model-push-type (jbb, tipe) 
  else
    error ("not an op fragment in pushed field?!")
  end
end;


define constant $prim-array-code-lookup$ =  
  vector (#f, #f, #f, #f, 
          $java-bool-type$, $java-char-type$, 
          $java-float-type$, $java-double-type$,
          $java-byte-type$, $java-short-type$,
          $java-int-type$, $java-long-type$) ;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-array>) => (words :: <integer>)
  let  tipe = #f ;
  let  index :: <integer> = oper.op ;
  if (pushee.prim?)
    tipe := element ($prim-array-code-lookup$, index, default: #f) ;
    unless (tipe)
      error ("Huh? bad primitive array typecode")
    end;
  else
    let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
    tipe := pool[index].constant-javatype ;
  end;
  model-push-type (jbb, array-type (tipe))
end;



define function model-push-a-local (jbb :: <java-basic-block>, index :: <integer>) => (words :: <integer>)
  let  var-types = jbb.local-var-types ;
  let tipe = var-types & var-types [index] ;
  unless (tipe)
    if (*debug-jvm-instrs*)
      format-out ("pushing an Uninitialized local var %d\n", index) ;
      java-marker-op (jbb) ;
      tipe := $java/lang/Object$
    else
      error ("pushing an Uninitialized local var %d", index)
    end
  end;
  if (*debug-jvm-instrs* == #t)
    format-out ("@@@ model-push-a-local, index=%d, type=%s\n", index, tipe) 
  end;
  model-push-type (jbb, tipe)
end;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-typed>) => (words :: <integer>)
  let  tipe = pushee.type-constraint ;
  unless (tipe)
    error ("model-push has bad type-constraint")
  end;
  model-push-type (jbb, tipe)
end;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-local-num>) => (words :: <integer>)
  model-push-a-local (jbb, pushee.local-var-num)
end;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-local-ind>) => (words :: <integer>)
  model-push-a-local (jbb, oper.op)
end;


define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-metavar>) => (words :: <integer>)
  let  tipe = pushee.type-variable ;
  if (tipe == #f)
    if (*debug-jvm-instrs*)
      format-out ("unassigned type metavar\n") ;
      java-marker-op (jbb) ;
      tipe := $java/lang/Object$
    else
      error ("unassigned type metavar")
    end
  end;
  if (instance? (pushee, <push-pop-model-metavar2>))
    if (instance? (tipe, <pair>))
      jbb.stack-model := pair (tipe.tail, pair (tipe.head, jbb.stack-model)) ;
    else
      jbb.stack-model := pair (tipe, jbb.stack-model) ;
    end;
    jbb.stack-depth := jbb.stack-depth + 2 ;
    2
  else
    jbb.stack-model := pair (tipe, jbb.stack-model) ;
    jbb.stack-depth := jbb.stack-depth + 1 ;
    1
  end
end;


define function model-pop-a-type (jbb :: <java-basic-block>, tipe :: <java-type>) => (words :: <integer>)
  let  list  = jbb.stack-model ;
  let  top = #f ;
  let  rest = #f ;
  if (empty? (list))
    if (*debug-jvm-instrs*)
      format-out ("trying to pop a model from empty stack model! faking it\n") ;
      java-marker-op (jbb) ;
      top  := $java/lang/Object$ ;
      rest := list
    else
      error ("trying to pop a model from empty stack model")
    end
  else
    top  := list.head ;
    rest := list.tail ;
  end;
  let  count = tipe.java-type-words ;
  unless (assignment-compatible? (top, tipe))
    if (*debug-jvm-instrs*)
      format-out ("@@@@@@ not ass comp, %s, %s, ignoring problem\n", top, tipe) ;
      java-marker-op (jbb) ;
      if (*break-on-non-verify*)
        my-break (jbb)
      end
    else
      error ("not java assignment compatible")
    end
  end;
  jbb.stack-model := rest ;
  jbb.stack-depth := jbb.stack-depth - count ;
  count
end;

define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-typed>) => (words :: <integer>)
  model-pop-a-type (jbb, poppee.type-constraint)
end;


define constant $max-local-number$ = #x100 ;  // don't support "wide" yet (well, nearly)

define function model-set-a-local (jbb :: <java-basic-block>, index :: <integer>, new-tipe :: <java-type>) => (final-tipe :: <java-type>)
  unless (new-tipe)
    error ("whhops in model-set-a-local")
  end;
  let  var-types = jbb.local-var-types ;
  unless (var-types)
    var-types := make (<simple-object-vector>, size: $max-local-number$, fill: #f) ;
    jbb.local-var-types := var-types
  end;
  let  old-tipe = var-types [index] ;
  if (*debug-jvm-instrs* == #t)
    format-out ("@@@ model-set-local %d, was %s, new %s\n", index, old-tipe, new-tipe) 
  end;
  if (old-tipe)
    if (assignment-compatible? (new-tipe, old-tipe))
      old-tipe
    else
      /*
      if (*debug-jvm-instrs*)
	java-marker-op (jbb) ;
        format-out ("@@@@@@ not ass comp, %s, %s\n", new-tipe, old-tipe) 
      else
        error ("badly typed local assign")
      end;
      */
      // HACK to allow arbitrary retypings for now
      var-types [index] := new-tipe
    end
  else
    var-types [index] := new-tipe ;
  end
end;
  

define function model-pop-a-local (jbb :: <java-basic-block>, index :: <integer>) => (words :: <integer>)
  let  list  = jbb.stack-model ;
  let  top-type = list.head ;
format-out ("top type is %s\n", top-type) ;
  jbb.stack-model := list.tail ;

  let  var-new-type = model-set-a-local (jbb, index, top-type) ;
  let  count = var-new-type.java-type-words ;
  jbb.stack-depth := jbb.stack-depth - count ;
  count
end;


define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-field>) => (words :: <integer>)
  let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
  if (instance? (oper, <java-op1-frag>))
    let  index :: <integer> = oper.op ;
    let  constant = pool [index] ;
    if (instance? (constant, <java-slot-constant>))
      let  tipe = constant.constant-javatype ;
      if (*debug-jvm-instrs* == #t)
        format-out ("popping a field, type %s\n", tipe) 
      end;
      model-pop-a-type (jbb, tipe) 
    else
      error ("not a java slot in popped field?!")
    end
  else
    error ("not an op fragment in popped field?!")
  end
end;

define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-instance>) => (words :: <integer>)
  let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
  if (instance? (oper, <java-op1-frag>))
    let  index :: <integer> = oper.op ;
    let  constant = pool [index] ;
    if (instance? (constant, <java-slot-constant>))
      let  tipe = constant.java-class.java-class ;  // want the class of the field!
      if (*debug-jvm-instrs* == #t)
        format-out ("popping a field instance, type %s\n", tipe) 
      end;
      model-pop-a-type (jbb, tipe)
    else
      error ("not a java slot in popped field instance?!")
    end
  else
    error ("not an op fragment in popped field instance?!")
  end
end;


define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-local>) => (words :: <integer>)
  error ("model-a-pop with an ABSTRASCT <push-pop-model-local>!")
end;

define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-local-num>) => (words :: <integer>)
  model-pop-a-local (jbb, poppee.local-var-num)
end;

define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-local-ind>) => (words :: <integer>)
  model-pop-a-local (jbb, oper.op)
end;

define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-metavar>) => (words :: <integer>)
  let  list  = jbb.stack-model ;
  if (empty? (list))
    if (*debug-jvm-instrs*)
      format-out ("@@@@@ popping from empty stack model\n")
    end;
    if (*break-on-non-verify*)
      my-break (jbb)
    end
  end;
  let  top = list.head ;
  let  rest = list.tail ;
  let  count = top.java-type-words ;
  if (count < 1 | count > 2)
    error ("wrong word size for a popped JVM stack item %d", count)
  end;
  if (count = 2)
    if (instance? (poppee, <push-pop-model-metavar2>))
      jbb.stack-model := rest ;
      poppee.type-variable := top
    else
      if (*debug-jvm-instrs*)
	java-marker-op (jbb) ;
        format-out ("@@@@@ popping double word value into singleword metavar\n") 
      end;
      if (*break-on-non-verify*)
        my-break (jbb) 
      end
    end
  elseif (instance? (poppee, <push-pop-model-metavar2>))
    let  top2 = rest.head ;
    rest := rest.tail ;
    if (top2.java-type-words ~= 1)
      if (*debug-jvm-instrs*)
        format-out ("@@@@@ popping misaligned doubleword into doubleword metavar\n") ;
        java-marker-op (jbb) ;
        if (*break-on-non-verify*)
          my-break (jbb)
        end
      else
        error ("popping misaligned doubleword into doubleword metavar")
      end
    end;
    jbb.stack-model := rest ;
    poppee.type-variable := pair (top, top2) ;
    count := 2
  else
    jbb.stack-model := rest ;
    poppee.type-variable := top
  end;
  jbb.stack-depth := jbb.stack-depth - count ;
  count
end;  


define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, poppee :: <push-pop-model-metavar-checked>) => (words :: <integer>)
  let  pool :: <stretchy-vector> = jbb.meth.java-class.concrete-implementation.constants ;
  if (instance? (oper, <java-op2-frag>))
    let  index :: <integer> = oper.op ;
    let  constant = pool [index] ;
    if (instance? (constant, <java-class-constant>))
      let  tipe = constant.java-class ;  // want the class of the field!
      if (*debug-jvm-instrs* == #t)
        format-out ("@@@ popping a checkcast metavar, type %s\n", tipe) 
      end;
      let  model = jbb.stack-model ;
      if (instance? (model, <pair>))
        // was wrong:  poppee.type-variable := tipe ;
        jbb.stack-model := pair (tipe, model.tail) ;
        next-method ()   // this does the actual work
      else
        error ("empty stack in checkcast")
      end
    else
      error ("not a class constant in checkcast")
    end
  else
    error ("not a proper checkcast instruction type")
  end
end;

define method model-a-push (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-address>) => (words :: <integer>)
//format-out ("model pushing a return address\n") ;
  model-push-type (jbb, $java-return-address$)
end;


define method model-a-pop (jbb :: <java-basic-block>, oper :: <java-frag>, pushee :: <push-pop-model-address>) => (words :: <integer>)
  model-pop-a-type (jbb, $java-return-address$)
end;

define function maintain-stack-types (jbb :: <java-basic-block>, 
				      frag :: <java-frag>,
				      things-to-pop :: <list>,
				      things-to-push :: <list>) => ()
  let  depth :: <integer> = jbb.stack-depth ;
//format-out ("#!# depth before %d ", depth) ;
  for (poppee :: <push-pop-model> in things-to-pop)
    depth := depth - model-a-pop (jbb, frag, poppee) ;
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ ") ;
      for (i :: <integer> from 0 below depth + 1)
        format-out (". ")
      end;
      format-out ("%s popping %s\n", frag, poppee)
    end;
    if (negative? (depth))
//format-out ("!depth after! %d\n", depth) ;
      error ("JVM stack underflow internal error")
    end
  end;
  for (pushee :: <push-pop-model> in things-to-push)
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ ") ;
      for (i :: <integer> from 0 below depth + 1)
        format-out (". ")
      end;
      format-out ("%s pushing %s\n", frag, pushee)
    end;
    depth := depth + model-a-push (jbb, frag, pushee)
  end;
//format-out ("depth after %d\n", depth) ;
  if (*debug-jvm-instrs* == #t)  format-out ("@@@\n") end;
  if (depth > jbb.max-stack)
    if (depth > #xFFFF)
      error ("JVM stack overflow")
    end;
    jbb.max-stack := depth 
  end;
  jbb.stack-depth := depth 
end;



define function merge-stack-types (types1 :: <list>, types2 :: <list>)
  if (types1 == types2)  // optimize shared common stack
    types1
  else
    if (instance? (types1, <pair>) & instance? (types2, <pair>))
      let  rest = merge-stack-types (types1.tail, types2.tail) ;
      let  this = java-type-merge (types1.head, types2.head) ;
      // try to keep sharing structure
      if (this == types1.head & rest == types1.tail)
        types1
      elseif (this == types2.head & rest == types2.tail)
        types2
      else
        pair (this, rest)
      end
    elseif (empty? (types1) & empty? (types2))
      #()
    else
      if (*debug-jvm-instrs*)
        format-out ("@@@@@ failed to merge stack types\n") ;
        // can't do this!! java-marker-op (jbb) ;
        if (*break-on-non-verify*)
          my-break (types1)
        end
      else
        error ("failed to merge stack types") 
      end
    end
  end
end;



define function merge-local-var-types (vars1 :: <simple-object-vector>, vars2 :: <simple-object-vector>) => (result :: <simple-object-vector>)
  let  new = make (<vector>, size: $max-local-number$, fill: #f) ;
  for (i :: <integer> from 0 below $max-local-number$)
    let  type1 = if (vars1) vars1[i] end ;
    let  type2 = if (vars2) vars2[i] end ;
    new [i] := type1 & type2 & java-type-merge (type1, type2) ;
  end;
  new
end;


define function merge-bbs-types (src-bb :: <java-basic-block>,
                                 dest-bb :: <java-basic-block>)
  if (dest-bb.initial-local-var-types)
    if (*debug-jvm-instrs*)
      format-out ("@@@ augmenting stack models from bb to bb\n") 
    end;
    if (dest-bb.initial-stack-depth ~= src-bb.stack-depth)
      if (*debug-jvm-instrs*)
	format-out ("#################### mismatched JVM stack depths\n")
      else
	error ("mismatched JVM stack depths in Java backend")
      end
    end;

    dest-bb.initial-stack-model := 
      merge-stack-types (src-bb.stack-model,
                         dest-bb.initial-stack-model) ;
    dest-bb.initial-local-var-types :=
      merge-local-var-types (src-bb.local-var-types,
                             dest-bb.initial-local-var-types)
  else // first seen...
    if (*debug-jvm-instrs*)
      format-out ("@@@ copying stack models from bb to bb\n") 
    end;
    dest-bb.stack-depth  := dest-bb.initial-stack-depth  := src-bb.stack-depth ;
    dest-bb.stack-model  := dest-bb.initial-stack-model  := src-bb.stack-model ;
    if (src-bb.local-var-types)
      dest-bb.initial-local-var-types :=  copy-sequence (src-bb.local-var-types) ;
      dest-bb.local-var-types         :=  copy-sequence (src-bb.local-var-types) ;
    end
  end
end;




// the max-locals is not normally maintained on a per-instruction basis, but
// some low-level code generation (not using number-local-var) might
// set max-locals for a <java-basic-block>, so we propagate this out as
// well as the max stack depth.
define function finish-with-jbb (jbb :: <java-basic-block>, jmeth :: <java-method>) => ()
//  unless (*inside-bb-generation*)
//    error ("finish-with-jbb called outside bb generation")
//  end;
  if (*debug-jvm-instrs* == #t)
    format-out ("@@@ propagating pc %d to method\n", jbb.pc) 
  end;
  jmeth.pc := jbb.pc ;
  jmeth.max-stack  := max (jmeth.max-stack,  jbb.max-stack) ;
  jmeth.max-locals := max (jmeth.max-locals, jbb.max-locals) ;
  *inside-bb-generation* := #f ;
  if (*debug-jvm-instrs* == #t)
    format-out ("end bb\n")
  end;
  if (jbb.stack-depth > 0)
    // some bbs will have something on the stack
    // but I'll catch the obviously wrong ones this way
    format-out ("############# BB has non-empty stack at end!\n") ;
    for (ppm in jbb.stack-model, n from 0)
      format-out ("@@@%s:  %s\n", n, ppm)
    end;
    if (*break-on-non-verify*)
      my-break (jbb)
    end
  end
end;



define variable *check-stack-types* :: <boolean> = #t ;



define function ensure-stack-model (from-jbb :: <java-basic-block>, to-jbb :: <java-basic-block>) => ()
  if (*debug-jvm-instrs*)
    format-out ("@@@ ensure-stack-model\n") 
  end;
  if (*check-stack-types*)
    merge-bbs-types (from-jbb, to-jbb)
  else
    let  from-depth = from-jbb.stack-depth ;
    let  to-depth   = to-jbb.initial-stack-depth ;
    if (to-depth)
      if (to-depth ~= from-depth)
        if (*debug-jvm-instrs*)
	  java-marker-op (from-jbb) ;
          format-out ("#################### mismatched JVM stack depths\n")
        else
          error ("mismatched JVM stack depths in Java backend")
        end
      end
    else
      to-jbb.initial-stack-depth := from-depth ;
      to-jbb.stack-depth         := from-depth ;
    end
  end;
end;


// this reduces consing
define variable *java-simple-op-cache* = make (<simple-object-vector>, size: #x100) ;

// this is for all the argument-less 1-byte opcodes, reduce pointless allocation
// could just use an integer, of course!
define function java-simple-op (jbb :: <java-basic-block>, oper :: <java-abstract-bytecode>) => ()
  let  cache  = *java-simple-op-cache* ;
  let  opcde = oper.opcode ;
  let  cached-frag = cache [opcde] | (cache [opcde] := make (<java-frag>, opcode: oper)) ;
  add-bytecode (jbb, cached-frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, cached-frag, oper.pop-list, oper.push-list)
  else
    maintain-stack-depth (jbb, oper.push-count, oper)
  end
end;

define function java-marker-op (jbb :: <java-basic-block>) => ()
/*
  let  cache  = *java-simple-op-cache* ;
  let  opcde = j-nop.opcode ;
  let  cached-frag = cache [opcde] | (cache [opcde] := make (<java-frag>, opcode: j-nop)) ;
  add-bytecode (jbb, cached-frag) ;
  add-bytecode (jbb, cached-frag) ;
  add-bytecode (jbb, cached-frag)
*/
end;

define function java-op1-op (jbb :: <java-basic-block>, oper :: <java-abstract-bytecode>, op :: <integer>) => ()
  let  frag = make (<java-op1-frag>, opcode: oper, op: op) ;
  add-bytecode (jbb, frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, frag, oper.pop-list, oper.push-list)
  else
    maintain-stack-depth (jbb, oper.push-count, oper)
  end
end;


// this turns a spec  into a <java-constant> - isn't this really redundant?
define sealed generic make-java-constant (thing) => (const :: <java-constant>) ;

define method make-java-constant (thing :: <java-class>) => (const :: <java-class-constant>)
  make (<java-class-constant>, java-class: thing)
end;

// weird inconsistency in JVM, classes can be named by just name, array-classes 
// require a signature proper
define method make-java-constant (thing :: <java-array-type>) => (const :: <java-class-constant>)
  make (<java-class-constant>, java-class: thing)
end;

define method make-java-constant (thing :: <java-slot-spec>) => (const :: <java-slot-constant>)
  make (<java-slot-constant>, 
        java-class: make (<java-class-constant>, java-class: thing.java-class),
        nat:   make (<java-nat-constant>,
                     name: thing.slot-name, 
                     type: thing.slot-type))
end;

define method make-java-constant (thing :: <java-method-spec>) => (const :: <java-meth-constant>)
  make (<java-meth-constant>,
        java-class: make (<java-class-constant>, java-class: thing.java-class),
        nat:   make (<java-nat-constant>, 
                     name: thing.slot-name, 
                     type: thing.slot-type))
end;


define function java-op2-op (jbb :: <java-basic-block>, oper :: <java-abstract-bytecode>, op :: <integer>)
  let frag = make (<java-op2-frag>, opcode: oper, op: op, constants: jbb.constants) ;
  add-bytecode (jbb, frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, frag, oper.pop-list, oper.push-list)
  else
    maintain-stack-depth (jbb, oper.push-count, oper)
  end
end;

define function java-op2 (jbb :: <java-basic-block>, oper :: <java-abstract-bytecode>, thing)
  java-op2-op (jbb, oper, pool-index (make-java-constant (thing), jbb.meth.java-class.concrete-implementation))
end;

define function java-call (jbb :: <java-basic-block>, meth-spec :: <java-method-spec>)
  let  oper :: <java-call-bytecode> = meth-spec.invoke-op ;
  let  frag = make (<java-op2-frag>, 
                    opcode: oper,
                    op: pool-index (make-java-constant (meth-spec), jbb.meth.java-class.concrete-implementation),
                    constants: jbb.constants) ;
  add-bytecode (jbb, frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, frag, meth-spec.pop-list, meth-spec.push-list)
  else
    maintain-stack-depth (jbb, meth-spec.total-pushes, meth-spec.invoke-op)
  end
end;

define function java-if-call (jbb :: <java-basic-block>, meth-spec :: <java-method-spec>)
  let  oper :: <java-call-bytecode> = meth-spec.invoke-op ;
  let  frag = make (<java-op22-frag>, 
                    opcode: oper,
                    op:     pool-index (make-java-constant (meth-spec), jbb.meth.java-class.concrete-implementation),
                    nargs:  meth-spec.slot-type.java-function-arg-types.size,
                    constants: jbb.constants) ;
  add-bytecode (jbb, frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, frag, meth-spec.pop-list, meth-spec.push-list)
  else
    maintain-stack-depth (jbb, meth-spec.total-pushes, meth-spec.invoke-op)
  end
end;

// caller has to deal with instance for getfield
define function java-read (jbb :: <java-basic-block>, slot-spec :: <java-slot-spec>)
  // should test for 2-words too
  let  oper = if (slot-spec.static?) j-getstatic else j-getfield end;
  java-op2 (jbb, oper, slot-spec)
end;

// caller has to deal with instance for putfield
define function java-write (jbb :: <java-basic-block>, slot-spec :: <java-slot-spec>)
  // should test for 2-words too
  let  oper = if (slot-spec.static?) j-putstatic else j-putfield end;
  java-op2 (jbb, oper, slot-spec)
end;

define function java-imm (jbb :: <java-basic-block>, imm :: <integer>)
  let frag = make (<java-imm-frag>, imm-value: imm, meth: jbb.meth) ;
  add-bytecode (jbb, frag) ;
end;

define function java-branch-op (jbb :: <java-basic-block>, oper :: <java-abstract-bytecode>, dest)
  let frag = make (<java-branch-frag>, opcode: oper, dest: dest, meth: jbb.meth) ;
  add-bytecode (jbb, frag) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, frag, oper.pop-list, oper.push-list)
  else
    maintain-stack-depth (jbb, oper.push-count, oper)
  end;
  // now check/set dest block to have right stack depth
end;



define function output-bytecodes (outvec :: <byte-vector>, jbb :: <java-basic-block>, peecee :: <integer>)
  if (peecee ~= jbb.the-label.pc)
    format-out ("warn: bytecode PCs don't match up %s %s\n", peecee, jbb.the-label.pc)
  end;
  let  bcodes :: <simple-object-vector> = jbb.bytecodes ;
  let  icnt :: <integer> = jbb.icount ;
  for (n :: <integer> from 0 below icnt)
    peecee := output-frag (peecee, outvec, bcodes[n])
  end;
  peecee
end;




// some code gen utilities


define constant $j-local-var-pushes = 
  vector (vector (j-iload-0, j-iload-1, j-iload-2, j-iload-3, j-iload),
          vector (j-lload-0, j-lload-1, j-lload-2, j-lload-3, j-lload),
          vector (j-fload-0, j-fload-1, j-fload-2, j-fload-3, j-fload),
          vector (j-dload-0, j-dload-1, j-dload-2, j-dload-3, j-dload),
          vector (j-aload-0, j-aload-1, j-aload-2, j-aload-3, j-aload)) ;

define constant $j-local-var-pops = 
  vector (vector (j-istore-0, j-istore-1, j-istore-2, j-istore-3, j-istore),
          vector (j-lstore-0, j-lstore-1, j-lstore-2, j-lstore-3, j-lstore),
          vector (j-fstore-0, j-fstore-1, j-fstore-2, j-fstore-3, j-fstore),
          vector (j-dstore-0, j-dstore-1, j-dstore-2, j-dstore-3, j-dstore),
          vector (j-astore-0, j-astore-1, j-astore-2, j-astore-3, j-astore)) ;


define function emit-local-var-op
  (jbb :: <java-basic-block>, offset :: <integer>, jtype :: <integer>, opv :: <simple-object-vector>)
  let  ops :: <simple-object-vector> = opv [jtype] ;
  if (offset < 4)
    java-simple-op (jbb, ops [offset])
  else
    if (offset >= #x100)
      java-simple-op (jbb, j-wide) ;
      java-op1-op (jbb, ops [4], ash (offset, -8)) ;
      java-imm (jbb, logand (offset, #xff))
    else
      java-op1-op (jbb, ops [4], offset)
    end
  end
end;

define function emit-ret (jbb :: <java-basic-block>, offset :: <integer>)
  if (offset >= #x100)
    java-simple-op (jbb, j-wide) ;
    java-op1-op (jbb, j-ret, ash (offset, -8)) ;
    java-imm (jbb, logand (offset, #xff))
  else
    java-op1-op (jbb, j-ret, offset)
  end
end;




define function emit-java-ldc (jbb :: <java-basic-block>, ind :: <integer>) => ()
  if (ind < #x100)
    java-op1-op (jbb, j-ldc1, ind)
  else
    java-op2-op (jbb, j-ldc2, ind)
  end
end;


define function emit-java-int (jbb :: <java-basic-block>, int :: <integer>) => ()
  if (int <= 5 & int >= -1)
    java-simple-op (jbb, j-iconsts [int + 1])
  else
    if (int < #x80 & int >= - #x80)
      java-op1-op (jbb, j-bipush, int)
    else
      if (int < #x8000 & int >= - #x8000)
	java-op2-op (jbb, j-sipush, int) ;
      else
        let  rep = make (<java-int-constant>, value: int) ;
	let  ind = pool-index (rep, jbb.meth.java-class.concrete-implementation) ;
        emit-java-ldc (jbb, ind)
      end
    end
  end
end;

define function emit-java-string (jbb :: <java-basic-block>, str :: <string>) => ()
  let  con = make (<java-string-constant>,
                   utf: make (<java-utf-constant>, string: str.uniq));
  emit-java-constant-load (jbb, con)
end;



define open generic emit-java-constant-load (jbb :: <java-basic-block>, const) => () ;

// handle constants
define method emit-java-constant-load (jbb :: <java-basic-block>, const :: <java-constant>) => ()
  emit-java-ldc (jbb, pool-index (const, jbb.meth.java-class.concrete-implementation))
end;

// handle some common types
define method emit-java-constant-load (jbb :: <java-basic-block>, flt :: <single-float>) => ()
  emit-java-ldc (jbb, pool-index (make (<java-float-constant>, value: flt), jbb.meth.java-class.concrete-implementation))
end;

define method emit-java-constant-load (jbb :: <java-basic-block>, flt :: <double-float>) => ()
  emit-java-ldc (jbb, pool-index (make (<java-double-constant>, value: flt), jbb.meth.java-class.concrete-implementation))
end;


define function emit-push-local (jbb :: <java-basic-block>, offset :: <integer>, jtype :: <integer>)
  emit-local-var-op (jbb, offset, jtype, $j-local-var-pushes)
end;

define function emit-push-this (jbb :: <java-basic-block>)
  emit-local-var-op (jbb, 0, j-ref-code, $j-local-var-pushes) // j-code-for (jbb.meth.java-class)
end;

define function emit-pop-local (jbb :: <java-basic-block>, offset :: <integer>, jtype :: <integer>)
  emit-local-var-op (jbb, offset, jtype, $j-local-var-pops)
end;

define function emit-pop (jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-pop)
end;

define function emit-dup (jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-dup)
end;

define function emit-swap (jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-swap)
end;

define constant j-returns = vector (j-ireturn, j-lreturn, j-freturn, j-dreturn, j-areturn, j-return) ;

define function emit-return (jbb :: <java-basic-block>, jtype :: <integer>) => ()
  java-simple-op (jbb, j-returns[jtype])
end;


// handle a slot
define method emit-java-constant-load (jbb :: <java-basic-block>, const :: <java-slot-spec>) => ()
  if (const.static?)
    java-read (jbb, const)
  else
    emit-push-this (jbb) ;
    java-read (jbb, const)
  end
end;

format-out ("inited java-emit-code.dylan\n") ;
