Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *print-insns* = #f;
define variable *break-methods* = #f ;

// not normally used
define function show-bb-progress (node :: <computation>, uenv :: <list>) => ()
  if (instance? (node, <bind>))
    if (*break-methods*)
      my-break (node)
    end;
    format-out ("\n") ;
    pprint-dfms (node.next-computation, #f, 1) ;
    format-out ("\n") ;
    desc-dfm (node) ;
    format-out ("\n\n")
  end;

  if (*print-insns*)
    let  depth = uenv.size ;
    format-out ("BB-pass  ") ;
    for (i :: <integer> from 0 below depth)
      format-out ("     ")
    end;
    format-out ("%s %s\n", node.object-class, node) 
  end;
  #f
end;


// in order to accumulate a more BB-oriented view of the code,
// we need a notion of a merge with a sense
define sealed abstract class <faked-transfer> (<object>)
  sealed slot merge      :: <binary-merge>, required-init-keyword: merge: ;
end;

define abstract class <merge-transfer> (<faked-transfer>)
  sealed slot temp :: <temporary>, required-init-keyword: temp: ;
end;

define class <merge-transfer-l> (<merge-transfer>) end ;
define class <merge-transfer-r> (<merge-transfer>) end ;

// ARSE this is SEALED.  BUM!
define constant $faked-transfer-accs$ =   
  list (make (<temporary-accessors>, getter: computation-value, setter: computation-value-setter)) ;
define method used-temporary-accessors (et :: <faked-transfer>) => (list :: <list>)
  $faked-transfer-accs$
end;

define function left-merge (merge :: <binary-merge>) => (mt :: <merge-transfer>)
  let  temp = merge.temporary ;
  let  temp-copy = make (<merging-temporary>,
                         actual-temporary: temp,
                         environment: merge.environment) ;
  temp-copy.users := copy-sequence (temp.users) ;                        
  let  merge-transfer = make (<merge-transfer-l>, merge: merge, temp: temp-copy) ;
  temp-copy.generator := merge-transfer ;
  merge-transfer
end;

define function right-merge (merge :: <binary-merge>) => (mt :: <merge-transfer>)
  let  temp = merge.temporary ;
  let  temp-copy = make (<merging-temporary>,
                         actual-temporary: temp,
                         environment: merge.environment) ;
  temp-copy.users := copy-sequence (temp.users) ;                        
  let  merge-transfer = make (<merge-transfer-r>, merge: merge, temp: temp-copy) ;
  temp-copy.generator := merge-transfer ;
  merge-transfer
end;

// ARSE, SEALED AGAIN
define method temporary (ft :: <faked-transfer>) => (tmp :: <temporary>)
  ft.merge.temporary
end;
define method temporary (mt :: <merge-transfer>) => (tmp :: <temporary>)
  mt.temp
end;
// ARSE, SEALED AGAIN
define method computation-value (mt :: <merge-transfer-l>) => (tmp :: <temporary>)
  mt.merge.merge-left-value
end;
define method computation-value (mt :: <merge-transfer-r>) => (tmp :: <temporary>)
  mt.merge.merge-right-value
end;



define class <exit-transfer> (<faked-transfer>)
  sealed slot exit     :: <exit>,            required-init-keyword: exit: ;
end;

define function exit-merge (merge :: <bind-exit-merge>, exit :: <exit>) => (et :: <exit-transfer>)
  make (<exit-transfer>, merge: merge, exit: exit)
end;

// ARSE, SEALED AGAIN
define method computation-value (et :: <exit-transfer>) => (tmp :: <temporary>)
  et.exit.computation-value
end;


/* was needed to prevent BB duplication on <binary-merge>s ?
define function seen-bb-before (seen :: <list>, c :: <computation>) => (seen? :: <boolean>)
  block (return)
    for (bb :: <pair> in seen)
      let  nodelist :: <list> = bb.head ;
      if (nodelist.size > 0 & nodelist[0] == c)
        return (#t) ;
      end
    end;
    #f
  end
end;
*/





define sealed class <bb-collection> (<object>)
  sealed slot  label-tab :: <object-table> = make (<object-table>) ;
  sealed slot  bb-vec :: <stretchy-vector> = make (<stretchy-vector>) ;
  sealed slot  seqnum :: <integer> = 0 ;
  sealed slot  linearize-seq :: <integer> = 0 ;
  sealed slot  protected-blocks :: <list> = #() ;
end ;

define sealed class <dylan-bb> (<object>)
  sealed slot  comp-list :: <stretchy-vector> = make (<stretchy-vector>) ;
  sealed slot  preds :: <list> = #() ;
  sealed slot  succs :: <list> = #() ;
  sealed slot  seqnum :: <integer> = 0 ;
  sealed slot  complete? :: <boolean> = #f ;
  sealed slot  unwind-env :: <list> = #() ;
  sealed slot  collection :: <bb-collection>, required-init-keyword: collection: ;
  sealed slot  linearize-seq :: <integer> = -1 ;
  sealed slot  java-bb :: false-or (<java-basic-block>) = #f ;
  sealed slot  handler? :: <boolean> = #f ;
end;

define method print-object (dbb :: <dylan-bb>, str :: <stream>) => ()
  format (str, "<dylan-bb %d,%d>", dbb.seqnum, dbb.linearize-seq)
end;

define class <fake-bb> (<dylan-bb>)
  inherited slot  complete? = #t ;
end;

// extend branch resolution protocol
define method resolve-branch-dest (thing :: <dylan-bb>, meth :: <java-method>, peecee :: <integer>) 
 => (offset :: <integer>)
  bb-label (thing, meth).pc - peecee
end;



define sealed generic gen-one-bb (jmeth :: <java-method>, gen :: <function>, arg1, arg2) => ();


define thread variable *entry-stack-model* = #f ;

define method gen-one-bb (jmeth :: <java-method>, gen :: <function>, bb :: <dylan-bb>, type) => ()
  // this function is called once for every basic-block in the method,
  // and label resolution relies on a one-one correspondance
  // between the jbb's that make-jbb constructs and meth.basic-blocks
  
  if (bb.comp-list.empty?)
    jmeth.label-table [bb] := #"fall-through" ;
  else
    let  jbb = make-jbb (jmeth) ;
    bb.java-bb := jbb ;
    if (type == #"entry")
      if (*debug-jvm-instrs* == #t)
        format-out ("@@@ capturing entry stack model\n") 
      end;
      jbb.initial-stack-depth := 0 ;
      jbb.stack-depth         := 0 ;
      if (*check-stack-types*)
        jbb.initial-local-var-types := *entry-stack-model* ;
        jbb.local-var-types         := *entry-stack-model*
      end
    elseif (type == #"handler")
      jbb.initial-stack-depth := 1 ;
      jbb.stack-depth         := 1
    end;
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ about to scan prev bbs to ensure-stack-model...%s\n", bb.preds) 
    end;
    for (prev-bb :: <dylan-bb> in bb.preds)
      if (*debug-jvm-instrs* == #t)
        format-out ("@@@ a prev bb\n") 
      end;
      let  from-jbb = prev-bb.java-bb ;
      if (from-jbb & from-jbb ~== jbb)
        ensure-stack-model (from-jbb, jbb)
      elseif (*debug-jvm-instrs* == #t)
        format-out ("@@@ a null prev bb\n")
      end
    end;
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ ... scanned prev bbs to ensure-stack-model\n") 
    end;
    begin
      jmeth.label-table [bb] := jbb ;
      gen (jbb, bb)
    end;
    finish-with-jbb (jbb, jmeth) ;

// currently I don't think I generate code that leaves stuff on the stack
// across BB boundaries...
    if (jbb.stack-depth > 0)
      format-out ("############# BB has non-empty stack at end!\n")
    end;
// was here
    for (next-bb :: <dylan-bb> in bb.succs)
      let  to-jbb = next-bb.java-bb ;
      if (to-jbb)
        ensure-stack-model (jbb, to-jbb)
      end
    end
  end;
  #f
end;

define method gen-one-bb (jmeth :: <java-method>, gen :: <function>, handlur :: <java-handler>, arg2) => ()
  // this function is called to generate a handler
  // and no <dylan-bb> is involved
  let  jbb = make-jbb (jmeth) ;
  jbb.initial-stack-depth := 1 ;  // the thrown thing
  jbb.stack-depth := 1 ;  // the thrown thing
  begin
    gen (jbb, handlur)
  end;
  finish-with-jbb (jbb, jmeth) ;
  #f
end;

// used for ordinary lambdas
define function gen-from-dfmc-bb (jbb :: <java-basic-block>, bb :: <dylan-bb>)
  process-bb (jbb.meth.java-class, bb, jbb)
end;

define thread variable *emit-returns* :: <boolean> = #t ;

// used for the fake lambdas that are really portions of init code:
// inhibit actually returning 
define function gen-from-dfmc-bb-inline (jbb :: <java-basic-block>, bb :: <dylan-bb>)
  dynamic-bind (*emit-returns* = #f)
    process-bb (jbb.meth.java-class, bb, jbb)
  end dynamic-bind
end;

define thread variable *jmc* = #f ;
// duplicate // define thread variable *uenv* = #() ;


define function process-bbs  (meth :: <java-method>, bbcoll :: <bb-collection>)
  linearize-bbs (bbcoll) ;
  let  bbvec = bbcoll.bb-vec ;
  let  bb :: <dylan-bb> = bbvec [0] ;
  if (*print-insns*)  print-a-bb (bb) end;
  gen-one-bb (meth, gen-from-dfmc-bb, bb, #"entry") ;
  for (n :: <integer> from 1 below bbvec.size)
    bb := bbvec [n] ;
    if (*print-insns*)  print-a-bb (bb) end;
    gen-one-bb (meth, gen-from-dfmc-bb, bb, if (bb.handler?) #"handler" end)
  end
end;


define function print-a-bb (thing)
  format-out ("\nBBgen:\n");
  for (ins in thing.head)
    format-out ("   %s  %s\n", ins.object-class, ins)
  end;
  format-out ("\n")
end;


// things that can side-effect local vars?
define method has-side-effect (comp :: <computation>)    #f end;
define method has-side-effect (comp :: <faked-transfer>) #f end;
define method has-side-effect (comp :: <set-cell-value!>)   #t end;
define method has-side-effect (comp :: <set!>)             #t end;
// seem to need this for now, stil haven't debugged the side-effect-call stuff
define method has-side-effect (comp :: <call>)             #t end;
define method has-side-effect (comp :: <loop-call>)          #t end; // was #f ...
define method has-side-effect (comp :: <primitive-call>)     #f end;
define method has-side-effect (comp :: <multiple-value-spill>)     #t end;
define method has-side-effect (comp :: <multiple-value-unspill>)   #t end;
define method has-side-effect (comp :: <values>)
  comp.rest-value | (comp.fixed-values.size > 1)
end;
define method has-side-effect (comp :: <unwind-protect>)      #t end;
define method has-side-effect (comp :: <end-protected-block>) #t end;
define method has-side-effect (comp :: <end-cleanup-block>)   #t end;
define method has-side-effect (comp :: <slot-value-setter>)   #t end;
define method has-side-effect (comp :: <repeated-slot-value-setter>)   #t end;
// closure creation mustn't move if/lambda-boundaries!
define method has-side-effect (comp :: <make-closure>)        #t end;
define method has-side-effect (comp :: <initialize-closure>)  #t end;

// things that affect heap, must remain ordered
define method side-effect-call? (comp :: <computation>)          #f end;
define method side-effect-call? (comp :: <faked-transfer>)       #f end;
define method side-effect-call? (comp :: <call>)                   #t end;
define method side-effect-call? (comp :: <loop-call>)                #t end; // was #f
define method side-effect-call? (comp :: <primitive-call>)           #f end;
define method side-effect-call? (comp :: <multiple-value-spill>)   #t end;
define method side-effect-call? (comp :: <multiple-value-unspill>) #t end;

// actually need to know that things that affect heap
// could change variable bindings, whereas currently only set! is 
// assumed to do that (I think)


define function table-concatenate (tab0 :: <object-table>, #rest others :: <object-table>) => (result :: <object-table>)
  let  new = make (<object-table>) ;
  begin
    let  (state, lim, next, fin, key, elt) = tab0.forward-iteration-protocol;
    until (fin (tab0, state, lim))
      let  v = elt (tab0, state) ;
      let  k = key (tab0, state) ;
      new [k] := v ;
      state := next (tab0, state)
    end
  end;
  let  unique = pair (#f, #f) ;
  for (tab :: <object-table> in others, n :: <integer> from 0)
    let  (state, lim, next, fin, key, elt) = tab.forward-iteration-protocol;
    until (fin (tab, state, lim))
      let  v = elt (tab, state) ;
      let  k = key (tab, state) ;
      if (element (new, k, default: unique) ~== unique)
	error ("table-concatenate saw duplicates")
      end;
      new [k] := v ;
      state := next (tab, state)
    end
  end;
  new
end;

define variable *debug-java-walk* = #f ;

define function process-bb (jc :: <java-concrete-class>, bb :: <dylan-bb>, jbb :: <java-basic-block>)
 => ()
  internal-process-bb (jc, bb.comp-list, bb.unwind-env, jbb) ;
  let  successor-bbs = bb.succs ;
  if (successor-bbs.size = 1)
    let  succ-bb = successor-bbs.first ;
    if (next-bb (bb) ~== succ-bb)
      java-branch-op (jbb, j-goto, succ-bb)
    end
  end
end;


define function internal-process-bb (jc :: <java-concrete-class>, computations, uenv, jbb :: <java-basic-block>) => ()

  let  temps = make (<object-table>) ;
  for (comp :: <computation> in computations)
    let  val = comp.temporary ;
    // not sure at all about this logic...
    if (val &
	  (~ closed-over? (val)) &
//	  (~ indirect? (val)) &      // this now broken, what did it do?
	  empty? (val.assignments))
      temps[val] := #t ;
    end
  end;

  if (*debug-java-walk*)
    format-out ("\n\n  BASIC BLOCK START\n\n")
  end;

  let  trees = make (<stretchy-vector>) ;  // current set of expression trees to process
  let  tree-sets = make (<object-table>);         // maps computation to a set of (side-effecting-)calls it uses
  let  cell-sets = make (<object-table>) ;   // map from distinquished computation to a set of cells it "get-cell-value"s

  let  last-comp = #f ;
  let  side-effect-calls = make (<stretchy-vector>) ;  // ordering over the side-effect calls
  let  first-call-in-tree = make (<object-table>) ;  // map from tree's distinquished computation to 
                                              // sequence number of first side-effect call

  local
      method actually-emit (c, nodes)
        if (*debug-java-walk*)
          format-out ("actual emit %s, node-count %d\n", c, nodes.size) 
        end;
        emit-expression-tree-and-store (jbb, c, nodes) ;
      end,

      method splurge-out-a-tree (comp)
	let  used-trees = #() ;
        let  nodes = tree-sets [comp] ;

	// first ensure any expressions containing arbitrary calls that come
	// first are actually emitted beforehand.
	for (i :: <integer> from 0 below first-call-in-tree[comp])
	  let  call = side-effect-calls[i] ;
	  for (c :: <computation> in trees)
	    if (member? (call, tree-sets[c]))  // if a pending tree contains an earlier call
	      actually-emit (c, nodes) ;
	      used-trees := pair (c, used-trees)
	    end
	  end;
	  for (c :: <computation> in used-trees)  // defer removal until end of iteration
	    trees := remove! (trees, c)
	  end;
	  used-trees := #()
	end;

	// secondly ensure that all expressions reading a local var that we set are
	// emitted before they see the new value that they shouldn't

	if (instance? (comp, <set-cell-value!>))
	  let  the-cell    = comp.computation-cell.generator ;
	  for (c :: <computation> in trees)
	    if (c ~== comp)
	      let  cells = cell-sets[c] ;
	      if (cells & element (cells, the-cell, default: #f))
		actually-emit (c, nodes) ;
		used-trees := pair (c, used-trees) 
	      end
	    end
	  end;
	  for (c :: <computation> in used-trees)
    if (*debug-java-walk*) format-out ("+++ removing node %s\n", comp) end;
	    trees := remove! (trees, c)
	  end
	end;
	    
	// can now output this expression

	actually-emit (comp, nodes);  // check new-set is eq tree-sets[comp]
	tree-sets [comp] := #f ;
    if (*debug-java-walk*) format-out ("+++ removing node %s\n", comp) end;
	trees := remove! (trees, comp)
      end;

  // initially remember pc for protected forms

  let  start-pc = jbb.pc ;


  // we scan the computations in the bb gluing together trees where
  // possible, and outputing them only when forced
  for (comp :: <computation> in computations)
    if (comp == #f)
      format-out ("computation is #f in bb, %s\n", computations)
    end;

    // bugger, this doesn't let the test expression be inlined!!
//    if (instance? (comp, <if>))
//      until (trees.empty?)
//        splurge-out-a-tree (trees.first)
//      end
//    end;

    last-comp := comp ;
    let  out = comp.temporary ;
    let  new-set = make (<stretchy-vector>) ;
    let  new-cells = make (<object-table>) ;  // full of <make-cell> nodes
    let  n = 0 ;
    let  first-call = 999999 ;  // large value, we want the min.
    // catch any trees whose values are used by > one temp
    do-used-value-references
      (method (i)
         if (instance? (i, <temporary>))
           if (i.users.size > 1)
             let icomp = i.generator ;
             if (member? (icomp, trees))
               if (*debug-java-walk*)
                 format-out ("spurge because multiple users %s %s %d\n",
                             icomp, i, i.users.size) 
               end;
               splurge-out-a-tree (icomp)
             end
           end
         end
       end,
       comp);
    // now merge new node with existing trees if sensible
    do-used-value-references
      (method (i)
	 if (instance? (i, <temporary>))
	   let  icomp = i.generator ;
	   if (icomp == #f)
//	     format-out ("computation with no generator: %s\n", i) ;
//           lexical variables, I believe, fall into this camp
	   end;
	   if (member? (icomp, trees))
	     // here we glue subtrees together maintaining state
             if (*debug-java-walk*)
               format-out ("\nmerge trees %s\n",comp) ;
               format-out ("egrem trees %s\n\n",icomp) ;
             end;
	     new-set := concatenate (new-set, tree-sets[icomp]) ;
	     new-set := add! (new-set, icomp) ;
	     new-cells := table-concatenate (new-cells, cell-sets[icomp]) ;
	     tree-sets [icomp] := #() ;
	     cell-sets  [icomp] := #f ;
	     trees := remove! (trees, icomp);
	     first-call := min (first-call, first-call-in-tree[icomp])
	   end
	 end;
	 n = n + 1
       end,
       comp);
    if (*debug-java-walk*) format-out ("+++ adding node %s\n", comp) end;
    trees := add! (trees, comp) ;
    tree-sets [comp] := new-set ;

    // maintain the info on which trees read which local cells
    if (instance? (comp, <get-cell-value>))
      new-cells[comp.computation-cell.generator] := #t
    end;
    cell-sets [comp]  := new-cells ;

    // record side-effecting calls, so we can enforce their ordering later
    if (comp.side-effect-call?)
//      format-out ("identified side-effect-call %d as %s  ", side-effect-calls.size, comp);
      first-call := min (first-call, side-effect-calls.size) ;
//      format-out ("  first-call=%d\n", first-call);
      side-effect-calls := add! (side-effect-calls, comp)
    end;
    first-call-in-tree[comp] := if (first-call = 999999) 0 else first-call end;
    // 0 means don't force anything to be output

    // we may have to emit now if this is some sort of assignment
    // (although could probably schedule better with more effort)
    if ((~out) | 
        (~ element (temps, out, default: #f)) |
        comp.has-side-effect
        )
      splurge-out-a-tree (comp)
    end;
  end;

  // have scanned every computation in the BB, now clear the pending queue
  // thus forcing everything to emit.
  let  any-last-if-node = #f ;
  until (trees.empty?)
    let  c = trees.first ;
    if (instance? (c, <if>))
      any-last-if-node := c ;
      trees := remove! (trees, c)
    else
      splurge-out-a-tree (trees.first)
    end
  end;
  // ensure control flow <if> node is last
  if (any-last-if-node)
    splurge-out-a-tree (any-last-if-node)
  end;

  maintain-protect-ranges (start-pc, jbb.pc, uenv) ;

  if (*debug-java-walk*)
    format-out ("\n\n  BASIC BLOCK END\n\n")
  end

end;




define sealed generic fall-through-comp (c :: <object>) => (fall-thru :: false-or (<computation>)) ;

define method fall-through-comp (c :: <computation>) => (fall-thru :: false-or (<computation>))    c.next-computation end;
define method fall-through-comp (c :: <faked-transfer>) => (fall-thru :: false-or (<computation>)) c.merge.next-computation end;
define method fall-through-comp (c :: <loop-call>) => (fall-thru :: false-or (<computation>))      #f end;
define method fall-through-comp (c :: <exit>) => (fall-thru :: false-or (<computation>))           #f end;
define method fall-through-comp (c :: <end-cleanup-block>) => (fall-thru :: false-or (<computation>)) #f end;


define function maintain-protect-ranges (start-pc :: <integer>, end-pc :: <integer>, uenv :: <list>)
  while ((~uenv.empty?) & instance? (uenv.head, <entry-state>))
    uenv := uenv.tail
  end;
  if (end-pc ~== start-pc & ~uenv.empty?)
    let  protect = uenv.head ;
    let  rangelist = protect.finally-handler-ranges ;
    block (return)
      if (~rangelist.empty?)
        let  prev-entry = rangelist.head ;
        if (instance? (prev-entry, <pair>))
          let prev-end = rangelist.head.tail ;
          if (prev-end == start-pc)
            rangelist.head.tail := end-pc ;
            return ()
          end
        end
      end;
      protect.finally-handler-ranges := pair (pair (start-pc, end-pc), rangelist)
    end;
    let  parent = uenv.tail ;

    while ((~parent.empty?) & instance? (parent.head, <entry-state>))
      parent := parent.tail
    end;

    while (~parent.empty?)
      let parent-protect = parent.head ;
      let parent-rl = parent-protect.finally-handler-ranges ;
      if (parent-rl.empty?)
        parent-protect.finally-handler-ranges := pair (protect, parent-rl)
      else
        if (parent-rl.head ~== protect)
          parent-protect.finally-handler-ranges := pair (protect, parent-rl)
        end
      end;
      uenv := parent ;
      protect := parent-protect ;
      parent := parent.tail ;
      while ((~parent.empty?) & instance? (parent.head, <entry-state>))
        parent := parent.tail
      end
    end
  end
end;


// not normally used
define function describe-bbs (bbs-seen)
  format-out ("Basic Block Dump:\n") ;
  for (bb in bbs-seen)
    format-out ("Basic Block:\n\n") ;
    for (comp :: <computation> in bb.head)
      format-out ("  %s %s\n", comp.object-class, comp) ;
    end
  end;
  format-out ("\n\n")
end;

/*
define function bb-label-node (node :: <computation>, meth :: <java-method>) => (lab :: <integer>)
  let  labels = meth.label-table ;
  labels[node].label | error ("WHOOPS, unlabelled node %s", node)
end;
*/


define function bb-label (bb :: <dylan-bb>, meth :: <java-method>) => (lab :: <java-label>)
  let  labels = meth.label-table ;
  let  value = labels[bb] ;
  if (instance? (value, <java-basic-block>))
    value.the-label
  else
    if (value == #"fall-through")
      if (~ (bb.comp-list.empty?))
        format-out (" unexpected valid bb as fall-through case")
      end;
      if (bb.succs.size ~= 1)
        format-out (" unexpected succs-length of %s for fall-through case", bb.succs.size)
      end;
      bb-label (bb.succs.first, meth)
    else 
      error ("WHOOPS, unlabelled bb %s", bb)
    end
  end;
end;


// New BB gathering stuff, more sane now there is a definite tree-structure
// dominators and all that



define thread variable *uenv* :: <list> = #() ;
define thread variable *udepth* :: <integer> = 0 ;
define thread variable *uenv-mapping* :: <object-table> = make (<object-table>) ;

define variable *print-bbs* = #f ;

define function identify-bbs-top-top-level (c :: <bind>)
  let  bbs = make (<bb-collection>) ;
  let  first-bb = new-bb (bbs) ;
  linearize-stamp (bbs, first-bb) ;
  dynamic-bind (*uenv* = #())
    dynamic-bind (*udepth* = 0)
      dynamic-bind (*uenv-mapping* = make (<object-table>))
        let  last-bb  = collect-bbs (bbs, first-bb, c.next-computation, #f) ;
        if (*print-bbs*)
          describe-dylan-bbs (bbs) 
        end;
        values (bbs, first-bb, last-bb, *uenv-mapping* /* bbs.protected-blocks */)
      end dynamic-bind
    end dynamic-bind
  end dynamic-bind
end;


define function record-uenv-level (comp :: <computation>)
  *uenv-mapping* [comp] := pair (*udepth*, *uenv*)
end;

define function get-uenv-level (comp :: <computation>) => (depth :: <integer>, uenv :: <list>)
  let  cons = *uenv-mapping* [comp] ;
  values (cons.head, cons.tail)
end;

define sealed generic collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <computation>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>) ;


// most computations do this
define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <computation>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  add! (then-bb.comp-list, dfm) ;
  collect-bbs-check (bbs, then-bb, dfm.next-computation, uptil)
end;

define sealed generic raw-false? (obj) => (res :: <boolean>) ;
define method raw-false? (obj) => (res :: <boolean>)
  #f
end ;

define method raw-false? (obj :: <&raw-boolean>) => (res :: <boolean>)
  ~ obj.^raw-object-value
end;

define function collect-bbs-if-special (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <if>, uptil :: false-or (<computation>), next-if :: <if>) => (now-bb :: <dylan-bb>)
  let  con = dfm.consequent ;
  let  alt = dfm.alternative ;
  let  merge = dfm.next-computation ;
  add! (then-bb.comp-list, dfm) ;
  *unwind-handlers* [dfm] := then-bb ;
  let  con-bb = #f ;
  let  alt-bb = #f ;
  let  con-end-bb = #f ;
  let  alt-end-bb = #f ;
  if (con ~== merge)
    con-bb := new-bb (bbs) ;
    linearize-stamp (bbs, con-bb) ;
    con-end-bb := collect-bbs-check (bbs, con-bb, con, merge)
  end ;
  if (alt ~== merge)
    alt-bb := new-bb (bbs) ;
    linearize-stamp (bbs, alt-bb) ;
    alt-end-bb := collect-bbs-check (bbs, alt-bb, alt, merge)
  end ;
  let  con-temp = merge.merge-left-value ;
  let  alt-temp = merge.merge-right-value ;
  if (con-bb == #f &
      instance? (con-temp, <object-reference>))
//    format-out ("con-temp.reference-value = %s %s\n", con-temp.reference-value, con-temp.reference-value.object-class) ;
    con-bb := if (con-temp.reference-value == #f) #"alt" else #"con" end ;
  else
    if (~ con-bb) 
      con-end-bb := con-bb := new-bb (bbs) ;
      linearize-stamp (bbs, con-bb)
    end ;
    let  the-left-merge = left-merge (merge) ;
    let  next-if2 = make (<if>, test: the-left-merge.temporary,
                                consequent:  next-if.consequent,
                                alternative: next-if.alternative,
                                environment: next-if.environment) ;
    add! (con-end-bb.comp-list, the-left-merge) ;
    add! (con-end-bb.comp-list, next-if2) ;
    *unwind-handlers* [next-if2] := con-end-bb ;
  end;
  if (alt-bb == #f &
      instance? (alt-temp, <object-reference>))
//    format-out ("alt-temp.reference-value = %s %s\n", alt-temp.reference-value, alt-temp.reference-value.object-class) ;
    alt-bb := if (alt-temp.reference-value == #f) #"alt" else #"con" end ;
  else
    if (~ alt-bb)
      alt-end-bb := alt-bb := new-bb (bbs) ;
      linearize-stamp (bbs, alt-bb)
    end ;
    let  the-right-merge = right-merge (merge) ;
    let  next-if2 = make (<if>, test: the-right-merge.temporary,
                                consequent:  next-if.consequent,
                                alternative: next-if.alternative,
                                environment: next-if.environment) ;
    add! (alt-end-bb.comp-list, the-right-merge) ;
    add! (alt-end-bb.comp-list, next-if2) ;
    *unwind-handlers* [next-if2] := alt-end-bb ;
  end;
  
  // generate the next if, with next-con-bb, next-alt-bb
  let  escape-bb = new-bb (bbs); 
  let  next-con-bb = escape-bb ;
  let  next-alt-bb = escape-bb ;
  let  next-con-end-bb = #f ;
  let  next-alt-end-bb = #f ;
  let  next-merge  = next-if.next-computation ;
  if (next-if.consequent ~== next-merge)
    next-con-bb := new-bb (bbs) ;
    linearize-stamp (bbs, next-con-bb) ;
    next-con-end-bb := collect-bbs-check (bbs, next-con-bb, next-if.consequent, next-merge);
    if (next-merge.temporary & ~ next-con-end-bb.complete?)
      add! (next-con-end-bb.comp-list, left-merge (next-merge))
    end;
    if (~ (next-con-end-bb.complete?))
      bb-link (next-con-end-bb, escape-bb)
    end
  end;
  if (next-if.alternative ~== next-merge)
    next-alt-bb := new-bb (bbs) ;
    linearize-stamp (bbs, next-alt-bb) ;
    next-alt-end-bb := collect-bbs-check (bbs, next-alt-bb, next-if.alternative, next-merge);
    if (next-merge.temporary & ~ next-alt-end-bb.complete?)
      add! (next-alt-end-bb.comp-list, right-merge (next-merge))
    end;
    if (~ (next-alt-end-bb.complete?))
      bb-link (next-alt-end-bb, escape-bb)
    end
  end;

  // link two ifs together
  bb-link (then-bb, if (con-bb == #"con")
                      next-con-bb
                    elseif (con-bb == #"alt")
                      next-alt-bb
                    else 
                      con-bb
                    end) ;
  bb-link (then-bb, if (alt-bb == #"con")
                      next-con-bb
                    elseif (alt-bb == #"alt")
                      next-alt-bb
                    else 
                      alt-bb
                    end) ;
  if (con-end-bb)
    bb-link (con-end-bb, next-con-bb) ;
    bb-link (con-end-bb, next-alt-bb)
  end ;
  if (alt-end-bb)
    bb-link (alt-end-bb, next-con-bb) ;
    bb-link (alt-end-bb, next-alt-bb)
  end ;

  linearize-stamp (bbs, escape-bb) ;
  collect-bbs-check (bbs, escape-bb, next-merge.next-computation, uptil)
end;
  

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <if>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  let  merge = dfm.next-computation ;
  let  merge-temp = merge.temporary ;
  if (merge-temp &
      merge-temp.users.size = 1 &
      instance? (merge-temp.users.first, <if>) &
      merge-temp.users.first == merge.next-computation)
    collect-bbs-if-special (bbs, then-bb, dfm, uptil, merge-temp.users.first)
  else      
    let  con = dfm.consequent ;
    let  alt = dfm.alternative ;
    add! (then-bb.comp-list, dfm) ;
    *unwind-handlers* [dfm] := then-bb ;
    let  con-bbkey = con ;
    let  left-merge-node = #f ;
    if (con == merge & merge-temp)
      con-bbkey := left-merge-node := left-merge (merge)
    end;

    let  alt-bbkey = alt ;
    let  right-merge-node = #f ;
    if (alt == merge & merge-temp)
      alt-bbkey := right-merge-node := right-merge (merge)
    end;

    // label the bb's appropriately
    let  con-bb = if (con-bbkey == merge) find-bb (bbs, con-bbkey) else new-bb (bbs) end;
    let  alt-bb = if (alt-bbkey == merge) find-bb (bbs, alt-bbkey) else new-bb (bbs) end ;

    if (con-bb == alt-bb)
      error ("whoops, con==alt")
    else
      // order of links matters
      bb-link (then-bb, con-bb) ;
      bb-link (then-bb, alt-bb) ;

      // get the bodies
      if (con ~== merge)
        linearize-stamp (bbs, con-bb)
      end;
      let  con-end-bb = collect-bbs-check (bbs, con-bb, con, merge) ;
      if (merge-temp)
        // add any merging instructions
        if (~ con-end-bb.complete?)
          linearize-stamp (bbs, con-end-bb) ;
          add! (con-end-bb.comp-list, left-merge-node  | left-merge (merge))
        end
      end;
      if (alt ~== merge)
        linearize-stamp (bbs, alt-bb)
      end;
      let  alt-end-bb = collect-bbs-check (bbs, alt-bb, alt, merge) ;
      if (merge-temp)
        if (~ alt-end-bb.complete?)
          linearize-stamp (bbs, alt-end-bb) ;
          add! (alt-end-bb.comp-list, right-merge-node | right-merge (merge))
        end
      end ;

      // should really burrow through the merges without temporaries?
      if (~ (con-end-bb.complete? & alt-end-bb.complete?))
        let  new-bb = find-bb (bbs, merge) ;
        if ((~ con-end-bb.complete?) &
            (new-bb ~== con-end-bb))
          bb-link (con-end-bb, new-bb) 
        end ;
        if ((~ alt-end-bb.complete?) &
            (new-bb ~== alt-end-bb))
          bb-link (alt-end-bb, new-bb) 
        end ;
        linearize-stamp (bbs, new-bb) ;
        collect-bbs-check (bbs, new-bb, merge.next-computation, uptil)
      else
        new-fake-bb (bbs)
      end
    end
  end
end;


define function unlink-bb-pred (bbs :: <bb-collection>, bb :: <dylan-bb>)
  let  pred = bb.preds.first ;
  pred.succs := remove (pred.succs, bb) ;
//  remove! (bbs.bb-vec, bb) ;  // should really mark as dead
  pred
end;



define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <bind-exit>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  let  escape-bb = find-bb (bbs, dfm.entry-state) ;
  let  body-bb   = collect-bbs-check (bbs, then-bb, dfm.body, #f) ;
  let  maybe-merge = dfm.next-computation ;
  let  fake-body? = instance? (body-bb, <fake-bb>) ;
  // fake-body? means that no actual code in the body, so can lose it.
  if ((~ fake-body?) & 
      (body-bb.comp-list.empty?) &
      (body-bb.preds.size = 1))
    body-bb := unlink-bb-pred (bbs, body-bb)
  end;
  record-uenv-level (dfm) ;
  if (instance? (maybe-merge, <bind-exit-merge>))
    if (fake-body?)
      error ("<fake-bb> seen in merging context")
    else
      add! (body-bb.comp-list, right-merge (maybe-merge)) ;
      bb-link (body-bb, escape-bb) ;
      linearize-stamp (bbs, escape-bb) ;
      collect-bbs-check (bbs, escape-bb, maybe-merge.next-computation, uptil)
    end
  else
    if (~ fake-body?)
      bb-link (body-bb, escape-bb)
    end;
    linearize-stamp (bbs, escape-bb) ;
    collect-bbs-check (bbs, escape-bb, maybe-merge, uptil)
  end
end;

define thread variable *unwind-handlers* :: <object-table> = make (<object-table>) ;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <unwind-protect>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  let  cleanup-code = dfm.cleanups ;
  if (instance? (cleanup-code, <end-cleanup-block>))
    // simple case, no cleanups
    let  body-bb = collect-bbs-check (bbs, then-bb, dfm.body, #f) ;
    if (instance? (body-bb, <fake-bb>))
      body-bb
    else
      collect-bbs-check (bbs, body-bb, dfm.next-computation, uptil)
    end
  else
    let escape-bb = new-bb (bbs) ;
    let  body-bb = then-bb ;
    if (~ (then-bb.comp-list.empty?) )
      body-bb := new-bb (bbs) ;
      linearize-stamp (bbs, body-bb) ;
      bb-link (then-bb, body-bb)
    end;
    let  handlur = make (<finally-handler>) ;
    *unwind-handlers* [dfm] := handlur ;
    let  end-prot-bb = 
      dynamic-bind (*uenv* = pair (dfm, *uenv*))
        dynamic-bind (*udepth* = *udepth* + 1)
          collect-bbs-check (bbs, body-bb, dfm.body, #f)
        end dynamic-bind
      end dynamic-bind ;

    let  clean-bb = find-bb (bbs, dfm.entry-state) ;
    // note that the <unwind-protect> itself marks the start of the cleanups for
    // the actual code generation
    linearize-stamp (bbs, clean-bb) ;
    clean-bb.handler? := #t ;
    add! (clean-bb.comp-list, dfm) ;
    collect-bbs-check (bbs, clean-bb, cleanup-code, #f) ;
  
    if (~ end-prot-bb.complete?)
      bb-link (end-prot-bb, escape-bb) 
    end;
    linearize-stamp (bbs, escape-bb) ;
    collect-bbs-check (bbs, escape-bb, dfm.next-computation, uptil)
  end
end;


define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <loop>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  record-uenv-level (dfm) ;
  let escape-bb = find-bb (bbs, dfm) ;
  for (m :: <computation> in dfm.loop-merges)
    add! (then-bb.comp-list, left-merge (m))
  end ;
  let  body-bb = find-bb (bbs, dfm.loop-body) ;
  linearize-stamp (bbs, body-bb) ;
  bb-link (then-bb, body-bb) ;
  collect-bbs-check (bbs, body-bb, dfm.loop-body, #f) ;
  linearize-stamp (bbs, escape-bb) ;
  collect-bbs-check (bbs, escape-bb, dfm.next-computation, uptil)
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <loop-call>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  record-uenv-level (dfm) ;
  for (m :: <computation> in dfm.loop-call-merges)
    add! (then-bb.comp-list, right-merge (m))
  end;
  add! (then-bb.comp-list, dfm) ;  // not required
  bb-link (then-bb, find-bb (bbs, dfm.loop-call-loop.loop-body)) ;
  then-bb.complete? := #t ;
  then-bb
end;
  
define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <end-loop>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  if (uptil)   format-out ("!!!! unexpected uptil computation during <end-loop>\n")  end ;
  record-uenv-level (dfm) ;
  bb-link (then-bb, find-bb (bbs, dfm.ending-loop)) ;
  then-bb.complete? := #t ;
  then-bb
end;


define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <return>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  record-uenv-level (dfm) ;
  add! (then-bb.comp-list, dfm) ;
  then-bb.complete? := #t ;
  then-bb
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <end-exit-block>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  if (uptil)   format-out ("!!!! unexpected uptil computation during <end-exit-block>\n")  end ;
  then-bb
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <end-protected-block>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  if (uptil)   format-out ("!!!! unexpected uptil computation during <end-protected-block>\n")  end ;
  record-uenv-level (dfm) ;
  add! (then-bb.comp-list, dfm) ;
  let  es = dfm.entry-state ;
//  bb-link (then-bb, find-bb (bbs, es)) ;  // the cleanup bb is a pseudo-link
  bbs.protected-blocks := pair (find-bb (bbs, es), bbs.protected-blocks) ;
    // the above appears to just collect the cleanup blocks
  then-bb
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <end-cleanup-block>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  if (uptil)   format-out ("!!!! unexpected uptil computation during <end-cleanup-block>\n")  end ;
  add! (then-bb.comp-list, dfm) ;
  then-bb
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <exit>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  record-uenv-level (dfm) ;
  let  es = dfm.entry-state ;
  let  bind-exit = es.me-block ;
  if (bind-exit.environment ~== dfm.environment)
    format-out ("###### out of context <exit>\n") ;
    error ("out of context <exit> not handled yet") ;
  else
    let  be-next = bind-exit.next-computation ;
    if (instance? (be-next, <bind-exit-merge>))
      add! (then-bb.comp-list, exit-merge (be-next, dfm))
    end ;
    //format-out ("##### lost an <exit> in walk-bbs, IS THIS RIGHT\n") ;
    add! (then-bb.comp-list, dfm) ;   // not needed ??!! = but used in emit-expression-tree!!
    bb-link (then-bb, find-bb (bbs, es))
  end;
  then-bb.complete? := #t ;
  then-bb
end;

// redundant junk
define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <loop-merge>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  collect-bbs-check (bbs, then-bb, dfm.next-computation, uptil)
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <bind-exit-merge>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  collect-bbs-check (bbs, then-bb, dfm.next-computation, uptil)
end;

define method collect-bbs (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm :: <if-merge>, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  error ("whoops, if-merge")
end;


define function collect-bbs-check (bbs :: <bb-collection>, then-bb :: <dylan-bb>, dfm, uptil :: false-or (<computation>)) => (now-bb :: <dylan-bb>)
  if (dfm == uptil)
    then-bb
  elseif (~ dfm)
    then-bb
  else
    collect-bbs (bbs, then-bb, dfm, uptil)
  end
end;

define function find-bb (bbs :: <bb-collection>, tag) => (bb :: <dylan-bb>)
  element (bbs.label-tab, tag, default: #f) 
  | (bbs.label-tab [tag] := new-bb (bbs))
end;


define function new-bb (bbs :: <bb-collection>) => (bb :: <dylan-bb>)
  let  new = make (<dylan-bb>, collection: bbs) ;
  let  seq = bbs.seqnum ;
  new.seqnum := seq ;
  bbs.seqnum := seq + 1 ;
  add! (bbs.bb-vec, new) ;
  new
end;

define function linearize-stamp (bbs :: <bb-collection>, bb :: <dylan-bb>) => ()
  if (bb.linearize-seq = -1)
    bb.linearize-seq :=  bbs.linearize-seq ;
    bbs.linearize-seq := bbs.linearize-seq + 1
  end
end;

define function next-bb (bb :: <dylan-bb>)
  let bbs :: <bb-collection> = bb.collection ;
  let  seq = bb.seqnum + 1;
  if (seq = bbs.seqnum)
    #f
  else
    bbs.bb-vec [seq]
  end
end;

define function linearize-bbs (bbs :: <bb-collection>) => (bbs :: <bb-collection>)
  bbs.bb-vec := sort (bbs.bb-vec, test:
    method (a :: <dylan-bb>, b :: <dylan-bb>)
      a.linearize-seq < b.linearize-seq
    end) ;
  for (bb :: <dylan-bb> in bbs.bb-vec)
    bb.seqnum := bb.linearize-seq
  end;
  bbs
end;

define variable *the-fake-bb* = make (<fake-bb>, collection: #f) ;

define function new-fake-bb (bbs :: <bb-collection>) => (bb :: <fake-bb>)
  *the-fake-bb*
end;

define function bb-link (from :: <dylan-bb>, to :: <dylan-bb>) => ()
  from.succs := pair (to, from.succs) ;
  to.preds   := pair (from, to.preds) ;
end;


define function desc-dfm (dfm :: <bind>)
  let  bbs = make (<bb-collection>) ;
  let  new = new-bb (bbs) ;
  collect-bbs-check (bbs, new, dfm.next-computation, dfm) ;
  describe-dylan-bbs (bbs) ;
  #f
end;
  
define function describe-dylan-bbs (bbs :: <bb-collection>)
  for (el :: <dylan-bb> in bbs.bb-vec)
    format-out ("\na bb #%s [%s]:\n", el.seqnum, el.linearize-seq) ;
    format-out ("   { ") ;
    for (pred :: <dylan-bb> in el.preds)
      format-out ("%s ", pred.seqnum)
    end;
    format-out ("}\n") ;
    for (comp :: <computation> in el.comp-list)
      if (instance? (comp, <merge-transfer>))
        format-out ("       %s := %s  // merge-transfer\n", comp.temporary, comp.computation-value)
      else
        if (instance? (comp, <exit-transfer>))
          format-out ("       %s := %s  // exit-transfer\n", comp.temporary, comp.computation-value)
        else
          format-out ("       %s\n", comp)
        end
      end
    end;
    format-out ("   { ") ;
    for (succ :: <dylan-bb> in el.succs.reverse)
      format-out ("%s ", succ.seqnum)
    end;
    format-out ("}\n")
  end
end;

    
// eof
