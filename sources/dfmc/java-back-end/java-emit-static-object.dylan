Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* emitting static objects 
 *
 * for java, need to firstly associate some runtime name
 * with each statically modeled object.
 *
 * These names can be local variables if only a few (less than 200, say),
 * else we create static variables in the special class we are using for
 * this init (the hope is this special "init-helper class" gets GCed by
 * the JVM)
 *
 * Note that all modelled objects already assigned to bindings (whether constant
 * or not) can use those bindings as names.
 * [ Late Note:  I think this information is lost by modeling time (only in the init code),
 *   so shall assume no models match module bindings at all! ]
 *
 * step 2 is to create code that allocates the bare objects (out of language)
 * for each name, and assigns all the names.
 *
 * step 3 is to create code to initialize all of the slots of all of the objects,
 * basically a whole series of assignments.  Any reference to a used-library will,
 * by the magic of Java class static init ensure that that library is already
 * resolved in this manner too, so that external heap references are OK.
 *
 * Another point - external heap references (other than via bindings) constitute
 * a strong reference to the object, and such must be represented by a field in
 * the library class, not the init-helper class (which might later by GC'd, and
 * then re-invoked as a carbon-copy.
 *
 * We have to ensure that loaded libraries (their java classes, that is) are
 * roots, whereas the init-helper classes are not, and will not be loaded more than 
 * once during the relevant library init.
 *
 *
 * Thus in more detail:
 *
 * have to classify heap objects into several categories
 *
 * o  firstly those which already have a binding (which may be constant)
 * o  secondly those which have no constant binding, but which are
 * referenced or referencable by other libraries - these need a "strong"
 * reference in the library class.
 * o  thirdly those which will fit as local variables in the init-helper
 * method (may need a heuristic as to whether just one method will suffice)
 * o  lastly those that don't fit, and get a "weak" reference in the 
 * init-helper class (as a static field).
 *
 * create a mapping from all these objects to their Java names.  For
 * locals, this is defered, since haven't created the method yet??!
 *
 * For all objects, generate code to make the bare object.  Can at this
 * stage init any direct fields too, although not critical (shares work
 * between phases better?) - could make the repeated slot if size is known
 *
 * For all objects, generate code to set all its slots (in the Dylan world),
 * and the repeated slots.
 *
 * For those objects with both a variable binding and a constant weak reference,
 * at some point we must copy to the variable binding.
 *
 * Having done the init, we can assume that all those objects not directly
 * referenced from bindings or to other libraries can be detached by clobbering
 * the weak references (local variables have this happen automatically).
 *
 * mark the init-helper class as dead, and ensure there is a duplicate-
 * initialization check in there somewhere!  (one in the library class to
 * detect duplicate init-helper class loading?)
 *
 * note that the library class mustn't directly reference the init-helper
 * class - except perhaps from its <clinit> ??.
 */

define variable *emit-debug-strings-in-java-code* :: <boolean> = #t ;

define function emit-debug-string (jbb, str :: <byte-string>) => ()
  if (*emit-debug-strings-in-java-code*)
    emit-java-string (jbb, str) ;
    emit-pop (jbb)
  end
end;

define thread variable *optimized-classes* :: <object-table> = make (<object-table>) ;


// needs better naming
define function emit-virtual-transfer (jbb :: <java-basic-block>,
                                       obj-num :: <integer>, 
                                       type :: <java-type>,
                                       local-var-fn,
                                       static-slot-fn) => ()
  if (obj-num < #x100)
    local-var-fn (jbb, obj-num, j-code-for (type))
  else
    let  name :: <byte-string> = format-to-string ("LOC%d", obj-num) ;
    let  spec =  slot-spec (jbb.meth.java-class, name, type, #t) ;
    java-field (spec);  // ensure its created
    static-slot-fn (jbb, spec)
  end
end;

define function emit-pop-virtual (jbb :: <java-basic-block>,
                                  obj-num :: <integer>, 
                                  type :: <java-type>) => ()
  emit-virtual-transfer (jbb, obj-num, type, emit-pop-local, java-write)
end;


define function emit-push-virtual (jbb :: <java-basic-block>,
                                   obj-num :: <integer>, 
                                   type :: <java-type>) => ()
  emit-virtual-transfer (jbb, obj-num, type, emit-push-local, java-read)
end;


define function emit-push-model-reference (jbb :: <java-basic-block>,
                                           val :: <&object>,
                                           tab :: <object-table>) => ()
  let  refed-num = element (tab, val, default: #f) ;
  if (refed-num)
    let  refed-cls = java-class-for-thing (val.^object-class) ;
    emit-push-virtual (jbb, refed-num, refed-cls) ;
  else
    emit-expression-leaf (jbb, val)
  end
end;  



define sealed generic create-repeated-slot (slotd, jbb :: <java-basic-block>, obj) => () ;

define method create-repeated-slot (slotd == #f,
				    jbb :: <java-basic-block>,
				    obj) => ()
end;

define method create-repeated-slot (slotd :: <&repeated-slot-descriptor>, 
				    jbb :: <java-basic-block>,
				    obj) => ()
  let  size = ^slot-value (obj, slotd.^size-slot-descriptor) ;
  let  el-type = java-class-for-thing (slotd.^slot-type) ;
  emit-dup (jbb) ;
  emit-java-int (jbb, size) ;
  java-op2 (jbb, j-anewarray, el-type) ; // need actual type!
  bare-write-model-slot (jbb, slotd, #t)
end;


define function create-java-model (jbb :: <java-basic-block>, obj :: <&object>, java-cls :: <java-class>) => ()
  error ("not used now") ;
  // format-out ("### doing new for a %s\n", java-cls) ;
  let sclass = &object-class(obj) ;
  ^ensure-slots-initialized (sclass) ;

//  java-op2 (jbb, j-new, java-cls) ;
  emit-java-new-init-0 (jbb, java-cls) ;
  create-repeated-slot (sclass.^repeated-slot-descriptor, jbb, obj) ;
end;



// this assumes the instance is on the stack, and should remain so
define function fixup-model-slot 
     (jbb :: <java-basic-block>,
      val,
      slotd :: <&slot-descriptor>,
      tab :: <object-table>) => ()
  emit-dup (jbb) ;
  emit-push-model-reference (jbb, val, tab) ;
  bare-write-model-slot (jbb, slotd, #f)
end;



define constant $fill-array-meth$ =
  meth-spec ($dylan-class-<simple-object-vector>$,
             "blat_fill_array",
             meth-type ($java-void-type$,
                        array-type ($dylan-class-<object>$),
                        $dylan-class-<object>$, 
                        $java-int-type$),
             j-invokestatic) ;


define function fixup-identical-repeated 
    (jbb :: <java-basic-block>,
     val,
     size :: <integer>, 
     tab :: <object-table>,
     num-on-stack :: <integer>) => (num-on-stack :: <integer>)
  // the array is on stack
  emit-push-model-reference (jbb, val, tab) ; // the repeated value
  emit-java-int (jbb, size) ;  // element-count
  java-call (jbb, $fill-array-meth$) ;
  num-on-stack - 1
end;

define function java-jsr-op (jbb :: <java-basic-block>, dest-offset) => ()
  let  jbbpc = jbb.pc ;  // record instruction address in closure
  java-branch-op (jbb, j-jsr, method () dest-offset - jbbpc end) ;
end;

define function fixup-java-model (jbb :: <java-basic-block>, 
                                  obj :: <&object>, 
                                  tab :: <object-table>,
                                  aux2,
                                  aux4, 
                                  auxs) => ()
  let  obj-num = tab [obj] ;
  let  obj-cls = obj.^object-class ;
  let  java-cls = java-class-for-thing (obj-cls) ;
  let  num-on-stack :: <integer> = 0 ;

  let  optimizer = element (*optimized-classes*, obj-cls, default: #f) ;
  let  prev-val = list () ;  // unique value
  local
    method ensure-instance-on-stack () => ()
      if (num-on-stack = 0)
        emit-push-virtual (jbb, obj-num, java-cls);
        num-on-stack := num-on-stack + 1
      end
    end;
      
  let sclass = &object-class(obj) ;
  ^ensure-slots-initialized (sclass) ;
  let rep-slotd = sclass.^repeated-slot-descriptor ;
  let hack-slotd = if (rep-slotd) rep-slotd.^size-slot-descriptor end;
  let hack-done = #f ;
  let count-pushed :: <integer> = 0 ;

  for-layout-fixed-slot-value (val described-by slotd in obj)
    unless (slotd == hack-slotd & hack-done)
      ensure-instance-on-stack () ;
      if (optimizer)
        count-pushed := count-pushed + 1 ;
        if (val == prev-val)  // handle repeats smarter
          emit-dup (jbb)
        else
          emit-push-model-reference (jbb, val, tab) ;
          prev-val := val
        end
      else
        fixup-model-slot (jbb, val, slotd, tab)
      end
    end;
    if (slotd == hack-slotd & ~hack-done)
      hack-done := #t
    end;
  end;

  if (optimizer & num-on-stack > 0)
    java-jsr-op (jbb, optimizer) ;
    model-pop-discards (jbb, count-pushed + 2) ;
    num-on-stack := num-on-stack - 1
  end;

  if (rep-slotd)
    num-on-stack := 
      maybe-specialized-fixup (obj, jbb, obj-num, tab, obj-cls, java-cls, aux2, aux4, auxs, num-on-stack) ;
  end;
  if (num-on-stack > 0)
    pop-junk (jbb, num-on-stack)
  end
end;


define sealed generic maybe-specialized-create (obj,
					       jbb :: <java-basic-block>, 
					       obj-num :: <integer>,
                                               obj-cls :: <&class>,
					       java-cls :: <java-class>,
                                               auxs) => () ;

define method  maybe-specialized-create (obj,
                                         jbb :: <java-basic-block>, 
                                         obj-num :: <integer>, 
                                         obj-cls :: <&class>,
                                         java-cls :: <java-class>,
                                         auxs) => ()
  let sclass = &object-class(obj) ;
  ^ensure-slots-initialized (sclass) ;

//  java-op2 (jbb, j-new, java-cls) ;
  emit-java-new-init-0 (jbb, java-cls) ;
  create-repeated-slot (sclass.^repeated-slot-descriptor, jbb, obj) ;
end;


// standard Java method
define constant $java-string-length-method$ =
  meth-spec ($java/lang/String$,
             "size",
             meth-type ($java-int-type$),
             j-invokevirtual) ;



// a runtime method we must provide
define constant $java-string-splat-method$ =
  meth-spec ($dylan-runtime-class$,
             "splat_string_contents",
             meth-type ($dylan-class-<byte-string>$,
                        $java/lang/String$,
                        $dylan-class-<byte-string>$,
                        array-type ($java-byte-type$)),
             j-invokestatic) ;

define function generate-bytestring-routine (meth :: <java-method>) => (auxs :: <integer>)
  let  jbb = make-jbb (meth) ;
  emit-debug-string (jbb, "<byte-string> create/initializer") ;
  // assume a java/lang/String is on stack, replace with Dylan <byte-string>
  let sclass = &object-class ("A <byte-string>") ;
  ^ensure-slots-initialized (sclass) ;
  let  rep-slotd = sclass.^repeated-slot-descriptor ;
  let  size-slotd = rep-slotd.^size-slot-descriptor ;
  let  obj-cls = "A <byte-string>".^object-class ;
  let  java-cls = java-class-for-thing (obj-cls) ;

  let  skip-point :: <integer> = 0 ;
  let  this-point :: <integer> = jbb.pc ;

  java-branch-op (jbb, j-goto, method () skip-point - this-point end) ;
  let  auxs :: <integer> = jbb.pc ;

  model-push-type (jbb, $java/lang/String$) ;
  model-push-type (jbb, $java-return-address$) ;
  let  ret-temp = get-temp-local-var () ;
  emit-pop-local (jbb, ret-temp, j-ref-code) ;
  // pop return-address
  // first get the length
  
  emit-dup (jbb) ;
  java-call (jbb, $java-string-length-method$) ;
//  java-op2 (jbb, j-new, java-cls) ;
  emit-java-new-init-0 (jbb, java-cls) ;
  emit-swap (jbb) ;
  java-simple-op (jbb, j-dup2) ;
  bare-write-model-slot (jbb, size-slotd, #f) ;
  java-op1-op (jbb, j-newarray, j-byte-code) ;
  java-simple-op (jbb, j-dup2) ;
  bare-write-model-slot (jbb, rep-slotd, #t) ;
  // now have String, <byte-string> object and byte[] on stack
  java-call (jbb, $java-string-splat-method$) ;
  // now just have <byte-string>
  emit-ret (jbb, ret-temp) ;
  model-pop-a-type (jbb, java-cls) ;
  skip-point := jbb.pc ;
  finish-with-jbb (jbb, meth) ;
  auxs
end;

define method  maybe-specialized-create (str :: <byte-string>,
                                         jbb :: <java-basic-block>, 
                                         obj-num :: <integer>, 
                                         obj-cls :: <&class>,
                                         java-cls :: <java-class>, 
                                         auxs) => ()
/*
  let sclass = &object-class (str) ;
  ^ensure-slots-initialized (sclass) ;
  let  rep-slotd = sclass.^repeated-slot-descriptor ;
  let  len = str.size ;
  let  size-slotd = rep-slotd.^size-slot-descriptor ;

//  java-op2 (jbb, j-new, java-cls) ;
  emit-java-new-init-0 (jbb, java-cls) ;
  create-repeated-slot (rep-slotd, jbb, str) ;
  emit-dup (jbb);
  emit-java-int (jbb, len) ;
  bare-write-model-slot (jbb, size-slotd, #f) ;
  emit-dup (jbb);
  fixup-<byte-string>-model (jbb, str, obj-num, java-cls) ;
*/
  emit-java-string (jbb, str) ;
  java-jsr-op (jbb, auxs) ;
  model-pop-discards (jbb, 2) ;
  model-push-type (jbb, java-cls) ;
end;


define sealed generic maybe-specialized-fixup (obj,
					       jbb :: <java-basic-block>, 
					       obj-num :: <integer>, tab,
					       obj-cls :: <&class>, 
					       java-cls :: <java-class>, aux2, aux4, auxs,
					       num-on-stack :: <integer>) =>
    (num-on-stack :: <integer>) ;
 
define method maybe-specialized-fixup (str :: <byte-string>,
				       jbb :: <java-basic-block>, 
				       obj-num :: <integer>, tab,
				       obj-cls :: <&class>, 
				       java-cls :: <java-class>, aux2, aux4, auxs,
				       num-on-stack :: <integer>) =>
    (num-on-stack :: <integer>)
  // strings done at create time
  num-on-stack
end;

// default method
define method maybe-specialized-fixup (obj,
				       jbb :: <java-basic-block>, 
				       obj-num :: <integer>, tab,
				       obj-cls :: <&class>, 
				       java-cls :: <java-class>, aux2, aux4, auxs,
				       num-on-stack :: <integer>) =>
    (num-on-stack :: <integer>)
  let sclass = &object-class(obj) ;
//  ^ensure-slots-initialized (sclass) ;
  let rep-slotd = sclass.^repeated-slot-descriptor ;
  let  size :: <integer> = ^slot-value (obj, rep-slotd.^size-slot-descriptor) ;
  if (size > 0)
    if (num-on-stack = 0)
      emit-push-virtual (jbb, obj-num, java-cls);
      num-on-stack := num-on-stack + 1
    end;
    // let  el-type = java-class-for-thing (rep-slotd.^slot-type) ;
    bare-read-model-slot (jbb, rep-slotd, #t) ;

    let  first = ^repeated-slot-value (obj, rep-slotd, 0) ;
    // would like an iterator for this:
    // if (every? (method (x) x == first end, ^repeated-slot-values (obj, rep-slotd)))
    if (size > 2 &
	  block (break)
	    for (index :: <integer> from 1 below size)
	      unless (^repeated-slot-value (obj, rep-slotd, index) == first)
		break (#f)
	      end
	    end;
	    #t
	  end)
      num-on-stack := fixup-identical-repeated (jbb, first, size, tab, num-on-stack)
    else
      emit-java-int (jbb, 0) ;  // seed
      num-on-stack := num-on-stack + 1 ;
      num-on-stack := fixup-repeated-slots (jbb, aux2, aux4, obj, tab, rep-slotd, 0, size, num-on-stack)
    end
  end;
  num-on-stack
end;

define function fixup-repeated-slots (jbb :: <java-basic-block>, aux2, aux4,
                                      obj, tab,
                                      slotd :: <&repeated-slot-descriptor>,
                                      index :: <integer>, 
                                      size :: <integer>,
                                      num-on-stack :: <integer>) => (num-on-stack :: <integer>)
  // attempt to bunch together in groups of 2 or 4, saving instruction space
  if (index + 4 <= size)
    let  val0 = ^repeated-slot-value (obj, slotd, index) ;
    let  val1 = ^repeated-slot-value (obj, slotd, index + 1) ;
    let  val2 = ^repeated-slot-value (obj, slotd, index + 2) ;
    let  val3 = ^repeated-slot-value (obj, slotd, index + 3) ;
    emit-push-model-reference (jbb, val0, tab) ;
    // simple attempt to use dup/dup2 to avoid duplicate value fetching
    if (val1 == val0)  emit-dup (jbb)  else  emit-push-model-reference (jbb, val1, tab) end ;
    if (val2 == val0 & val3 == val1)
      java-simple-op (jbb, j-dup2)
    else
      if (val2 == val1)  emit-dup (jbb)  else  emit-push-model-reference (jbb, val2, tab) end ;
      if (val3 == val2)  emit-dup (jbb)  else  emit-push-model-reference (jbb, val3, tab) end ;
    end;
    java-jsr-op (jbb, aux4) ;
    model-pop-discards (jbb, 5) ;
    index := index + 4 ;
    fixup-repeated-slots (jbb, aux2, aux4, obj, tab, slotd, index, size, num-on-stack)
  elseif (index + 2 <= size)
    let  val0 = ^repeated-slot-value (obj, slotd, index) ;
    let  val1 = ^repeated-slot-value (obj, slotd, index + 1) ;
    emit-push-model-reference (jbb, val0, tab) ;
    // simple attempt to use dup to avoid duplicate value fetching
    if (val1 == val0)  emit-dup (jbb)  else  emit-push-model-reference (jbb, val1, tab) end ;
    java-jsr-op (jbb, aux2) ;
    model-pop-discards (jbb, 3) ;
    index := index + 2 ;
    fixup-repeated-slots (jbb, aux2, aux4, obj, tab, slotd, index, size, num-on-stack)
  elseif (index + 1 <= size)
    let  val = ^repeated-slot-value (obj, slotd, index) ;
    emit-push-model-reference (jbb, val, tab) ;
    java-simple-op (jbb, j-aastore) ;  // loses index and array
    num-on-stack := num-on-stack - 2 ;
  else
    num-on-stack
  end
end;

define function pop-junk (jbb :: <java-basic-block>, count :: <integer>) => ()
  while (count >= 2)
    java-simple-op (jbb, j-pop2) ;
    count := count - 2
  end;
  while (count >= 1)
    emit-pop (jbb) ;
    count := count - 1
  end
end;


// hacks for now - these methods need implementing, at the very least!
define constant $blat-string-to-array-meth =
  meth-spec ($dylan-class-<string>$, 
             "blat_to_array",
             meth-type ($java-void-type$, 
//                        $dylan-class-<byte-string>$,
                        array-type ($java-byte-type$),
                        $java/lang/String$),
             j-invokestatic) ;


/* out of date?
// assume instance and byte-array on the stack, and pop them when done
define function fixup-<byte-string>-model (jbb :: <java-basic-block>, obj :: <byte-string>, obj-num, java-cls) => ()
  let slotd = &object-class(obj).^repeated-slot-descriptor ;
  unless (slotd) error ("byte string without repeated slot!") end;
  // emit-push-virtual (jbb, obj-num, java-cls);
  bare-read-model-slot (jbb, slotd, #t) ;
  /*
  let (slot-kind, slot-type-class, reader-spec) = 
    java-getter-and-setter (slotd, must-have-writer?: #t, repeated?: #t) ;
  if (slot-kind == #"interface")
    java-if-call (jbb, reader-spec) ;    
  else
    java-read (jbb, reader-spec) ;  // get the array (repeated slot)
  end;
  */
  emit-java-string (jbb, obj) ;  // put out Java string
  java-call (jbb, $blat-string-to-array-meth) ;
end;
*/


define function generate-opt-fixed-routine (jbb :: <java-basic-block>,
					    obj, obj-cls,
					    optimized-classes) => ()
  let  skip-point :: <integer> = -1 ;
  let  this-point :: <integer> = jbb.pc ;
  java-branch-op (jbb, j-goto, method () skip-point - this-point end) ;  // close over pcs
  optimized-classes [obj-cls] := jbb.pc ;
  let  java-cls = obj-cls.java-class-for-thing ;
  model-push-type (jbb, java-cls) ;

  emit-debug-string (jbb, format-to-string ("*** fixup routine for %s", obj-cls)) ;

  // for the slots, create init code, assume obj then fixed slots, pop all but object
  // into locals and then store them.
  // this may break the rules about locals, but we need a local-colouring algorithm
  // that takes care of Java verification rules anyway.
  let  rev-slotd-list :: <list> = #() ;
  let  rev-temp-list :: <list> = #() ;
  let  first-temp = #f ;

  let sclass = &object-class(obj) ;
  ^ensure-slots-initialized (sclass) ;
  let rep-slotd = sclass.^repeated-slot-descriptor ;
  let hack-slotd = if (rep-slotd) rep-slotd.^size-slot-descriptor end;
  let hack-done = #f ;

  for-layout-fixed-slot-value (val described-by slotd in obj) // ignoring value
    unless (slotd == hack-slotd & hack-done)
      let  temp = get-temp-local-var() ;
      unless (first-temp) first-temp := temp end;
      rev-slotd-list := pair (slotd, rev-slotd-list) ;
      rev-temp-list  := pair (temp,  rev-temp-list) ;
      model-push-type (jbb, slotd.^slot-type.java-class-for-thing) ;
    end;
    if (slotd == hack-slotd & ~hack-done)
      hack-done := #t
    end
  end;

  model-push-type (jbb, $java-return-address$) ;
  let  ret-pc-temp = get-temp-local-var () ;
  emit-pop-local (jbb, ret-pc-temp, j-ref-code) ;
  for (temp  in rev-temp-list, 
       slotd in rev-slotd-list)
    emit-pop-local (jbb, temp, j-ref-code) ;  // j-ref-code????
  end;
  for (temp  in rev-temp-list, 
       slotd in rev-slotd-list)
    unless (temp == first-temp)
      emit-dup (jbb) ;   // copy the instance
    end;
    emit-push-local (jbb, temp, j-ref-code) ;  // j-ref-code ???
    bare-write-model-slot (jbb, slotd, #f);
  end;
  unless (first-temp) emit-pop (jbb) end;
  emit-ret (jbb, ret-pc-temp) ;
  skip-point := jbb.pc ;
end;


define variable *unique-numbering* :: <integer> = 1000000 ;
define function cache-java-model (model) => (spec :: <java-slot-spec>)
//  my-break (model) ;
  let  models-lib = model.model-library.language-definition ;
  let  lib-class = models-lib.java-class-for-thing ;
  let  slot-name = format-to-string ("LOC_%s", model.^debug-name | (*unique-numbering* := *unique-numbering* + 1)) ;
  make (<java-slot-spec>, java-class: lib-class,
        name: slot-name.uniq,
        type: java-class-for-thing (model.^object-class),
        static?: #t)
end;

define variable *model-cache* :: <object-table> = make (<object-table>) ;

define function find-cached-model (model :: <object>) => (spec :: <java-slot-spec>)
  let  cached = element (*model-cache*, model, default: #f) ;
  unless (cached)
    cached := cache-java-model (model) ;
    *model-cache* [model] := cached ;
  end;
  cached
end;


define function dump-static-objects (cr :: <compilation-record>) => ()
  let heap :: <model-heap> = cr.compilation-record-model-heap;

  format-out ("#!# cr = %s\n", cr) ;
  format-out ("#!#   defined bindings %d\n", heap.heap-defined-bindings.size) ;
  format-out ("#!#   refed bindings   %d\n", heap.heap-referenced-bindings.size) ;
  format-out ("#!#   defined objects  %d (%d)\n", heap.heap-defined-objects.size, heap.heap-defined-object-sequence.size) ;
  format-out ("#!#   refed objects    %d\n", heap.heap-referenced-objects.size) ;
  format-out ("#!#   defined repeated object sizes %d\n", heap.heap-defined-repeated-object-sizes.size) ;
  format-out ("#!#   refed repeated object sizes %d\n",   heap.heap-referenced-repeated-object-sizes.size) ;
  format-out ("#!#   load-bound refs  %d\n", heap.heap-load-bound-references.size) ;
  format-out ("#!#   symbols          %d\n", heap.heap-symbols.size) ;


  // gather the models into table
  let  tab = make (<object-table>) ;
  let  n :: <integer> = 0 ;
  for (obj in heap.heap-defined-object-sequence)
    if (~ element (tab, obj, default: #f))
//      tab [obj] := n ;
      let  loc-num = get-temp-local-var () ;
      tab [obj] := loc-num ;
      n := n + 1
    end
  end;

/* // print out details
  for (obj in tab.key-sequence)
    format-out ("#!#  Obj%d :: %s:\n", tab [obj], obj.object-class) ;
    describe-an-object (tab, obj) ;
    format-out ("\n")
  end;
  for (bnd in heap.heap-defined-bindings)
    format-out ("#!# defined binding %s\n", bnd.name)
  end;
  for (bnd in heap.heap-referenced-bindings)
    format-out ("#!# refed binding %s\n", bnd.name)
  end;
*/

  let  optimized-classes = make (<object-table>) ;
  if (tab.size > 10 /*was 40*/)  
    let  type-stats = make (<object-table>) ;
    for (obj in tab.key-sequence)
      let  cls = obj.^object-class ;
      element (type-stats, cls) := element (type-stats, cls, default: 0) + 1
    end;
    for (cls in type-stats.key-sequence)
      if (type-stats[cls] > 1 /* was 4 */)
        format-out ("##!!## Have a candidate optimized class for static dump %s\n", cls) ; 
        optimized-classes [cls] := #t
      end;
    end
  end;


  // temporarily use library class as holder for static model init code.
  let  cls = *current-module-java-class* ;
  if (cls == #f)
    format-out ("whats the library class?") ;
  end;


  // crude model initialization code creation
  // For one thing this code is likely to exceed a JVM limit,
  // and we need machinery to chop this up into as
  // many separate methods as required.
  // may also overflow constants pool, so may have to 
  // split in several classes (shouldn't be using the library class)


  let  meth = java-method (meth-spec (cls,
                                      "Jcreate_models",
                                      meth-type ($java-void-type$),
                                      j-invokestatic)) ;
  // generate aux methods for splatting repeated slots in chunks
  let  (aux2, aux4) = generate-array-splat-routines (meth) ;
  let  auxs = generate-bytestring-routine (meth) ;

  meth.max-locals := min (n, #x100) ;  // hacky, no real check
  let  jbb = make-jbb (meth) ;

  dynamic-bind (*temp-map* = make (<object-table>),
                *temp-seq* = 1)  // "this" is 0 in trampoline

    emit-debug-string (jbb, "*** Creating model objects") ;
    // pass to create the models
    for (obj in tab.key-sequence)
      let  obj-num = tab [obj] ;
      let  obj-cls = obj.^object-class ;
      let  java-cls = java-class-for-thing (obj-cls) ;
      maybe-specialized-create (obj, jbb, obj-num, obj-cls, java-cls, auxs) ;
      emit-pop-virtual (jbb, obj-num, java-cls);

      unless (jbb.stack-model.empty?)
        format-out ("#!# failed to have empty stack between creation frags\n") ;
        my-break (jbb)
      end;
      // now ensure an optimized slot-setter aux routine exists:
      if (element (optimized-classes, obj-cls, default: #f) == #t)
	generate-opt-fixed-routine (jbb, obj, obj-cls, optimized-classes) ;
        unless (jbb.stack-model.empty?)
          format-out ("#!# failed to have empty stack after gened opt routine\n") ;
          my-break (jbb)
        end
      end
    end;

    emit-debug-string (jbb, "*** Fixing up model objects") ;

    // pass to fixup (write slots) the models
    dynamic-bind (*optimized-classes* = optimized-classes)
      for (obj in tab.key-sequence)
        emit-debug-string (jbb, format-to-string ("* fix a %s", obj.^object-class)) ;
        fixup-java-model (jbb, obj, tab, aux2, aux4, auxs) ;
        unless (jbb.stack-model.empty?)
          format-out ("#!# failed to have empty stack between splatting frags\n") ;
          my-break (jbb)
        end;
      end;
      format-out ("##!!##  %d optimized classes\n", *optimized-classes*.size)
    end dynamic-bind;
    emit-return (jbb, j-void-code)
  end dynamic-bind;
  finish-with-jbb (jbb, meth) ;
end;

define function generate-array-splat-routines (meth :: <java-method>)
  let  jbb = make-jbb (meth) ;

    let  tempr = get-temp-local-var() ;
    let  temp0 = get-temp-local-var() ;
    let  temp1 = get-temp-local-var() ;
    let  temp2 = get-temp-local-var() ;
    let  temp3 = get-temp-local-var() ;

    //need to skip over routines
    let  skip-point :: <integer> = 0 ;
    let  this-point :: <integer> = jbb.pc ;

    java-branch-op (jbb, j-goto, method () skip-point - this-point end) ;


    let aux2 = #f ;
    let aux4 = #f ;
    model-push-type (jbb, $dylan-class-<object>$) ;  // the instance
    model-push-type (jbb, $dylan-class-<object>$.array-type) ;  // the rep slots array
    model-push-type (jbb, $java-int-type$) ;  // the index

    for (i from 0 below 2)
      let four? :: <boolean> = (i = 0) ;
      if (four?)
        aux4 := jbb.pc ;
      else
        aux2 := jbb.pc ;
      end;

      emit-debug-string (jbb, "array initializer aux routine") ;

      model-push-type (jbb, $dylan-class-<object>$) ;
      model-push-type (jbb, $dylan-class-<object>$) ;
      if (four?)
        model-push-type (jbb, $dylan-class-<object>$) ;
        model-push-type (jbb, $dylan-class-<object>$) ;
      end;

      model-push-type (jbb, $java-return-address$) ;
      emit-pop-local (jbb, tempr, j-ref-code) ; // return address
      if (four?)
        emit-pop-local (jbb, temp3, j-ref-code) ;
        emit-pop-local (jbb, temp2, j-ref-code) ;
      end;
      emit-pop-local (jbb, temp1, j-ref-code) ;
      emit-pop-local (jbb, temp0, j-ref-code) ;

      java-simple-op (jbb, j-dup2) ;
      emit-push-local (jbb, temp0, j-ref-code) ;
      java-simple-op (jbb, j-aastore) ;

      emit-raw-expression-leaf (jbb, 1) ;
      java-simple-op (jbb, j-iadd) ;

      java-simple-op (jbb, j-dup2) ;
      emit-push-local (jbb, temp1, j-ref-code) ;
      java-simple-op (jbb, j-aastore) ;

      emit-raw-expression-leaf (jbb, 1) ;
      java-simple-op (jbb, j-iadd) ;
      if (four?)
        java-simple-op (jbb, j-dup2) ;
        emit-push-local (jbb, temp2, j-ref-code) ;
        java-simple-op (jbb, j-aastore) ;

        emit-raw-expression-leaf (jbb, 1) ;
        java-simple-op (jbb, j-iadd) ;

        java-simple-op (jbb, j-dup2) ;
        emit-push-local (jbb, temp3, j-ref-code) ;
        java-simple-op (jbb, j-aastore) ;

        emit-raw-expression-leaf (jbb, 1) ;
        java-simple-op (jbb, j-iadd) ;
      end;
      emit-ret (jbb, tempr) 
    end;
    model-pop-a-type (jbb, $java-int-type$) ;  // the index
    model-pop-a-type (jbb, $dylan-class-<object>$.array-type) ;  // the rep slots array
    model-pop-a-type (jbb, $dylan-class-<object>$) ;  // the instance
    skip-point := jbb.pc ;

  finish-with-jbb (jbb, meth) ;
  values (aux2, aux4)
end;  



define generic describe-an-object (tab :: <object-table>, obj) => () ;

define method describe-an-object (tab :: <object-table>, obj :: <byte-string>) => ()
  format-out ("#!#    value: %s\n", obj)
end;

define method describe-an-object (tab :: <object-table>, obj :: <uninterned-symbol>) => ()
  format-out ("#!#    value: %s\n", obj)
end;

define method describe-an-object (tab :: <object-table>, obj) => ()
  for-layout-fixed-slot-value (val described-by slotd in obj)
    describe-slot (tab, slotd, val, #f)
  end;
  for-layout-repeated-slot-value (val described-by slotd keyed-by index in obj)
    describe-slot (tab, slotd, val, index)
  end
end;

define function describe-slot (tab :: <object-table>, sd :: <&slot-descriptor>, val, index :: false-or (<integer>)) => ()
  let  direct :: <boolean> = direct-object? (val) ;
  let  refed-n = element (tab, val, default: #f) ;
  let  extern :: <boolean> = ~ refed-n ;
  format-out ("#!#     %sslot", if (direct) "I" elseif (extern) "X" else "" end) ;
  if (index)
    format-out ("[%d]", index)
  end;
  if (direct | extern)
    format-out (": %s :: %s", val, val.object-class)
  else
    format-out (": Obj%d", refed-n, sd)
  end;
  format-out ("    // %s", if (sd.^init-keyword) sd.^init-keyword else "!" end) ;
  if (sd.^slot-getter & sd.^slot-getter.^debug-name)
    format-out ("   %s", sd.^slot-getter.^debug-name) 
  end;
  format-out ("\n")
end; 



/*
define sealed generic cache-java-model (model :: <&object>) => (spec);

define method cache-java-model (model :: <&object>) => (spec)
  let  emitted-name = model.emitted-name ;
  let  full-name = format-to-string ("LOC%s", emitted-name) ;
//  format-out ("##!!## name %s\n", full-name) ;
  let  lib = model.model-library.language-definition ;
  let  lib-cls = java-class-for-thing (lib) ;
//  format-out ("##!!##  cls %s\n", lib-cls) ;
  let  slot-spec = slot-spec (lib-cls, name, $dylan-class-<object>$, #t) ;
  slot-spec
end;
*/
// eof

