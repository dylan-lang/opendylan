Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Stuff to build up JVM exception records, basically mapping ranges of
// bytecode addresses to a Java class and a handler pc value

define sealed class <java-method-context> (<object>)
  sealed constant slot method-context-method, init-keyword: method: ;
  sealed slot method-context-used-handlers :: <list> = #() ;
  sealed slot method-context-range-tree :: <list> = #() ;
end;


define function enter-java-method-context (meth :: <java-method>)
 => (context :: <java-method-context>)
  make (<java-method-context>, method: meth)
end;

define function exit-java-method-context (context :: <java-method-context>)
  generate-java-handlers (context);
  #f
end;


define sealed class <java-handler> (<object>)
  sealed constant slot handler-context, init-keyword: context: ;
  sealed slot handler-pc :: <integer> = -1 ;
  sealed slot handler-code-gen :: <function> = generate-simple-handler ;
  sealed constant slot handler-exception-class = #f, init-keyword: exception-class: ;
  sealed slot handler-info  = #f ;
  sealed slot handler-use-list = #() ;
  sealed slot handler-explanatory-msg = #f;
end;

define function find (value, collection :: <collection>, keyfn :: <function>)
  block (return)
    for (el in collection)
      if (keyfn (el) == value)
        return (el)
      end
    end;
    #f
  end
end;
  

define function find-simple-java-handler (context :: <java-method-context>, java-class :: <java-class>)
  let  existing = find (java-class, context.method-context-used-handlers, handler-exception-class) ;
  existing |
    begin
      let  h = make (<java-handler>, context: context, exception-class: java-class) ;
      context.method-context-used-handlers := pair (h, context.method-context-used-handlers);
      h
    end
end;



define function enter-handler-scope (hand :: <java-handler>, jbb :: <java-basic-block>)
  // start recording ranges
  //format-out ("  in handler range for %s, at %d\n", hand, hand.context.meth.pc) ;
  jbb.pc
end;

define function exit-handler-scope (hand :: <java-handler>, jbb :: <java-basic-block>, entry :: <integer>)
  hand.handler-use-list := pair (pair (entry, jbb.pc), hand.handler-use-list) ;
  #f
end;


define constant $java-throwable-class-name$ = java-lang ("Throwable") ;
define constant $java-throwable-class-sig$  = classsig ($java-throwable-class-name$) ;

define constant $java-string-class-name$ = java-lang ("String") ;
define constant $java-string-class-sig$  = classsig ($java-string-class-name$) ;


define constant $dylanexception-signal-method$ =
  meth-spec ($dylan/dylancondition$, "signal", 
	     meth-type ($java/lang/Throwable$, $java/lang/Throwable$),
	     j-invokestatic) ;


define function generate-simple-handler (jbb :: <java-basic-block>, hand :: <java-handler>)
  // invoke-static ... stuff for making a dylan exception & signalling, finally punt to throw RTE
  // first must set the stack models
  if (*check-stack-types*)
    jbb.initial-stack-depth := jbb.stack-depth := 1 ;
    jbb.initial-stack-model := jbb.stack-model := list ($java/lang/Throwable$) ;
  end;
  let  jc = jbb.meth.java-class ;
//  java-op2 (jbb, j-new, hand.handler-exception-class) ;
//  java-simple-op (jbb, j-dup) ;
//  if (hand.handler-explanatory-msg)
//    emit-expression-leaf (jbb, hand.handler-explanatory-msg)
//  end;
//  // note that initializers must be done via invokespecial
//  let  typey = if (hand.handler-explanatory-msg)
//		 meth-type ($java-void-type$, $java/lang/String$)
//	       else
//		 meth-type ($java-void-type$)
//	       end;
//  let  spec = meth-spec (hand.handler-exception-class, $java-init-methname$, typey, j-invokespecial) ;
//  java-call (jbb, spec) ;
  java-call (jbb, $dylanexception-signal-method$) ;
  java-simple-op (jbb, j-athrow)
end;


define sealed class <ecpt-range> (<object>)
  sealed slot range-start :: <integer> = -1 ;
  sealed slot range-endd ::  <integer> = -1 ;
  sealed slot range-pc ::    <integer> = -1 ;
  sealed slot range-handler = #f ;
  sealed slot range-java-class   = #f ;
  sealed slot range-sub-ranges :: <list> = #() ;  // not utilized properly yet?
end;

define function generate-java-handlers (context :: <java-method-context>)
  let  jmeth = context.method-context-method ;
  local method
    append-to-exception-table (s :: <integer>, e :: <integer>, cls, h :: <integer>)
      let  entry = make (<java-exception-entry>, 
                         start-pc: s, end-pc: e,
                         excep-type: if (cls) pool-index (cls, jmeth.java-class) else 0 end,
                         excep-pc: h) ;
      jmeth.excep-table := add! (jmeth.excep-table, entry)
    end,
    gen (ranges :: <list>)
      if (instance? (ranges, <pair>))
                gen-range (ranges.head) ;
		gen (ranges.tail)
      end
    end,
    gen-range (range :: <ecpt-range>)
      gen (range.range-sub-ranges) ;
      append-to-exception-table (range.range-start, range.range-endd, range.range-java-class, range.range-handler.pc)
    end;
  // add handler code to that for the method
  for (hand in context.method-context-used-handlers)
    hand.handler-pc := jmeth.pc ;
    gen-one-bb (jmeth, hand.handler-code-gen, hand, #"handler")  // jbb isn't really a BB, but a lump of code
  end;
  // now know all the handler offsets, fill the exception table
  gen (context.method-context-range-tree) ;

  // fake it for finallys

  local method
    append-finally-ranges (hand :: <finally-handler>)
	  if (hand.finally-handler-been-appended?)
        #()
      else
	    hand.finally-handler-been-appended? := #t ;
        let  peecee = hand.handler-pc ;
        let  total-ranges = #() ;
	for (range in hand.finally-handler-ranges)
          if (instance? (range, <pair>))
            append-to-exception-table (range.head, range.tail, #f, peecee) ;
            total-ranges := pair (range, total-ranges)
          else
            let  subranges = append-finally-ranges (range) ;
            for (rng in subranges)
              append-to-exception-table (rng.head, rng.tail, #f, peecee) ;
              total-ranges := pair (rng, total-ranges)
            end
          end
        end;
        total-ranges
      end
    end;
              
      
  for (hand in context.method-context-used-handlers)
    if (instance? (hand, <finally-handler>))
      append-finally-ranges (hand) 
    else
      let  peecee = hand.handler-pc ;
      for (rng in hand.handler-use-list)
        append-to-exception-table (rng.head, rng.tail, #f, peecee)
      end
    end
  end;
  #f
end;

// eof
