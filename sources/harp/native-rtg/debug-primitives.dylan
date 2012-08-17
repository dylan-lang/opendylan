module:    harp-native-rtg
Synopsis:  Debugger Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define open generic op--output-debug-string
    (be :: <harp-back-end>, string :: <register>) => ();


/// Debugger primitives ...


define runtime-primitive break
  // On entry:
  //    
  // On exit: Don't exit. Break to debugger.
  // 
  ins--halt(be);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define runtime-primitive error
  // On entry: No args
  //    
  // On exit:  No exit. The code calls Dylan's ERROR
  // 
  op--call-xep(be, dylan-error-function, primitive-error-string);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


// Under-debugger is a variable which indicates whether there is an interest in 
// communicating with the debugger. By default it's set to 0. The Dylan debugger
// will set it to something else if it is involved in communication with the app.
//
define c-full-indirect runtime-variable under-debugger = "%running-under-dylan-debugger?";



define runtime-primitive debug-message
  // On entry: 
  //    (format-string, number-of-args, vector-of-args)
  //    NB: this takes an addition parameter from the Dylan
  //    primitive call. It turns out that this is unnecessary,
  //    so this value simply exists for historical and compatibility
  //    reasons. It's ignored by the implementation.
  //    
  // If the debugger has asked to be informed of debug messages,
  // then this primitive will break to the debugger with the 
  // stack containing the format string, followed by
  // the number of format args followed by the format args
  //
  // On exit:  No meaningful value
  // 
  result result;
  stack stack;
  nreg argnum, data, size-in-words, size-in-bytes;
  reg string, arg-vector;
  tag done;

  ins--beq(be, done, under-debugger, 0);
  op--load-arguments(be, string, argnum, arg-vector);

  op--vector-size(be, size-in-words, arg-vector);
  ins--asl(be, size-in-bytes, size-in-words, 2);
  ins--sub(be, stack, stack, size-in-bytes);
  ins--add(be, data, arg-vector, 8);
  ins--copy-words-down-w(be, stack, data, size-in-words);
  ins--push(be, size-in-words);
  ins--push(be, string);
  ins--halt(be);         // break to the debugger
  ins--pop(be, result);  // return the format string

  ins--tag(be, done);
  op--rts-dropping-n-args(be, 3);
end runtime-primitive;





define runtime-primitive invoke-debugger
  // On entry: 
  //    (format-string, vector-of-args)
  //    This has a similar interface and implementation to debug-message
  //   
  // If the dylan debugger is not parenting the application at the
  // time this primitive is called, then the primitive will call
  // OutputDebugString with the format-string template. This is so that
  // any foreign debugger that might be available will at least be
  // able to relay some information about the error. 
  // Following this, the primitive pushes the format arguments onto
  // the stack, followed by a counter of those arguments, and finally
  // by the format-string itself. The dylan debugger will expect them
  // to be so positioned. The primitive then executes an interrupt,
  // which the Dylan debugger will be able to interpret as an unhandled
  // condition. Any foreign debugger will simply report this as a
  // hard-coded breakpoint.
  //
  // On exit:  No meaningful value
  // 
  result result;
  stack stack;
  nreg data, size-in-words, size-in-bytes, raw-string;
  reg string, arg-vector;
  tag shared-dbg;

  op--load-arguments(be, string, arg-vector);
  ins--bne(be, shared-dbg, under-debugger, 0);

  ins--add(be, raw-string, string, 8);
  op--output-debug-string(be, raw-string);

  ins--tag(be, shared-dbg);
  op--vector-size(be, size-in-words, arg-vector);
  ins--asl(be, size-in-bytes, size-in-words, 2);
  ins--sub(be, stack, stack, size-in-bytes);
  ins--add(be, data, arg-vector, 8);
  ins--copy-words-down-w(be, stack, data, size-in-words);
  ins--push(be, size-in-words);
  ins--push(be, string);
  ins--halt(be);               // break to the debugger
  ins--pop(be, result);        // return the format string
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;


define c-runtime-primitive class-allocation-break 
  c-result result;
  nreg string, class, count, size;
  tag shared-dbg, done;

  ins--beq(be, done, class-profiling?, dylan-true);
  op--c-load-arguments(be, string, class, count, size);
  ins--bne(be, shared-dbg, under-debugger, 0);

  op--output-debug-string(be, string);

  ins--tag(be, shared-dbg);
  ins--push(be, size);
  ins--push(be, class);
  ins--halt(be);               // break to the debugger
  ins--pop(be, result); 

  ins--tag(be, done);
  ins--rts(be);
end c-runtime-primitive;


define runtime-primitive inside-debugger?
  result result;
  tag false, done;

  ins--beq(be, false, under-debugger, 0);
  ins--move(be, result, dylan-true);
  ins--bra(be, done);

  ins--tag(be, false);
  ins--move(be, result, dylan-false);

  ins--tag(be, done);
  op--rts-dropping-n-args(be, 0);
end runtime-primitive;
