module:      access-path-implementation
synopsis:    Functions for handling stack traces
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $nub-dynamic-chain-terminator = 0;


///// <STACK-FRAME>

define abstract class <stack-frame> (<object>)

       slot nub-descriptor :: <integer>,
            required-init-keyword: descriptor:;

       slot pointer :: <remote-value>,
            init-keyword: pointer:;

       slot return-address :: <remote-value>,
            init-keyword: return-address:;

       slot next-instruction :: <remote-value>,
            init-keyword: next-instruction:;

       slot next :: false-or(<stack-frame>),
            init-value: #f;

       slot previous :: false-or(<stack-frame>),
            init-value: #f;

       slot lexicals :: <vector>,
            init-value: #[];

end class;


///// <FUNCTION-FRAME>

define class <function-frame> (<stack-frame>)
end class;


///// <UNBUILT-FUNCTION-FRAME>

define class <unbuilt-function-frame> (<function-frame>)
end class;


///// <LEXICAL-VARIABLE>

define abstract class <lexical-variable> (<object>)

       slot C-name :: <string>,
            required-init-keyword: name:;

       slot lexical-variable-address :: <remote-value>,
            required-init-keyword: address:;

//     slot lexical-variable-type :: <remote-type>;

       // DUMMY ONLY HACK

       slot argument? :: <boolean>,
         init-value: #f,
         init-keyword: argument?:;

end class;


///// GENERIC FUNCTIONS

define generic initialize-stack-trace (ap :: <access-path>,
                                       thread :: <remote-thread>)
                                       => (_ :: <function-frame>);

define generic next-frame (ap :: <access-path>, frame :: <function-frame>)
                           => (_ ::false-or(<function-frame>));

define generic previous-frame (ap :: <access-path>, frame :: <function-frame>)
                               => (_ :: false-or(<function-frame>));

define generic frame-pointer (ap :: <access-path>, frame :: <function-frame>)
                              => (_ :: <remote-value>);

define generic frame-return-address (ap :: <access-path>,
                                     frame :: <stack-frame>)
                                     => (_ :: <remote-value>);

define generic frame-instruction-address (ap :: <access-path>,
                                          frame :: <function-frame>)
                                          => (_ :: <remote-value>);

define generic lexical-variable-name (v :: <lexical-variable>) => (_ :: <string>);

define generic do-frame-arguments (function :: <function>,
                                   ap :: <access-path>,
                                   frame :: <function-frame>) => ();

define generic do-frame-lexicals (function :: <function>,
                                  ap :: <access-path>,
                                  frame :: <function-frame>) => ();


///// INITIALIZE-STACK-TRACE

define method initialize-stack-trace 
  (ap :: <access-path>, thread :: <remote-thread>)
     => (_ :: <function-frame>)

  // Pull out the topmost stack frame for this thread.

  if (thread.stack)
    thread.stack
  else
    let (closest, previous, nxt)
      = nearest-symbols(ap, thread.nub-descriptor.eip);
    if ((closest) &
        (closest.remote-symbol-address == thread.nub-descriptor.eip))
      thread.stack := get-unbuilt-function-frame(ap.connection, thread);
    else
      thread.stack := get-top-function-frame (ap.connection, thread);
    end if;
    thread.stack
  end if;
end method;


///// GET-TOP-FUNCTION-FRAME
//    Looks at the frame-pointer in the frozen runtime (for the given
//    thread) and constructs a <function-frame> object for the topmost
//    stack frame.

define method get-top-function-frame 
  (conn :: <local-access-connection-32>, thread :: <remote-thread>)
    => (_ :: <function-frame>)

  // Pull all information about the topmost stack frame out of the 
  // debugger nub.

  let nub-top-frame :: <integer>
    = thread.nub-descriptor.ebp;

  let top-frame-pointer :: <remote-value> 
    = nub-top-frame;

  let top-frame-return :: <remote-value>
    = conn.process.memory.contents[nub-top-frame + 1];

  // As this is the top frame, the next instruction to be executed in the
  // frame is known to be the next instruction executed by this thread.

  let top-frame-next-instr :: <remote-value>
    = thread.nub-descriptor.eip;
       
  // Create the corresponding instance of <function-frame> and
  // return it.

  let top-frame = make (<function-frame>,
                        descriptor: nub-top-frame,
                        pointer: top-frame-pointer,
                        return-address: top-frame-return,
                        next-instruction: top-frame-next-instr);

  top-frame;
end method;


///// GET-UNBUILT-FUNCTION-FRAME
//    Looks at the frame-pointer in the frozen runtime (for the given
//    thread) and constructs a proxy for the yet-to-be-built
//    stack frame.

define method get-unbuilt-function-frame 
  (conn :: <local-access-connection-32>, thread :: <remote-thread>)
    => (_ :: <unbuilt-function-frame>)

  // Pull all information about the topmost stack frame out of the 
  // debugger nub.

  let nub-top-frame :: <integer>
    = thread.nub-descriptor.ebp;

  let top-frame-pointer :: <remote-value> 
    = nub-top-frame;

  // For an unbuilt frame, the return address is known to be at the
  // top of stack (since a CALL will have just been executed).

  let top-frame-return :: <remote-value>
    = conn.process.memory.contents[thread.nub-descriptor.esp];

  // As this is the top frame, the next instruction to be executed in the
  // frame is known to be the next instruction executed by this thread.

  let top-frame-next-instr :: <remote-value>
    = thread.nub-descriptor.eip;
       
  // Create the corresponding instance of <function-frame> and
  // return it.

  let top-frame = make (<unbuilt-function-frame>,
                        descriptor: nub-top-frame,
                        pointer: top-frame-pointer,
                        return-address: top-frame-return,
                        next-instruction: top-frame-next-instr);

  top-frame;
end method;


///// GET-PREVIOUS-FUNCTION-FRAME
//    Given a function frame, examines the state of the stack and returns
//    the <function-frame> object for the previous stack frame, if one
//    exists.

define method get-previous-function-frame 
     (conn :: <local-access-connection-32>, frame :: <function-frame>)
        => (_ :: false-or(<function-frame>))

  let nub-frame :: <integer> = 
    conn.process.memory.contents[frame.pointer];

  let frame-pointer :: <remote-value> = nub-frame;

  // If the frame pointer is a termination of the dynamic chain, then
  // return #f rather than an instance of <function-frame>.

  if ((frame-pointer == 0) | (frame-pointer == $NOP))          
    #f;
  else
    // We still have a valid frame, so get its return address from the
    // nub.

    let frame-return
      = conn.process.memory.contents[frame-pointer + 1];

    // The next instruction address is not pulled from the nub, since
    // it is equal to the return address of the last frame that was
    // pulled.

    let older-frame = make (<function-frame>,
                            descriptor: nub-frame,
                            pointer: frame-pointer,
                            return-address: frame-return,
                            next-instruction: frame.return-address);

     // Link the frames.

     older-frame.next := frame;
     frame.previous := older-frame;
     older-frame.previous := #f;
     older-frame;

   end if
end method;


define method get-previous-function-frame 
     (conn :: <local-access-connection-32>, frame :: <unbuilt-function-frame>)
        => (_ :: false-or(<function-frame>))

  let nub-frame :: <integer> = frame.nub-descriptor;
  let frame-pointer :: <remote-value> = frame.pointer;
  let frame-return
      = conn.process.memory.contents[frame-pointer + 1];

  // The next instruction address is not pulled from the nub, since
  // it is equal to the return address of the last frame that was
  // pulled.

  let older-frame = make (<function-frame>,
                          descriptor: nub-frame,
                          pointer: frame-pointer,
                          return-address: frame-return,
                          next-instruction: frame.return-address);

  // Link the frames.

  older-frame.next := frame;
  frame.previous := older-frame;
  older-frame.previous := #f;
  older-frame;
end method;


///// NEXT-FRAME

define method next-frame (ap :: <access-path>, frame :: <function-frame>)
    => (_ :: false-or(<function-frame>))
  frame.next
end method;


///// PREVIOUS-FRAME

define method previous-frame (ap :: <access-path>, frame :: <function-frame>)
    => (_ :: false-or(<function-frame>))
  if (frame.previous)
    frame.previous
  else
    let f = get-previous-function-frame (ap.connection, frame);
    f;
  end if;
end method;


///// NUMBER-OF-ACTIVE-THREADS
//    Counts the number of threads in an access path.

define method number-of-active-threads (ap :: <access-path>)
    => (i :: <integer>)
  size(ap.threads);
end method;


///// NUMBER-OF-FRAMES-ON-STACK
//    Counts the stack frames for a thread.

define method number-of-frames-on-stack 
    (ap :: <access-path>, thread :: <remote-thread>)
       => (i :: <integer>)
  let i = 0;
  let current-frame = initialize-stack-trace(ap, thread);
  while (current-frame)
    i := i + 1;
    current-frame := previous-frame(ap, current-frame);
  end while;
  i;
end method;


///// FRAME-POINTER

define method frame-pointer 
  (ap :: <access-path>, frame :: <function-frame>)
    => (_ :: <remote-value>)
  frame.pointer
end method;

define method frame-pointer 
  (ap :: <access-path>, frame :: <unbuilt-function-frame>)
    => (_ :: <remote-value>)
  0
end method;


///// FRAME-RETURN-ADDRESS

define method frame-return-address 
    (ap :: <access-path>, frame :: <function-frame>)
  => (_ :: <remote-value>)
  frame.return-address
end method;


///// FRAME-INSTRUCTION-ADDRESS

define method frame-instruction-address 
    (ap :: <access-path>, frame :: <function-frame>)
       => (_ :: <remote-value>)
  frame.next-instruction
end method;


///// DO-FRAME-ARGUMENTS

define method do-frame-arguments 
    (function :: <function>, ap :: <access-path>,
     frame :: <function-frame>) => ()

  let frame-ptr :: <remote-value>
     = frame-pointer (ap, frame);

  // Since we are lazily pulling debug info, we may not yet have
  // examined the lexicals for this frame. Read them now if
  // necessary.

  if (frame.lexicals = #[])
    read-frame-lexicals (ap.connection, frame)
  end if;

  // Apply the function only to frame arguments - these being lexicals
  // with positive frame offsets.

  for (lexvar in frame.lexicals)
           
    let addr :: <remote-value>
      = lexical-variable-address (lexvar);

    if (lexvar.argument?)
      function (lexvar)
    end if

  end for;

end method;


///// DO-FRAME-LEXICALS

define method do-frame-lexicals 
    (function :: <function>, ap :: <access-path>,
     frame :: <function-frame>) => ()

  // Again, now is the time to pull lexical variable information if
  // we need it.

  if (frame.lexicals = #[])
    read-frame-lexicals (ap.connection, frame)
  end if;

  // Now we apply "function" to all lexicals.

  for (lexvar in frame.lexicals)
    function (lexvar)
  end for;
end method;


///// LIVE-LEXICALS-COUNT
//    Pretty poor but valid implementation

define method live-lexicals-count
    (ap :: <access-path>, frame :: <function-frame>) => (count :: <integer>)
  let i = 0;
  do-frame-lexicals(method (x :: <lexical-variable>)
                      i := i + 1;
                    end method,
                    ap,
                    frame);
  i;
end method;


///// LEXICAL-VARIABLE-NAME
//    This is a virtual slot accessor. The name is stored as a C-string. 
//    This function converts the name to a dylan <string>.

define method lexical-variable-name (v :: <lexical-variable>) 
    => (_ :: <string>)
  v.C-name;
end method;


///// FIND-LEXICAL-VARIABLE

define method find-lexical-variable 
    (ap :: <access-path>, frame :: <function-frame>, name :: <string>)
      => (_ :: false-or(<lexical-variable>))

  // Maybe this function should be implemented in terms of do-frame-lexicals
  // (in the same way as find-symbol), except that I'll run into the
  // closure bug again...

  let target = #f;

  // Once again, get the lexicals vector if necessary.

  if (frame.lexicals = #[])
    read-frame-lexicals (ap.connection, frame)
  end if;

  // Point target to a matching lexical if it is found. (There can only
  // be at most one matching lexical variable, otherwise the program
  // being debugged is completely knackered anyway!)

  for (lexvar in frame.lexicals)
    if (lexvar.lexical-variable-name = name)
      target := lexvar
    end if
  end for;

  // Just return whatever we managed to find.
  target;
end method;


///// READ-FRAME-LEXICALS

define method read-frame-lexicals (conn :: <local-access-connection-32>,
                                   frame :: <function-frame>) => ()

  let func = map-eip-to-function(conn.process, frame.next-instruction);
  let args = func.arguments;
  let lexs = func.lexicals;
  let list = make(<vector>, size: size(args) + size(lexs));
  let i = 0;
  for (arg in args)
    list[i] := make(<lexical-variable>,
                    name: arg,
                    address: frame.pointer - (i + 1),
                    argument?: #t);
    i := i + 1;
  end for;
  for (lex in lexs)
    list[i] := make(<lexical-variable>,
                    name: lex,
                    address: frame.pointer - (i + 1),
                    argument?: #f);
    i := i + 1;
  end for;
  frame.lexicals := list;
end method;

define method older-stack-frame?
  (ap :: <access-path>, this-one :: <remote-value>, 
   than-this-one :: <remote-value>)
     => (answer :: <boolean>)
  this-one > than-this-one
end method;
