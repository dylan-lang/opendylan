module:      devel-dbg-ui
synopsis:    Console IO and the stop button
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <DEBUGGER-COMMAND-QUEUE>
//    Implements the queue of debugger commands, believe it or not!

define class <debugger-command-queue> (<object>)
  constant slot command-lock :: <simple-lock> = make(<simple-lock>);
  slot accepting-commands :: <notification>;
  constant slot command-available :: <semaphore> = make(<semaphore>);
  constant slot commands :: <deque> = make(<deque>);
end class;


///// INITIALIZE (Dylan)
//    Initializes the command queue, constructing the notifications.

define sealed method initialize 
    (x :: <debugger-command-queue>, #key) => ()
  x.accepting-commands := make(<notification>, lock: x.command-lock);
end method;


///// *GLOBAL-COMMAND-QUEUE*
//    The queue of debugger commands. Only used when
//    *command-queueing-enabled?* is #t.

define variable *global-command-queue* = make(<debugger-command-queue>);


///// POST-DEBUGGER-COMMAND
//    Pushes a constructed debugger command onto the queue.

define method post-debugger-command
    (command :: <debugger-command>) => ()
  with-lock(*global-command-queue*.command-lock)
    push-last(*global-command-queue*.commands, command);
    release(*global-command-queue*.command-available);
  end with-lock;
end method;


///// FETCH-DEBUGGER-COMMAND
//    Pops a constructed debugger command from the queue.

define method fetch-debugger-command
    () => (command :: <debugger-command>)
  with-lock(*global-command-queue*.command-lock)
    release(*global-command-queue*.accepting-commands);
  end with-lock;
  wait-for(*global-command-queue*.command-available);
  with-lock(*global-command-queue*.command-lock)
    pop(*global-command-queue*.commands);
  end with-lock;
end method;


///// READ-COMMAND
//    Picks up a command entered at the console

define method read-command () => (s :: <string>)
  let maybe-s = read-line(*standard-input*);
  if (instance?(maybe-s, <string>))
    maybe-s
  else
    ""
  end if;
end method;

/*
///// TERMINATE-STOP-BUTTON
//    Closes the stop button. Should be called before
//    quitting the debugger.
//    (Currently not used. It isn't really essential, and sometimes
//    crashes for no reason).

define method terminate-stop-button () => ()
  ffi-terminate-stop-button();
end method;
*/

///// READ-CONSOLE-COMMANDS
//    A function to run in a special thread when command queueing is
//    enabled.

define method read-console-commands () => ()
  let command = #f;
  while(~*global-quit*)
    with-lock(*global-command-queue*.command-lock)
      wait-for(*global-command-queue*.accepting-commands);
    end with-lock;
    command := #f;
    while(~command)
      format-out("Command> ");
      let command-string = read-command();
      let seq = tokenize(command-string);
      if ((seq.size > 1) & (check-lexical-correctness(seq)))
        set-parse-sequence(seq);
        command := build-debugger-command();
      end if;
    end while;
    post-debugger-command(command);
  end while
end method;


///// INITIALIZE-COMMAND-READER
//    If command queueing is enabled, this fires off the thread that
//    reads console commands.

define method initialize-command-reader () => ()
  if (*command-queueing-enabled?*)
    let reader-thread =
      make(<thread>,
           name: "Console Command Reader",
           function: read-console-commands);
  end if;
end method;
