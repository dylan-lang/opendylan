module: thread-test
synopsis: stress-test thread creation
author: Andreas Bogk

define library thread-test
  use common-dylan;
  use io;
end library;

define module thread-test
  use common-dylan;
  use threads;
  use streams;
  use standard-io;
  use format-out;
end module;

define thread variable *thread-id* :: <integer> = -1;

define function say(format :: <string>, #rest args)
  with-stream-locked(*standard-output*)
    apply(format-out, format, args);
    force-output(*standard-output*);
  end with-stream-locked;
end function;

define variable *launch-lock* = make(<simple-lock>);
define variable *launch-note* = make(<notification>, lock: *launch-lock*);

define function test-thread (i :: <integer>)
 => ();
  block()
    *thread-id* := i;

    say("Thread %d launched with id %d.\n", i, *thread-id*);
    
    with-lock(*launch-lock*)
      wait-for(*launch-note*);
    end with-lock;
    
    say("Thread %d working.\n", i);
    
    for(x from 0 below 100)
      say("Thread %d loop %=\n", i, x);
      
      if(modulo(x, 5) == 0)
        thread-yield();
      end if;
    end for;
    
    say("Thread %d finished with id %d.\n", i, *thread-id*);

  end block;
end function;

begin  
  let threads :: <vector> = make(<simple-object-vector>);

  let priorities = list($low-priority,
                        $background-priority,
                        $normal-priority,
                        $interactive-priority,
                        $high-priority);

  let names = list("Zero", "One", "Two", "Three", "Four");

  say("Starting.\n");

  with-lock(*launch-lock*)
    for(i from 0 below 5, prio in priorities, name in names)

      say("Launching thread %d at priority %d.\n", i, prio);

      let thread = make(<thread>,
                        name: name,
                        number: i,
                        priority: prio,
                        function: curry(test-thread, i));

      threads := add(threads, thread);

    end for;
  end with-lock;

  say("Waiting for launches.\n");
  sleep(2.0);

  say("Notifying all.\n");
  with-lock(*launch-lock*)
    release-all(*launch-note*);
  end with-lock;

  say("Joining\n");
  while(~threads.empty?)
    let t = apply(join-thread, threads);
    threads := remove(threads, t);
    say("Joined %=, remaining threads %=\n", t, threads);
  end while;

  say("Finished.\n");
end
