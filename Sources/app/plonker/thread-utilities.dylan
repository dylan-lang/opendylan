Module:    plonker
Synopsis:  Play with MIDI
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// This thread synchronization model lifted wholesale from the environment
    
define variable *thread-count* :: <integer> = 0;

define constant $thread-lock :: <lock> = make(<lock>);

define constant $final-thread-notification :: <notification>
    = make(<notification>, lock: $thread-lock);

define method make-application-thread (#rest args, #key function) => ()
  with-lock ($thread-lock)
    apply(make, <thread>, function: application-thread-function(function), args);
    *thread-count* := *thread-count* + 1;
  end
end method make-application-thread;

define method application-thread-function 
    (function :: <function>) => (new-function :: <function>)
  method ()
    block ()
      function()
    cleanup
      with-lock ($thread-lock)
        *thread-count* := *thread-count* - 1;
        when (*thread-count* = 0)
	  exit-application(0)
        end;
      end;
    end;
  end method;
end method application-thread-function;

define variable *exit-code* :: <integer> = 0;

define function wait-for-shutdown () => (exit-code :: <integer>)
  with-lock ($thread-lock)
    wait-for($final-thread-notification)
  end;
  *exit-code*
end function wait-for-shutdown;

// NB: Must be called with $thread-lock held!
define function exit-application (status-code :: <integer>) => ()
  *exit-code* := status-code;
  release($final-thread-notification)
end function exit-application;

// eof

