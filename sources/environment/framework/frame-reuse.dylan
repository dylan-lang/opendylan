Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Jason Trenouth, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// PROTOCOL

define open generic frame-reusable?
    (frame :: <frame>)
 => (reusable? :: <boolean>);

define open generic frame-reusable?-setter
    (resuable? :: <boolean>, frame :: <frame>)
 => (reusable? :: <boolean>);

define open generic reuse-frames?
    (class :: subclass(<frame>))
 => (reuse-frames? :: <boolean>);

define open generic reuse-frames?-setter
    (value :: <boolean>, class :: subclass(<frame>))
 => (value :: <boolean>);

define open generic call-in-environment-frame
    (function :: <function>, portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key, #all-keys)
 => ();

// obsolete (use ensure-environment-frame)
define open generic find-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => ();

define open generic ensure-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => ();

define open generic reuse-environment-frame (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frame :: false-or(<frame>));

define open generic find-matching-frames 
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frames :: <sequence>);    

define open generic choose-matching-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frame :: false-or(<frame>));    

define open generic choose-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frame :: false-or(<frame>));    

define open generic choose-current-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frame :: false-or(<frame>));    

define open generic current-environment-frame
    (portd :: <port-designator>)
 => (frame :: false-or(<frame>));    

define open generic reuse-matching-frame? 
    (portd :: <port-designator>, frame :: <frame>, class :: subclass(<frame>), #key, #all-keys)
 => (reuse? :: <boolean>);    

define open generic choose-frame
    (portd :: <port-designator>, class :: subclass(<frame>), frames :: <sequence>, #key, #all-keys)
 => (frame :: false-or(<frame>));

define open generic reinitialize-frame 
    (frame :: <frame>, #key, #all-keys)
 => ();

define open generic fork-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => ();

define open generic make-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #key, #all-keys)
 => (frame :: <frame>);

define open generic start-environment-frame
    (frame :: <frame>)
 => ();

define open generic make-environment-thread
    (portd :: <port-designator>, class :: subclass(<frame>), #key name :: <string>, function :: <function>)
 => ();

define open generic do-environment-frame
    (frame :: <frame>, function :: false-or(<function>))
 => ();

define open generic frame-thread-name
    (portd :: <port-designator>, class :: subclass(<frame>))
 => (thread-name :: <string>);

/// <PORT-DESIGNATOR> (internal)

define constant <port-designator> :: <type> = type-union(<frame>, <display>, <port>);


/// PORT-DESIGNATOR-PORT (internal)
//--- cpage: 1998.10.09 These are currently unused. Why? Should we remove them altogether?
/*
define method port-designator-port (_port :: <port>) => (_port :: <port>)
  _port
end method;

define method port-designator-port (frame :: <frame>) => (_port :: <port>)
  port(frame)
end method;

define method port-designator-port (display :: <display>) => (_port :: <port>)
  port(display)
end method;
*/

/// *REUSE-FRAMES?* (internal)

define variable *reuse-frames?* = #t;


/// *FRAME-REUSABLE?-DEFAULT* (internal)

define variable *frame-reusable?-default* = #t;


/// <FRAME-REUSE-MIXIN> (environment-framework)

define open abstract class <frame-reuse-mixin> (<frame>)
  slot frame-reusable? :: <boolean> = *frame-reusable?-default*,
    init-keyword: reusable?:;
end class <frame-reuse-mixin>;


/// FRAME-REUSABLE? (environment-framework)

define method frame-reusable?
    (frame :: <frame>) => (reusable? :: <boolean>)
  #t
end method frame-reusable?;


/// REUSE-FRAMES? (environment-framework)

define method reuse-frames?
    (class :: subclass(<frame>)) => (reuse-frames? :: <boolean>)
  *reuse-frames?*
end method reuse-frames?;

define method reuse-frames?-setter
    (value :: <boolean>, class :: subclass(<frame>)) => (value :: <boolean>)
  *reuse-frames?* := value
end method reuse-frames?-setter;


/// FRAME-REUSE MESSAGE CLASSES (environment-framework)

define open abstract class <frame-reuse-message> (<environment-message>)
end class <frame-reuse-message>;

define class <frame-found-message> (<frame-reuse-message>)
  constant slot message-frame :: false-or(<frame>) = #f,
    init-keyword: frame:;
end class <frame-found-message>;


/// FIND-ENVIRONMENT-FRAME (environment-framework)
/// 
/// obsolete (use ensure-environment-frame)

define method find-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => ()
  apply(ensure-environment-frame, portd, class, initargs)
end method find-environment-frame;


/// ENSURE-ENVIRONMENT-FRAME (environment-framework)

define method ensure-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => ()
  apply(reuse-environment-frame, portd, class, initargs)
    | apply(fork-environment-frame, portd, class, initargs)
end method ensure-environment-frame;



/// REUSE-ENVIRONMENT-FRAME (environment-framework)

define method reuse-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frame :: false-or(<frame>))
  if (reuse-frames?(class))
    let frame = apply(choose-environment-frame, portd, class, initargs);
    if (frame)
      apply-in-frame(frame, reinitialize-frame, frame, initargs);
      call-in-frame(frame, do-environment-frame, frame, *environment-frame-function*);
      frame
    else
      #f
    end if;
  else
    #f
  end if;
end method;


/// CHOOSE-ENVIRONMENT-FRAME (environment-framework)

define method choose-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frame :: false-or(<frame>))
  apply(choose-current-frame, portd, class, initargs)
  | apply(choose-matching-frame, portd, class, initargs);
end method;


/// CHOOSE-CURRENT-FRAME (environment-framework)

define method choose-current-frame (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frame :: false-or(<frame>))
  let frame = current-environment-frame(portd);
  if (frame & apply(reuse-matching-frame?, portd, frame, class, initargs))
    frame
  end if;
end method;


/// CHOOSE-MATCHING-FRAME (environment-framework)

define method choose-matching-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frame :: false-or(<frame>))
  let frames = apply(find-matching-frames, portd, class, initargs);
  apply(choose-frame, portd, class, frames, initargs);
end method;


/// $FORKED-FRAMES-LOCK (internal)

define constant $forking-environment-frames-lock = make(<lock>);

/// WAIT-FOR-FORKED-FRAMES (internal)

define macro wait-for-forking-frames
  { wait-for-forking-frames (?class:expression)
     ?body:body
    end }
    =>
    { with-lock($forking-environment-frames-lock)
        let forking-frame = find-forking-environment-frame(?class);
        if (forking-frame)
          if (wait-for(forking-frame.environment-frame-notification))
            ?body
	  end if;
        else
          ?body
	end if;
      end }
end macro;

/// $FORKING-ENVIRONMENT-FRAMES (internal)

define variable *forking-environment-frames* :: <sequence> = make(<stretchy-vector>);

/// <FORKING-ENVIRONMENT-FRAME> (internal)

define sealed class <forking-environment-frame> (<object>)
  sealed slot environment-frame-class :: subclass(<frame>), required-init-keyword: class:;
  sealed slot environment-frame-count :: <integer> = 0;
  sealed slot environment-frame-notification :: <notification> = make(<notification>, lock: $forking-environment-frames-lock);
end class <forking-environment-frame>;

define sealed domain make (subclass(<forking-environment-frame>));
define sealed domain initialize (<forking-environment-frame>);

/// NOTE-ENVIRONMENT-FRAME-FORKING (internal)

define method note-environment-frame-forking (class :: subclass(<frame>))
 => ()
  with-lock($forking-environment-frames-lock)
    let forking-frame = find-forking-environment-frame(class) | make-forking-environment-frame(class);
    forking-frame.environment-frame-count := forking-frame.environment-frame-count + 1;
  end;
end method;

/// MAKE-FORKING-ENVIRONMENT-FRAME (internal)

define method make-forking-environment-frame (class :: subclass(<frame>))
 => (forking-frame :: <forking-environment-frame>)
  let forking-frame = make(<forking-environment-frame>, class: class);
  *forking-environment-frames* := add!(*forking-environment-frames*, forking-frame);
  forking-frame
end method;

/// FIND-FORKING-ENVIRONMENT-FRAME (internal)

define method find-forking-environment-frame (class :: subclass(<frame>))
 => (forking-frame :: false-or(<forking-environment-frame>))
  block (found)
    for (forking-frame in *forking-environment-frames*)
      // NB test both ways around in case MAKE makes a subclass
      // of requested class.
      if (subtype?(forking-frame.environment-frame-class, class)
	    | subtype?(class, forking-frame.environment-frame-class))
	found(forking-frame);
      end if;
    end for;
  end block;
end method;

/// NOTE-ENVIRONMENT-FRAME-FORKED (internal)

define method note-environment-frame-forked (class :: subclass(<frame>))
 => ()
  with-lock($forking-environment-frames-lock)
    let forking-frame = find-forking-environment-frame(class);
    if (forking-frame)
      forking-frame.environment-frame-count := forking-frame.environment-frame-count - 1;
      if (forking-frame.environment-frame-count = 0)
	*forking-environment-frames* := remove!(*forking-environment-frames*, forking-frame);
	release-all(forking-frame.environment-frame-notification)
      end if;
    end if;
  end;
end method;

/// FIND-MATCHING-FRAMES (environment-framework) 

define method find-matching-frames 
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frames :: <sequence>)
  let frames :: <list> = #();
  wait-for-forking-frames (class)
    // The result will contain the frames in Z order from front to back,
    // but we do this iteration from the bottom up because 'add!' on a
    // list adds to the front...
    do-frames
      (method (frame :: <frame>) => ()
	 when (apply(reuse-matching-frame?, portd, frame, class, initargs))
	   frames := add!(frames, frame)
	 end
       end method,
       z-order: #"bottom-up");
  end;
  frames
end method find-matching-frames;


/// REUSE-MATCHING-FRAME? (environment-framework)

define method reuse-matching-frame?
    (portd :: <port-designator>, frame :: <frame>, class :: subclass(<frame>), #rest initargs, #key)
 => (reuse? :: <boolean>)
  instance?(frame, class)
  & frame-reusable?(frame)
end method;


/// CHOOSE-FRAME (environment-framework)

define method choose-frame 
    (portd :: <port-designator>, class :: subclass(<frame>), frames :: <sequence>, #rest initargs, #key) 
 => (frame :: false-or(<frame>))
  ~empty?(frames) & frames[0];
end method choose-frame;


/// REINITIALIZE-FRAME (environment-framework)

define method reinitialize-frame 
    (frame :: <frame>, #rest initargs, #key) => ()
  #f
end method reinitialize-frame;



/// FORK-ENVIRONMENT-FRAME (environment-framework)

define method fork-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key) 
 => ()
  note-environment-frame-forking(class);
  fork-environment-function(portd,
			    class,
			    method ()
			      let frame = #f;
			      block ()
				frame := apply(make-environment-frame, portd, class, initargs);
			      cleanup
				unless (frame)
				  note-environment-frame-forked(class);
				end unless;
			      end block;
			    end method);
end method;

/// HANDLE-EVENT

define method handle-event (frame :: <frame-reuse-mixin>, event :: <frame-created-event>)
 => ()
  next-method();
  note-environment-frame-forked(object-class(frame));
end method;

/// FORK-ENVIRONMENT-FUNCTION

//---*** cpage: 1998.10.09 TESTING: Use one thread and event queue for all frames.
//              Set this to true to turn this experimental code on.
define variable *one-thread-for-all-frames* :: <boolean> = #f;

define method fork-environment-function
    (portd :: <port-designator>, class :: subclass(<frame>), frame-maker :: <function>)
 => ()
  // Must ensure we evaluate *environment-frame-function* in the "calling"
  // thread, rather than inside the local method below, which will run on
  // the new thread, where it will always be #f!
  let function = *environment-frame-function*;
  local method make-and-start-frame ()
	  let frame = frame-maker();
	  call-in-frame(frame, do-environment-frame, frame, function);
	  start-environment-frame(frame);
	end method;
  //---*** cpage: 1998.10.09 TESTING: Use one thread and event queue for all frames.
  if (*one-thread-for-all-frames*)
    make-and-start-frame();
  else
    make-environment-thread(portd,
			    class,
			    name: frame-thread-name(portd, class),
			    function: frame-thread-function(portd,
							    class,
							    make-and-start-frame));
  end;
end method;
							    

/// MAKE-ENVIRONMENT-FRAME (environment-framework)

//---*** cpage: 1998.10.09 TESTING: Use one thread and event queue for all frames.
define constant $event-queue :: <event-queue> = make(<event-queue>);

define method make-environment-frame
    (portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => (frame :: <frame>)
  //---*** cpage: 1998.10.09 TESTING: Use one thread and event queue for all frames.
  if (*one-thread-for-all-frames*)
    apply(make, class, event-queue: $event-queue, initargs)
  else
    apply(make, class, initargs)
  end
end method make-environment-frame;


/// START-ENVIRONMENT-FRAME (environment-framework)

define method start-environment-frame (frame :: <simple-frame>)
 => ()
  start-frame(frame)
end method;

define method start-environment-frame (frame :: <dialog-frame>)
 => ()
  start-dialog(frame)
end method;


/// Support for managing the thread state
    
define variable *environment-thread-count* :: <integer> = 0;

define constant $environment-thread-lock :: <lock>
    = make(<lock>);

define constant $final-thread-notification :: <notification>
    = make(<notification>, lock: $environment-thread-lock);


/// MAKE-ENVIRONMENT-THREAD (environment-framework)

define method make-environment-thread
    (portd :: <port-designator>, class :: subclass(<frame>), #key name :: <string>, function :: <function>)
 => ()
  duim-debug-message("Creating thread '%s' for frame class %=", name, class);
  with-lock ($environment-thread-lock)
    make(<thread>,
         name: name,
         function: function);
    // Count the new environment thread
    *environment-thread-count* := *environment-thread-count* + 1;
  end
end method make-environment-thread;


/// FRAME-THREAD-FUNCTION (environment-framework)

define method frame-thread-function
    (portd :: <port-designator>, class :: subclass(<frame>), function :: <function>)
 => (new-function :: <function>)
  method ()
    block ()
      with-abort-restart ()
	function()
      end
    cleanup
      with-lock ($environment-thread-lock)
        // This thread is now gone, so notice that and quit
        // if this is the last environment thread
        *environment-thread-count* := *environment-thread-count* - 1;
        when (*environment-thread-count* = 0)
	  exit-environment(0)
        end
      end
    end
  end method
end method frame-thread-function;


/// WAIT-FOR-SHUTDOWN (environment-framework)

define variable *exit-code* :: <integer> = 0;

define function wait-for-shutdown () => (exit-code :: <integer>)
  with-lock ($environment-thread-lock)
    wait-for($final-thread-notification)
  end;
  *exit-code*
end function wait-for-shutdown;


/// EXIT-ENVIRONMENT (environment-framework)

// NB: Must be called with $environment-thread-lock held!
define function exit-environment (status-code :: <integer>) => ()
  *exit-code* := status-code;
  release($final-thread-notification)
end function exit-environment;


/// $THREAD-COUNT-TABLE (internal)

define variable $thread-count-table = make(<table>);


/// FRAME-CLASS-TITLE (environment-framework)
///
/// ---*** Maybe put this in DUIM

define open generic frame-class-title (class :: subclass(<frame>))
 => (title :: <string>);

define method frame-class-title (class :: subclass(<frame>))
 => (title :: <string>)
  format-to-string("%= Thread", class);
end method;


/// FRAME-THREAD-NAME (environment-framework)

define method frame-thread-name
    (portd :: <port-designator>, class :: subclass(<frame>)) => (thread-name :: <string>)
  let title = frame-class-title(class);
  let key = as(<symbol>, title);
  let number = element($thread-count-table, key, default: 1);
  $thread-count-table[key] := number + 1;
  format-to-string("%s %d", title, number)
end method frame-thread-name;


/// *CURRENT-ENVIRONMENT-FRAME* (internal)

define thread variable *current-environment-frame* :: false-or(<frame>)= #f;


/// CURRENT-ENVIRONMENT-FRAME (environment-framework)

define method current-environment-frame (frame :: <frame>)
 => (frame :: <frame>)
  frame
end method current-environment-frame;

define method current-environment-frame (portd :: <port-designator>)
 => (frame :: false-or(<frame>))
  *current-environment-frame*
end method current-environment-frame;


/// WITH-CURRENT-ENVIRONMENT-FRAME (environment-framework)

define macro with-current-environment-frame 
  { with-current-environment-frame (?frame:expression)
     ?body:body
  end }
    =>
    { dynamic-bind (*current-environment-frame* = ?frame)
       ?body
    end }
end macro;       


/// *ENVIRONMENT-FRAME-FUNCTION* (internal)

define thread variable *environment-frame-function* :: false-or(<function>) = #f;


/// DO-ENVIRONMENT-FRAME (internal)

define method do-environment-frame (frame :: <frame>, function == #f)
 => ()
  broadcast($environment-channel, make(<frame-found-message>, frame: frame));
  deiconify-frame(frame);
  raise-frame(frame);
end method;

define method do-environment-frame (frame :: <frame>, function :: <function>)
 => ()
  broadcast($environment-channel, make(<frame-found-message>, frame: frame));
  function(frame);
end method;


/// WITH-ENVIRONMENT-FRAME (environment-framework)

define macro with-environment-frame 
  { with-environment-frame (?frame:name = ?portd:expression, ?class:expression, ?initargs:*)
     ?body:body
  end }
    =>
    { call-in-environment-frame(method (?frame) ?body end method, ?portd, ?class, ?initargs) }
end macro;       


/// CALL-IN-ENVIRONMENT-FRAME (environment-framework)

define method call-in-environment-frame
    (function :: <function>, portd :: <port-designator>, class :: subclass(<frame>), #rest initargs, #key)
 => ()
  dynamic-bind (*environment-frame-function* = function)
    apply(find-environment-frame, portd, class, initargs);
  end;
end method;
