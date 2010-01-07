Module:    timers
Synopsis:  Timer objects
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Useful macro.

define macro with-execution-times
  { with-execution-times (?vars of ?expression) ?body end }
    => { let _timer = make(<execution-timer>);
         set-start(_timer);
         ?expression;
         set-end(_timer);
         let ?vars = values(user-time(_timer), 
                            system-time(_timer), 
                            total-time(_timer));
         ?body }
vars:
  { (?user, ?system, ?total) }
    => { (?user, ?system, ?total) }
end macro;

// Execution timer objects.

define class <execution-timer> (<object>)
  slot start-cpu-time :: <integer>, required-init-keyword: start-cpu-time:;
  slot end-cpu-time   :: <integer>, required-init-keyword: end-cpu-time:;
  slot state          :: <symbol>,  init-value: #"uninitialized";
end class;

define constant usecs = 1000000.0;

define method make (clss == <execution-timer>, #rest vars)
  next-method(clss, start-cpu-time: make-cpu-times(), end-cpu-time: make-cpu-times())
end method;

define method set-start(et :: <execution-timer>)
  et.state := #"started";
  get-cpu-times(et.start-cpu-time);
  #f
end method;

define method set-end(et :: <execution-timer>)
  if (et.state == #"started") et.state := #"stopped" end;
  get-cpu-times(et.end-cpu-time);
  #f
end method;

define method system-time(et :: <execution-timer>)
  if (et.state == #"stopped")
    (cpu-times-system-secs(et.end-cpu-time)
       - cpu-times-system-secs(et.start-cpu-time))
    + ((cpu-times-system-micros(et.end-cpu-time)
        - cpu-times-system-micros(et.start-cpu-time))
          / usecs)
  else
    #f
  end
end method;

define method user-time(et :: <execution-timer>)
  if (et.state == #"stopped")
    (cpu-times-user-secs(et.end-cpu-time)
       - cpu-times-user-secs(et.start-cpu-time))
    + ((cpu-times-user-micros(et.end-cpu-time)
          - cpu-times-user-micros(et.start-cpu-time))
            / usecs)
  else
    #f
  end
end method;

define method total-time(et :: <execution-timer>)
  let st = system-time(et);
  let ut = user-time(et);

  if (st & ut)
    st + ut
  else
    #f
  end
end method;
