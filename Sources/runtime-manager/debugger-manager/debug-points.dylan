module:        dm-internals
synopsis:      Breakpoints and watchpoints - high-level support.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <PER-ADDRESS-DEBUG-POINT-LIST>
//    Holds a list of <debug-point> objects whose registered addresses
//    are the same. When a breakpoint (for example) is hit at that
//    address, all debug points in the associated list are selected and
//    processed.

define class <per-address-debug-point-list> (<dm-registered-descriptor>)
  constant slot registered-address :: <remote-value>,
    required-init-keyword: address:;
  slot debug-point-sequence :: <stretchy-vector> 
    = make(<stretchy-vector>);
end class;


/////
///// <DEBUG-POINT>
/////

define open abstract class <debug-point> (<dm-registered-descriptor>)
  constant slot debug-point-address :: <remote-value>,
    required-init-keyword: address:;
  constant slot debug-point-callback :: <function>,
    required-init-keyword: callback:;
end class;


///// <DEBUG-POINT-ERROR>
//    Signalled when functions in this file go pear-shaped.

define class <debug-point-error> (<error>)
end class;


///// <BREAKPOINT>

define open abstract class <breakpoint> (<debug-point>)
end class;


///// <WATCHPOINT>

define open abstract class <watchpoint> (<debug-point>)
end class;


///// HANDLE-DEBUG-POINT-EVENT (and its default method)

define open generic handle-debug-point-event 
  (application :: <debug-target>, debug-point :: <debug-point>,
   thr :: <remote-thread>) 
    => (register-interest? :: <boolean>);


define method handle-debug-point-event
  (application :: <debug-target>, debug-point :: <debug-point>,
   thr :: <remote-thread>) 
    => (register-interest? :: <boolean>)
  let interested? =
     debug-point.debug-point-callback(application, debug-point, thr);
  if (interested?)
    interested?;
  else
    #f;
  end if
end method;


///// REGISTER-DEBUG-POINT

define method register-debug-point
  (application :: <debug-target>, debug-point :: <debug-point>) => ()

  // get-per-address-list
  // If there is already a <per-address-debug-point-list> for the address
  // that the client wants to register this new debug-point at, then
  // this method returns it. Otherwise, it creates a new
  // <per-address-debug-point-list>, adds it to the known ones, and
  // returns it. (The boolean result is #t if a new list was created,
  // since this means that an actual new breakpoint has to be set in the
  // access-path.

  local method get-per-address-list () 
      => (l :: <per-address-debug-point-list>, n :: <boolean>)
    let i = 0;
    let found = #f;
    let new? = #f;
    while ((~found) & (i < size (application.registered-debug-points)))
      if (debug-point.debug-point-address = 
        application.registered-debug-points[i].registered-address)
        found := application.registered-debug-points[i];
      else
        i := i + 1;
      end if
    end while;
    unless (found)
      let new-list = make (<per-address-debug-point-list>,
                           address: debug-point.debug-point-address);
      dm-register (new-list);
      application.registered-debug-points :=
        add-new! (application.registered-debug-points, new-list);
      found := new-list;
      new? := #t;
    end unless;
    values (found, new?);
  end method;

  let (addr-list, new?) = get-per-address-list();
  dm-register (debug-point);
  addr-list.debug-point-sequence :=
    add! (addr-list.debug-point-sequence, debug-point);
  if (new?)
    let worked =
      enable-breakpoint (application.debug-target-access-path,
                         debug-point.debug-point-address);
    if (~worked)
      signal (make (<debug-point-error>))
    end if;
  end if
end method;


///// DEREGISTER-DEBUG-POINT

define method deregister-debug-point
   (application :: <debug-target>, debug-point :: <debug-point>) => ()

  // Registered debug-points are stored in sequences.
  // We CANNOT physically remove the debug point at this time,
  // since debug-points are allowed to deregister themselves at
  // callback time, meaning we might be halfway through an iteration
  // over the self-same sequence that we're removing from.
  // This is deadlier than a four-second egg, so just mark the
  // thing as "deregistered" for now, and remove it "safely"
  // later on.

  dm-deregister (debug-point);
  application.need-to-clear-debug-points? := #t;
end method;


///// CLEAR-DEREGISTERED-DEBUG-POINTS
//    Is called just before continuing the application. This takes care of
//    removing all deregistered debug points.

define method clear-deregistered-debug-points
    (application :: <debug-target>) => ()

  // For each address that has registered debug-points, clear
  // away all debug points that have since been deregistered.

  if (application.need-to-clear-debug-points?)
    for (per-address-list in application.registered-debug-points)
      per-address-list.debug-point-sequence :=
        dm-tidy-sequence (per-address-list.debug-point-sequence);
      if (per-address-list.debug-point-sequence = #[])
        let worked? = 
          disable-breakpoint (application.debug-target-access-path,
                              per-address-list.registered-address);
        dm-deregister (per-address-list);
        if (~worked?)
          signal (make (<debug-point-error>))
        end if
      end if
    end for;

    application.registered-debug-points :=
      dm-tidy-sequence (application.registered-debug-points);

    application.need-to-clear-debug-points? := #f;

  end if;
end method;


///// PROCESS-DEBUG-POINTS
//    Is called when a <debug-point-stop-reason> is received. Selects all
//    debug-points registered at this address and calls the generic
//    handle-debug-point-event on each one. Also builds up a sequence of 
//    those <debug-point> objects that signalled themselves as 
//    being "interesting"

define method process-debug-points
    (application :: <debug-target>, stop-reason :: <source-step-stop-reason>)
  => (interested-debug-points :: <sequence>)
  #[]
end method;

define method process-debug-points 
    (application :: <debug-target>, stop-reason :: <debug-point-stop-reason>) 
  => (interested-debug-points :: <sequence>)

  local method get-per-address-list ()
       =>  (l :: <per-address-debug-point-list>)
      
    let stopped-address = stop-reason-debug-point-address (stop-reason);
    let i = 0;
    let lst = #f;

    // Assumption:
    // It is impossible to receive a <debug-point-stop-reason> if there
    // have been no debug points registered at this address, hence I'm not
    // handling the case where there is no <per-address-debug-point-list>
    // for this address.
    // This should be safe, since the access path differentiates between
    // <debug-point-stop-reason> (a debug point that the debugger nub knew
    // about, which _must_ have been registered), and <break-stop-reason>,
    // which is probably a hard-coded breakpoint.

    // THIS CAN BREAK IF SOMEBODY DOES AN END-RUN AROUND THE DM AND
    // SETS A BREAKPOINT DIRECTLY.

    while (~lst)
      if (application.registered-debug-points[i].registered-address =
          stopped-address)
        lst := application.registered-debug-points[i];
      else
        i := i + 1;
      end if;
    end while;
    lst;
  end method;

  let interesting-debug-points = make (<stretchy-vector>, size: 0);
  let selected-debug-points = get-per-address-list ();

  for (this-debug-point in selected-debug-points.debug-point-sequence)
    unless (this-debug-point.marked-for-deregistration?)
      if (handle-debug-point-event (application, this-debug-point,
                                    stop-reason-thread (stop-reason)))
        interesting-debug-points
          := add! (interesting-debug-points, this-debug-point);
      end if
    end unless;
  end for;
  interesting-debug-points;
end method;


/////
///// <THREAD-SENSITIVE-BREAKPOINT>
/////

define open abstract class <thread-sensitive-breakpoint> (<breakpoint>)

  constant slot
    breakpoint-affected-thread :: <remote-thread>,
    required-init-keyword: thread:;

end class;

define method handle-debug-point-event
    (application :: <debug-target>, bp :: <thread-sensitive-breakpoint>,
     signalling-thread :: <remote-thread>)
      => (interested? :: <boolean>)

  if (signalling-thread == bp.breakpoint-affected-thread)
    next-method();
  else
    #f;
  end if
end method;


