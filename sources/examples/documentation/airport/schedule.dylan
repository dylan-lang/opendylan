module: airport 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the airport example scheduling system for the 
        airport library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// The following generic functions comprise the essential protocol for
// interaction between containers and vehicles. 

// Returns true if container is available for aircraft in direction.
define generic available? (vehicle, container, direction); 

// Moves vehicle into container in the given direction.
define generic move-in-vehicle (vehicle, container, direction); 

// Moves vehicle out of container in the given direction.
define generic move-out-vehicle (vehicle, container, direction); 

// Returns the aircraft next in line to move out of container in direction.
define generic next-out (container, direction); 

// Returns the next class of container to move vehicle into, and how long
// it will take to get there.
define generic next-landing-step (container, vehicle); 

// A single storage container is available if the aircraft fits into the
// the container, and there isn't already a vehicle in the container.
define method available?
    (vehicle :: <aircraft>, container :: <single-storage>,
     direction :: <symbol>)
 => (container-available? :: <boolean>)
  object-fits?(vehicle, container)
  & ~ (container.vehicle-currently-occupying);
end method available?; 

// A multiple storage container is available if the aircraft fits into the
// the container, and there are not too many aircraft already queued in the
// container for the specified direction.
define method available? 
    (vehicle :: <aircraft>, container :: <multiple-storage>,
     direction :: <symbol>)
 => (container-available? :: <boolean>)
  object-fits?(vehicle, container)
  & size(container.vehicles-by-direction[direction])
    < container.maxima-by-direction[direction];
end method available?; 

// Avoids jamming the runway with inbound traffic which would prevent
// outbound aircraft from taking off. The runway is only clear to inbound
// traffic if there is space in the next container inbound from the runway.
define method available?
    (vehicle :: <aircraft>, container :: <runway>,
     direction :: <symbol>) 
 => (container-available? :: <boolean>)
  next-method()
    & select (direction)
        #"outbound" => #t;
        #"inbound"
          => let (class) = next-landing-step(container, vehicle);
             if (class)
               find-available-connection(container, class, vehicle) ~== #f;
             end if;
      end select;
end method available?; 

// A slot is used to keep track of which aircraft is in a single 
// storage container.
define method move-in-vehicle
    (vehicle :: <aircraft>, container :: <single-storage>,
     direction :: <symbol>)
 => ()
  container.vehicle-currently-occupying := vehicle;
  values();
end method move-in-vehicle; 

// A deque is used to keep track of which aircraft are traveling in a
// particular direction in a multiple storage container.
define method move-in-vehicle
    (vehicle :: <aircraft>, container :: <multiple-storage>,
     direction :: <symbol>)
 => ()
  let vehicles = container.vehicles-by-direction[direction];
  push-last(vehicles, vehicle);
  values();
end method move-in-vehicle; 

// When an aircraft reaches the gate, it then begins its outbound journey.
define method move-in-vehicle
    (vehicle :: <aircraft>, container :: <gate>, 
     direction :: <symbol>)
 => ()
  next-method();
  vehicle.direction := #"outbound";
  values();
end method move-in-vehicle; 

define method move-out-vehicle
    (vehicle :: <aircraft>, container :: <single-storage>,
     direction :: <symbol>)
 => ()
  container.vehicle-currently-occupying := #f;
  values();
end method move-out-vehicle; 

define method move-out-vehicle
    (vehicle   :: <aircraft>,
     container :: <multiple-storage>, direction :: <symbol>)
 => ()
  let vehicles = container.vehicles-by-direction[direction];
  // Assumes aircraft always exit container in order, and this aircraft
  // is next.
  pop(vehicles);
  values();
end method move-out-vehicle; 

// Determines what vehicle, if any, could move to the next container. If 
// there is such a vehicle, then this method returns the vehicle, 
// the next container in the direction of travel, and the time it 
// would take to make that transition.
define method next-out
    (container :: <vehicle-storage>, direction :: <symbol>)
 => (next-vehicle :: false-or(<vehicle>),
     next-storage :: false-or(<vehicle-storage>),
     time-to-execute :: false-or(<time-offset>));
  let next-vehicle = next-out-internal(container, direction);
  if (next-vehicle)
    let (class, time) = next-landing-step(container, next-vehicle);
    if (class)
      let next-container
        = find-available-connection(container, class, next-vehicle);
      if (next-container)
        values(next-vehicle, next-container, time);
      end if;
    end if;
  end if;
end method next-out; 

// This is just a helper method for the next-out method. We need different
// methods based on the class of container.
define method next-out-internal
    (container :: <single-storage>, desired-direction :: <symbol>)
 => (vehicle :: false-or(<aircraft>))
  let vehicle = container.vehicle-currently-occupying;
  if (vehicle & vehicle.direction == desired-direction) vehicle; end;
end method next-out-internal; 

define method next-out-internal
    (container :: <multiple-storage>, desired-direction :: <symbol>)
 => (vehicle :: false-or(<aircraft>))
  let vehicle-queue = container.vehicles-by-direction[desired-direction];
  if (vehicle-queue.size > 0) vehicle-queue[0]; end;
end method next-out-internal; 

// These methods return the next class of container a vehicle may 
// move to from a particular container. They also return an estimate 
// of how long that transition will take.
define method next-landing-step
    (storage :: <sky>, aircraft :: <aircraft>)
 => (next-class :: false-or(<class>), duration :: false-or(<time-offset>))
  if (aircraft.direction == #"inbound")
    values(<runway>, flying-time(aircraft, storage.airport-below));
  end if;
end method next-landing-step; 

define method next-landing-step
    (storage :: <runway>, aircraft :: <aircraft>)
 => (next-class :: <class>, duration :: <time-offset>)
  select (aircraft.direction)
    #"inbound"  => values(<taxiway>, brake-time(aircraft, storage));
    #"outbound" => values(<sky>, takeoff-time(aircraft, storage));
  end select;
end method next-landing-step; 

define method next-landing-step
    (storage :: <taxiway>, aircraft :: <aircraft>)
 => (next-class :: <class>, duration :: <time-offset>)
  select (aircraft.direction)
    #"inbound"  => values(<gate>, gate-time(aircraft, storage));
    #"outbound" => values(<runway>, runway-time(aircraft, storage));
  end select;
end method next-landing-step; 

define method next-landing-step
    (storage :: <gate>, aircraft :: <aircraft>)
 => (next-class :: <class>, duration :: <time-offset>)
  values(<taxiway>, gate-turnaround(aircraft, storage));
end method next-landing-step; 

// Searches all of the vehicle storage of class class-of-next which is
// connected to container and which has some room for aircraft.
define method find-available-connection
    (storage :: <vehicle-storage>, class-of-next :: <class>,
     aircraft :: <aircraft>)
 => (next-container :: false-or(<vehicle-storage>))
  block (return)
    for (c in storage.connected-to)
      if (instance?(c, class-of-next)
          & available?(aircraft, c, aircraft.direction))
        return(c);
      end if;
    end for;
  end block;
end method find-available-connection; 

// Generate new transitions to be considered for the next move. 
// The transitions will be placed in the sorted sequence, which will order
// them by earliest arrival time.
define method generate-new-transitions
    (container :: <vehicle-storage>, active-transitions :: <sorted-sequence>,
     containers-visited :: <object-table>)
 => ()
  unless(element(containers-visited, container, default: #f))
    // Keep track of which containers we have searched for new possible
    // transitions. We avoid looping forever by checking each container just 
    // once.
    containers-visited[container] := #t; 

    local method consider-transition (direction)
      // See if any vehicle is ready to transition out of a container.
      let (vehicle, next-container, time) = next-out(container, direction);
      unless (vehicle == #f | vehicle.next-transition.pending?)
        // If there is a vehicle ready, and it isn't already in the 
        // sorted sequence of pending transitions, then prepare the 
        // transition instance associated with the vehicle.
        let transition = vehicle.next-transition;
        transition.from-container := container;
        transition.to-container := next-container;
        // The vehicle may have been waiting. Take this into account when
        // computing the earliest arrival into the next container.
        transition.earliest-arrival := transition.earliest-arrival + time;
        // Flag the vehicle as pending to save searching through the
        // active-transitions sorted sequence later.
        transition.pending? := #t;
        // Add the transition to the set to be considered.
        add!(active-transitions, transition);
      end unless;
    end method consider-transition; 

    // Consider both inbound and outbound traffic.
    consider-transition(#"outbound");
    consider-transition(#"inbound");
    // Make sure every container connected to this one is checked.
    for (c in container.connected-to)
      generate-new-transitions(c, active-transitions, containers-visited);
    end for;
  end unless;
end method generate-new-transitions; 

// Main loop of the program. See what possible transitions exist, then
// execute the earliest transitions which may be completed. Returns the time
// of the last transition.
define method process-aircraft 
    (airport :: <airport>, #key time = $midnight) => (time :: <time-of-day>)
  format-out("Detailed aircraft schedule for ");
  say(airport);
  format-out("\n\n");
  let sky = airport.sky-above;
  let containers-visited = make(<object-table>);
  let active-transitions = make(<sorted-sequence>, 
                                value-function: earliest-arrival); 

  // We don't have to always name the exit procedure return.
  block (done)
    while (#t)
      // Each time through, start by considering every container.
      fill!(containers-visited, #f);
      // For every container, see if any vehicles are ready to transition.
      // If so, add transition instances to the active-transitions 
      // sorted sequence.
      generate-new-transitions(sky, active-transitions, 
                               containers-visited);
      // If there are no more transitions, we have completed our task.
      if (empty?(active-transitions)) done(); end;
      // Find the earliest transition which can complete because there is 
      // still room available in the destination container.
      let transition-index
        = find-key(active-transitions,
                   method (transition)
                     available?(transition.transition-aircraft,
                                transition.to-container,
                                transition.transition-aircraft.direction);
                   end); 

      // If none can complete, there is a problem with the simulation. This
      // should never happen, but is useful for debugging incorrect
      // container configurations.
      if (transition-index == #f)
        error("Pending transitions but none can complete.");
      end if; 

      // Otherwise, the earliest transition that can complete has been
      // found. Execute the transition.
      let transition = active-transitions[transition-index];
      let vehicle = transition.transition-aircraft;
      let vehicle-direction = vehicle.direction;
      move-out-vehicle(vehicle, transition.from-container, 
                       vehicle-direction);
      move-in-vehicle(vehicle, transition.to-container, vehicle-direction); 

      // This transition is complete. Remove it from consideration.
      transition.pending? := #f;
      remove!(active-transitions, transition);
      // Compute the actual time of arrival at the next container, and       /
      // display the message.
      time := (transition.earliest-arrival 
                := max(time, transition.earliest-arrival));
      say(transition);
      format-out("\n");
    end while;
  end block;
  time;
end method process-aircraft; 
