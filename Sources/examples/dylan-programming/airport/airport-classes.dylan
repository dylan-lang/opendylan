module: airport
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The classes and a few basic methods used to implement the airport example.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// PHYSICAL OBJECTS AND SIZE

// Used to keep track of object dimensions and object capacities.
// All dimensions are in feet.
define class <size> (<object>)
  slot length :: <positive-integer>, init-keyword: length:;
  slot width :: <positive-integer>, init-keyword: width:;
  slot height :: <positive-integer>, init-keyword: height:;
end class <size>;

define abstract class <physical-object> (<object>)
  slot current-position :: <position>, init-keyword: current-position:;
  slot physical-size :: <size>, init-keyword: physical-size:;
end class <physical-object>;

define method say (physical-object :: <physical-object>) => ()
  format-out("object at ");
  say(physical-object.current-position);
end method say; 

// VEHICLE STORAGE 

// The default size for a vehicle container.
define constant $default-capacity
  = make(<size>, length: 350, width: 200, height: 100);

// This class represents a location where an aircraft could be stored.
define abstract class <vehicle-storage> (<physical-object>)
  slot storage-capacity :: <size> = $default-capacity,
    init-keyword: capacity:;
  each-subclass slot name-prefix :: <string> = "Storage", setter: #f;
  slot identifier :: <string>, required-init-keyword: id:;
  slot connected-to :: <simple-object-vector>;
end class <vehicle-storage>;

// By using the name-prefix each-subclass slot we share one say method for
// all vehicle containers.
define method say (storage :: <vehicle-storage>) => ()
  format-out("%s %s", storage.name-prefix, storage.identifier);
end method say;

define method object-fits?
    (object :: <physical-object>, container :: <vehicle-storage>)
 => (fits? :: <boolean>)
  let object-size = object.physical-size;
  let container-capacity = container.storage-capacity;
  object-size.length < container-capacity.length
    & object-size.height < container-capacity.height
    & object-size.width < container-capacity.width;
end method object-fits?;

// Vehicle storage that can hold only 1 aircraft irresepective of direction.
// Direction in this context is either #"inbound" or #"outbound".
define abstract class <single-storage> (<vehicle-storage>)
  slot vehicle-currently-occupying :: false-or(<aircraft>) = #f;
end class <single-storage>;

// Vehicle storage that can hold multiple aircraft, with distinct queues for 
// each direction.
define abstract class <multiple-storage> (<vehicle-storage>)
  slot vehicles-by-direction :: <object-table> = make(<object-table>);
  slot maxima-by-direction :: <object-table> = make(<object-table>);
  keyword directions:;
  keyword maxima:;
end class <multiple-storage>; 

// In a real airport there would be many paths an aircraft could take. For 
// our simple airport example, we define only the #"inbound" and 
// #"outbound" paths. The directions parameter is a sequence of these 
// aircraft path names. Multiple storage containers can limit the number 
// of aircraft they can hold for each path. This is the maxima parameter.
// The initialize method creates a queue to hold aircraft for each direction,
// and stores the queue in a table indexed by direction. The method also
// stores the maximum number of aircaft for that direction in a different
// table.
define method initialize 
    (object :: <multiple-storage>, #key directions :: <sequence>, 
     maxima :: <sequence>)
  next-method ();
  for (direction in directions,
       maximum in maxima)
    object.vehicles-by-direction[direction] := make(<deque>);
    object.maxima-by-direction[direction] := maximum;
  end for;
end method initialize; 

// From the basic vehicle containers above, we can build specific containers 
// for each aircraft transition location.
define class <gate> (<single-storage>)
  inherited slot name-prefix, init-value: "Gate";
end class <gate>; 

// Given a zero based terminal number, and a one based gate number, create
// an return a string with a gate letter and a terminal number in it.
define method generate-gate-id 
    (term :: <non-negative-integer>, gate :: <positive-integer>)
 => (gate-id :: <string>)
  format-to-string("%c%d", $letters[term], gate);
end method generate-gate-id; 

// Gates-per-terminal is a vector. Each element of the vector is the
// number of gates to create for the terminal at that index.
// Returns a vector of all the gate instances.
define method generate-gates
    (gates-per-terminal :: <vector>, default-gate-capacity :: <size>)
 => (gates :: <vector>)
  let result = make(<vector>, size: reduce1(\+, gates-per-terminal));
  let result-index = 0;
  for (term from 0 below gates-per-terminal.size)
    for (gate from 1 to gates-per-terminal[term])
      result[result-index]
        := make(<gate>, id: generate-gate-id(term, gate),
                capacity: default-gate-capacity);
      result-index := result-index + 1;
    end for;
  end for;
  result;
end method generate-gates; 

// This class represents the part of the airspace over a particular airport.
define class <sky> (<multiple-storage>)
  // The airport which this piece of sky is over.
  slot airport-below :: <airport>, required-init-keyword: airport:;
  inherited slot name-prefix, init-value: "Sky";
  required keyword inbound-aircraft:;
end class <sky>; 

// When a sky instance is created, a sequence of inbound aircraft is
// provided.
// This method initializes the direction slot of the aircraft to #"inbound", 
// and places them in the inbound queue of the sky instance.
define method initialize (sky :: <sky>, #key inbound-aircraft :: <sequence>)
  next-method(sky, directions: #[#"inbound", #"outbound"],
              maxima: vector(inbound-aircraft.size,
                             inbound-aircraft.size));
  let inbound-queue = sky.vehicles-by-direction [#"inbound"];
  for (vehicle in inbound-aircraft)
    vehicle.direction := #"inbound";
    push-last(inbound-queue, vehicle);
  end for;
  // Connect the airport to the sky.
  sky.airport-below.sky-above := sky;
end method initialize; 

// This class represents the strip of land where aircraft land and take off.
define class <runway> (<single-storage>)
  inherited slot name-prefix, init-value: "Runway";
end class <runway>; 

// Taxiways connect runways and gates.
define class <taxiway> (<multiple-storage>)
  inherited slot name-prefix, init-value: "Taxiway";
end class <taxiway>; 

// VEHICLES 

// The class which represents all self-propelled devices.
define abstract class <vehicle> (<physical-object>)
  // Every vehicle has some unique identification code
  slot vehicle-id :: <string>, required-init-keyword: id:;
  // The normal operating speed of this class of vehicle in miles per hour.
  each-subclass slot cruising-speed :: <positive-integer> = 300 ;
  // Allow individual differences in the size of particular aircraft, while
  // providing a suitable default for each class of aircraft.
  each-subclass slot standard-size :: <size> = make (<size>, length: 1, width: 1, height: 1) ;
end class <vehicle>; 

define method initialize (vehicle :: <vehicle>, #key)
  next-method();
  unless (slot-initialized?(vehicle, physical-size))
    vehicle.physical-size := vehicle.standard-size;
  end unless;
end method initialize; 

define method say (object :: <vehicle>) => ()
  format-out("Vehicle %s", object.vehicle-id);
end method say; 

// This class represents companies which fly commercial aircraft.
define class <airline> (<object>)
  slot name :: <string>, required-init-keyword: name:;
  slot code :: <string>, required-init-keyword: code:;
end class <airline>; 

define method say (object :: <airline>) => ()
  format-out("Airline %s", object.name);
end method say; 

// This class represents a regularly scheduled trip for a commercial airline.
define class <flight> (<object>)
  slot airline :: <airline>, required-init-keyword: airline:;
  slot number :: <non-negative-integer>,
    required-init-keyword: number:;
end class <flight>; 

define method say (object :: <flight>) => ()
  format-out("Flight %s %d", object.airline.code, object.number);
end method say; 

// This class represents vehicles which normally fly for a portion of
// their trip.
define abstract class <aircraft> (<vehicle>)
  slot altitude :: <integer>, init-keyword: altitude:;
  // Direction here is either #"inbound" or #"outbound".
  slot direction :: <symbol>;
  // The next step this aircraft might be able to make.
  slot next-transition :: <aircraft-transition>,
    required-init-keyword: transition:, setter: #f;
end class <aircraft>; 

define method initialize (vehicle :: <aircraft>, #key)
  next-method();
  // There is a one-to-one correspondance between aircraft instances and
  // transition instances. An aircraft can only make one transition 
  // at a time. Connect the aircraft to its transition. 
  vehicle.next-transition.transition-aircraft := vehicle;
end method initialize; 

// The next step an aircraft might be able to make.
define class <aircraft-transition> (<object>)
  slot transition-aircraft :: <aircraft>, init-keyword: aircraft:;
  slot from-container :: <vehicle-storage>, init-keyword: from:;
  slot to-container :: <vehicle-storage>, init-keyword: to:;
  // The earliest possible time the transition could take place.
  slot earliest-arrival :: <time-of-day>, init-keyword: arrival:;
  // Has this transition already been entered in the sorted sequence? 
  // This flag saves searching the sorted sequence.
  slot pending? :: <boolean> = #f, init-keyword: pending?:;
end class <aircraft-transition>; 

// Describes one step of an aircraft's movements.
define method say (transition :: <aircraft-transition>) => ()
  say(transition.earliest-arrival);
  format-out(": ");
  say(transition.transition-aircraft);
  format-out(" at ");
  say(transition.to-container);
end method say; 

// Commercial aircraft are aircrqaft which may have a flight
// assigned to them.
define abstract class <commercial-aircraft> (<aircraft>)
  slot aircraft-flight :: false-or(<flight>) = #f, init-keyword: flight:;
end class <commercial-aircraft>; 

define method say (object :: <commercial-aircraft>) => ()
  let flight = object.aircraft-flight;
  if (flight)
    say(flight);
  else
    format-out("Unscheduled Aircraft %s", object.vehicle-id);
  end if;
end method say; 

// The class which represents all commericial Boeing 707 aircraft.
define class <B707> (<commercial-aircraft>)
  inherited slot cruising-speed, init-value: 368;
  inherited slot standard-size,
    init-value: make(<size>, length: 153, width: 146, height: 42);
end class <B707>; 

define method say (aircraft :: <B707>) => ()
  if (aircraft.aircraft-flight)
    next-method();
  else
    format-out("Unscheduled B707 %s", aircraft.vehicle-id);
  end if;
end method say; 

// AIRPORTS 

// The class which represents all places where people and aircraft meet.
define class <airport> (<physical-object>)
  // The name of the airport, such as "San Fransisco International Airport"
  slot name :: <string>, init-keyword: name:;
  // The three letter abbreviation, such as "SFO"
  slot code :: <string>, init-keyword: code:;
  // The airspace above the airport
  slot sky-above :: <sky>;
end class <airport>; 

define method say (airport :: <airport>) => ()
  format-out("Airport %S", airport.code);
end method say; 
