module: airport 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of vehicles for the airport example.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// We don't need to strongly type these constants, because the Dylan
// compiler will figure them out for us. 

define constant $average-b707-brake-speed = 60.0; // Miles per hour 

define constant $feet-per-mile = 5280.0; 

define constant $average-b707-takeoff-speed = 60.0; // Miles per hour 

define constant $takeoff-pause-time = 120; // Seconds 

define constant $average-b707-taxi-speed = 10.0; 

define constant $average-b707-gate-turnaround-time
  = 34 * $seconds-per-minute; // Seconds 

// Computes how long it will take an aircraft to reach an airport.
define method flying-time
    (aircraft :: <aircraft>, destination :: <airport>)
 => (duration :: <time-offset>)
  // A simplistic calculation which assumes that the aircraft will
  // average a particular cruising speed for the trip.
  make(<time-offset>,
       total-seconds:
         ceiling/(distance-3d(aircraft, destination), 
                  aircraft.cruising-speed
                    / as(<single-float>, $seconds-per-hour)));
end method flying-time; 

// Computes the distance between an aircraft and an airport, 
// taking into account the altitude of the aircraft. 
// Assumes the altitude of the aircraft is the height 
// above the ground level of the airport.
define method distance-3d
    (aircraft :: <aircraft>, destination :: <airport>)
 => (distance :: <single-float>)  // Miles
  // A squared plus b squared equals c squared, where c is the hypotenuse,
  // and a and b are the other sides of a right triange.
  sqrt((aircraft.altitude / $feet-per-mile) ^ 2
       + distance-2d(aircraft.current-position,
                     destination.current-position) ^ 2);
end method distance-3d; 

// The distance between two positions, ignoring altitude.
define method distance-2d
    (position1 :: <relative-position>, position2 :: <absolute-position>)
 => (distance :: <single-float>) // Miles
  // When we have a relative position for the first argument (the aircraft),
  // we assume the relative position is relative to the second argument
  // (the airport).
  position1.distance;
end method distance-2d; 

// It would be sensible to provide a distance-2d method which computed the
// great circle distance between two absolute positions. Our example
// does not need this, and this computation is beyond the scope of this 
// book. 

// The time it takes to go from the point of touchdown to the entrance
// to the taxiway.
define method brake-time 
    (aircraft :: <b707>, runway :: <runway>)
 => (duration :: <time-offset>)
  make(<time-offset>,
       total-seconds:
         ceiling/(runway.physical-size.length / $feet-per-mile,
                  $average-b707-brake-speed / $seconds-per-hour));
end method brake-time; 

// The time it takes to go from the entrance of the taxiway to the point
// of takeoff.
define method takeoff-time 
    (aircraft :: <b707>, runway :: <runway>)
 => (duration :: <time-offset>)
  make(<time-offset>,
       total-seconds:
         ceiling/(runway.physical-size.length / $feet-per-mile,
                  $average-b707-takeoff-speed / $seconds-per-hour)
           + $takeoff-pause-time);
end method takeoff-time; 

// The time it takes to taxi from the runway entrance across the taxiway
// to the gate.
define method gate-time 
    (aircraft :: <b707>, taxiway :: <taxiway>)
 => (duration :: <time-offset>)
  make(<time-offset>,
       total-seconds:
         ceiling/(taxiway.physical-size.length / $feet-per-mile,
                  $average-b707-taxi-speed / $seconds-per-hour));
end method gate-time; 

// The time it takes to taxi from the gate across the taxiway to the
// entrance of the runway.
define method runway-time 
    (aircraft :: <b707>, taxiway :: <taxiway>)
 => (duration :: <time-offset>)
  gate-time(aircraft, taxiway);
end method runway-time; 

// The time it takes to unload, service, and load an aircraft.
define method gate-turnaround 
    (aircraft :: <b707>, gate :: <gate>) => (duration :: <time-offset>)
  make(<time-offset>, total-seconds: $average-b707-gate-turnaround-time);
end method gate-turnaround; 

