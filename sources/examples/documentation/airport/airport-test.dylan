module: airport-test 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the test program for the airport example.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// To keep the example relatively simple, we will use variables to hold
// test data for the flights and aircraft.
// Ordinarily this information would be read from a file or database. 

define variable *flight-numbers* = #[62, 7, 29, 12, 18, 44]; 

define variable *aircraft-distances* 
  = #[3.0, 10.0, 175.0, 450.0, 475.0, 477.0];       // Miles 

define variable *aircraft-headings* 
  = #[82, 191, 49, 112, 27, 269];       // Degrees 

define variable *aircraft-altitudes*
  = #[7000, 15000, 22000, 22500, 22000, 21000];    // Feet 

define variable *aircraft-ids*
  = #["72914", "82290", "18317", "26630", "43651", "40819"]; 

define constant $default-runway-size
  = make(<size>, length: 10000, width: 200, height: 100);    // Feet 

define constant $default-taxiway-size
  = make(<size>, length: 900, width: 200, height: 100);    // Feet 

// Assumes there is only one runway, and one taxiway. The taxiway-count
// variable will determine how many aircraft can wait in line for each
// direction of the taxiway.
define method build-simple-airport
    (#key gates-per-terminal :: <vector> = #[2],
     capacity :: <size> = $default-capacity,
     runway-size :: <size> = $default-runway-size,
     taxiway-size :: <size> = $default-taxiway-size,
     taxiway-count :: <positive-integer> = 5,
     position-report-time :: <time-of-day>
       = make(<time-of-day>, total-seconds: encode-total-seconds(6, 0, 0)))
 => (airport :: <airport>) 

  let gates = generate-gates(gates-per-terminal, capacity);
  let taxiway
    = make(<taxiway>, id: "Echo", directions: #[#"inbound", #"outbound"],
           maxima: vector(taxiway-count, taxiway-count),
           capacity: capacity, physical-size: taxiway-size);
  let runway = make(<runway>, id: "11R-29L", capacity: capacity,
                    physical-size: runway-size);
  let keystone-air = make(<airline>, name: "keystone Air", code: "KN");
  let flights
    = map(method (fn) 
          make(<flight>, airline: keystone-air, number: fn) end,
          *flight-numbers*); 

  let aircraft
    = map(method (aircraft-flight, aircraft-distance, aircraft-heading,
                  aircraft-altitude, aircraft-id)
            make(<b707>, 
                 flight: aircraft-flight,
                 current-position:
                   make(<relative-position>,
                        distance: aircraft-distance,
                        angle: 
                          make(<relative-angle>,
                               total-seconds: 
                                 encode-total-seconds
                                   (aircraft-heading, 0, 0))),
                 altitude: aircraft-altitude,
                 id: aircraft-id,
                 transition: make(<aircraft-transition>,
                                  arrival: position-report-time));
          end,
          flights, *aircraft-distances*, *aircraft-headings*,
          *aircraft-altitudes*, *aircraft-ids*); 

  let airport
    = make(<airport>,
           name: "Belefonte Airport",
           code: "BLA",
           current-position:
             make(<absolute-position>,
                  latitude:
                    make(<latitude>,
                         total-seconds: encode-total-seconds(40, 57, 43),
                         direction: #"North"),
                  longitude:
                    make(<longitude>,
                         total-seconds: encode-total-seconds(77, 40, 24),
                         direction: #"West"))); 


  let sky = make(<sky>, inbound-aircraft: aircraft, airport: airport,
                 id: concatenate("over ", airport.code));
  airport.sky-above := sky;
  runway.connected-to := vector(taxiway, sky);
  let taxiway-vector = vector(taxiway);
  for (gate in gates)
    gate.connected-to := taxiway-vector;
  end for;
  let runway-vector = vector(runway);
  taxiway.connected-to := concatenate(runway-vector, gates);
  sky.connected-to := runway-vector;
  airport;
end method build-simple-airport; 

define method test-airport () => (last-transition :: <time-of-day>)
  process-aircraft(build-simple-airport());
end method test-airport; 

test-airport();

// Detailed aircraft schedule for Airport BLA
// 6:00: Flight KN 62 at Runway 11R-29L
// 6:02: Flight KN 62 at Taxiway Echo
// 6:02: Flight KN 7 at Runway 11R-29L
// 6:03: Flight KN 62 at Gate A1
// 6:04: Flight KN 7 at Taxiway Echo
// 6:05: Flight KN 7 at Gate A2
// 6:28: Flight KN 29 at Runway 11R-29L
// 6:30: Flight KN 29 at Taxiway Echo
// 6:37: Flight KN 62 at Taxiway Echo
// 6:37: Flight KN 29 at Gate A1
// 6:38: Flight KN 62 at Runway 11R-29L
// 6:39: Flight KN 7 at Taxiway Echo
// 6:42: Flight KN 62 at Sky over BLA
// 6:42: Flight KN 7 at Runway 11R-29L
// 6:46: Flight KN 7 at Sky over BLA
// 7:11: Flight KN 29 at Taxiway Echo
// 7:12: Flight KN 29 at Runway 11R-29L
// 7:16: Flight KN 29 at Sky over BLA
// 7:16: Flight KN 12 at Runway 11R-29L
// 7:18: Flight KN 12 at Taxiway Echo
// 7:18: Flight KN 18 at Runway 11R-29L 
// 7:19: Flight KN 12 at Gate A1
// 7:20: Flight KN 18 at Taxiway Echo
// 7:20: Flight KN 44 at Runway 11R-29L
// 7:21: Flight KN 18 at Gate A2
// 7:22: Flight KN 44 at Taxiway Echo
// 7:53: Flight KN 12 at Taxiway Echo
// 7:53: Flight KN 44 at Gate A1
// 7:54: Flight KN 12 at Runway 11R-29L
// 7:55: Flight KN 18 at Taxiway Echo
// 7:58: Flight KN 12 at Sky over BLA
// 7:58: Flight KN 18 at Runway 11R-29L
// 8:02: Flight KN 18 at Sky over BLA
// 8:27: Flight KN 44 at Taxiway Echo
// 8:28: Flight KN 44 at Runway 11R-29L
// 8:32: Flight KN 44 at Sky over BLA
// {class <TIME-OF-DAY>} 
