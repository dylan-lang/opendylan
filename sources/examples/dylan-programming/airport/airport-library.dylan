module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and modules of the airport example.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define library airport
  export airport;
  use dylan;
  use common-dylan,
    import: {transcendentals};
  use say;
  use io;
  use definitions;
  use sorted-sequence;
  use angle;
  use time;
end library airport; 

define module airport
  export <size>, length, height, width, current-position, 
    current-position-setter;
  export physical-size, physical-size-setter, $default-capacity;
  export storage-capacity, storage-capacity-setter, identifier;
  export connected-to, connected-to-setter;
  export <gate>, generate-gates, <sky>, <runway>, <taxiway>;
  export <airline>, name, name-setter, code, code-setter, <flight>;
  export aircraft-flight, aircraft-flight-setter, number, number-setter, altitude, 
    altitude-setter;
  export <aircraft-transition>, <b707>, <airport>, sky-above, 
    sky-above-setter;
  export process-aircraft;
  use dylan;
  use transcendentals, import: {sqrt};
  use say;
  use format;
  use format-out;
  use definitions;
  use sorted-sequence;
  use time;
  use angle, export: {direction, direction-setter};
  use position;
end module airport; 
