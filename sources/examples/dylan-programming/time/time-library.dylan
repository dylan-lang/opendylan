Module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module definitions of the time component.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// Library definition
define library time
  // Interface module
  export time;
  // Substrate libraries
  use sixty-unit;
  use say;
  use dylan;
end library time; 

// Interface module
define module time
  // Classes
  create <time>, <time-of-day>, <time-offset>;
  // Constants
  create $midnight, $tomorrow;
  // Shared protocol
  use say, export: all;
  use sixty-unit, import: { encode-total-seconds }, export: all;
end module time; 

// Implementation module
define module time-implementation
  // External interface
  use time;
  // Substrate modules
  use sixty-unit;
  use say-implementor;
  use dylan;
end module time-implementation; 
