Module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module definitions of the position component.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// Library definition
define library angle
  // Interface module
  export angle, position;
  // Substrate libraries
  use sixty-unit;
  use say;
  use dylan;
end library angle; 

// Interface module
define module angle
  // Classes
  create <angle>, <relative-angle>, <directed-angle>, <latitude>, <longitude>;
  // Generics
  create direction, direction-setter;
  // Shared protocol
  use say, export: all;
  use sixty-unit, import: { encode-total-seconds }, export: all;
end module angle; 

// Interface module
define module position
  // Classes
  create <position>, <absolute-position>, <relative-position>;
  // Generics
  create angle, distance, latitude, longitude;
  // Shared protocol
  use say, export: all;
end module position; 

// Implementation module
define module angle-implementation
  // External interface
  use angle;
  // Substrate modules
  use sixty-unit;
  use say-implementor;
  use dylan;
end module angle-implementation; 

// Implementation module
define module position-implementation
  // External interface
  use position;
  // Substrate modules
  use angle;
  use say-implementor;
  use dylan;
end module position-implementation; 
