Module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module definitions for the sixty-unit library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// Library definition
define library sixty-unit
  // Interface module
  export sixty-unit;
  // Substrate library
  use dylan;
end library sixty-unit; 

// Interface module
define module sixty-unit
  // Classes
  create <sixty-unit>;
  // Generics
  create total-seconds, encode-total-seconds, decode-total-seconds;
end module sixty-unit; 

// Implementation module
define module sixty-unit-implementation
  // External interface
  use sixty-unit;
  // Substrate module
  use dylan;
end module sixty-unit-implementation; 

