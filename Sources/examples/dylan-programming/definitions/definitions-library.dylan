module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module specifications for the definitions component.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define library definitions
  export definitions;
  use dylan;
end library definitions; 

define module definitions
  export $letters, <non-negative-integer>, <positive-integer>;
  export $hours-per-day, $minutes-per-hour;
  export $seconds-per-minute, $seconds-per-hour, false-or;
  use dylan;
end module definitions; 
