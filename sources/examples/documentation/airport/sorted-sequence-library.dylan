module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module definitions of the sorted sequence component.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define library sorted-sequence
  export sorted-sequence;
  use dylan;
  use definitions;
end library sorted-sequence; 

define module sorted-sequence
  export <sorted-sequence>;
  use dylan;
  use definitions;
end module sorted-sequence; 
