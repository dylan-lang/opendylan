Module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module definitions of the say component.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// Library definition
define library say
  // Interface modules
  export say, say-implementor;
  // Substrate libraries
  use io;
  use dylan;
end library say; 

// Protocol interface
define module say
  create say;
end module say; 

// Implementor interface
define module say-implementor
  use say, export: all;
  use format, export: all;
  use format-out, export: all;
end module say-implementor; 

// Implementation module
define module say-implementation
  use say;
  use dylan;
end module say-implementation; 
