module: dylan-user 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The library and module of the test program for the airport example.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define library airport-test
  export airport-test;
  use dylan;
  use definitions;
  use time;
  use angle;
  use airport;
end library airport-test; 

define module airport-test
  export test-airport;
  use dylan;
  use definitions;
  use time;
  use angle;
  use airport;
  use position;
end module airport-test; 
