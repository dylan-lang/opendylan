Module: position-implementation 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of positions for the angle library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define abstract class <position> (<object>)
end class <position>; 

define class <absolute-position> (<position>)
  slot latitude :: <latitude>, required-init-keyword: latitude:;
  slot longitude :: <longitude>, required-init-keyword: longitude:;
end class <absolute-position>; 

define method say (position :: <absolute-position>) => ()
  say(position.latitude);
  say(position.longitude);
end method say; 

define class <relative-position> (<position>)
  // Distance is in miles.
  slot distance :: <single-float>, required-init-keyword: distance:;
  // Angle is in degrees
  slot angle :: <angle>, required-init-keyword: angle:;
end class <relative-position>; 

define method say (position :: <relative-position>) => ()
  format-out("%s miles away at heading ", position.distance);
  say(position.angle);
end method say; 
