module: definitions
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the definitions library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// This file contains constants and other definitions used in common with
// the other parts of the airport example. 

// The capital letters of the alphabet
define constant $letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; 

// This type represents non-negative integers. Non-negative integers are 
// either positive integers or zero.
define constant <non-negative-integer> = limited(<integer>, min: 0); 

// This type represents positive integers
define constant <positive-integer> = limited(<integer>, min: 1); 

define constant $hours-per-day = 24; 

define constant $minutes-per-hour = 60; 

define constant $seconds-per-minute = 60;

define constant $seconds-per-hour 
  = $minutes-per-hour * $seconds-per-minute;

/*--- Use this from common-dylan
// This method returns the union of the false type and a type you specify, 
// as a simple shorthand. 
// This method may already be provided by your Dylan implementation.
define method false-or (other-type :: <type>) => (combined-type :: <type>)
  type-union(singleton(#f), other-type);
end method false-or;

*/

