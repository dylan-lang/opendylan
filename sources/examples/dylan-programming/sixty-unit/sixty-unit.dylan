Module: sixty-unit-implementation 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the sixty-unit library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

define open abstract class <sixty-unit> (<object>)
  slot total-seconds :: <integer>, required-init-keyword: total-seconds:;
end class <sixty-unit>; 

define method encode-total-seconds 
    (max-unit :: <integer>, minutes :: <integer>, seconds :: <integer>) 
 => (total-seconds :: <integer>)
  ((max-unit * 60) + minutes) * 60 + seconds;
end method encode-total-seconds; 

define method decode-total-seconds 
    (sixty-unit :: <sixty-unit>)
 => (max-unit :: <integer>, minutes :: <integer>, seconds :: <integer>)
  decode-total-seconds(sixty-unit.total-seconds);
end method decode-total-seconds; 

define method decode-total-seconds
    (total-seconds :: <integer>)
 => (max-unit :: <integer>, minutes :: <integer>, seconds :: <integer>)
  let(total-minutes, seconds) = truncate/(abs(total-seconds), 60);
  let(max-unit, minutes) = truncate/(total-minutes, 60);
  values(max-unit, minutes, seconds);
end method decode-total-seconds; 

