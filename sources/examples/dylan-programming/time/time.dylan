Module: time-implementation 
Author:    Neal Feinberg, Sonya E. Keene, Robert O. Mathews, P. Tucker Withington
Synopsis:  The implementation of the time library.
Copyright: N Feinberg/S E Keene/R Mathews/P Tucker Withington, 
	DYLAN PROGRAMMING, Copyright (c) 1997-2000 Functional Objects, Inc. 
	Reproduced by permission of Addison-Wesley Longman 
	Publishing Company, Inc.  All rights reserved. No further 
	copying, downloading or transmitting of this material
	is allowed without the prior written permission of the
	publisher.

// Define non-negative integers as integers which are >= zero.
define constant <non-negative-integer> = limited(<integer>, min: 0); 

define abstract class <time> (<sixty-unit>)
end class <time>; 

define method say (time :: <time>) => ()
  let (hours, minutes) = decode-total-seconds (time); 
  format-out("%d:%s%d", hours, if (minutes < 10) "0" else "" end, minutes);
end method say; 

// A specific time of day from 00:00 (midnight) to below 24:00 (tomorrow)
define class <time-of-day> (<time>)
end class <time-of-day>; 

define method total-seconds-setter
    (total-seconds :: <integer>, time :: <time-of-day>)
 => (total-seconds :: <non-negative-integer>)
  if (total-seconds >= 0)
    next-method();
  else
    error("%d cannot be negative", total-seconds);
  end if;
end method total-seconds-setter; 

define method initialize (time :: <time-of-day>, #key) 
  next-method();
  if (time.total-seconds < 0)
    error("%d cannot be negative", time.total-seconds);
  end if;
end method initialize; 

// A relative time between -24:00 and +24:00
define class <time-offset> (<time>)
end class <time-offset>; 

define method past? (time :: <time-offset>) => (past? :: <boolean>)
  time.total-seconds < 0;
end method past?; 

define method say (time :: <time-offset>) => ()
  format-out("%s ", if (time.past?) "minus" else "plus" end);
  next-method();
end method say; 


define method \+ 
    (offset1 :: <time-offset>, offset2 :: <time-offset>) 
 => (sum :: <time-offset>)	 
  let sum = offset1.total-seconds + offset2.total-seconds; 
  make(<time-offset>, total-seconds: sum); 
end method \+; 

define method \+ 
    (offset :: <time-offset>, time-of-day :: <time-of-day>)
 => (sum :: <time-of-day>)
  make(<time-of-day>, 
       total-seconds: offset.total-seconds + time-of-day.total-seconds);
end method \+; 

define method \+ (time-of-day :: <time-of-day>, offset :: <time-offset>)
 => (sum :: <time-of-day>)
  offset + time-of-day;
end method \+; 

define method \< (time1 :: <time-of-day>, time2 :: <time-of-day>)
 => (less? :: <boolean>)
  time1.total-seconds < time2.total-seconds;
end method \<; 

define method \< (time1 :: <time-offset>, time2 :: <time-offset>)
 => (less? :: <boolean>)
  time1.total-seconds < time2.total-seconds;
end method \<; 

define method \= (time1 :: <time-of-day>, time2 :: <time-of-day>)
 => (equal? :: <boolean>)
  time1.total-seconds = time2.total-seconds;
end method \=; 

define method \= (time1 :: <time-offset>, time2 :: <time-offset>)
 => (equal? :: <boolean>)
  time1.total-seconds = time2.total-seconds;
end method \=; 

// Some useful time constants
define constant $midnight
  = make(<time-of-day>, total-seconds: encode-total-seconds(0, 0, 0)); 

define constant $tomorrow
  = make(<time-of-day>,
         total-seconds: encode-total-seconds(24, 0, 0)); 
