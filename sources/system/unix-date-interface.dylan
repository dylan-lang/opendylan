Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter
Synopsis:     Native UNIX specific portion of the Date library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From <time.h> ...

define inline-only function tm-seconds (tm :: <machine-word>) => (seconds :: <integer>)
  raw-as-integer
    (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
			       integer-as-raw(0),
			       integer-as-raw(0)))
end function tm-seconds;

define inline-only function tm-minutes (tm :: <machine-word>) => (minutes :: <integer>)
  raw-as-integer
    (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
			       integer-as-raw(1),
			       integer-as-raw(0)))
end function tm-minutes;

define inline-only function tm-hours (tm :: <machine-word>) => (hours :: <integer>)
  raw-as-integer
    (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
			       integer-as-raw(2),
			       integer-as-raw(0)))
end function tm-hours;

define inline-only function tm-day (tm :: <machine-word>) => (day :: <integer>)
  raw-as-integer
    (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
			       integer-as-raw(3),
			       integer-as-raw(0)))
end function tm-day;

define inline-only function tm-month (tm :: <machine-word>) => (month :: <integer>)
  1				// UNIX returns a zero-based month (ugh)
  + raw-as-integer
      (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
				 integer-as-raw(4),
				 integer-as-raw(0)))
end function tm-month;

define inline-only function tm-year (tm :: <machine-word>) => (year :: <integer>)
  1900				// UNIX returns years since 1900
  + raw-as-integer
      (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
				 integer-as-raw(5),
				 integer-as-raw(0)))
end function tm-year;

define inline-only function tm-dst? (tm :: <machine-word>) => (dst? :: <boolean>)
  primitive-raw-as-boolean
    (primitive-c-signed-int-at(primitive-unwrap-machine-word(tm),
			       integer-as-raw(8),
			       integer-as-raw(0)))
end function tm-dst?;

define system-offset tm-tz (alpha 5) 9;
define system-offset tm-tz-name (alpha 6) 10;

define inline-only function tm-tz-offset (tm :: <machine-word>) => (tz-offset :: <integer>)
  truncate/(raw-as-integer
	      (primitive-c-signed-long-at(primitive-unwrap-machine-word(tm),
					  integer-as-raw($tm-tz-offset),
					  integer-as-raw(0))),
	    60)			// UNIX returns time zone offset in seconds
end function tm-tz-offset;

define inline-only function tm-tz-name (tm :: <machine-word>) => (tz-name :: <byte-string>)
  primitive-raw-as-string
  (primitive-c-pointer-at(primitive-unwrap-machine-word(tm),
			  integer-as-raw($tm-tz-name-offset),
			  integer-as-raw(0)))
end function tm-tz-name;


///

define function read-clock () => (time :: <machine-word>)
  let time = primitive-wrap-machine-word
               (%call-c-function ("time") 
		    (timeloc :: <raw-c-pointer>) => (time :: <raw-c-signed-int>)
		  (primitive-cast-raw-as-pointer(integer-as-raw(0)))
	        end);
  if (primitive-machine-word-equal?(primitive-unwrap-machine-word(time),
				    integer-as-raw(-1)))
    error("Can't get time of day")
  end;
  time
end function read-clock;

define generic native-clock-to-tm (time) => (tm :: <machine-word>);

define method native-clock-to-tm (time :: <integer>) => (tm :: <machine-word>)
  native-clock-to-tm(primitive-wrap-machine-word(abstract-integer-as-raw(time)))
end method native-clock-to-tm;

/// UNIX strikes again!  The localtime function takes a pointer to the clock reading
/// rather than the clock reading directly.  Unfortunately, there's no simple way to
/// do that with our Dylan primitives.  So, we're forced to actually allocate a small
/// block of storage, store the time therein, and then pass the block's address.  (Sigh)
define method native-clock-to-tm (time :: <machine-word>) => (tm :: <machine-word>)
  let timeloc = primitive-wrap-machine-word(integer-as-raw(0));
  block ()
    timeloc := primitive-wrap-machine-word
                 (primitive-cast-pointer-as-raw
		    (%call-c-function ("GC_malloc")
		         (nbytes :: <raw-c-unsigned-long>) => (p :: <raw-c-pointer>)
		       (primitive-word-size())
		     end));
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(timeloc),
				      integer-as-raw(0)))
      error("Can't get space to decode native clock value")
    end;
    primitive-c-signed-int-at(primitive-unwrap-machine-word(timeloc),
			      integer-as-raw(0),
			      integer-as-raw(0))
      := primitive-unwrap-machine-word(time);
    let tm = primitive-wrap-machine-word
               (primitive-cast-pointer-as-raw
		  (%call-c-function ("localtime")
		       (time :: <raw-c-pointer>) => (tm :: <raw-c-pointer>)
		     (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(timeloc)))
		   end));
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(tm),
				      integer-as-raw(0)))
      error("Can't decode native clock value")
    end;
    tm
  cleanup
    if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(timeloc),
					  integer-as-raw(0)))
      %call-c-function ("GC_free") (p :: <raw-c-pointer>) => (void :: <raw-c-void>)
	(primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(timeloc)))
      end;
      #f
    end
  end
end method native-clock-to-tm;

define function encode-native-clock-as-date (native-clock) => (date :: <date>)
  let tm = native-clock-to-tm(native-clock);
  make(<date>, year: tm-year(tm),
               month: tm-month(tm),
               day: tm-day(tm),
               hours: tm-hours(tm),
               minutes: tm-minutes(tm),
               seconds: tm-seconds(tm),
               time-zone-offset: tm-tz-offset(tm))
end function encode-native-clock-as-date;

define function current-date () => (now :: <date>)
  encode-native-clock-as-date(read-clock())
end function current-date;

/// UNIX strikes again!  The time-of-day clock is only accurate to seconds.  So, to
/// avoid getting duplicate timestamps on successive calls, we'll add a counter to
/// the timestamp that's incremented on each call (modulo 1000).  (Sigh!)
define variable *ts-counter* :: <integer> = 0;

define function current-timestamp () => (milliseconds :: <integer>, days :: <integer>)
  let tm = native-clock-to-tm(read-clock());
  let (ud, ut) = compute-universal-time(tm-year(tm), tm-month(tm), tm-day(tm),
					tm-hours(tm), tm-minutes(tm), tm-seconds(tm),
					tm-tz-offset(tm));
  values(1000 * ut
	   + begin
	       let tsc = *ts-counter*;
	       *ts-counter* := modulo(*ts-counter* + 1, 1000);
	       tsc
	     end,
	 ud)
end function current-timestamp;

define function local-time-zone-offset () => (zone-offset :: <integer>)
  tm-tz-offset(native-clock-to-tm(read-clock()))
end function local-time-zone-offset;

define function local-time-zone-name () => (zone-name :: <string>)
  tm-tz-name(native-clock-to-tm(read-clock()))
end function local-time-zone-name;

define function local-daylight-savings-time? () => (is-dst? :: <boolean>)
  tm-dst?(native-clock-to-tm(read-clock()))
end function local-daylight-savings-time?;
