Module:       system-internals
Author:       Gary Palter
Synopsis:     Native Mac OS Carbon specific portion of the Date library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-unsigned-long-buffer
  { with-unsigned-long-buffer (?:name = ?init:expression) ?:body end }
    => { begin
	   let ?name :: <byte-string>
	     = make(<byte-string>, size: raw-as-integer(primitive-word-size()), fill: '\0');
	   primitive-c-unsigned-long-at
	       (primitive-cast-raw-as-pointer(primitive-string-as-raw(?name)),
		integer-as-raw(0), integer-as-raw(0))
	     := integer-as-raw(?init);
	   ?body
	 end }
end macro with-unsigned-long-buffer;

define macro cast-unsigned-long-buffer-as-pointer
  { cast-unsigned-long-buffer-as-pointer(?buffer:name) }
    => { primitive-cast-raw-as-pointer(primitive-string-as-raw(?buffer)) }
end macro cast-unsigned-long-buffer-as-pointer;

define inline-only function unsigned-long-buffer-as-machine-word (buffer :: <byte-string>)
 => (value :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
	integer-as-raw(0), integer-as-raw(0)))
end function unsigned-long-buffer-as-machine-word;


/// The native clock is an <integer> or <machine-word> representing the number of seconds
/// since January 1, 1904.  Further, it is always interpreted as local time.

define constant $DATETIMEREC-SIZE = 14;

define inline-only function decode-native-clock (native-clock :: <machine-word>)
 => (year :: <integer>, month :: <integer>, day :: <integer>, 
     hours :: <integer>, minutes :: <integer>, seconds :: <integer>)
  let datetimerec :: <byte-string>
    = make(<byte-string>, size: $DATETIMEREC-SIZE, fill: '\0');
  %call-c-function ("SecondsToDate", c-modifiers: "pascal")
      (secs :: <raw-c-unsigned-long>, d :: <raw-c-pointer>) => (void :: <raw-c-void>)
    (primitive-unwrap-machine-word(native-clock),
     primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)))
  end;
  let year :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(0)));
  let month :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(2)));
  let day :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(4)));
  let hours :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(6)));
  let minutes :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(8)));
  let seconds :: <integer>
    = raw-as-integer
        (primitive-c-signed-short-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(datetimerec)),
	    integer-as-raw(0), integer-as-raw(10)));
  values(year, month, day, hours, minutes, seconds)
end function decode-native-clock;

define generic encode-native-clock-as-date (native-clock) => (date :: <date>);

define method encode-native-clock-as-date (native-clock :: <integer>) => (date :: <date>)
  encode-native-clock-as-date(primitive-wrap-machine-word(integer-as-raw(native-clock)))
end method encode-native-clock-as-date;

define method encode-native-clock-as-date (native-clock :: <machine-word>) => (date :: <date>)
  let (year :: <integer>, month :: <integer>, day :: <integer>, 
       hours :: <integer>, minutes :: <integer>, seconds :: <integer>)
    = decode-native-clock(native-clock);
  make(<date>, year: year,
               month: month,
               day: day,
               hours: hours,
               minutes: minutes,
               seconds: seconds,
               microseconds: 0,
               time-zone-offset: local-time-zone-offset())
end method encode-native-clock-as-date;

define function current-date () => (now :: <date>)
  with-unsigned-long-buffer (current-date-buffer = 0)
    %call-c-function ("GetDateTime", c-modifiers: "pascal")
	(secs :: <raw-c-pointer>) => (void :: <raw-c-void>)
      (cast-unsigned-long-buffer-as-pointer(current-date-buffer))
    end;
    encode-native-clock-as-date(unsigned-long-buffer-as-machine-word(current-date-buffer))
  end
end function current-date;

/// Mac OS calendar clock is accurate only to the second (sigh) so fake the milliseconds...
///---*** (NOTE: This code isn't 100% thread safe but should be good enough for now.)

define variable *last-timestamp-ud* = 0;
define variable *last-timestamp-ut* = 0;
define variable *last-timestamp-ms* = 0;

define function current-timestamp () => (milliseconds :: <integer>, days :: <integer>)
  with-unsigned-long-buffer (current-date-buffer = 0)
    %call-c-function ("GetDateTime", c-modifiers: "pascal")
	(secs :: <raw-c-pointer>) => (void :: <raw-c-void>)
      (cast-unsigned-long-buffer-as-pointer(current-date-buffer))
    end;
    let (year :: <integer>, month :: <integer>, day :: <integer>, 
	 hours :: <integer>, minutes :: <integer>, seconds :: <integer>)
      = decode-native-clock(unsigned-long-buffer-as-machine-word(current-date-buffer));
    let (ud, ut) = compute-universal-time(year, month, day, hours, minutes, seconds,
					  local-time-zone-offset());
    if (ud = *last-timestamp-ud* & ut = *last-timestamp-ut*)
      *last-timestamp-ms* := *last-timestamp-ms* + 1;
    else
      *last-timestamp-ms* := 0;
    end;
    *last-timestamp-ud* := ud;
    *last-timestamp-ut* := ut;
    values(1000 * ut + *last-timestamp-ms*, ud)
  end
end function current-timestamp;

define constant $MACHINE-LOCATION-SIZE = 12;

define function timezone-info () => (delta :: <integer>, dst? :: <boolean>)
  let machine-location :: <byte-string>
    = make(<byte-string>, size: $MACHINE-LOCATION-SIZE, fill: '\0');
  %call-c-function ("ReadLocation", c-modifiers: "pascal")
      (machineLocation :: <raw-c-pointer>) => (void :: <raw-c-void>)
    (primitive-cast-raw-as-pointer(primitive-string-as-raw(machine-location)))
  end;
  // According to Apple's Q&A OPS 24, "Greenwich Mean Time offsets and the Map control panel",
  // only the high-order bit (bit 7) of the dlsDelta field is used as the Daylight Savings
  // Time indicator.  (Contrary to its Inside Macintosh writeup, the remaining bits are zero.)
  let dst? :: <boolean>
    = primitive-machine-word-logbit?
        (integer-as-raw(7),
	 primitive-c-unsigned-char-at
	   (primitive-cast-raw-as-pointer(primitive-string-as-raw(machine-location)),
	    integer-as-raw(0), integer-as-raw(8)));
  // The gmtDelta field is stored as seconds from GMT in the low-order three bytes of 
  // the same longword that contains the dlsDelta field, above.
  let delta :: <integer>
    = raw-as-integer
        (primitive-machine-word-logand
	   (primitive-c-unsigned-long-at
	      (primitive-cast-raw-as-pointer(primitive-string-as-raw(machine-location)),
	       integer-as-raw(0), integer-as-raw(8)),
	    integer-as-raw(#x00FFFFFF)));
  if (logbit?(23, delta))
    values(truncate/(delta - #x1000000, 60), dst?)
  else
    values(truncate/(delta, 60), dst?)
  end
end function timezone-info;

define function local-time-zone-offset () => (zone-offset :: <integer>)
  let (delta, dst?) = timezone-info();
  ignore(dst?);
  delta
end function local-time-zone-offset;

///---*** How do I get the timezone name?
define function local-time-zone-name () => (zone-name :: <string>)
  let (delta, dst?) = timezone-info();
  ignore(dst?);
  local method format-integer (x :: <integer>, n :: <integer>) => (s :: <string>)
	  let s = make(<byte-string>, size: n, fill: '0');
	  for (i from 0 below n)
	    s[n - i - 1] := $digits[modulo(x, 10)];
	    x := floor/(x, 10);
	  end;
	  s
	end method format-integer;
  let (hours, minutes) = truncate/(abs(delta), 60);
  concatenate(if (delta < 0) "-" else "+" end,
	      format-integer(hours, 2),
	      ":",
	      format-integer(minutes, 2))
end function local-time-zone-name;

define function local-daylight-savings-time? () => (is-dst? :: <boolean>)
  let (delta, dst?) = timezone-info();
  ignore(delta);
  dst?
end function local-daylight-savings-time?;
