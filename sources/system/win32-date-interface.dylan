Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter
Synopsis:     Native Win32 specific portion of the Date library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Used instead of define C-struct to avoid relying on the C-FFI library ...

/// From WINBASE.H ...

define constant $SYSTEMTIME_SIZE = 16;        // 8 unsigned shorts

define inline-only function st-year (st :: <machine-word>) => (year :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(0),
                                 integer-as-raw(0)))
end function st-year;

define inline-only function st-month (st :: <machine-word>) => (month :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(1),
                                 integer-as-raw(0)))
end function st-month;

/// Slot 2 is wDayOfWeek

define inline-only function st-day (st :: <machine-word>) => (day :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(3),
                                 integer-as-raw(0)))
end function st-day;

define inline-only function st-hour (st :: <machine-word>) => (hour :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(4),
                                 integer-as-raw(0)))
end function st-hour;

define inline-only function st-minute (st :: <machine-word>) => (minute :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(5),
                                 integer-as-raw(0)))
end function st-minute;

define inline-only function st-second (st :: <machine-word>) => (second :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(6),
                                 integer-as-raw(0)))
end function st-second;

define inline-only function st-milliseconds (st :: <machine-word>)
 => (milliseconds :: <integer>)
  raw-as-integer
    (primitive-c-signed-short-at(primitive-unwrap-machine-word(st),
                                 integer-as-raw(7),
                                 integer-as-raw(0)))
end function st-milliseconds;


/// From WINBASE.H and WINNT.h ...

define constant $TIME_ZONE_ID_INVALID  = -1;
// define constant $TIME_ZONE_ID_UNKNOWN  = 0;
define constant $TIME_ZONE_ID_STANDARD = 1;
define constant $TIME_ZONE_ID_DAYLIGHT = 2;

define constant $TIME_ZONE_INFORMATION_SIZE =
  begin
    raw-as-integer(primitive-word-size())
    + (32 * 2)
    + $SYSTEMTIME_SIZE
    + raw-as-integer(primitive-word-size())
    + (32 * 2)
    + $SYSTEMTIME_SIZE
    + raw-as-integer(primitive-word-size())
  end;

define inline-only function tz-bias (tz :: <machine-word>) => (bias :: <integer>)
  0                                // Sign of Win32 bias is opposite ours ...
  - raw-as-integer
      (primitive-c-signed-long-at(primitive-unwrap-machine-word(tz),
                                  integer-as-raw(0),
                                  integer-as-raw(0)))
end function tz-bias;

define inline-only function tz-standard-bias
        (tz :: <machine-word>) => (bias :: <integer>)
  0                                // Sign of Win32 bias is opposite ours ...
  - raw-as-integer(
      primitive-c-signed-long-at(
        primitive-unwrap-machine-word(tz),
        integer-as-raw(0),
        integer-as-raw(raw-as-integer(primitive-word-size())
                       + (2 * 32) + $SYSTEMTIME_SIZE)))
  //---*** Perhaps should use the index rather than offset argument here ...
  //---*** (Verify that the index computation is properly optimized first)
end function tz-standard-bias;

define inline-only function tz-daylight-bias
        (tz :: <machine-word>) => (bias :: <integer>)
  0                                // Sign of Win32 bias is opposite ours ...
  - raw-as-integer(
      primitive-c-signed-long-at(
        primitive-unwrap-machine-word(tz),
        integer-as-raw(0),
        integer-as-raw(2 * (raw-as-integer(primitive-word-size())
                            + (2 * 32) + $SYSTEMTIME_SIZE))))
  //---*** Perhaps should use the index rather than offset argument here ...
  //---*** (Verify that the index computation is properly optimized first)
end function tz-daylight-bias;

define inline-only function extract-string (tz :: <machine-word>, offset :: <integer>)
 => (name :: <string>)
  let name = make(<stretchy-vector>);
  block (return)
    for (i :: <integer> from 0 below 32)
      let w = raw-as-integer
                (primitive-c-unsigned-short-at(primitive-unwrap-machine-word(tz),
                                               integer-as-raw(i),
                                               integer-as-raw(offset)));
      if (w = 0)
        return (as(<string>, name))
      else
        add!(name, as(<character>, w))
      end
    end;
    as(<string>, name)
  end
end function extract-string;

define inline-only function tz-standard-name (tz :: <machine-word>)
 => (name :: <string>)
  extract-string(tz, raw-as-integer(primitive-word-size()))
end function tz-standard-name;

define inline-only function tz-daylight-name (tz :: <machine-word>)
 => (name :: <byte-string>)
  extract-string(tz, raw-as-integer(primitive-word-size())
                     + (2 * 32)
                     + $SYSTEMTIME_SIZE
                     + raw-as-integer(primitive-word-size()))
end function tz-daylight-name;


///

define function timezone-info () => (bias :: <integer>, name :: <string>, dst? :: <boolean>)
  let tz = primitive-wrap-machine-word(integer-as-raw(0));
  block ()
    tz := primitive-wrap-machine-word
            (primitive-cast-pointer-as-raw
               (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
                    (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
                 => (pointer :: <raw-c-pointer>)
                  (integer-as-raw(0),
                   integer-as-raw($TIME_ZONE_INFORMATION_SIZE))
                end));
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(tz),
                                      integer-as-raw(0)))
      error("Can't get space for local time zone information")
    end;
    let raw-zone = primitive-wrap-machine-word
                     (%call-c-function ("GetTimeZoneInformation", c-modifiers: "__stdcall")
                          (tz :: <raw-c-pointer>) => (zone :: <raw-c-unsigned-long>)
                        (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(tz)))
                      end);
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(tz),
                                      integer-as-raw
                                        ($TIME_ZONE_ID_INVALID)))
      error("Can't get local time zone information")
    end;
    let zone = raw-as-integer(primitive-unwrap-machine-word(raw-zone));
    values(tz-bias(tz) +
           if (zone = $TIME_ZONE_ID_STANDARD) tz-standard-bias(tz)
           elseif (zone = $TIME_ZONE_ID_DAYLIGHT) tz-daylight-bias(tz)
           else 0 end if,
           if (zone = $TIME_ZONE_ID_STANDARD)
             tz-standard-name(tz)
           elseif (zone = $TIME_ZONE_ID_DAYLIGHT)
             tz-daylight-name(tz)
           else
             local method format-integer (x :: <integer>, n :: <integer>) => (s :: <string>)
                     let s = make(<byte-string>, size: n, fill: '0');
                     for (i from 0 below n)
                       s[n - i - 1] := $digits[modulo(x, 10)];
                       x := floor/(x, 10);
                     end;
                     s
                   end method format-integer;
             let bias = tz-bias(tz);
             let (hours, minutes) = truncate/(abs(bias), 60);
             concatenate(if (bias < 0) "-" else "+" end,
                         format-integer(hours, 2),
                         ":",
                         format-integer(minutes, 2))
           end,
           zone = $TIME_ZONE_ID_DAYLIGHT)
  cleanup
    if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(tz),
                                          integer-as-raw(0)))
      %call-c-function ("LocalFree", c-modifiers: "__stdcall")
          (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
        (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(tz)))
      end
    end
  end
end function timezone-info;


///

/// Native clock is a <machine-word> containing the address of a FILETIME structure
define function encode-native-clock-as-date (native-clock) => (date :: <date>)
  let st = primitive-wrap-machine-word(integer-as-raw(0));
  block ()
    st := primitive-wrap-machine-word
            (primitive-cast-pointer-as-raw
               (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
                    (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
                 => (pointer :: <raw-c-pointer>)
                  (integer-as-raw(0),
                   integer-as-raw($SYSTEMTIME_SIZE))
                end));
    if (primitive-machine-word-equal?(primitive-unwrap-machine-word(st),
                                      integer-as-raw(0)))
      error("Can't get space for decoded filesystem time")
    end;
    unless (primitive-raw-as-boolean
              (%call-c-function ("FileTimeToSystemTime", c-modifiers: "__stdcall")
                   (ft :: <raw-c-pointer>, st :: <raw-c-pointer>)
                => (success? :: <raw-c-signed-int>)
                 (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(native-clock)),
                  primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
               end))
      error("Can't decode filesystem time")
    end;
    make(<date>, year: st-year(st),
                 month: st-month(st),
                 day: st-day(st),
                 hours: st-hour(st),
                 minutes: st-minute(st),
                 seconds: st-second(st),
                 microseconds: 1000 * st-milliseconds(st),
                 time-zone-offset: 0)
  cleanup
    if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(st),
                                          integer-as-raw(0)))
      %call-c-function ("LocalFree", c-modifiers: "__stdcall")
          (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
        (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(st)))
      end
    end
  end
end function encode-native-clock-as-date;

define macro with-localtime-as-systemtime
  { with-localtime-as-systemtime (?st:name) ?:body end }
    => { let ?st = primitive-wrap-machine-word(integer-as-raw(0));
         block ()
           ?st := primitive-wrap-machine-word
                    (primitive-cast-pointer-as-raw
                       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
                            (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
                         => (pointer :: <raw-c-pointer>)
                          (integer-as-raw(0),
                           integer-as-raw($SYSTEMTIME_SIZE))
                        end));
           if (primitive-machine-word-equal?(primitive-unwrap-machine-word(?st),
                                             integer-as-raw(0)))
             error("Can't get space for local time")
           end;
           %call-c-function ("GetLocalTime", c-modifiers: "__stdcall")
               (st :: <raw-c-pointer>) => (void :: <raw-c-void>)
             (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?st)))
           end;
           ?body
         cleanup
           if (primitive-machine-word-not-equal?(primitive-unwrap-machine-word(?st),
                                                 integer-as-raw(0)))
             %call-c-function ("LocalFree", c-modifiers: "__stdcall")
                 (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
               (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(?st)))
             end
           end
         end }
end macro with-localtime-as-systemtime;

define function current-date () => (now :: <date>)
  with-localtime-as-systemtime (st)
    make(<date>, year: st-year(st),
                 month: st-month(st),
                 day: st-day(st),
                 hours: st-hour(st),
                 minutes: st-minute(st),
                 seconds: st-second(st),
                 microseconds: 1000 * st-milliseconds(st),
                 time-zone-offset: local-time-zone-offset())
  end
end function current-date;

define function current-timestamp () => (milliseconds :: <integer>, days :: <integer>)
  with-localtime-as-systemtime (st)
    let (ud, ut) = compute-universal-time(st-year(st), st-month(st), st-day(st),
                                          st-hour(st), st-minute(st), st-second(st),
                                          local-time-zone-offset());
    values(1000 * ut + st-milliseconds(st), ud)
  end
end function current-timestamp;

define function local-time-zone-offset () => (zone-offset :: <integer>)
  let (bias, name, dst?) = timezone-info();
  ignore(name, dst?);
  bias
end function local-time-zone-offset;

define function local-time-zone-name () => (zone-name :: <string>)
  let (bias, name, dst?) = timezone-info();
  ignore(bias, dst?);
  name
end function local-time-zone-name;

define function local-daylight-savings-time? () => (is-dst? :: <boolean>)
  let (bias, name, dst?) = timezone-info();
  ignore(bias, name);
  dst?
end function local-daylight-savings-time?;
