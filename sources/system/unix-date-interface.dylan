Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter
Synopsis:     Native UNIX specific portion of the Date library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Used instead of define C-struct to avoid relying on the C-FFI library ...

define macro tm-getter-definer
  { define tm-getter ?:name (?c-type:name) }
    => { define inline-only function ?name (tm :: <machine-word>) => (value :: <integer>)
           raw-as-integer
             ("primitive-c-" ## ?c-type ## "-at"
                (primitive-unwrap-machine-word(tm),
                 integer-as-raw(0), integer-as-raw("$" ## ?name ## "-offset")))
         end function }
end macro;

define tm-getter tm-sec (signed-int);   // seconds (0 - 60)
define tm-getter tm-min (signed-int);   // minutes (0 - 59)
define tm-getter tm-hour (signed-int);  // hours (0 - 23)
define tm-getter tm-mday (signed-int);  // day of month (1 - 31)
define tm-getter tm-mon (signed-int);   // month of year (0 - 11)
define tm-getter tm-year (signed-int);  // year - 1900
//define tm-getter tm-wday (signed-int);  // day of week (Sunday = 0)
//define tm-getter tm-yday (signed-int);  // day of year (0 - 365)
define tm-getter tm-isdst (signed-int); // is summer time in effect?
define tm-getter tm-gmtoff (signed-long); // offset from UTC in seconds

define inline-only function tm-tz-name (tm :: <machine-word>) => (tz-name :: <string>)
  primitive-raw-as-string
    (primitive-c-pointer-at(primitive-unwrap-machine-word(tm),
                            integer-as-raw(0),
                            integer-as-raw($tm-zone-offset)))
end function tm-tz-name;

///

define function native-clock-to-tm
    (native-clock :: <machine-word>, tm :: <machine-word>)
  let tv-sec-addr = u%+(native-clock, $tv-sec-offset);
  let tm-result
    = primitive-wrap-machine-word
        (primitive-cast-pointer-as-raw
           (%call-c-function ("localtime_r")
                (clock :: <raw-c-pointer>, tm :: <raw-c-pointer>)
             => (tm :: <raw-c-pointer>)
              (primitive-cast-raw-as-pointer
                 (primitive-unwrap-machine-word(tv-sec-addr)),
               primitive-cast-raw-as-pointer
                 (primitive-unwrap-machine-word(tm)))
           end));
  if (primitive-machine-word-equal?
        (primitive-unwrap-machine-word(tm-result), integer-as-raw(0)))
    error("Can't decode native clock value")
  end;
end function;

define function encode-native-clock-as-date (native-clock) => (date :: <date>)
  let nsec
    = raw-as-integer
        (primitive-c-signed-long-at(primitive-unwrap-machine-word(native-clock),
                                    integer-as-raw(0),
                                    integer-as-raw($tv-nsec-offset)));
  with-stack-byte-storage (tm, $tm-size)
    native-clock-to-tm(native-clock, tm);
    make(<date>,
         year: 1900 + tm-year(tm),
         month: 1 + tm-mon(tm),
         day: tm-mday(tm),
         hours: tm-hour(tm),
         minutes: tm-min(tm),
         seconds: tm-sec(tm),
         microseconds: truncate/(nsec, 1000),
         time-zone-offset: truncate/(tm-gmtoff(tm), 60))
  end
end function encode-native-clock-as-date;

define function read-clock (timespec :: <machine-word>)
  let rc
    = raw-as-integer(%call-c-function ("system_clock_realtime")
                         (clock-ptr :: <raw-c-pointer>)
                      => (result :: <raw-c-signed-int>)
                       (primitive-cast-raw-as-pointer
                          (primitive-unwrap-machine-word(timespec)))
                     end);
  if (~zero?(rc))
    error("clock read failed");
  end if;
end function;

define function current-date () => (now :: <date>)
  with-stack-byte-storage (timespec, $timespec-size)
    read-clock(timespec);
    encode-native-clock-as-date(timespec)
  end
end function current-date;

define function current-timestamp () => (milliseconds :: <integer>, days :: <integer>)
  with-stack-byte-storage (timespec, $timespec-size)
    read-clock(timespec);
    let nsec
      = raw-as-integer
          (primitive-c-signed-long-at(primitive-unwrap-machine-word(timespec),
                                      integer-as-raw(0),
                                      integer-as-raw($tv-nsec-offset)));
    with-stack-byte-storage (tm, $tm-size)
      native-clock-to-tm(timespec, tm);
      let (ud, ut)
        = compute-universal-time(1900 + tm-year(tm),
                                 1 + tm-mon(tm), tm-mday(tm),
                                 tm-hour(tm), tm-min(tm), tm-sec(tm),
                                 truncate/(tm-gmtoff(tm), 60));
      values(1000 * ut + truncate/(nsec, 1_000_000), ud)
    end
  end
end function current-timestamp;

define function local-time-zone-offset () => (zone-offset :: <integer>)
  with-stack-byte-storage (timespec, $timespec-size)
    read-clock(timespec);
    with-stack-byte-storage (tm, $tm-size)
      native-clock-to-tm(timespec, tm);
      truncate/(tm-gmtoff(tm), 60)
    end
  end
end function local-time-zone-offset;

define function local-time-zone-name () => (zone-name :: <string>)
  with-stack-byte-storage (timespec, $timespec-size)
    read-clock(timespec);
    with-stack-byte-storage (tm, $tm-size)
      native-clock-to-tm(timespec, tm);
      tm-tz-name(tm)
    end
  end
end function local-time-zone-name;

define function local-daylight-savings-time? () => (is-dst? :: <boolean>)
  with-stack-byte-storage (timespec, $timespec-size)
    read-clock(timespec);
    with-stack-byte-storage (tm, $tm-size)
      native-clock-to-tm(timespec, tm);
      ~zero?(tm-isdst(tm))
    end
  end
end function local-daylight-savings-time?;
