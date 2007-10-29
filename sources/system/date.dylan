Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter
Synopsis:     Platform independent portion of the Date library API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <date> (<number>)
  slot %universal-date :: false-or(<integer>) = #f;
  slot %universal-time :: false-or(<integer>) = #f;
  slot date-year :: <integer>, init-keyword: year:;
  slot date-month :: <integer>, init-keyword: month:;
  slot date-day :: <integer>, init-keyword: day:;
  slot date-hours :: <integer> = 0, init-keyword: hours:;
  slot date-minutes :: <integer> = 0, init-keyword: minutes:;
  slot date-seconds :: <integer> = 0, init-keyword: seconds:;
  slot date-microseconds :: <integer> = 0, init-keyword: microseconds:;
  slot date-time-zone-offset :: <integer> = 0, init-keyword: time-zone-offset:, 
					       setter: %date-time-zone-offset-setter;
end class <date>;

define method make (class == <date>, #rest init-keywords,
                                     #key iso8601-string :: false-or(<string>) = #f,
		                          native-clock = #f,
                                     #all-keys)
 => (date :: <date>)
  if (iso8601-string)
    let (year, month, day, hours, minutes, seconds, microseconds, zone-offset)
      = parse-iso8601-string(iso8601-string);
    make(<date>, year: year,
		 month: month,
		 day: day,
		 hours: hours,
		 minutes: minutes,
		 seconds: seconds,
                 microseconds: microseconds,
                 time-zone-offset: zone-offset)
  elseif (native-clock)
    encode-native-clock-as-date(native-clock)
  else
    next-method()
  end
end method make;

define method initialize (date :: <date>, #key, #all-keys) => (#rest objects)
  unless (slot-initialized?(date, date-year)
          & slot-initialized?(date, date-month)
          & slot-initialized?(date, date-day))
    error("Either year, month, and day or an ISO 8601 string must be given to make(<date>)")
  end;
  local method validate (value, low, high, inclusive?, name, replacement)
	 => (ok? :: <boolean>)
	  if (value < low | if (inclusive?) value > high else value >= high end)
	    cerror(replacement,
		   "A date's %s must be between %d, inclusive, and %d, %s",
		   name, low, high, if (inclusive?) "inclusive" else "exclusive" end);
	    #f
	  else
	    #t
	  end
	end method validate;
  unless (validate(date-year(date), 1800, 2200, #f, "year", "Use the current year"))
    date-year(date) := date-year(current-date())
  end;
  unless (validate(date-month(date), 1, 12, #t, "month", "Use the current month"))
    date-month(date) := date-month(current-date())
  end;
  unless (validate(date-day(date), 1, days-in-month(date-year(date), date-month(date)),
		   #t, "day of the month", "Use the first"))
    date-day(date) := 1
  end;
  unless (validate(date-hours(date), 0, 24, #f, "hour of the day", "Use midnight"))
    date-hours(date) := 0
  end;
  unless (validate(date-minutes(date), 0, 60, #f, "minutes", "Use zero"))
    date-minutes(date) := 0
  end;
  unless (validate(date-seconds(date), 0, 60, #f, "seconds", "Use zero"))
    date-seconds(date) := 0
  end;
  unless (validate(date-microseconds(date), 0, 1000000, #f, "microseconds", "Use zero"))
    date-microseconds(date) := 0
  end;
  unless (validate(date-time-zone-offset(date), -24 * 60, 24 * 60, #t,
		   "time zone offset", "Use the local time zone offset"))
    date-time-zone-offset(date) := local-time-zone-offset()
  end;
  date
end method initialize;

define method date-universal-date (date :: <date>) => (ut :: <integer>)
  %universal-date(date) | begin 
                            update-date-universal-slots(date);
                            %universal-date(date)
                          end
end method date-universal-date;

define method date-universal-time (date :: <date>) => (ut :: <integer>)
  %universal-time(date) | begin 
                            update-date-universal-slots(date);
                            %universal-time(date)
                          end
end method date-universal-time;

/// Changing the time zone offset of a <date> is defined as an immutable operation.
/// Therefore, we need to  update the other fields to reflect the new time zone.
define method date-time-zone-offset-setter (new-zone :: <integer>, date :: <date>)
 => (new-zone :: <integer>)
  let zone-change = new-zone - date-time-zone-offset(date);
  let (zone-change-hours, zone-change-minutes) = floor/(zone-change, 60);
  date-minutes(date) := date-minutes(date) + zone-change-minutes;
  date-hours(date) := date-hours(date) + zone-change-hours;
  canonicalize-date(date);
  //
  // Finally, we can set the time zone.  We don't need to update the internal
  // representation as we haven't actually changed the time represented by this <date>.
  %date-time-zone-offset(date) := new-zone
end method date-time-zone-offset-setter;

/// Ensure that all fields of a <date> have proper, canonical values by adjusting
/// the other fields to compensate for out of range values produced by either changing
/// the time zone offset or adding a <duration>.
define inline function canonicalize-date (date :: <date>) => (date :: <date>)
  unless (-1 < date-microseconds(date) & date-microseconds(date) < 1000000)
    let (seconds-change, new-microseconds) = floor/(date-microseconds(date), 1000000);
    date-microseconds(date) := new-microseconds;
    date-seconds(date) := date-seconds(date) + seconds-change
  end;
  //
  unless (-1 < date-seconds(date) & date-seconds(date) < 60)
    let (minutes-change, new-seconds) = floor/(date-seconds(date), 60);
    date-seconds(date) := new-seconds;
    date-minutes(date) := date-minutes(date) + minutes-change
  end;
  //
  unless (-1 < date-minutes(date) & date-minutes(date) < 60)
    let (hour-change, new-minutes) = floor/(date-minutes(date), 60);
    date-minutes(date) := new-minutes;
    date-hours(date) := date-hours(date) + hour-change
  end;
  //
  unless (-1 < date-hours(date) & date-hours(date) < 24)
    let (day-change, new-hours) = floor/(date-hours(date), 24);
    date-hours(date) := new-hours;
    date-day(date) := date-day(date) + day-change
  end;
  //
  while (date-day(date) > days-in-month(date-year(date), date-month(date)))
    date-day(date) := date-day(date) - days-in-month(date-year(date), date-month(date));
    date-month(date) := if (date-month(date) = 12) 
			   date-year(date) := date-year(date) + 1;
			   1
			 else
			   date-month(date) + 1 
			 end
  end;
  while (date-day(date) < 1)
    date-month(date) := if (date-month(date) = 1)
			   date-year(date) := date-year(date) - 1;
			   12
			 else
			   date-month(date) - 1 
			end;
    date-day(date) := date-day(date) + days-in-month(date-year(date), date-month(date))
  end;
  date
end function canonicalize-date;

define constant $month-names
  = #["January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"];

define constant <day-of-week> = one-of(#"Sunday", #"Monday", #"Tuesday", #"Wednesday",
				       #"Thursday", #"Friday", #"Saturday");

define constant $dow-Jan-1-1900-offset = 1;	// 1900/1/1 is a Monday

define constant $days-of-week = #[#"Sunday", #"Monday", #"Tuesday", #"Wednesday",
				  #"Thursday", #"Friday", #"Saturday"];

define function date-day-of-week (date :: <date>) => (dow :: <day-of-week>)
  let days = days-since-1900(date-year(date), date-month(date), date-day(date));
  let dow = modulo(days + $dow-Jan-1-1900-offset, 7);
  $days-of-week[dow]
end function date-day-of-week;

define method \= (x :: <date>, y :: <date>) => (equals? :: <boolean>)
  date-universal-date(x) = date-universal-date(y)
  & date-universal-time(x) = date-universal-time(y)
end method \=;

define method \< (x :: <date>, y :: <date>) => (less? :: <boolean>)
  date-universal-date(x) < date-universal-date(y)
  | (date-universal-date(x) = date-universal-date(y)
     & date-universal-time(x) < date-universal-time(y))
end method \<;

define method update-date-universal-slots (date :: <date>) => ()
  let (ud, ut) = compute-universal-time(date-year(date), date-month(date),
                                        date-day(date), date-hours(date),
                                        date-minutes(date), date-seconds(date),
					date-time-zone-offset(date));
  %universal-date(date) := ud;
  %universal-time(date) := ut;
end method update-date-universal-slots;

define constant $month-days
  = #[0,
      31,  // January
      28,  // February
      31,  // March
      30,  // April
      31,  // May
      30,  // June
      31,  // July
      31,  // August
      30,  // September
      31,  // October
      30,  // November
      31,  // December
      0];

define inline-only function leap-year? (year :: <integer>) => (leaping? :: <boolean>)
  modulo(year, 4) = 0
  & (modulo(year, 100) ~= 0 | modulo(year, 400) = 0)
end function leap-year?;

define function days-in-month (year :: <integer>, month :: <integer>) => (days :: <integer>)
  $month-days[month] + if (leap-year?(year) & month = 2) 1 else 0 end
end function days-in-month;

define function days-since-1900 (year :: <integer>, month :: <integer>, day :: <integer>)
 => (days :: <integer>)
  if (year < 1900)
    (day
       - 1		 		// Account for the day itself
       - days-in-month(year, month)
       - if (leap-year?(year) & month < 2)
	   1				// In a leap year, count leap day but only in ...
	 else				// ... January as days-in-month handles February
	   0
	 end
       - reduce1(\+, copy-sequence($month-days, start: month + 1))
       - 365 * (1899 - year))		// Normal days in all years after this year
      - truncate/(1899 - year, 4)	// Leap days after this year
      + truncate/(1899 - year, 100)	// ... excluding centuries
      - truncate/(1899 + 100 - year, 400)	// ... but including mod 400 centuries
  else
    day
      + if (leap-year?(year) & month > 2)
	  0				// In a leap year, count Februrary 29th
	else
	  -1				// Normal year or before the 29th
	end
      + reduce1(\+, copy-sequence($month-days, end: month))
      + 365 * (year - 1900)		// Normal days in all years before this year
      + truncate/(year - 1901, 4)	// Leap days before this year
      - truncate/(year - 1901, 100)	// ... excluding centuries
      + truncate/(year - 1901 + 300, 400)	// ... but including mod 400 centuries
  end
end function days-since-1900;

define method compute-universal-time
    (year :: <integer>, month :: <integer>, day :: <integer>, hours :: <integer>,
     minutes :: <integer>, seconds :: <integer>, time-zone-offset :: <integer>)
 => (ud :: <integer>, ut :: <integer>)
  let ut = seconds + 60 * (minutes + 60 * hours);
  let ud = days-since-1900(year, month, day);
  //
  // Now adjust the time (and, possibly the date) to account for the time zone
  let tzs = 60 * time-zone-offset;
  let (date-offset, time-offset) = floor/(tzs, 86400);
  ut := ut - time-offset;		// Converting from local time to UTC
  ud := ud - date-offset;
  unless (-1 < ut & ut < 86400)
    let (date-offset, new-ut) = floor/(ut, 86400);
    ut := new-ut;
    ud := ud + date-offset
  end;
  //
  values(ud, ut)
end method compute-universal-time;


/// Some convenience functions ...

define inline function encode-date 
    (year :: <integer>, month :: <integer>, day :: <integer>, hours :: <integer>,
     minutes :: <integer>, seconds :: <integer>, 
     #key microseconds :: <integer> = 0,
	  time-zone-offset :: <integer> = local-time-zone-offset())
 => (date :: <date>)
  make(<date>, year: year, month: month, day: day, 
	       hours: hours, minutes: minutes, seconds: seconds, 
	       microseconds: microseconds, time-zone-offset: time-zone-offset)
end function encode-date;

define inline function decode-date (date :: <date>)
 => (year :: <integer>, month :: <integer>, day :: <integer>, hours :: <integer>,
     minutes :: <integer>, seconds :: <integer>, day-of-week :: <day-of-week>,
     time-zone-offset :: <integer>)
  values(date-year(date), date-month(date), date-day(date), date-hours(date),
	 date-minutes(date), date-seconds(date), date-day-of-week(date),
	 date-time-zone-offset(date))
end function decode-date;

define function clone-date (date :: <date>) => (date :: <date>)
  encode-date(date-year(date), date-month(date), date-day(date),
	      date-hours(date), date-minutes(date), date-seconds(date),
	      microseconds: date-microseconds(date),
	      time-zone-offset: date-time-zone-offset(date))
end function clone-date;

///
define table *short-day-of-week-names* = {
  #"monday" => "Mon", #"tuesday" => "Tue",
  #"wednesday" => "Wed", #"thursday" => "Thu",
  #"friday" => "Fri", #"saturday" => "Sat",
  #"sunday" => "Sun" };

define table *day-of-week-names* = {
  #"monday" => "Monday", #"tuesday" => "Tuesday",
  #"wednesday" => "Wednesday", #"thursday" => "Thursday",
  #"friday" => "Friday", #"saturday" => "Saturday",
  #"sunday" => "Sunday" };

define variable *short-month-names* =
  #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

define variable *month-names* =
  #["January", "February", "March", "April",
    "May", "June", "July", "August", "September",
    "October", "November", "December"];

define constant $digits = #['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

define generic append (string :: <string>, appendant :: <object>)
 => (result :: <string>);

define method append (string :: <string>, appendant :: <string>)
 => (result :: <string>);
  string := concatenate(string, appendant);
  string;
end;

define method append (string :: <string>, appendant :: <character>)
 => (result :: <string>);
  string := add!(string, appendant);
  string;
end;

define method format-date (format :: <string>, date :: <date>)
 => (date-string :: <string>);
  let (year, month, day, hours, minutes, seconds,
       day-of-week, time-zone-offset) = decode-date(date);
  let absolute-time-zone-offset :: <integer> = abs(time-zone-offset);
  local method wrap (wrap :: <string>, i :: <integer>) => (string :: <string>)
      if (i < 10) concatenate(wrap, integer-to-string(i));
        else integer-to-string(i) end;
    end;
  local method format-integer (integer :: <integer>, length :: <integer>) => (string :: <string>)
      let string = make(<byte-string>, size: length, fill: '0');
      for (position from 0 below length)
        string[length - position - 1] := $digits[modulo(integer, 10)];
        integer := floor/(integer, 10);
      end;
      string
    end;
  let date-string :: <string> = "";
  let format? :: <boolean> = #f;
  let use-dots? :: <boolean> = #f;
  for (char in format)
    if (char = '%' & ~ format?)
      format? := #t;
    elseif (char = ':' & format?)
      use-dots? := #t;
    elseif (format?)
      date-string := append(date-string, select (char)
        'Y' => integer-to-string(year);
        'y' => format-integer(year, 2);
        'H' => wrap("0", hours);
        'k' => wrap(" ", hours);
        'M' => wrap("0", minutes);
        'S' => wrap("0", seconds);
        'm' => wrap("0", month);
        'd' => wrap("0", day);
        'e' => wrap(" ", day);
        'A' => *day-of-week-names*[day-of-week];
        'a' => *short-day-of-week-names*[day-of-week];
        'B' => *month-names*[month - 1];
        'b' => *short-month-names*[month - 1];
        'z' => concatenate(if (negative?(time-zone-offset))
                  "-" else "+" 
                end if, wrap("0", floor/(absolute-time-zone-offset, 60)),
                if (use-dots?) ":" else "" end if,
                wrap("0", modulo(absolute-time-zone-offset, 60)));
        'n' => '\n';
        '%' => '%';
        otherwise => char;
      end select);
      format? := #f;
      use-dots? := #f;
    else
      date-string := append(date-string, char);
    end if;
  end for;
  date-string;
end;

define function as-rfc822-string (date :: <date>)
 => (rfc822-date :: <string>);
  format-date("%a, %d %b %y %H:%M:%S %z", date);
end;

define function as-rfc1123-string (date :: <date>)
 => (rfc1123-date :: <string>);
  format-date("%a, %d %b %Y %H:%M:%S %z", date);
end;

define function as-iso8601-string (date :: <date>, #key precision :: <integer> = 0)
 => (iso8601-string :: <string>)
/*
    if (precision > 0)
		  concatenate(".", format-integer(round/(date-microseconds(date),
							 10 ^ (6 - precision)),
						  precision))
		else
		  ""
    end;
*/
  format-date("%Y-%m-%dT%H:%M:%S%:z", date);
end;

define variable *default-date-formats* :: <sequence> = #();

define method parse-date (date :: <string>, format :: <string>)
 => (date :: false-or(<date>));

  let date-stream = make(<string-stream>, contents: date);
  let date = make(<date>, year: 1970, month: 1, day: 1);
  let format? :: <boolean> = #f;
  let use-dots? :: <boolean> = #f;

  local method parse-month (month-names :: <sequence>)
      let start = stream-position(date-stream);
      date.date-month := block (return)
          for (month-name in month-names)
            if (month-name = read(date-stream, size(month-name)))
              return(find-key(month-names, curry(\=, month-name)) + 1);
            else
              date-stream.stream-position := start;
            end if;
          end for;
        end block;
    end; 

  for (char in format)
    if (char = '%' & ~ format?)
      format? := #t;
    elseif (char = ':' & format?)
      use-dots? := #t;
    elseif (format?)
      select (char)
        'Y' => date.date-year := string-to-integer(read(date-stream, 4));
        'y' => date.date-year := begin 
                  let year = string-to-integer(read(date-stream, 2));
                  if (year > 70) 1900 else 2000 end if + year;
                end;
        'H', 'k' => date.date-hours := string-to-integer(read(date-stream, 2));
        'M' => date.date-minutes := string-to-integer(read(date-stream, 2));
        'S' => date.date-seconds := string-to-integer(read(date-stream, 2));
        'm' => date.date-month := string-to-integer(read(date-stream, 2));
        'd', 'e' => date.date-day := string-to-integer(read(date-stream, 2));
        'B' => parse-month(*month-names*);
        'b' => parse-month(*short-month-names*);
        'z' => date.date-time-zone-offset := begin
                let sign = read(date-stream, 1);
                let hours =  read(date-stream, 2);
                if (use-dots?)
                  read(date-stream, 1);
                end if;
                let minutes = read(date-stream, 2);
                string-to-integer(concatenate(sign, "1")) *
                  string-to-integer(hours) * 60 + string-to-integer(minutes);
              end;
        'n', '%' => read(date-stream, 1);
        otherwise => #f;
      end select;
      format? := #f;
      use-dots? := #f;
    else
      read(date-stream, 1);
    end if;
  end for;
  date;
end;

///---*** Doesn't yet parse all legitimate forms of ISO 8601 strings.
///---*** For now, just parse a small superset of the strings produced above ...
// parse-date(foo, "%Y-%m-%dT%H:%M:%S%:z") , "%Y-%m-%dT%H:%M:%S%z", ...
define function parse-iso8601-string (string :: <string>)
 => (year :: <integer>, month :: <integer>, day :: <integer>,
     hours :: <integer>, minutes :: <integer>, seconds :: <integer>,
     microseconds :: <integer>, time-zone-offset :: <integer>)
  let digits = size(string);
  local method read-integer (start :: <integer>, endpos :: <integer>)
	 => (value :: <integer>)
	  unless (digits >= endpos)
	    error("Malformed ISO 8601 string %=: Not enough digits", string)
	  end;
	  let value = 0;
	  for (i from start below endpos)
	    if (string[i] < '0' | string[i] > '9')
	      error("Malformed ISO 8601 string %=: Unexpected character '%s'",
		    string, string[i])
	    end;
	    value := 10 * value + as(<integer>, string[i]) - as(<integer>, '0')
	  end;
	  value
	end method;
  let year = read-integer(0, 4);
  let month = read-integer(4, 6);
  let day = read-integer(6, 8);
  let hours = 0;
  let minutes = 0;
  let seconds = 0;
  let microseconds = 0;
  let time-zone-offset = 0;
  if (digits > 8)
    unless (string[8] = 'T' | string[8] = 't')
      error("Malformed ISO 8601 string %=: Date and time must be separated by 'T'",
	    string)
    end;
    hours := read-integer(9, 11);
    local method zone? (pos :: <integer>) => (zone? :: <boolean>)
	    digits > pos
	    & (string[pos] = 'Z' | string[pos] = 'z'
		 | string[pos] = '+' | string[pos] = '-')
	  end method;
    let zone-pos = 11;
    if (~zone?(zone-pos))
      minutes := read-integer(11, 13);
      zone-pos := 13;
      if (~zone?(zone-pos))
	seconds := read-integer(13, 15);
	zone-pos := 15;
	if (digits > 15 & string[15] = '.')
	  let precision = 0;
	  let i = 16;
	  while (~zone?(i) & i < digits)
	    precision := precision + 1;
	    if (string[i] < '0' | string[i] > '9')
	      error("Malformed ISO 8601 string %=: Unexpected character '%s'",
		    string, string[i])
	    end;
	    if (precision > 6)
	      error("Malformed ISO 8601 string %=: Too many digits in fraction", string)
	    end;
	    microseconds := 10 * microseconds + as(<integer>, string[i]) - as(<integer>, '0');
	    i := i + 1
	  end;
	  microseconds := microseconds * 10 ^ (6 - precision);
	  zone-pos := i;
	end
      end
    end;
    if (digits > zone-pos)
      if (string[zone-pos] = '+' | string[zone-pos] = '-')
	time-zone-offset := 60 * read-integer(zone-pos + 1, zone-pos + 3);
	if (digits > zone-pos + 3)
	  time-zone-offset := time-zone-offset + read-integer(zone-pos + 3,
							      zone-pos + 5)
	end;
	if (string[zone-pos] = '-')
	  time-zone-offset := 0 - time-zone-offset
	end;
	unless (digits = zone-pos + 3 | digits = zone-pos + 5)
	  error("Malformed ISO 8601 string %=: Too many characters", string)
	end
      elseif (string[zone-pos] = 'Z' | string[zone-pos] = 'z')
	unless (digits = zone-pos + 1)
	  error("Malformed ISO 8601 string %=: Too many characters", string)
	end
      else
	error("Malformed ISO 8601 string %=: Bad timezone indicator '%s'", string,
	      string[zone-pos])
      end
    end
  end;
  values(year, month, day, hours, minutes, seconds, microseconds, time-zone-offset)
end function parse-iso8601-string;
