Module:       system-internals
Author:       Gary Palter
Synopsis:     Date/Time intervals (Durations) and related mathematical functionality
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// We're following the SQL model of having two distinct classes of <duration>s.
/// The first class represents an interval that spans months.
/// The second class represents an interval that spans days.
/// While the two classes are both <duration>s, they are, in essence, disjoint
/// in that you can't perform arithmetic on one instance of each class.

define abstract class <duration> (<number>)
  slot %duration-months :: <integer> = 0;
  slot %duration-days :: <integer> = 0;
  slot %duration-seconds :: <integer> = 0;
  slot %duration-microseconds :: <integer> = 0;
end class <duration>;

define class <year/month-duration> (<duration>) 
end class <year/month-duration>;

define class <day/time-duration> (<duration>)
end class <day/time-duration>;

define sealed domain \= (<duration>, <duration>);
define sealed domain \< (<duration>, <duration>);
define sealed domain \+ (<duration>, <duration>);
define sealed domain \+ (<date>, <duration>);
define sealed domain \+ (<duration>, <date>);
define sealed domain \- (<duration>, <duration>);
define sealed domain \- (<date>, <date>);
define sealed domain \- (<date>, <duration>);
define sealed domain \* (<duration>, <real>);
define sealed domain \* (<real>, <duration>);
define sealed domain \/ (<duration>, <real>);

define method make (class == <duration>, #rest init-keywords,
					 #key years :: false-or(<integer>) = #f,
					      months :: false-or(<integer>) = #f,
					      days :: false-or(<integer>) = #f,
					      hours :: false-or(<integer>) = #f,
					      minutes :: false-or(<integer>) = #f,
					      seconds :: false-or(<integer>) = #f,
					      microseconds :: false-or(<integer>) = #f,
					 #all-keys)
 => (duration :: <duration>)
  case
    years | months =>
      if (days | hours | minutes | seconds | microseconds)
	error("Can't make(<duration>) with both years/months and days/hours/minutes/seconds")
      end;
      apply(make, <year/month-duration>, init-keywords);
    days | hours | minutes | seconds | microseconds =>
      apply(make, <day/time-duration>, init-keywords);
    otherwise =>
      error
	("Either years/months or days/hours/minutes/seconds must be given to make(<duration>)"
	   )
  end
end method make;

define method initialize (duration :: <year/month-duration>, #key years :: <integer> = 0,
								  months :: <integer> = 0,
							     #all-keys)
 => (#rest objects)
  %duration-months(duration) := 12 * years + months;
  duration
end method initialize;

define method initialize (duration :: <day/time-duration>, #key days :: <integer> = 0,
								hours :: <integer> = 0,
								minutes :: <integer> = 0,
								seconds :: <integer> = 0,
								microseconds :: <integer> = 0,
							   #all-keys)
 => (#rest objects)
  %duration-days(duration) := days;
  %duration-seconds(duration) := seconds + 60 * (minutes + 60 * hours);
  %duration-microseconds(duration) := microseconds;
  canonicalize-duration(duration)
end method initialize;


/// Utility functions

define sealed inline method clone-duration (duration :: <year/month-duration>)
 => (new :: <year/month-duration>)
  make(<year/month-duration>, months: %duration-months(duration))
end method clone-duration;

define sealed inline method clone-duration (duration :: <day/time-duration>)
 => (new :: <day/time-duration>)
  make(<day/time-duration>, days: %duration-days(duration),
			    seconds: %duration-seconds(duration),
			    microseconds: %duration-microseconds(duration))
end method clone-duration;

/// Ensure that all fields of a <duration> have proper, canonical values by adjusting
/// the other fields to compensate for out of range values produced by the operations
/// defined below or supplied by the user when creating the <duration>.
define inline function canonicalize-duration (duration :: <duration>)
 => (duration :: <duration>)
  unless (-1000000 < %duration-microseconds(duration)
	    & %duration-microseconds(duration) < 1000000)
    let (seconds-change, new-microsecs) = floor/(%duration-microseconds(duration), 1000000);
    %duration-microseconds(duration) := new-microsecs;
    %duration-seconds(duration) := %duration-seconds(duration) + seconds-change
  end;
  unless (-86400 < %duration-seconds(duration) & %duration-seconds(duration) < 86400)
    let (days-change, new-seconds) = floor/(%duration-seconds(duration), 86400);
    %duration-seconds(duration) := new-seconds;
    %duration-days(duration) := %duration-days(duration) + days-change
  end;
  unless (negative?(%duration-seconds(duration)) = negative?(%duration-days(duration))
	    | zero?(%duration-seconds(duration)) | zero?(%duration-days(duration)))
    let negative-seconds? :: <boolean> = negative?(%duration-seconds(duration));
    %duration-days(duration)
      := %duration-days(duration) + if (negative-seconds?) -1 else 1 end;
    %duration-seconds(duration) 
      := %duration-seconds(duration) + if (negative-seconds?) 86400 else -86400 end
  end;
  unless (negative?(%duration-microseconds(duration)) = negative?(%duration-seconds(duration))
	    | zero?(%duration-microseconds(duration)) | zero?(%duration-seconds(duration)))
    let negative-microseconds? :: <boolean> = negative?(%duration-microseconds(duration));
    %duration-seconds(duration)
      := %duration-seconds(duration) + if (negative-microseconds?) -1 else 1 end;
    %duration-microseconds(duration) 
      := %duration-microseconds(duration) + if (negative-microseconds?) 1000000
					    else -1000000 end
  end;
  duration
end function canonicalize-duration;


/// Errors and restarts

define abstract class <date-arithmetic-error> (<error>)
  constant slot dae-year :: <integer>, required-init-keyword: year:;
  constant slot dae-month :: <integer>, required-init-keyword: month:;
  constant slot dae-day :: <integer>, init-keyword: day:;
end class <date-arithmetic-error>;

define abstract class <date-arithmetic-error-restart> (<date-arithmetic-error>, <restart>)
end class <date-arithmetic-error-restart>;

define class <date-arithmetic-invalid-day> (<date-arithmetic-error>)
  required keyword day:;
end class <date-arithmetic-invalid-day>;

define sealed method condition-to-string (condition :: <date-arithmetic-invalid-day>) 
 => (description :: <string>)
  format-to-string("%s, %d does not have %d days",
		   $month-names[dae-month(condition) - 1], 
		   dae-year(condition),
		   dae-day(condition))
end method condition-to-string;

define sealed method return-allowed? (condition :: <date-arithmetic-invalid-day>)
 => (return-allowed? :: <boolean>)
  #t
end method return-allowed?;

define class <date-arithmetic-use-last-valid-day> (<date-arithmetic-error-restart>)
end class <date-arithmetic-use-last-valid-day>;

define sealed method condition-to-string (condition :: <date-arithmetic-use-last-valid-day>)
 => (description :: <string>)
  format-to-string("Use %s %d, %d", 
		   $month-names[dae-month(condition) - 1], 
		   days-in-month(dae-year(condition), dae-month(condition)),
		   dae-year(condition))
end method condition-to-string;

define sealed method return-description (condition :: <date-arithmetic-invalid-day>)
 => (description :: <date-arithmetic-use-last-valid-day>)
  make(<date-arithmetic-use-last-valid-day>, year: dae-year(condition),
					     month: dae-month(condition))
end method return-description;


/// Exported interfaces

define inline function encode-year/month-duration (years :: <integer>, months :: <integer>)
 => (duration :: <year/month-duration>)
  make(<year/month-duration>, years: years, months: months)
end function encode-year/month-duration;

define inline function encode-day/time-duration
    (days :: <integer>, hours :: <integer>, minutes :: <integer>,
     seconds :: <integer>, microseconds :: <integer>)
 => (duration :: <day/time-duration>)
  make(<day/time-duration>, days: days, hours: hours, minutes: minutes,
			    seconds: seconds, microseconds: microseconds)
end function encode-day/time-duration;

define sealed generic decode-duration (duration :: <duration>)
 => (#rest components :: <integer>);

define sealed method decode-duration (duration :: <year/month-duration>)
 => (years :: <integer>, months :: <integer>)
  truncate/(%duration-months(duration), 12)
end method decode-duration;

define sealed method decode-duration (duration :: <day/time-duration>)
 => (days :: <integer>, hours :: <integer>, minutes :: <integer>, 
     seconds :: <integer>, microseconds :: <integer>)
  let (minutes, seconds) = truncate/(%duration-seconds(duration), 60);
  let (hours, minutes) = truncate/(minutes, 60);
  values(%duration-days(duration), hours, minutes, seconds, %duration-microseconds(duration))
end method decode-duration;

define sealed method \= (x :: <duration>, y :: <duration>) => (equals? :: <boolean>)
  %duration-months(x) = %duration-months(y)
  & %duration-days(x) = %duration-days(y)
  & %duration-seconds(x) = %duration-seconds(y)
  & %duration-microseconds(x) = %duration-microseconds(y)
end method \=;

define sealed method \< (x :: <year/month-duration>, y :: <year/month-duration>)
 => (less? :: <boolean>)
  %duration-months(x) < %duration-months(y)
end method \<;

define sealed method \< (x :: <day/time-duration>, y :: <day/time-duration>)
 => (less? :: <boolean>)
  %duration-days(x) < %duration-days(y)
  | (%duration-days(x) = %duration-days(y)
       & (%duration-seconds(x) < %duration-seconds(y)
	    | (%duration-seconds(x) = %duration-seconds(y)
		 & %duration-microseconds(x) < %duration-microseconds(y))))
end method \<;

/// Ensure that a <date>'s month and year have proper, canonical values by adjusting
/// them to compensate for out of range values produced by adding a <duration>.
/// If the resulting <date> is invalid (e.g., February 30th), signal a proceedable
/// error; if the user proceeds, set the <date> to the last valid date in the month.
define inline function canonicalize-date-month-year (date :: <date>) => (date :: <date>)
  let month :: <integer> = date-month(date) - 1;
  unless (-1 < month & month < 12)
    let (year-change, new-month) = floor/(month, 12);
    date-month(date) := new-month + 1;
    date-year(date) := date-year(date) + year-change
  end;
  unless (date-day(date) <= days-in-month(date-year(date), date-month(date)))
    block ()
      signal(make(<date-arithmetic-invalid-day>, year: date-year(date),
						 month: date-month(date),
						 day: date-day(date)));
    exception(<date-arithmetic-use-last-valid-day>, 
	      init-arguments: vector(year: date-year(date), month: date-month(date)))
      date-day(date) := days-in-month(date-year(date), date-month(date))
    end;
  end;
  date
end function canonicalize-date-month-year;

define inline-only function add-duration-to-date (x :: <date>, y :: <duration>)
 => (date :: <date>)
  let new :: <date> = clone-date(x);
  date-month(new) := date-month(new) + %duration-months(y);
  canonicalize-date-month-year(new);
  date-day(new) := date-day(new) + %duration-days(y);
  date-seconds(new) := date-seconds(new) + %duration-seconds(y);
  date-microseconds(new) := date-microseconds(new) + %duration-microseconds(y);
  canonicalize-date(new)
end function add-duration-to-date;

define sealed method \+ (x :: <date>, y :: <duration>) => (date :: <date>)
  add-duration-to-date(x, y)
end method \+;

define sealed method \+ (x :: <duration>, y :: <date>) => (date :: <date>)
  add-duration-to-date(y, x)
end method \+;

define sealed method \- (x :: <date>, y :: <date>) => (duration :: <day/time-duration>)
  make(<day/time-duration>, days: date-universal-date(x) - date-universal-date(y),
			    seconds: date-universal-time(x) - date-universal-time(y),
			    microseconds: date-microseconds(x) - date-microseconds(y))
end method \-;

define sealed method \- (x :: <date>, y :: <duration>) => (date :: <date>)
  let new :: <date> = clone-date(x);
  date-month(new) := date-month(new) - %duration-months(y);
  canonicalize-date-month-year(new);
  date-day(new) := date-day(new) - %duration-days(y);
  date-seconds(new) := date-seconds(new) - %duration-seconds(y);
  date-microseconds(new) := date-microseconds(new) - %duration-microseconds(y);
  canonicalize-date(new)
end method \-;

define inline-only function add-durations (x :: <duration>, y :: <duration>)
 => (duration :: <duration>)
  let new :: <duration> = clone-duration(x);
  %duration-months(new) := %duration-months(new) + %duration-months(y);
  %duration-days(new) := %duration-days(new) + %duration-days(y);
  %duration-seconds(new) := %duration-seconds(new) + %duration-seconds(y);
  %duration-microseconds(new) := %duration-microseconds(new) + %duration-microseconds(y);
  canonicalize-duration(new)
end function add-durations;

define sealed method \+ (x :: <year/month-duration>, y :: <year/month-duration>)
 => (duration :: <year/month-duration>)
  add-durations(x, y)
end method \+;

define sealed method \+ (x :: <day/time-duration>, y :: <day/time-duration>)
 => (duration :: <day/time-duration>)
  add-durations(x, y)
end method \+;

define inline-only function subtract-durations (x :: <duration>, y :: <duration>)
 => (duration :: <duration>)
  let new :: <duration> = clone-duration(x);
  %duration-months(new) := %duration-months(new) - %duration-months(y);
  %duration-days(new) := %duration-days(new) - %duration-days(y);
  %duration-seconds(new) := %duration-seconds(new) - %duration-seconds(y);
  %duration-microseconds(new) := %duration-microseconds(new) - %duration-microseconds(y);
  canonicalize-duration(new)
end function subtract-durations;

define sealed method \- (x :: <year/month-duration>, y :: <year/month-duration>)
 => (duration :: <year/month-duration>)
  subtract-durations(x, y)
end method \-;

define sealed method \- (x :: <day/time-duration>, y :: <day/time-duration>)
 => (duration :: <day/time-duration>)
  subtract-durations(x, y)
end method \-;

define inline-only function scale-duration (x :: <duration>, y :: <real>)
 => (duration :: <duration>)
  let new :: <duration> = clone-duration(x);
  %duration-months(new) := round(y * %duration-months(new));
  %duration-days(new) := round(y * %duration-days(new));
  %duration-seconds(new) := round(y * %duration-seconds(new));
  %duration-microseconds(new) := round(y * %duration-microseconds(new));
  canonicalize-duration(new)
end function scale-duration;

define sealed method \* (x :: <duration>, y :: <real>) => (duration :: <duration>)
  scale-duration(x, y)
end method \*;

define sealed method \* (x :: <real>, y :: <duration>) => (duration :: <duration>)
  scale-duration(y, x)
end method \*;

define sealed method \/ (x :: <duration>, y :: <real>) => (duration :: <duration>)
  let new :: <duration> = clone-duration(x);
  %duration-months(new) := round/(%duration-months(new), y);
  %duration-days(new) := round/(%duration-days(new), y);
  %duration-seconds(new) := round/(%duration-seconds(new), y);
  %duration-microseconds(new) := round/(%duration-microseconds(new), y);
  canonicalize-duration(new)
end method \/;
