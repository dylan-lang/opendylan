Module:       system-test-suite
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Date class test cases

define constant $limits = #(#(year:, 1800, 2199),
			    #(month:, 1, 12),
			    #(day:, 1, 28),   // handled specially below
			    #(hours:, 0, 23),
			    #(minutes:, 0, 59),
			    #(seconds:, 0, 59),
			    #(microseconds:, 0, 999999),
			    #(time-zone-offset:, -1440, 1440));  // 24 * 60

define constant $month-days = #[0,
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
				31   // December
				];

define inline-only function leap-year? (year :: <integer>)
 => (leaping? :: <boolean>)
  modulo(year, 4) = 0
  & (modulo(year, 100) ~= 0 | modulo(year, 400) = 0)
end function leap-year?;

define inline function minmax-values (field :: <symbol>, #key date)
 => (min :: <integer>, max :: <integer>)
  let limits = head(choose(method(x) first(x) == field end, $limits));
  values(second(limits),
	 if (date & field == day:)
	   element($month-days, date.date-month)
	     + if (date.date-month == 2 & leap-year?(date.date-year)) 1 else 0 end
	 else
	   third(limits)
	 end)
end function minmax-values;

define function failure-values (field :: <symbol>, #key date)
 => (values :: <list>)
  let (min, max) = minmax-values(field, date: date);
  list(min - 2, min - 1, max + 1, max + 2)
end function failure-values;

define function success-values (field :: <symbol>, #key date)
 => (values :: <list>)
  let (min, max) = minmax-values(field, date: date);
  list(min, min + 1, round/(min + max, 2), max - 1, max);
end function success-values;

define date class-test <date> ()
  //
  // Wasn't sure where to put these tests.
  // Just verifies that the args checking for make(<date>) is reasonable.
  //
  let valid-args = #(year:, 2001, month:, 1, day:, 1, hours:, 1,
		     minutes:, 1, seconds:, 1, microseconds:, 1,
		     time-zone-offset:, 1);
  let valid-date = apply(make, <date>, valid-args);
  local method replace-arg(key, new-value) => (new-args :: <list>)
	  let prev = #f;
	  let new-args = list();
	  for (item in valid-args)
	    new-args := add(new-args, if (prev = key) new-value else item end);
            prev := item;
	  end;
	  reverse(new-args)
	end method,
        method test-one-arg (key)
	  for (bad in failure-values(key, date: valid-date))
	    let check-name = format-to-string("Make date with illegal %s = %s errs",
					      key, bad);
	    check-condition(check-name, <error>,
			    apply(make, <date>, replace-arg(key, bad)));
	  end for;
	  for (good in success-values(key, date: valid-date))
	    let check-name = format-to-string("Make date with legal %s = %s",
					      key, good);
	    check-true(check-name, apply(make, <date>, replace-arg(key, good)));
	  end for;
	end method;
  do(test-one-arg, map(first, $limits));

  //
  // Do a few extra checks that aren't covered above.
  //
  check("Make date with day = 31",     // Not covered above.
	make, <date>, year:, 2003, month:, 12, day:, 31);

  check-condition("Make a date with illegal month/day combo = 2/30.",
		  <error>, make(<date>, year: 2000, month: 2, day: 30));

  // 2000/2/29 is a leap year day.
  check-true("Make a date with Feb 29 on a leap year ok?",
	     make(<date>, year: 2000, month: 2, day: 29));
end;

define date constant-test <day-of-week> ()
  check-instance?("<day-of-week> is a <type>?", 
		  <type>, <day-of-week>);
end constant-test <day-of-week>;


/// Date function test cases

define function encode-full-date (year, month, day, hours, minutes,
				  seconds, microseconds, time-zone-offset)
  encode-date(year, month, day, hours, minutes, seconds,
	      microseconds: microseconds, time-zone-offset: time-zone-offset)
end function encode-full-date;

define function copy-date (date :: <date>) => (new-date :: <date>)
  encode-date(date.date-year, date.date-month, date.date-day,
	      date.date-hours, date.date-minutes, date.date-seconds,
	      microseconds: date.date-microseconds,
	      time-zone-offset: date.date-time-zone-offset)
end function copy-date;

define date function-test encode-date ()
  //---*** Fill this in.
end;

define date function-test decode-date ()
  //---*** Fill this in.
end;

define date function-test \< ()
  let date = current-date();
  let (mins, maxes) = values(#(), #());
  for (e in reverse($limits))
    let (min, max) = minmax-values(first(e), date: date);
    mins := add!(mins, min);
    maxes := add!(maxes, max);
  end for;
  check-true("Max date < min date?",
	     apply(encode-full-date, mins) < apply(encode-full-date, maxes));
  check-true("Current date later than when this test was written?",
	     encode-date(1997, 5, 12, 17, 32, 45) < current-date());
  // Algo mas?
end;

define date function-test \= ()
  check-true("Basic date1 = date2 check",
	     begin
	       let now = current-date();
	       now = encode-date(now.date-year, now.date-month, now.date-day,
				 now.date-hours, now.date-minutes, now.date-seconds);
	     end);
  check-true("date1 = date2 ignores microseconds?",
	     encode-date(2000, 1, 1, 1, 1, 1, microseconds: 2)
	       = encode-date(2000, 1, 1, 1, 1, 1, microseconds: 9999));
end;

define date function-test date-year ()
  //---*** Fill this in.
end;

define date function-test date-month ()
  //---*** Fill this in.
end;

define date function-test date-day ()
  //---*** Fill this in.
end;

define date function-test date-hours ()
  //---*** Fill this in.
end;

define date function-test date-minutes ()
  //---*** Fill this in.
end;

define date function-test date-seconds ()
  //---*** Fill this in.
end;

define date function-test date-microseconds ()
  //---*** Fill this in.
end;

define date function-test date-time-zone-offset ()
  //---*** Fill this in.
end;

define date function-test date-time-zone-offset-setter ()
  let d1 = encode-date(2100, 12, 31, 23, 0, 0, time-zone-offset: 0);
  let d2 = copy-date(d1);
  d1.date-time-zone-offset := 90;
  check-true("Change of time zone modifies date components correctly",
             d1.date-year == 2101 & d1.date-month == 1 & d1.date-day == 1
	       & d1.date-hours == 0 & d1.date-minutes == 30);
  check-true("Change of time zone doesn't modify actual time represented",
	     d1 = d2);
end;

define date function-test date-day-of-week ()
  let date = make(<date>, year: 1997, month: 5, day: 5);
  check-equal("date-day-of-week", date-day-of-week(date), #"monday");
end;

define date function-test as-iso8601-string ()
  // This check makes a big assumption about how the ISO8601 string
  // will be formatted.  There are various legal formats according to
  // the standard but this seems to be the one used by our implementation.
  check-equal("ISO8601 string format",
	      as-iso8601-string
		(make(<date>, year: 2000, month: 1, day: 1, hours: 0,
		      minutes: 0, seconds: 0, time-zone-offset: 0,
		      microseconds: 0)),
	      "2000-01-01T00:00:00+00:00");
  
				     
end;

define date function-test current-date ()
  //---*** Fill this in.
end;

define date function-test local-time-zone-offset ()
  //---*** Fill this in.
end;

define date function-test local-time-zone-name ()
  //---*** Fill this in.
end;

define date function-test local-daylight-savings-time? ()
  //---*** Fill this in.
end;
