module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: !binding.dylan(D-kan.6) $


define sideways method default-conversion(value :: <lptimestamp-struct>)
 => (converted-value :: <date>)
  make(<date>, 
       year: value.year-value, 
       month: value.month-value, 
       day: value.day-value, 
       hours: value.hour-value, 
       minutes: value.minute-value, 
       seconds: value.second-value, 
       microseconds: 0)
end method;


define open generic sql-binding-info(object :: <object>) 
// should be sealed but due to a bug in the compiler...
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>);

define method sql-binding-info(object == $null-value)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  sql-binding-info(0)  // Since the value is null, we don't care
                                // what we are binding to.
end method;

define method sql-binding-info(object :: <integer>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  values($sql-integer, 0, 0)
end method;

define method sql-binding-info(object :: big/<integer>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  values($sql-integer, 0, 0)
end method;

define method sql-binding-info(object :: <character>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  values($sql-char, 1, 0)
end method;

define method sql-binding-info(object :: <float>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  values($sql-double, 5, 5); //+++ Correct precision and scale should be used!
end method;

define method sql-binding-info(object :: <string>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  //+ The let statement is needed to squelch a bogus type inference warning
  let string-size :: <integer> = object.size; 
  values($sql-varchar, string-size, 0)
end method;

define method sql-binding-info(object :: <date>)
 => (sql-data-type :: <sql-data-type>,
     precision :: <integer>,
     scale :: <integer>)
  values($sql-type-timestamp, 19, 0)
end method;


define generic create-storage(sql-data-type :: <object>,
			      precision :: <integer>,
			      scale :: <integer>,
			      #key initial-value :: <object>)
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>);


define method create-storage(sql-data-type :: <object>,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: <object>)
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>);
  signal(make(<simple-warning>, 
         format-string: "Binding to column whose datatype (%=) is not supported\n"
                        "Using instance of <sql-unsupported-type> instead.\n",
         format-arguments: list(sql-data-type))); 
  values($sql-unsupported-type, null-pointer(<c-int*>), 0, 0);
end method;

define method create-storage(sql-data-type == $sql-unknown-type,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: <object>)
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>);
  signal(make(<simple-warning>, 
         format-string: "Attempting to bind to a column with a datatype that "
                        "ODBC does not recognize.\n"
                        "Using instance of <sql-unknown-type> instead.\n"));
  values($sql-unknown-type, null-pointer(<c-int*>), 0, 0);
end method;


define method create-storage(sql-data-type == $sql-char,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<character>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  // storage includes space for the null-termination byte
  let storage-size = precision + 1;
  let storage = make(<C-string>, size: storage-size); 
  if (initial-value ~= #f)
    pointer-value(storage, index: 0) := initial-value;
  end if;

  values($sql-c-char, storage, storage-size, precision);
end method;


define method create-storage(sql-data-type == $sql-type-date,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<lpdate-struct>);
  let storage-size = size-of(<date-struct>);
  if (initial-value ~= #f)
    storage.year-value := initial-value.date-year;
    storage.month-value := initial-value.date-month;
    storage.day-value := initial-value.date-day;
  end if;

  values($sql-c-type-date, storage, storage-size, precision);
end method;


define method create-storage(sql-data-type == $sql-type-time,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<lptime-struct>);
  let storage-size = size-of(<time-struct>);
  if (initial-value ~= #f)
    storage.hour-value := initial-value.date-hours;
    storage.minute-value := initial-value.date-minutes;
    storage.second-value := initial-value.date-seconds;
    storage.fraction-value := 0;
  end if;

  values($sql-c-type-time, storage, storage-size, precision);
end method;


define method create-storage(sql-data-type == $sql-type-timestamp,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<lptimestamp-struct>);
  let storage-size = size-of(<timestamp-struct>);
  if (initial-value ~= #f)
    storage.year-value := initial-value.date-year;
    storage.month-value := initial-value.date-month;
    storage.day-value := initial-value.date-day;
    storage.hour-value := initial-value.date-hours;
    storage.minute-value := initial-value.date-minutes;
    storage.second-value := initial-value.date-seconds;
    storage.fraction-value := 0;
  end if;

  values($sql-c-type-timestamp, storage, storage-size, precision);
end method;

define method create-storage(sql-data-type == $sql-datetime,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<lptimestamp-struct>);
  let storage-size = size-of(<timestamp-struct>);
  if (initial-value ~= #f)
    storage.year-value := initial-value.date-year;
    storage.month-value := initial-value.date-month;
    storage.day-value := initial-value.date-day;
    storage.hour-value := initial-value.date-hours;
    storage.minute-value := initial-value.date-minutes;
    storage.second-value := initial-value.date-seconds;
    storage.fraction-value := 0;
  end if;

  values($sql-c-type-timestamp, storage, storage-size, precision);
end method;


define method create-storage(sql-data-type == $sql-numeric,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<number>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage-size = precision + 1;
  let storage = make(<C-string>, size: storage-size);

  values($sql-c-char, storage, storage-size, precision);
end method;


define method create-storage(sql-data-type == $sql-decimal,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<number>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage-size = precision + 1;
  let storage = make(<C-string>, size: storage-size);

  values($sql-c-char, storage, storage-size, precision);
end method;

/*
Because you can't specialize a method on a keyword argument, we have
three helper methods to actually create the appropriate storage.
Big-integers need the c-raw versions of the storage in order to stash
a machine-word into them; the non-raw (cooked?) versions of c storage
automatically convert integers, and can't handle big-integers.
*/
define method create-storage-helper(value :: <boolean>)
  => result :: <object>;
  let storage = make(<c-signed-long*>);
  storage;
end;

define method create-storage-helper(value :: <integer>)
 => result :: <object>;
  let storage = make(<c-signed-long*>);
  pointer-value(storage) := value;
  storage;
end;

define method create-storage-helper(value :: big/<integer>)
 => result :: <object>;
  let storage = make(<c-raw-signed-long*>);
  pointer-value(storage) := as(<machine-word>, value);
  storage;
end;

define method create-storage(sql-data-type == $sql-integer,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(big/<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = create-storage-helper(initial-value);

  values($sql-c-long, storage, 4, 4);
end method;


define method create-storage(sql-data-type == $sql-smallint,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<c-signed-short*>);
  if (initial-value ~= #f)
    pointer-value(storage) := initial-value;
  end if;

  values($sql-c-short, storage, 2, 2);
end method;


define method create-storage(sql-data-type == $sql-tinyint,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<c-signed-short*>);
  if (initial-value ~= #f)
    pointer-value(storage) := initial-value;
  end if;

  values($sql-c-short, storage, 2, 2);
end method;


define method create-storage(sql-data-type == $sql-float,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<float>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<c-double*>);

  if (initial-value ~= #f)
    pointer-value(storage) := initial-value;
  end if;

  values($sql-c-double, storage, 4, 4);
end method;


define method create-storage(sql-data-type == $sql-real,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  create-storage($sql-double, precision, scale, initial-value: initial-value);
end method;


define method create-storage(sql-data-type == $sql-double,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<float>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let storage = make(<c-double*>);
  if (initial-value ~= #f)
    pointer-value(storage) := as(<double-float>, initial-value);
  end if;
  values($sql-c-double, storage, 8, 8);
end method;


define method create-storage(sql-data-type == $sql-varchar,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<string>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  // string size includes the null termination byte which ODBC appends
  let storage-size = precision + 1;
  let storage = make(<C-string>, size: storage-size);
  if (initial-value ~= #f)
    for (ndx from 0 to min(initial-value.size, storage-size) - 1)
      pointer-value(storage, index: ndx) := initial-value[ndx];
    end for;
  end if;

  values($sql-c-char, storage, storage-size, precision);
end method;

define method create-storage(sql-data-type == $sql-longvarchar,
			     precision :: <integer>,
			     scale :: <integer>,
			     #key initial-value :: false-or(<string>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  // string size includes the null termination byte which ODBC appends
  let storage-size = precision + 1;
  let storage = make(<C-string>, size: storage-size);
  if (initial-value ~= #f)
    for (ndx from 0 to min(initial-value.size, storage-size) - 1)
      pointer-value(storage, index: ndx) := initial-value[ndx];
    end for;
  end if;

  values($sql-c-char, storage, storage-size, precision);
end method;



define sealed concrete class <binding> (<object>)
  constant slot binding-name :: <string> = "",
    init-keyword: name:;
  constant slot sql-data-type :: <integer>,
    required-init-keyword: sql-data-type:;
  slot c-data-type :: <sql-data-type> =  $sql-c-char;
  constant slot precision :: <integer>,
    required-init-keyword: precision:;
  constant slot scale :: <integer>,
    required-init-keyword: scale:;
  // This slot isn't used anywhere. Should it be?
  //constant slot nullable :: <boolean>,
  //  init-keyword: nullable:,
  //  init-value: #f;
  slot storage :: <object>;
  slot storage-size :: <integer>;
  slot data-length :: <c-int*> = make(<c-int*>);
end class;


define method initialize (binding :: <binding>, #key initial-value)
  next-method();
  let (the-data-type, a-storage, a-storage-size, a-data-size) 
    = create-storage(binding.sql-data-type,
		     binding.precision,
		     binding.scale,
		     initial-value: if (initial-value == $null-value)
				      #f
				    else
				      initial-value
				    end if);

  binding.c-data-type := the-data-type;
  binding.storage := a-storage;
  binding.storage-size := a-storage-size;
 
  if (initial-value == $null-value)
    pointer-value(binding.data-length) := $sql-null-data;
  else
    pointer-value(binding.data-length) := a-data-size;
  end if;

  finalize-when-unreachable(binding);
end method;

define method finalize(binding :: <binding>) 
 => ()
  if (null-pointer?(binding.storage))
    destroy(binding.storage);
    binding.storage := null-pointer(<c-int*>);
  end if;
  
  if (null-pointer?(binding.data-length))
    destroy(binding.data-length);
    binding.data-length := null-pointer(<c-int*>);
  end if;

  notify-of-finalization(binding);
  next-method();
end method;
