module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql-odbc!binding.dylan(trunk.9) $


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
                              element-count :: <integer>,
			      #key initial-value :: <object>)
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>);


define not-inline method create-storage(sql-data-type :: <object>,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: <object>)
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>);
  signal(make(<simple-warning>, 
         format-string: "Binding to column whose datatype (%=) is not supported\n"
                        "Using instance of <sql-unsupported-type> instead.\n",
         format-arguments: sql-data-type)); 
  values($sql-unsupported-type, null-pointer(<c-int*>), 0, 0);
end method;

define not-inline method create-storage(sql-data-type == $sql-unknown-type,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
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


define not-inline method create-storage(sql-data-type == $sql-char,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<character>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = precision;
  let storage-size = instance-size * element-count;
  let storage = make(<C-char*>, element-count: storage-size); 
  if (initial-value ~= #f)
    let target-value = as(<integer>, initial-value);  // ffi treats chars as ints--sigh.
    for (ndx :: <integer> from 0 below storage-size)
      storage[ndx] := target-value;
    end for;
  end if;

  values($sql-c-char, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-type-date,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<date-struct>);
  let storage-size = instance-size * element-count;
  let storage = make(<lpdate-struct>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx].year-value := initial-value.date-year;
      storage[ndx].month-value := initial-value.date-month;
      storage[ndx].day-value := initial-value.date-day;
    end for;
  end if;

  values($sql-c-type-date, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-type-time,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<time-struct>);
  let storage-size = instance-size * element-count;
  let storage = make(<lptime-struct>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx].hour-value := initial-value.date-hours;
      storage[ndx].minute-value := initial-value.date-minutes;
      storage[ndx].second-value := initial-value.date-seconds;
      storage[ndx].fraction-value := 0;
    end for;
  end if;

  values($sql-c-type-time, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-type-timestamp,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<timestamp-struct>);
  let storage-size = instance-size * element-count;
  let storage = make(<lptimestamp-struct>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx].year-value := initial-value.date-year;
      storage[ndx].month-value := initial-value.date-month;
      storage[ndx].day-value := initial-value.date-day;
      storage[ndx].hour-value := initial-value.date-hours;
      storage[ndx].minute-value := initial-value.date-minutes;
      storage[ndx].second-value := initial-value.date-seconds;
      storage[ndx].fraction-value := 0;  
    end for;
  end if;

  values($sql-c-type-timestamp, storage, storage-size, instance-size);
end method;

define not-inline method create-storage(sql-data-type == $sql-datetime,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<date>))
 => (c-data-type :: <object>,
     storage :: <object>,
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<timestamp-struct>);
  let storage-size = instance-size * element-count;
  let storage = make(<lptimestamp-struct>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx].year-value := initial-value.date-year;
      storage[ndx].month-value := initial-value.date-month;
      storage[ndx].day-value := initial-value.date-day;
      storage[ndx].hour-value := initial-value.date-hours;
      storage[ndx].minute-value := initial-value.date-minutes;
      storage[ndx].second-value := initial-value.date-seconds;
      storage[ndx].fraction-value := 0;
    end for;
  end if;

  values($sql-c-type-timestamp, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-numeric,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<number>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = precision + 1;
  let storage-size = instance-size * element-count;
  let storage = make(<C-string>, size: storage-size);
  if (initial-value ~= #f)
    let target-value = as(<byte-string>, initial-value);
    for (element-index :: <integer> from 0 below element-count)
      for (char-index :: <integer> from 0 below instance-size - 1)
        storage[element-index * char-index] := target-value[char-index];
      finally
        storage[element-index * char-index + 1] := null-pointer(<C-string>);
      end for;
    end for;
  end if;

  values($sql-c-char, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-decimal,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<number>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = precision;
  let storage-size = instance-size * element-count;
  let storage = make(<C-char*>, element-count: storage-size);
  if (initial-value ~= #f)
    let target-value = as(<byte-string>, initial-value);
    for (element-index :: <integer> from 0 below element-count)
      for (char-index :: <integer> from 0 below instance-size - 1)
        storage[element-index * char-index] := target-value[char-index];
      finally
        storage[element-index * char-index + 1] := null-pointer(<C-string>);
      end for;
    end for;
  end if;

  values($sql-c-char, storage, storage-size, instance-size);
end method;

/*
Because you can't specialize a method on a keyword argument, we have
three helper methods to actually create the appropriate storage.
Big-integers need the c-raw versions of the storage in order to stash
a machine-word into them; the non-raw (cooked?) versions of c storage
automatically convert integers, and can't handle big-integers.
*/
define method create-storage-helper(value :: <boolean>,
				    element-count :: <integer>)
  => result :: <object>;
  let storage = make(<c-signed-long*>, element-count: element-count);
  storage;
end;

define method create-storage-helper(value :: <integer>,
				    element-count :: <integer>)
 => result :: <object>;
  let storage = make(<c-signed-long*>, element-count: element-count);
  for (ndx :: <integer> from 0 below element-count)
    storage[ndx] := value;
  end for;
  storage;
end;

define method create-storage-helper(value :: big/<integer>,
				    element-count :: <integer>)
 => result :: <object>;
  let storage = make(<c-raw-signed-long*>, element-count: element-count);
  for (ndx :: <integer> from 0 below element-count)
    storage[ndx] := as(<machine-word>, value);
  end for;
end;

define not-inline method create-storage(sql-data-type == $sql-integer,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(big/<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)

  let instance-size = size-of(<c-signed-long>);
  let storage-size = instance-size * element-count;
  let storage = create-storage-helper(initial-value, element-count);
  clear-memory!(storage, storage-size);



  values($sql-c-long, storage, storage-size, instance-size);

end method;


define not-inline method create-storage(sql-data-type == $sql-smallint,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<c-signed-short>);
  let storage-size = instance-size * element-count;
  let storage = make(<c-signed-short*>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx] := initial-value;
    end for;
  end if;

  values($sql-c-short, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-tinyint,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<c-signed-short>);
  let storage-size = instance-size * element-count;
  let storage = make(<c-signed-short*>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx] := initial-value;
    end for;
  end if;

  values($sql-c-short, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-float,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<float>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<c-double>);
  let storage-size = instance-size * element-count;
  let storage = make(<c-double*>, element-count: element-count);

  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx] := initial-value;
    end for;
  end if;

  values($sql-c-double, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-real,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<integer>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  create-storage($sql-double, precision, scale, element-count, initial-value: initial-value);
end method;


define not-inline method create-storage(sql-data-type == $sql-double,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<float>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = size-of(<c-double>);
  let storage-size = instance-size * element-count;
  let storage = make(<c-double*>, element-count: element-count);
  if (initial-value ~= #f)
    for (ndx :: <integer> from 0 below element-count)
      storage[ndx] := as(<double-float>, initial-value);
    end for;
  end if;
  values($sql-c-double, storage, storage-size, instance-size);
end method;


define not-inline method create-storage(sql-data-type == $sql-varchar,
			     precision :: <integer>,
			     scale :: <integer>,
                             element-count :: <integer>,
			     #key initial-value :: false-or(<string>))
 => (c-data-type :: <object>,
     storage :: <object>, 
     storage-size :: <integer>,
     data-size :: <integer>)
  let instance-size = precision + 1;
  let storage-size = instance-size * element-count;
  let storage = make(<C-char*>, element-count: storage-size);
  if (initial-value ~= #f)
    let target-value = as(<byte-string>, initial-value);
    for (element-index :: <integer> from 0 below element-count)
      for (char-index :: <integer> from 0 below instance-size)
        storage[char-index * element-index] := target-value[char-index];
      finally
        storage[char-index * element-index + 1] := null-pointer(<C-char*>);
      end for;
    end for;
  end if;

  values($sql-c-char, storage, storage-size, instance-size);
end method;

define constant $none = #"none";
define constant <rowset-size> = type-union(<integer>, $none);

define sealed concrete class <binding> (<object>)
  //constant slot name :: <string>,
  //  init-keyword: name:,
  //  init-value: "";

  constant slot sql-data-type :: <integer>,
    required-init-keyword: sql-data-type:;

  slot c-data-type :: <sql-data-type>,
    init-value: $sql-c-char;

  constant slot precision :: <integer>,
    required-init-keyword: precision:;

  constant slot scale :: <integer>,
    required-init-keyword: scale:;

// This slot isn't used anywhere.  Should it be?
  //constant slot nullable :: <boolean>,
  //  init-keyword: nullable:,
  //  init-value: #f;

  slot storage :: <object>;

  slot storage-size :: <integer>;
  
  slot data-length :: <c-int*> = <c-int*>.null-pointer;

  required keyword rowset-size;
end class;


define method initialize(binding :: <binding>, 
                         #key initial-value, rowset-size :: <rowset-size>)
  next-method();
  let (the-data-type, a-storage, a-storage-size, a-data-size) 
    = create-storage(binding.sql-data-type,
		     binding.precision,
		     binding.scale,
                     rowset-size,
		     initial-value: if (initial-value == $null-value)
				      #f
				    else
				      initial-value
				    end if);

  binding.c-data-type := the-data-type;
  binding.storage := a-storage;
  binding.storage-size := a-storage-size;
  binding.data-length := make(<c-int*>, element-count: rowset-size);
 
  let data-length-value 
    = if (initial-value == $null-value) $sql-null-data else a-data-size end if;

  for (ndx :: <integer> from 0 below rowset-size)
    binding.data-length[ndx] := data-length-value;
  end for;

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
