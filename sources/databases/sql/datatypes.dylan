Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql!datatypes.dylan(trunk.3) $


define open abstract class <sql-datatype> (<object>)
end class;

define open concrete class <sql-unknown-type> (<sql-datatype>)
end class;

define open concrete class <sql-unsupported-type> (<sql-datatype>)
end class;

//  Exact Numeric datatypes

define open concrete class <sql-integer> (<sql-datatype>)
end class;

define open concrete class <sql-smallint> (<sql-datatype>)
end class;

define open concrete class <sql-numeric> (<sql-datatype>)
end class;

define open concrete class <sql-decimal> (<sql-datatype>)
end class;


//  Approximate Numeric datatypes

define open concrete class <sql-real> (<sql-datatype>)
end class;

define open concrete class <sql-double-precision> (<sql-datatype>)
end class;

define open concrete class <sql-float> (<sql-datatype>)
end class;


// Character String datatypes

define open concrete class <sql-character> (<sql-datatype>)
end class;

define open concrete class <sql-character-varying> (<sql-datatype>)
end class;

define open concrete class <sql-national-character> (<sql-character>)
end class;

define open concrete class <sql-national-character-varying> 
    (<sql-character-varying>)
end class;


// Bit String datatypes

define open concrete class <sql-bit> (<sql-datatype>)
end class;

define open concrete class <sql-bit-varying> (<sql-datatype>)
end class;


// Datetimes datatypes

define open concrete class <sql-date> (<sql-datatype>)
end class;

define open concrete class <sql-time> (<sql-datatype>)
end class;

define open concrete class <sql-timestamp> (<sql-datatype>)
end class;

define open concrete class <sql-time-with-time-zone> (<sql-datatype>)
end class;

define open concrete class <sql-timestamp-with-time-zone> (<sql-datatype>)
end class;


// Intervals

define open concrete class <sql-year-month-interval> (<sql-datatype>)
end class;

define open concrete class <sql-day-time-interval> (<sql-datatype>)
end class;


//  Non-ANSI datatypes in common use.

define open concrete class <sql-bigint> (<sql-datatype>)
end class;

define open concrete class <sql-binary> (<sql-datatype>)
end class;

define open concrete class <sql-double> (<sql-datatype>)
end class;

define open concrete class <sql-longvarbinary> (<sql-datatype>)
end class;

define open concrete class <sql-longvarchar> (<sql-datatype>)
end class;

define open concrete class <sql-tinyint> (<sql-datatype>)
end class;

define open concrete class <sql-varbinary> (<sql-datatype>)
end class;

define open concrete class <sql-type-timestamp> (<sql-datatype>)
end class;

