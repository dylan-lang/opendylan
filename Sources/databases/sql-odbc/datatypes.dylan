module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql-odbc!datatypes.dylan(trunk.3) $


define constant <sql-data-type> 
  = one-of($sql-bigint, $sql-binary, $sql-bit, $sql-char, $sql-date, 
	   $sql-decimal, $sql-double, $sql-float, $sql-integer, 
	   $sql-longvarbinary, $sql-longvarchar, $sql-numeric, 
	   $sql-real, $sql-smallint, $sql-time, $sql-timestamp,
	   $sql-tinyint, $sql-varbinary, $sql-varchar,
           $sql-type-date, $sql-type-time, $sql-type-timestamp,
           $sql-unknown-type, $sql-unsupported-type);

define constant $sql-unsupported-type = #"sql-unsupported-type";
define constant $odbc-class-id :: <table> =
  block ()
    let table = make(<table>);
    table[<sql-integer>]                    := $sql-integer;
    table[<sql-smallint>]                   := $sql-smallint;
    table[<sql-numeric>]                    := $sql-numeric;
    table[<sql-decimal>]                    := $sql-decimal;
    table[<sql-real>]                       := $sql-real;
    table[<sql-double-precision>]           := $sql-double;
    table[<sql-float>]                      := $sql-float;
    table[<sql-character>]                  := $sql-char;
    table[<sql-character-varying>]          := $sql-varchar;
    table[<sql-national-character>]         := $sql-char;
    table[<sql-national-character-varying>] := $sql-varchar;
    table[<sql-bit>]                        := $sql-bit;
    table[<sql-bit-varying>]                := $sql-varbinary;
    table[<sql-date>]                       := $sql-date;
    table[<sql-time>]                       := $sql-time;
    table[<sql-timestamp>]                  := $sql-timestamp;
    table[<sql-time-with-time-zone>]        := $sql-date;
    table[<sql-timestamp-with-time-zone>]   := $sql-timestamp;
    table[<sql-year-month-interval>]        := $sql-varchar;
    table[<sql-day-time-interval>]          := $sql-varchar;
    table[<sql-bigint>]                     := $sql-bigint;
    table[<sql-binary>]                     := $sql-binary;
    table[<sql-double>]                     := $sql-double;
    table[<sql-longvarbinary>]              := $sql-longvarbinary;
    table[<sql-longvarchar>]                := $sql-longvarchar;
    table[<sql-tinyint>]                    := $sql-tinyint;
    table[<sql-varbinary>]                  := $sql-varbinary;
    table[<sql-type-timestamp>]             := $sql-type-timestamp;

    table;
  end block;

define constant $sql-datatype-table :: <table> =
  block ()
    let table = make(<table>);
    table[$sql-integer]        := <sql-integer>;
    table[$sql-smallint]       := <sql-smallint>;
    table[$sql-numeric]        := <sql-numeric>;
    table[$sql-decimal]        := <sql-decimal>;
    table[$sql-real]           := <sql-real>;
    table[$sql-double]         := <sql-double-precision>;
    table[$sql-float]          := <sql-float>;
    table[$sql-char]           := <sql-character>;
    table[$sql-varchar]        := <sql-character-varying>;
    table[$sql-char]           := <sql-national-character>;
    table[$sql-varchar]        := <sql-national-character-varying>;
    table[$sql-bit]            := <sql-bit>;
    table[$sql-varbinary]      := <sql-bit-varying>;
    table[$sql-date]           := <sql-date>;
    table[$sql-time]           := <sql-time>;
    table[$sql-timestamp]      := <sql-timestamp>;
    table[$sql-date]           := <sql-time-with-time-zone>;
    table[$sql-timestamp]      := <sql-timestamp-with-time-zone>;
    table[$sql-varchar]        := <sql-year-month-interval>;
    table[$sql-varchar]        := <sql-day-time-interval>;
    table[$sql-bigint]         := <sql-bigint>;
    table[$sql-binary]         := <sql-binary>;
    table[$sql-double]         := <sql-double>;
    table[$sql-longvarbinary]  := <sql-longvarbinary>;
    table[$sql-longvarchar]    := <sql-longvarchar>;
    table[$sql-tinyint]        := <sql-tinyint>;
    table[$sql-varbinary]      := <sql-varbinary>;

    table;
  end block;

define table $sql-datatype-display-names =
 { $sql-integer => "SQL-integer",
   $sql-char => "SQL-char",
   $sql-float => "SQL-float",
   $sql-varchar => "SQL-varchar",
   $sql-type-timestamp => "SQL-type-timestamp"
 };

define table $c-datatype-display-names =
  { $SQL-C-NUMERIC => "SQL-C-Numeric",
    $SQL-C-DATE => "SQL-C-Date",
    $SQL-C-TIME => "SQL-C-Time",
    $SQL-C-TIMESTAMP => "SQL-C-Timestamp",
    $SQL-C-TYPE-DATE => "SQL-C-Type-Date",
    $SQL-C-TYPE-TIME => "SQL-C-Type-Time",
    $SQL-C-TYPE-TIMESTAMP => "SQL-C-Type-Timestamp",
    $SQL-C-INTERVAL-YEAR => "SQL-C-Interval-Year",
    $SQL-C-INTERVAL-MONTH => "SQL-C-Interval-Month",
    $SQL-C-INTERVAL-DAY => "SQL-C-Interval-Day",
    $SQL-C-INTERVAL-HOUR => "SQL-C-Interval-Hour",
    $SQL-C-INTERVAL-MINUTE => "SQL-C-Interval-Minute",
    $SQL-C-INTERVAL-SECOND => "SQL-C-Interval-Second",
    $SQL-C-INTERVAL-YEAR-TO-MONTH => "SQL-C-Interval-Year-to-Month",
    $SQL-C-INTERVAL-DAY-TO-HOUR => "SQL-C-Interval-Day-to-Hour",
    $SQL-C-INTERVAL-DAY-TO-MINUTE => "SQL-C-Interval-Day-to-Minute",
    $SQL-C-INTERVAL-DAY-TO-SECOND => "SQL-C-Interval-Day-to-Second",
    $SQL-C-INTERVAL-HOUR-TO-MINUTE => "SQL-C-Interval-Hour-to-Minute",
    $SQL-C-INTERVAL-HOUR-TO-SECOND => "SQL-C-Interval-Hour-to-Second",
    $SQL-C-INTERVAL-MINUTE-TO-SECOND => "SQL-C-Interval-Minute-to-Second",
    $SQL-C-BINARY => "SQL-C-Binary",
    $SQL-C-BIT => "SQL-C-Bit",
    $SQL-C-TINYINT => "SQL-C-Tinyint",
    $SQL-C-LONG => "SQL-C-Long",
    $SQL-C-SHORT => "SQL-C-Short",
    $SQL-C-FLOAT => "SQL-C-Float",
    $SQL-C-DOUBLE => "SQL-C-Double"
  };
