Module:    dfmc-reader
Synopsis:  Range source locations
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: PERFORMANCE: We can almost certainly save a slot in many source
// locations by having a special representation of a small, single-line
// range - e.g. for a short name, or a bit of single-character puntuation.
//// Range location source offsets.

// Small line/column numbers are encoded with shifts and masks into an
// integer. I'd expect 99.9% of source locations to fit into this
// compressed representation. A real object is created if the numbers
// overflow this rep.

define constant <small-range-source-offset> = <integer>;

define constant $column-bits :: <integer> = 12;
define constant $column-max  :: <integer> = 2 ^ $column-bits - 1;
define constant $column-mask :: <integer> = $column-max;
define constant $line-bits   :: <integer> = 16;
define constant $line-max    :: <integer> = 2 ^ $line-bits - 1;
define constant $line-mask   :: <integer> = ash($line-max, $column-bits);

define inline function small-source-offset?
    (line :: <integer>, column :: <integer>) => (small? :: <boolean>)
  column <= $column-max & line <= $line-max
end function;

define inline function make-small-source-offset
    (char :: <integer>, line :: <integer>, column :: <integer>)
  logior(ash(line, $column-bits), column);
end function;

// These method are sideways because of the integer packed representation.

define inline sealed sideways method source-offset-line
    (loc :: <small-range-source-offset>) => (line :: <integer>)
  ash(logand(loc, $line-mask), -$column-bits);
end method;

define inline sealed sideways method source-offset-column
    (loc :: <small-range-source-offset>) => (line :: <integer>)
  logand(loc, $column-mask)
end method;

define class <big-range-source-offset> (<big-source-offset>)
  constant slot source-offset-line :: <integer>,
    required-init-keyword: line:;
  constant slot source-offset-column :: <integer>,
    required-init-keyword: column:;
end class;

define compiler-sideways sealed domain make (subclass(<big-source-offset>));
define compiler-sideways sealed domain initialize (<big-source-offset>);

define sealed method \< (x :: <big-range-source-offset>,
                         y :: <small-range-source-offset>)
 => (well? :: <boolean>)
  // if the line of big offset is same as line of small offset, then the
  // column of the big offset must be larger than a maximum small offset
  // column, so don't need to check the equal line case.
  source-offset-line(x) < source-offset-line(y)
end;

define sealed method \= (x :: <big-range-source-offset>,
                         y :: <small-range-source-offset>)
 => (well? :: singleton(#f))
  #f
end;

define sealed method \< (x :: <small-range-source-offset>,
                         y :: <big-range-source-offset>)
 => (well? :: <boolean>)
  // if the line of big offset is same as line of small offset, then the
  // column of the big offset must be larger than a maximum small offset
  // column, so don't need to check the equal line case.
  source-offset-line(x) <= source-offset-line(y)
end;

define sealed method \= (x :: <small-range-source-offset>,
                         y :: <big-range-source-offset>)
 => (well? :: singleton(#f))
  #f
end;

define sealed method \< (x :: <big-range-source-offset>,
                         y :: <big-range-source-offset>)
 => (well? :: <boolean>)
  let x-line = source-offset-line(x);
  let y-line = source-offset-line(y);
  x-line < y-line |
    (x-line == y-line & source-offset-column(x) < source-offset-column(y))
end;

define sealed method \= (x :: <big-range-source-offset>,
                         y :: <big-range-source-offset>)
 => (well? :: <boolean>)
  source-offset-line(x) == source-offset-line(y) &
    source-offset-column(x) == source-offset-column(y)
end;

// Note that < and = on integers has the right behavior for both arguments
// being <small-range-source-offset>'s.


define constant <range-source-offset>
  = type-union(<small-range-source-offset>, <big-range-source-offset>);

define method range-source-offset-greater-than?
    (x :: <range-source-offset>, y :: <range-source-offset>)
 => (well? :: <boolean>)
  // TODO: compare source-offset-character
  source-offset-line(x) > source-offset-line(y)
    | (source-offset-line(x) = source-offset-line(y)
         & source-offset-column(x) > source-offset-column(y))
end method;

define function make-big-source-offset
    (char :: <integer>, line :: <integer>, column :: <integer>)
 => (offset :: <range-source-offset>)
  ignore(char);
  make(<big-range-source-offset>, line: line, column: column);
end function;

// Install as the default.

define inline sealed sideways method make-source-offset
    (char :: <integer>, line :: <integer>, column :: <integer>)
 => (offset :: <range-source-offset>)
  if (small-source-offset?(line, column))
    make-small-source-offset(char, line, column);
  else
    make-big-source-offset(char, line, column);
  end;
end method;


//// Range positions

define class <big-range-position> (<object>)
  constant slot source-position-start-offset,
    required-init-keyword: start-offset:;
  constant slot source-position-end-offset,
    required-init-keyword: end-offset:;
end class;

define sealed domain make (subclass(<big-range-position>));
define sealed domain initialize (<big-range-position>);

define constant <small-range-position> = <integer>;
define constant range$v-start-col :: <integer> = 0;
define constant range$s-start-col :: <integer> = 7;
define constant range$m-start-col :: <integer> = 2 ^ range$s-start-col - 1;
define constant range$v-start-line :: <integer> = range$v-start-col + range$s-start-col;
define constant range$s-start-line :: <integer> = 11;
define constant range$m-start-line :: <integer> = 2 ^ range$s-start-line - 1;
define constant range$v-delta-col :: <integer> = range$v-start-line + range$s-start-line;
define constant range$s-delta-col :: <integer> = 7;
define constant range$m-delta-col :: <integer> = 2 ^ range$s-delta-col - 1;
define constant range$v-delta-line :: <integer> = range$v-delta-col + range$s-delta-col;
define constant range$s-delta-line :: <integer> = 3;
define constant range$m-delta-line :: <integer> = 2 ^ range$s-delta-line - 1;

define method source-position-start-offset
    (pos :: <small-range-position>) => (res :: <small-range-source-offset>)
  let col = logand(ash(pos, - range$v-start-col), range$m-start-col);
  let line = logand(ash(pos, - range$v-start-line), range$m-start-line);
  make-small-source-offset(0, line, col)
end method;

define method source-position-end-offset
    (pos :: <small-range-position>) => (res :: <small-range-source-offset>)
  let sline = logand(ash(pos, - range$v-start-line), range$m-start-line);
  let dline = logand(ash(pos, - range$v-delta-line), range$m-delta-line);
  let dcol = logand(ash(pos, - range$v-delta-col), range$m-delta-col);
  let ecol = if (dline == 0)
               dcol + logand(ash(pos, - range$v-start-col), range$m-start-col)
             else
               dcol
             end;
  make-small-source-offset(0, sline + dline, ecol)
end method;

define inline method range-source-offset-greater-than?
    (x :: <small-range-source-offset>, y :: <small-range-source-offset>)
 => (well? :: <boolean>)
  x > y
end method;

// COPY DOWN FOR SPEED
define method compute-position-between
    (p1 :: <small-range-position>, p2 :: <small-range-position>)
  compute-position-between*(p1, p2)
end method;

define constant <range-position> = type-union(<small-range-position>,
                                              <big-range-position>);


define generic make-range-position
    (start-offset :: <range-source-offset>,
     end-offset :: <range-source-offset>)
 => (r :: <range-position>);

define method make-range-position
    (start-offset :: <range-source-offset>,
     end-offset :: <range-source-offset>) => (r :: <big-range-position>)
  make(<big-range-position>,
       start-offset: start-offset,
       end-offset: end-offset)
end method;

define method make-range-position
    (start-offset :: <small-range-source-offset>,
     end-offset :: <small-range-source-offset>) => (r :: <range-position>)
  let sline = source-offset-line(start-offset);
  if (sline <= range$m-start-line)
    let dline = source-offset-line(end-offset) - sline;
    if (dline <= range$m-delta-line)
      let scol = source-offset-column(start-offset);
      if (scol <= range$m-start-col)
        let ecol = source-offset-column(end-offset);
        let dcol = if (dline == 0) ecol - scol else ecol end;
        if (0 <= dcol & dcol <= range$m-delta-col)
          logior(ash(sline, range$v-start-line),
                 ash(dline, range$v-delta-line),
                 ash(scol, range$v-start-col),
                 ash(dcol, range$v-delta-col))
        end
      end
    end
  end | make(<big-range-position>,
             start-offset: start-offset,
             end-offset: end-offset)
end method;


//// Range source locations.

define class <compiler-range-source-location> (<source-location>)
  constant slot source-location-record :: <compilation-record>,
    required-init-keyword: source-record:;
  slot source-location-start-offset :: <range-source-offset>,
    required-init-keyword: start-offset:;
  slot source-location-end-offset :: <range-source-offset>,
    required-init-keyword: end-offset:;
end;

define sealed domain make (subclass(<compiler-range-source-location>));
define sealed domain initialize (<compiler-range-source-location>);

define method source-location-source-record (loc :: <compiler-range-source-location>)
 => (sr :: <source-record>)
  compilation-record-source-record(source-location-record(loc))
end method;

define method object-source-location-lines (loc :: <compiler-range-source-location>)
 => (start-line :: <integer>, end-line :: <integer>);
  let cr = loc.source-location-record;
  let sr = cr.compilation-record-source-record;
  let offset = cr.compilation-record-preceeding-line-count
                 + sr.source-record-start-line;
  let start-line = loc.source-location-start-offset.source-offset-line;
  let end-line = loc.source-location-end-offset.source-offset-line;
  values(offset + start-line, offset + end-line)
end;


define method source-location-source-position (loc :: <compiler-range-source-location>)
  make-range-position(source-location-start-offset(loc),
                      source-location-end-offset(loc))
end method;

define function record-position-as-location
    (record :: false-or(<compilation-record>), position)
  if (record)
    make(<compiler-range-source-location>,
         source-record: record,
         start-offset: source-position-start-offset(position),
         end-offset: source-position-end-offset(position))
  end
end function;


// Install as the default.

define sideways method make-source-location
    (record :: <compilation-record>,
     start-char :: <integer>,
       start-line :: <integer>, start-col :: <integer>,
     end-char :: <integer>,
       end-line :: <integer>, end-col :: <integer>)
 => (loc :: <compiler-range-source-location>)
  make(<compiler-range-source-location>,
       source-record: record,
       start-offset:  make-source-offset(start-char, start-line, start-col),
       end-offset:    make-source-offset(end-char, end-line, end-col));
end method;
