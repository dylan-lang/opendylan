Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stipple arrays, for use in creating patterned brushes

define macro stipple-definer
  { define stipple ?:name ?stipple:* end }
    => { define constant ?name :: <array> = make-stipple(list(?stipple)) }
 stipple:
  { } => { }
  { ?row:*; ... } => { list(?row), ... }
 row:
  { } => { }
  { ?cell:*, ... } => { ?cell, ... }
end macro stipple-definer;

define method make-stipple (rows :: <sequence>) => (stipple :: <array>)
  let height  :: <integer> = size(rows);
  let width   :: <integer> = size(rows[0]);
  let stipple :: <array>   = make(<array>, dimensions: list(width, height));
  without-bounds-checks
    for (row in rows, i :: <integer> from 0)
      assert(size(row) = width,
	     "All the rows in a stipple must have the same length");
      for (cell in row, j :: <integer> from 0)
	stipple[i, j] := cell
      end
    end
  end;
  stipple
end method make-stipple;


define stipple $horizontal-hatch
  1, 1, 1, 1, 1, 1, 1, 1;
  0, 0, 0, 0, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  0, 0, 0, 0, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  0, 0, 0, 0, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  0, 0, 0, 0, 0, 0, 0, 0;
end stipple $horizontal-hatch;

define stipple $vertical-hatch
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 0, 1, 0, 1, 0, 1, 0;
end stipple $vertical-hatch;

define stipple $cross-hatch
  1, 1, 1, 1, 1, 1, 1, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
end stipple $cross-hatch;

define stipple $diagonal-hatch-down
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
end stipple $diagonal-hatch-down;

define stipple $diagonal-hatch-up
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
  0, 1, 0, 1, 0, 1, 0, 1;
  1, 0, 1, 0, 1, 0, 1, 0;
end stipple $diagonal-hatch-up;

define stipple $bricks-stipple
  0, 0, 0, 1, 0, 0, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 1, 1, 1;
  0, 0, 0, 0, 0, 0, 0, 1;
  0, 0, 0, 0, 0, 0, 0, 1;
  0, 0, 0, 0, 0, 0, 0, 1;
  1, 1, 1, 1, 1, 1, 1, 1;
end stipple $bricks-stipple;

define stipple $tiles-stipple
  1, 0, 0, 0, 0, 0, 0, 0;
  1, 0, 0, 0, 0, 0, 0, 0 ;
  0, 1, 0, 0, 0, 0, 0, 1 ;
  0, 0, 1, 1, 1, 1, 1, 0;
  0, 0, 0, 0, 1, 0, 0, 0;
  0, 0, 0, 0, 1, 0, 0, 0 ;
  0, 0, 0, 1, 0, 1, 0, 0 ;
  1, 1, 1, 0, 0, 0, 1, 1;
end stipple $tiles-stipple;

define stipple $parquet-stipple
  1, 0, 0, 0, 0, 0, 0, 0;
  1, 1, 0, 0, 0, 0, 0, 1 ;
  0, 0, 1, 0, 0, 0, 1, 0 ;
  0, 0, 0, 1, 1, 1, 0, 0;
  0, 0, 0, 0, 1, 0, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 0;
  0, 0, 1, 0, 0, 0, 0, 0;
  0, 1, 0, 0, 0, 0, 0, 0;
end stipple $parquet-stipple;

define stipple $hearts-stipple
  0, 1, 1, 0, 1, 1, 0, 0;
  1, 0, 0, 1, 0, 0, 1, 0 ;
  1, 0, 0, 1, 0, 0, 1, 0 ;
  0, 1, 0, 0, 0, 1, 0, 0;
  0, 0, 1, 0, 1, 0, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 0 ;
  0, 0, 0, 0, 0, 0, 0, 0 ;
  0, 0, 0, 0, 0, 0, 0, 0;
end stipple $hearts-stipple;
