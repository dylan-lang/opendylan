Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Pens

define constant <pen-unit>
    = one-of(#"normal", #"point", #"device");

define constant <pen-joint-shape>
    = one-of(#"miter", #"bevel", #"round", #"none");

define constant <pen-cap-shape>
    = one-of(#"butt", #"square", #"round", #"no-end-point");

define protocol <<pen-protocol>> ()
  getter pen-width
    (pen :: <pen>) => (width :: <integer>);
  getter pen-units
    (pen :: <pen>) => (units :: <pen-unit>);
  getter pen-dashes
    (pen :: <pen>) => (dashes :: type-union(<boolean>, <sequence>));
  getter pen-joint-shape
    (pen :: <pen>) => (joint-shape :: <pen-joint-shape>);
  getter pen-cap-shape
    (pen :: <pen>) => (cap-shape :: <pen-cap-shape>);
end protocol <<pen-protocol>>;


//--- Do we need to scale the pen width by the medium transform?
define sealed class <standard-pen> (<pen>)
  sealed constant slot pen-width :: <integer> = 1,
    init-keyword: width:;
  sealed constant slot pen-units :: <pen-unit> = #"normal",
    init-keyword: units:;
  sealed constant slot pen-dashes :: type-union(<boolean>, <sequence>) = #f,
    init-keyword: dashes:;
  sealed constant slot pen-joint-shape :: <pen-joint-shape> = #"miter",
    init-keyword: joint-shape:;
  sealed constant slot pen-cap-shape :: <pen-cap-shape> = #"butt",
    init-keyword: cap-shape:;
end class <standard-pen>;

define sealed domain make (singleton(<standard-pen>));
define sealed domain initialize (<standard-pen>);

define sealed method \=
    (pen1 :: <standard-pen>, pen2 :: <standard-pen>) => (true? :: <boolean>)
  pen1 == pen2
  | begin
      pen-width(pen1) = pen-width(pen2)
      & pen-units(pen1) == pen-units(pen2)
      & pen-dashes(pen1) == pen-dashes(pen2)
      & pen-joint-shape(pen1) == pen-joint-shape(pen2)
      & pen-cap-shape(pen1) == pen-cap-shape(pen2)
    end
end method \=;


// Windows-like "stock" pens
define constant $solid-pen  :: <standard-pen>
    = make(<standard-pen>, width: 1, dashes: #f);
define constant $dashed-pen :: <standard-pen>
    = make(<standard-pen>, width: 1, dashes: #t);
define constant $dotted-pen :: <standard-pen>
    = make(<standard-pen>, width: 1, dashes: #[1, 1]);
define constant $dash-dot-pen :: <standard-pen>
    = make(<standard-pen>, width: 1, dashes: #[4, 1, 1, 1]);
define constant $dash-dot-dot-pen :: <standard-pen>
    = make(<standard-pen>, width: 1, dashes: #[4, 1, 1, 1, 1, 1]);

define constant $solid-pens :: <simple-object-vector>
    = vector(make(<standard-pen>, width: 0, dashes: #f),
	     $solid-pen,		// width: 1, dashes: #f
	     make(<standard-pen>, width: 2, dashes: #f),
	     make(<standard-pen>, width: 3, dashes: #f),
	     make(<standard-pen>, width: 4, dashes: #f));

define constant $dashed-pens :: <simple-object-vector>
    = vector(make(<standard-pen>, width: 0, dashes: #t),
	     $dashed-pen,		// width: 1, dashes: #t
	     make(<standard-pen>, width: 2, dashes: #t),
	     make(<standard-pen>, width: 3, dashes: #t),
	     make(<standard-pen>, width: 4, dashes: #t));

define sealed method make
    (class == <pen>, 
     #key width = 1, units = #"normal", dashes,
	  joint-shape = #"miter", cap-shape = #"butt")
 => (pen :: <standard-pen>)
  if (integral?(width)
      & (0 <= width & width <= 4)
      & units == #"normal"
      & (dashes == #t | dashes == #f)
      & joint-shape == #"miter"
      & cap-shape == #"butt")
    // Cache the common case when only DASHES: and WIDTH: are provided
    (if (dashes) $dashed-pens else $solid-pens end)[width]
  else
    make(<standard-pen>,
	 width: width, units: units, dashes: dashes,
	 joint-shape: joint-shape, cap-shape: cap-shape)
  end
end method make;


/// Contrasting dash patterns

define constant $dash-pattern-grain-size :: <integer> = 3;

define constant $contrasting-dash-patterns :: <simple-object-vector>
    = vector
	(#[1, 1],						// 2
	 #[2, 1], #[1, 2],					// 3
	 #[3, 1], #[2, 2],					// 4
	 #[2, 3], #[1, 4], #[2, 1, 1, 1], #[1, 2, 1, 1],	// 5
	 #[4, 2], #[3, 3], #[2, 4], #[3, 1, 1, 1], #[2, 2, 1, 1], #[3, 2, 1, 1],
	 #[3, 1, 2, 1]);

define sealed method make-contrasting-dash-patterns
    (n :: <integer>, #key k = $unsupplied) => (dashes)
  check-type(n, limited(<integer>, min: 2, max: 16));
  local method make-dash-pattern (index) => (pattern :: <vector>)
	  let known = $contrasting-dash-patterns[index];
	  let pattern :: <simple-object-vector> = make(<simple-vector>, size: size(known));
	  for (i :: <integer> from 0 below size(known))
	    pattern[i] := pattern[i] * $dash-pattern-grain-size
	  end;
	  pattern
	end method;
  if (unsupplied?(k))
    let patterns :: <simple-object-vector> = make(<simple-vector>, size: n);
    without-bounds-checks
      for (i :: <integer> from 0 below n)
	patterns[i] := make-dash-pattern(i)
      end
    end;
    patterns
  else
    assert(k < n,
	   "The index %d must be smaller than the count %d", k, n);
    make-dash-pattern(k)
  end
end method make-contrasting-dash-patterns;

define method contrasting-dash-patterns-limit
    (_port) => (limit :: <integer>)
  16
end method contrasting-dash-patterns-limit;
