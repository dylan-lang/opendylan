Module:       duim-layouts-internals
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Space requirements

define protocol-class space-requirement (<object>) end;

define protocol <<space-requirement>> ()
  function space-requirement-width
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (width :: false-or(<integer>));
  function space-requirement-min-width
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (min-width :: false-or(<integer>));
  function space-requirement-max-width
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (max-width :: false-or(<integer>));
  function space-requirement-height
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (height :: false-or(<integer>));
  function space-requirement-min-height
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (min-height :: false-or(<integer>));
  function space-requirement-max-height
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (max-height :: false-or(<integer>));
  function space-requirement-components
    (sheet :: <abstract-sheet>, space-req :: <space-requirement>)
 => (width      :: false-or(<integer>), 
     min-width  :: false-or(<integer>), 
     max-width  :: false-or(<integer>),
     height     :: false-or(<integer>),
     min-height :: false-or(<integer>),
     max-height :: false-or(<integer>));
end protocol <<space-requirement>>;

// This is the size we use for a "fill" component, the idea being that
// we should be able sum up at least 100 $fill's without overflowing
// into bignums (that is, fit into the 28 bits required by Dylan)
// 'floor(10 ^ floor(logn($maximum-integer, 10)) / 100)'
define constant $fill :: <integer> = 100000;


/// Simple space requirement classes

define sealed class <null-space-requirement> (<space-requirement>)
end class <null-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <null-space-requirement>)
 => (width      :: <integer>, 
     min-width  :: <integer>, 
     max-width  :: <integer>,
     height     :: <integer>,
     min-height :: <integer>,
     max-height :: <integer>)
  values(0, 0, 0, 0, 0, 0)
end method space-requirement-components;

define sealed method space-requirement-width
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (width :: <integer>)
  0
end method space-requirement-width;

define sealed method space-requirement-min-width
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (min-width :: <integer>)
  0
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (max-width :: <integer>)
  0
end method space-requirement-max-width;

define sealed method space-requirement-height
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (height :: <integer>)
  0
end method space-requirement-height;

define sealed method space-requirement-min-height
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (min-height :: <integer>)
  0
end method space-requirement-min-height;

define sealed method space-requirement-max-height
    (sheet :: <sheet>, sr :: <null-space-requirement>) => (max-height :: <integer>)
  0
end method space-requirement-max-height;

define variable $null-space-requirement :: <null-space-requirement>
    = make(<null-space-requirement>);


define sealed class <fixed-space-requirement> (<space-requirement>)
  sealed constant slot %width :: false-or(<integer>),
    required-init-keyword: width:;
  sealed constant slot %height :: false-or(<integer>),
    required-init-keyword: height:;
end class <fixed-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <fixed-space-requirement>)
 => (width      :: false-or(<integer>), 
     min-width  :: false-or(<integer>), 
     max-width  :: false-or(<integer>),
     height     :: false-or(<integer>),
     min-height :: false-or(<integer>),
     max-height :: false-or(<integer>))
  let width  = space-req.%width;
  let height = space-req.%height;
  values(width, width, width, height, height, height)
end method space-requirement-components;

define sealed method space-requirement-width 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (width :: false-or(<integer>))
  sr.%width
end method space-requirement-width;

define sealed method space-requirement-min-width 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (min-width :: false-or(<integer>))
  sr.%width
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (max-width :: false-or(<integer>))
  sr.%width
end method space-requirement-max-width;

define sealed method space-requirement-height 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (height :: false-or(<integer>))
  sr.%height
end method space-requirement-height;

define sealed method space-requirement-min-height 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (min-height :: false-or(<integer>))
  sr.%height
end method space-requirement-min-height;

define sealed method space-requirement-max-height 
    (sheet :: <sheet>, sr :: <fixed-space-requirement>) => (max-height :: false-or(<integer>))
  sr.%height
end method space-requirement-max-height;


define sealed class <unbounded-space-requirement> (<space-requirement>)
  sealed constant slot %width :: false-or(<integer>),
    required-init-keyword: width:;
  sealed constant slot %height :: false-or(<integer>),
    required-init-keyword: height:;
end class <unbounded-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <unbounded-space-requirement>)
 => (width      :: false-or(<integer>), 
     min-width  :: <integer>, 
     max-width  :: <integer>,
     height     :: false-or(<integer>),
     min-height :: <integer>,
     max-height :: <integer>)
  values(space-req.%width,  0, $fill,
	 space-req.%height, 0, $fill)
end method space-requirement-components;

define sealed method space-requirement-width 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (width :: false-or(<integer>))
  sr.%width
end method space-requirement-width;

define sealed method space-requirement-min-width 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (min-width :: <integer>)
  0
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (max-width :: <integer>)
  $fill
end method space-requirement-max-width;

define sealed method space-requirement-height 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (height :: false-or(<integer>))
  sr.%height
end method space-requirement-height;

define sealed method space-requirement-min-height 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (min-height :: <integer>)
  0
end method space-requirement-min-height;

define sealed method space-requirement-max-height 
    (sheet :: <sheet>, sr :: <unbounded-space-requirement>) => (max-height :: <integer>)
  $fill
end method space-requirement-max-height;


define sealed class <general-space-requirement> (<space-requirement>)
  sealed constant slot %width :: false-or(<integer>),
    required-init-keyword: width:;
  sealed constant slot %min-width :: false-or(<integer>),
    required-init-keyword: min-width:;
  sealed constant slot %max-width :: false-or(<integer>),
    required-init-keyword: max-width:;
  sealed constant slot %height :: false-or(<integer>),
    required-init-keyword: height:;
  sealed constant slot %min-height :: false-or(<integer>),
    required-init-keyword: min-height:;
  sealed constant slot %max-height :: false-or(<integer>),
    required-init-keyword: max-height:;
end class <general-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <general-space-requirement>)
 => (width      :: false-or(<integer>), 
     min-width  :: false-or(<integer>), 
     max-width  :: false-or(<integer>),
     height     :: false-or(<integer>),
     min-height :: false-or(<integer>),
     max-height :: false-or(<integer>))
  values(space-req.%width,  space-req.%min-width,  space-req.%max-width,
	 space-req.%height, space-req.%min-height, space-req.%max-height)
end method space-requirement-components;

define sealed method space-requirement-width 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (width :: false-or(<integer>))
  sr.%width
end method space-requirement-width;

define sealed method space-requirement-min-width 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (min-width :: false-or(<integer>))
  sr.%min-width
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (max-width :: false-or(<integer>))
  sr.%max-width
end method space-requirement-max-width;

define sealed method space-requirement-height 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (height :: false-or(<integer>))
  sr.%height
end method space-requirement-height;

define sealed method space-requirement-min-height 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (min-height :: false-or(<integer>))
  sr.%min-height
end method space-requirement-min-height;

define sealed method space-requirement-max-height 
    (sheet :: <sheet>, sr :: <general-space-requirement>) => (max-height :: false-or(<integer>))
  sr.%max-height
end method space-requirement-max-height;


/// Complex space requirement classes

// A space requirement that allocates enough room to hold the given label
define sealed class <label-space-requirement> (<space-requirement>)
  sealed constant slot %label :: type-union(<string>, <image>),
    required-init-keyword: label:;
  sealed constant slot %min-width :: false-or(<integer>) = #f,
    init-keyword: min-width:;
  sealed constant slot %max-width :: false-or(<integer>) = #f,
    init-keyword: max-width:;
  sealed constant slot %min-height :: false-or(<integer>) = #f,
    init-keyword: min-height:;
  sealed constant slot %max-height :: false-or(<integer>) = #f,
    init-keyword: max-height:;
end class <label-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <label-space-requirement>)
 => (width      :: <integer>,
     min-width  :: <integer>, 
     max-width  :: <integer>,
     height     :: <integer>,
     min-height :: <integer>,
     max-height :: <integer>)
  let (width, height) = space-requirement-label-size(sheet, space-req);
  values(width,  space-req.%min-width  | width,  space-req.%max-width  | width,
	 height, space-req.%min-height | height, space-req.%max-height | height)
end method space-requirement-components;

define method space-requirement-label-size
    (sheet :: <sheet>, sr :: <label-space-requirement>)
 => (width :: <integer>, height :: <integer>)
  let label = sr.%label;
  select (label by instance?)
    <string> =>
      let _port = port(sheet);
      let text-style = get-default-text-style(_port, sheet);
      // Use the computed width of the label, but the height of the font
      // Add a few pixels in each direction to keep the label from being squeezed
      values(ceiling(text-size(_port, label,	//--- what about tabs and newlines?
			       text-style: text-style)) + 2,
	     ceiling(font-height(text-style, _port)) + 2);
    <image> =>
      values(image-width(label), image-height(label));
  end
end method space-requirement-label-size;

define sealed method space-requirement-width
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (width :: <integer>)
  let (width, height) = space-requirement-label-size(sheet, sr);
  ignore(height);
  width
end method space-requirement-width;

define sealed method space-requirement-min-width 
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (min-width :: <integer>)
  sr.%min-width
  | begin
      let (width, height) = space-requirement-label-size(sheet, sr);
      ignore(height);
      width
    end
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (max-width :: <integer>)
  sr.%max-width
  | begin
      let (width, height) = space-requirement-label-size(sheet, sr);
      ignore(height);
      width
    end
end method space-requirement-max-width;

define sealed method space-requirement-height 
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (height :: <integer>)
  let (width, height) = space-requirement-label-size(sheet, sr);
  ignore(width);
  height
end method space-requirement-height;

define sealed method space-requirement-min-height 
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (min-height :: <integer>)
  sr.%min-height
  | begin
      let (width, height) = space-requirement-label-size(sheet, sr);
      ignore(width);
      height
    end
end method space-requirement-min-height;

define sealed method space-requirement-max-height 
    (sheet :: <sheet>, sr :: <label-space-requirement>) => (max-height :: <integer>)
  sr.%max-height
  | begin
      let (width, height) = space-requirement-label-size(sheet, sr);
      ignore(width);
      height
    end
end method space-requirement-max-height;


// A space requirement that calls a function to get the requirements
//--- Maybe we should have a "one-shot" functional space requirement
//--- that only computes its value once and caches the result?
define sealed class <functional-space-requirement> (<space-requirement>)
  sealed constant slot %function :: <function>,
    required-init-keyword: function:;
end class <functional-space-requirement>;

define sealed method space-requirement-components
    (sheet :: <sheet>, space-req :: <functional-space-requirement>)
 => (width      :: <integer>,
     min-width  :: <integer>, 
     max-width  :: <integer>,
     height     :: <integer>,
     min-height :: <integer>,
     max-height :: <integer>)
  space-req.%function(sheet)
end method space-requirement-components;

define sealed method space-requirement-width
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (width :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w-, w+, h, h-, h+);
  w
end method space-requirement-width;

define sealed method space-requirement-min-width 
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (min-width :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w, w+, h, h-, h+);
  w-
end method space-requirement-min-width;

define sealed method space-requirement-max-width 
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (max-width :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w, w-, h, h-, h+);
  w+
end method space-requirement-max-width;

define sealed method space-requirement-height 
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (height :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w, w-, w+, h-, h+);
  h
end method space-requirement-height;

define sealed method space-requirement-min-height 
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (min-height :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w, w-, w+, h, h+);
  h-
end method space-requirement-min-height;

define sealed method space-requirement-max-height 
    (sheet :: <sheet>, sr :: <functional-space-requirement>) => (max-height :: <integer>)
  let (w, w-, w+, h, h-, h+) = sr.%function(sheet);
  ignore(w, w-, w+, h, h-);
  h+
end method space-requirement-max-height;


/// Space requirement constructors

define sealed method make
    (class == <space-requirement>,
     #rest initargs,
     #key label, function,
	  width  = 0, min-width  = width,  max-width  = width,
          height = 0, min-height = height, max-height = height)
 => (space-req :: <space-requirement>)
  case
    label =>
      apply(make, <label-space-requirement>,
	    label: label, initargs);
    function =>
      apply(make, <functional-space-requirement>,
	    function: function, initargs);
    width    == min-width
    & width  == max-width
    & height == min-height
    & height == max-height =>
      // Compare with '==' to avoid blowout if width or height is #"compute"
      if (width == 0 & height == 0)
        $null-space-requirement
      else
        make(<fixed-space-requirement>,
             width: width, height: height)
      end;
    min-width    == 0
    & max-width  >= $fill
    & min-height == 0
    & max-height >= $fill =>
      make(<unbounded-space-requirement>,
	   width: width, height: height);
    otherwise =>
      make(<general-space-requirement>,
	   width:  width,  min-width:  min-width,  max-width:  max-width,
	   height: height, min-height: min-height, max-height: max-height);
  end
end method make;

// Seal the constructors and initializers for all space requirements
define sealed domain make (subclass(<space-requirement>));
define sealed domain initialize (<space-requirement>);


/// Space requirements arithmetic

define method space-requirement-combine
    (sheet :: <sheet>, function :: <function>,
     sr1 :: <space-requirement>, sr2 :: <space-requirement>)
 => (space-req :: <space-requirement>)
  let (w1, w1-, w1+, h1, h1-, h1+) = space-requirement-components(sheet, sr1);
  let (w2, w2-, w2+, h2, h2-, h2+) = space-requirement-components(sheet, sr2);
  make(<space-requirement>,
       width:      function(w1,  w2),
       min-width:  function(w1-, w2-),
       max-width:  function(w1+, w2+),
       height:     function(h1,  h2),
       min-height: function(h1-, h2-),
       max-height: function(h1+, h2+))
end method space-requirement-combine;

// Add two space requirements
define inline function space-requirement+*
    (sheet :: <sheet>, sr1 :: <space-requirement>, sr2 :: <space-requirement>)
 => (space-req :: <space-requirement>)
  space-requirement-combine(sheet, \+, sr1, sr2)
end function space-requirement+*;

// The "spread" version of the above...
define method space-requirement+
    (sheet :: <sheet>, space-req :: <space-requirement>,
     #key width      :: <integer> = 0,
     	  min-width  :: <integer> = width,
	  max-width  :: <integer> = width,
          height     :: <integer> = 0,
	  min-height :: <integer> = height,
	  max-height :: <integer> = height)
 => (space-req :: <space-requirement>)
  let (w :: <integer>, w- :: <integer>, w+ :: <integer>,
       h :: <integer>, h- :: <integer>, h+ :: <integer>)
    = space-requirement-components(sheet, space-req);
  make(<space-requirement>,
       width:      w  + width,
       min-width:  w- + min-width,
       max-width:  w+ + max-width,
       height:     h  + height,
       min-height: h- + min-height,
       max-height: h+ + max-height)
end method space-requirement+;
