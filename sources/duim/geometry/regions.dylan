Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The basic region protocol

define protocol <<region-protocol>> ()
  // Basic protocol
  function region-empty?
    (region :: <region>) => (true? :: <boolean>);
  function region-equal
    (region1 :: <region>, region2 :: <region>)
 => (true? :: <boolean>);
  function region-contains-position?
    (region :: <region>, x :: <real>, y :: <real>)
 => (true? :: <boolean>);
  function region-contains-region?
    (region1 :: <region>, region2 :: <region>)
 => (true? :: <boolean>);
  function region-intersects-region?
    (region1 :: <region>, region2 :: <region>)
 => (true? :: <boolean>);
  function region-union
    (region1 :: <region>, region2 :: <region>)
 => (region :: <region>);
  function region-intersection
    (region1 :: <region>, region2 :: <region>)
 => (region :: <region>);
  function region-difference
    (region1 :: <region>, region2 :: <region>)
 => (region :: <region>);
  // Compound regions
  function do-regions
    (function :: <function>, region :: <region>, #key normalize?) => ();
  function region-set-function
    (region :: <region>) => (function);
  function region-set-regions
    (region :: <region>, #key normalize?) => (regions :: <sequence>);
  // Regions meet transforms
  function transform-region
    (transform :: <transform>, region :: <region>)
 => (region :: <region>);
  function untransform-region
    (transform :: <transform>, region :: <region>)
 => (region :: <region>);
  function transform-region!
    (transform :: <transform>, region :: <region>)
 => (region :: <region>);
  function untransform-region!
    (transform :: <transform>, region :: <region>)
 => (region :: <region>);
end protocol <<region-protocol>>;


/// The basic region classes

define method untransform-region
    (transform :: <transform>, region :: <region>) => (region :: <region>)
  transform-region(invert-transform(transform), region)
end method untransform-region;

// The default method creates a new region
define method transform-region!
    (transform :: <transform>, region :: <region>) => (region :: <region>)
  transform-region(transform, region)
end method transform-region!;

// The default method creates a new region
define method untransform-region!
    (transform :: <transform>, region :: <region>) => (region :: <region>)
  transform-region!(invert-transform(transform), region)
end method untransform-region!;

define method \=
    (region1 :: <region>, region2 :: <region>) => (true? :: <boolean>)
  region1 == region2
  | region-equal(region1, region2)
end method \=;


/// General cases of region arithmetic

// Exclude the general case of 'region-equal'
define method region-equal
    (region1 :: <region>, region2 :: <region>) => (true? :: <boolean>)
  #f
end method region-equal;

// Exclude the general case of 'region-contains-position?'
define method region-contains-position?
    (region :: <region>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  #f
end method region-contains-position?;

// Exclude the general case of 'region-contains-region?'
define method region-contains-region?
    (region1 :: <region>, region2 :: <region>) => (true? :: <boolean>)
  #f
end method region-contains-region?;

// Exclude the general case of 'region-intersects-region?'
define method region-intersects-region?
    (region1 :: <region>, region2 :: <region>) => (true? :: <boolean>)
  #f
end method region-intersects-region?;

define method region-empty? (region :: <region>) => (true? :: <boolean>)
  #f
end method region-empty?;


/// Nowhere

define sealed class <nowhere> (<region>)
end class <nowhere>;

define sealed domain make (singleton(<nowhere>));
define sealed domain initialize (<nowhere>);

define constant $nowhere :: <nowhere> = make(<nowhere>);

define method region-equal
    (nowhere1 :: <nowhere>, nowhere2 :: <nowhere>) => (true? :: <boolean>)
  #t
end method region-equal;

define method region-contains-region?
    (region :: <region>, nowhere :: <nowhere>) => (true? :: <boolean>)
  #t
end method region-contains-region?;

define method region-contains-region?
    (nowhere :: <nowhere>, region :: <region>) => (true? :: <boolean>)
  #f
end method region-contains-region?;

define method region-contains-region?
    (nowhere1 :: <nowhere>, nowhere2 :: <nowhere>) => (true? :: <boolean>)
  #f
end method region-contains-region?;

define method region-empty? (region :: <nowhere>) => (true? :: <boolean>)
  #t
end method region-empty?;

define method transform-region
    (transform :: <transform>, region :: <nowhere>) => (region :: <region>)
  region
end method transform-region;


/// Everywhere

define sealed class <everywhere> (<region>)
end class <everywhere>;

define sealed domain make (singleton(<everywhere>));
define sealed domain initialize (<everywhere>);

define constant $everywhere :: <everywhere> = make(<everywhere>);

define inline function everywhere? (region) => (true? :: <boolean>)
  region == $everywhere
end function everywhere?;

define method region-equal
    (everywhere1 :: <everywhere>, everywhere2 :: <everywhere>) => (true? :: <boolean>)
  #t
end method region-equal;

define method region-contains-position?
    (everywhere :: <everywhere>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  #t
end method region-contains-position?;

define method region-contains-region?
    (region :: <region>, everywhere :: <everywhere>) => (true? :: <boolean>)
  #f
end method region-contains-region?;

define method region-contains-region?
    (everywhere :: <everywhere>, region :: <region>) => (true? :: <boolean>)
  #t
end method region-contains-region?;

define method region-contains-region?
    (everywhere1 :: <everywhere>, everywhere2 :: <everywhere>) => (true? :: <boolean>)
  #t
end method region-contains-region?;

define method region-intersects-region?
    (everywhere :: <everywhere>, region :: <region>) => (true? :: <boolean>)
  ~(region == $nowhere)
end method region-intersects-region?;

define method region-intersects-region?
    (region :: <region>, everywhere :: <everywhere>) => (true? :: <boolean>)
  ~(region == $nowhere)
end method region-intersects-region?;

define method region-intersects-region?
    (everywhere1 :: <everywhere>, everywhere2 :: <everywhere>) => (true? :: <boolean>)
  #t
end method region-intersects-region?;

define method region-empty? (region :: <everywhere>) => (true? :: <boolean>)
  #f
end method region-empty?;

define method transform-region
    (transform :: <transform>, region :: <everywhere>) => (region :: <region>)
  region
end method transform-region;


/// Points

define protocol <<point-protocol>> (<<region-protocol>>)
  function point-position
    (point :: <point>) => (x :: <real>, y :: <real>);
  getter point-x
    (point :: <point>) => (x :: <real>);
  getter point-y
    (point :: <point>) => (y :: <real>);
end protocol <<point-protocol>>;

define sealed class <standard-point> (<point>)
  sealed slot point-x :: <real>,
    required-init-keyword: x:;
  sealed slot point-y :: <real>,
    required-init-keyword: y:;
end class <standard-point>;

define inline function make-point
    (x :: <real>, y :: <real>) => (point :: <standard-point>)
  make(<standard-point>, x: x, y: y)
end function make-point;

define sealed inline method make
    (class == <point>, #key x, y)
 => (point :: <standard-point>)
  make-point(x, y)
end method make;

define sealed domain make (singleton(<standard-point>));
define sealed domain initialize (<standard-point>);

define sealed inline method point-position
    (point :: <standard-point>) => (x :: <real>, y :: <real>)
  values(point-x(point), point-y(point))
end method point-position;

define sealed method region-equal
    (point1 :: <standard-point>, point2 :: <standard-point>) => (true? :: <boolean>)
  point-x(point1) = point-x(point2)
  & point-y(point1) = point-y(point2)
end method region-equal;

define method transform-region
    (transform :: <transform>, point :: <standard-point>) => (point :: <standard-point>)
  let (x, y) = transform-position(transform, point-x(point), point-y(point));
  make-point(x, y)
end method transform-region;

define sealed method box-edges
    (point :: <standard-point>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  fix-box(point-x(point),     point-y(point),
	  point-x(point) + 1, point-y(point) + 1)
end method box-edges;

define sealed method region-contains-position?
    (point :: <standard-point>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  point-x(point) = x
  & point-y(point) = y
end method region-contains-position?;

define sealed method region-contains-region?
    (point1 :: <standard-point>, point2 :: <standard-point>) => (true? :: <boolean>)
  point-x(point1) = point-x(point2)
  & point-y(point1) = point-y(point2)
end method region-contains-region?;

define method region-contains-region?
    (region :: <region>, point :: <standard-point>) => (true? :: <boolean>)
  region-contains-position?(region, point-x(point), point-y(point))
end method region-contains-region?;

define sealed method region-intersects-region?
    (point1 :: <standard-point>, point2 :: <standard-point>) => (true? :: <boolean>)
  point-x(point1) = point-x(point2)
  & point-y(point1) = point-y(point2)
end method region-intersects-region?;

define method region-intersects-region?
    (point :: <standard-point>, region :: <region>) => (true? :: <boolean>)
  region-contains-position?(region, point-x(point), point-y(point))
end method region-intersects-region?;

define method region-intersects-region?
    (region :: <region>, point :: <standard-point>) => (true? :: <boolean>)
  region-contains-position?(region, point-x(point), point-y(point))
end method region-intersects-region?;

define sealed method region-intersection
    (point1 :: <standard-point>, point2 :: <standard-point>) => (region :: <region>)
  if (point-x(point1) = point-x(point2)
      & point-y(point1) = point-y(point2))
    point1
  else
    $nowhere
  end
end method region-intersection;


/// Region Sets

define method region-set-function (region :: <region>) => (function)
  union
end method region-set-function;

define method region-set-regions
    (region :: <region>, #key normalize?) => (regions :: <vector>)
  ignore(normalize?);
  vector(region)
end method region-set-regions;

define method do-regions
    (function :: <function>, region :: <region>, #key normalize?) => ()
  ignore(normalize?);
  function(region)
end method do-regions;

define method do-regions
    (function :: <function>, region :: <region-set>, #rest args, #key normalize?) => ()
  dynamic-extent(args);
  ignore(normalize?);
  do(function, apply(region-set-regions, region, args))
end method do-regions;

define method region-contains-position?
    (region-set :: <region-set>, x :: <real>, y :: <real>) => (true? :: <boolean>)
  block (return)
    local method contains-position? (region) => ()
	    when (region-contains-position?(region, x, y))
	      return(#t)
	    end
	  end method;
    do-regions(contains-position?, region-set);
    #f
  end
end method region-contains-position?;

define method region-contains-region?
    (region-set :: <region-set>, other-region :: <region>) => (true? :: <boolean>)
  block (return)
    local method contains-region? (region) => ()
	    when (region-contains-region?(region, other-region))
	      return(#t)
	    end
	  end method;
    do-regions(contains-region?, region-set);
    #f
  end
end method region-contains-region?;

define method box-edges
    (region-set :: <region-set>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let left :: <integer> = $largest-coordinate;
  let top  :: <integer> = $largest-coordinate;
  let right  :: <integer> = $smallest-coordinate;
  let bottom :: <integer> = $smallest-coordinate;
  local method add-region (region) => ()
	  let (rl, rt, rr, rb) = box-edges(region);
	  min!(left, rl);
	  min!(top, rt);
	  max!(right, rr);
	  max!(bottom, rb);
	end method;
  do-regions(add-region, region-set);
  values(left, top, right, bottom)
end method box-edges;
