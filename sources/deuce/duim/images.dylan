Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce standard images

define constant $breakpoint-red              = make-rgb-color(0.80, 0.20, 0.20);
define constant $breakpoint-red-highlight    = make-rgb-color(1.00, 0.70, 0.70);
define constant $breakpoint-red-shadow       = make-rgb-color(0.50, 0.00, 0.00);

define constant $breakpoint-green            = make-rgb-color(0.10, 0.80, 0.10);
define constant $breakpoint-green-highlight  = make-rgb-color(0.60, 1.00, 0.60);
define constant $breakpoint-green-shadow     = make-rgb-color(0.00, 0.50, 0.00);

define constant $breakpoint-yellow           = make-rgb-color(0.90, 0.90, 0.20);
define constant $breakpoint-yellow-highlight = make-rgb-color(1.00, 1.00, 0.70);
define constant $breakpoint-yellow-shadow    = make-rgb-color(0.60, 0.60, 0.00);

define constant $breakpoint-blue             = make-rgb-color(0.00, 0.00, 1.00);
define constant $breakpoint-blue-highlight   = make-rgb-color(0.50, 0.50, 1.00);
define constant $breakpoint-blue-shadow      = make-rgb-color(0.00, 0.00, 0.70);

define constant $breakpoint-gray             = make-rgb-color(0.60, 0.60, 0.60);


define pattern $potential-breakpoint-pattern
    (vector($background,
	    $breakpoint-gray))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $potential-breakpoint-pattern;

define pattern $enabled-breakpoint-pattern
    (vector($background,
	    $breakpoint-red-highlight,
	    $breakpoint-red,
	    $breakpoint-red-shadow))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  0, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $enabled-breakpoint-pattern;

define pattern $disabled-breakpoint-pattern
    (vector($background,
	    $breakpoint-red-highlight,
	    $breakpoint-red,
	    $breakpoint-red-shadow))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 3, 3, 3, 3, 2, 2, 2, 0;
  1, 2, 3, 3, 0, 0, 0, 2, 2, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 2, 2, 0, 0, 0, 1, 1, 2, 3;
  0, 2, 2, 2, 1, 1, 1, 1, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $disabled-breakpoint-pattern;

define pattern $step-breakpoint-pattern
    (list($background,
          $breakpoint-green-highlight,
          $breakpoint-green,
          $breakpoint-green-shadow))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  0, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $step-breakpoint-pattern;

define pattern $test-breakpoint-pattern
    (list($background,
          $breakpoint-red-highlight,
          $breakpoint-red,
          $breakpoint-red-shadow,
          $black))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 4, 2, 2, 4, 2, 2, 0;
  1, 2, 2, 2, 4, 2, 2, 4, 2, 2, 3;
  1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 3;
  1, 2, 2, 2, 4, 2, 4, 2, 2, 2, 3;
  1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 3;
  1, 2, 2, 4, 2, 2, 4, 2, 2, 2, 3;
  0, 2, 2, 4, 2, 2, 4, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $test-breakpoint-pattern;

define pattern $enabled-tracepoint-pattern
    (list($background,
          $breakpoint-yellow-highlight,
          $breakpoint-yellow,
          $breakpoint-yellow-shadow))
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 2, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 2, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;
end pattern $enabled-tracepoint-pattern;

define pattern $disabled-tracepoint-pattern
    (list($background,
          $breakpoint-yellow-highlight,
          $breakpoint-yellow,
          $breakpoint-yellow-shadow))
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 1, 3, 0, 2, 2, 0, 0, 0;
  0, 0, 0, 1, 3, 0, 2, 2, 0, 0, 0;
  0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 0;
  0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 0;
  0, 1, 3, 0, 0, 0, 0, 0, 2, 2, 0;
  0, 1, 2, 1, 1, 1, 1, 1, 1, 2, 0;
  1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;
end pattern $disabled-tracepoint-pattern;

define pattern $profile-point-pattern
    (list($background,
          $breakpoint-blue-highlight,
          $breakpoint-blue,
          $breakpoint-blue-shadow))
  1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
end pattern $profile-point-pattern;


define pattern $current-location-pattern
    (list($background,
          $breakpoint-green-highlight,
          $breakpoint-green,
          $breakpoint-green-shadow))
  0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 2, 2, 2, 0, 0, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  1, 3, 3, 3, 1, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $current-location-pattern;


define pattern $prompt-pattern
    (vector($background,
	    $foreground))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0;
  0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0;
  0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0;
  1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0;
  0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0;
  0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0;
  0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
end pattern $prompt-pattern;

define pattern $values-pattern
    (vector($background,
	    $foreground))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0;
  0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
end pattern $values-pattern;


define pattern $warning-pattern
    (vector($background,
	    $breakpoint-yellow,
	    $black))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 1, 1, 2, 1, 1, 0, 0, 0, 0;
  0, 0, 0, 1, 1, 2, 1, 1, 0, 0, 0, 0;
  0, 0, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $warning-pattern;

define pattern $serious-warning-pattern
    (vector($background,
	    $breakpoint-red,
	    $black))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 1, 1, 2, 1, 1, 0, 0, 0, 0;
  0, 0, 0, 1, 1, 2, 1, 1, 0, 0, 0, 0;
  0, 0, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 0, 0;
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $serious-warning-pattern;


// The idea is that we have internal DUIM <pattern> objects, but that
// there is one level of indirection so that the patterns can be replaced
// by a bitmap or icon from an external resource
define variable $potential-breakpoint-image = $potential-breakpoint-pattern;
define variable $enabled-breakpoint-image   = $enabled-breakpoint-pattern;
define variable $disabled-breakpoint-image  = $disabled-breakpoint-pattern;
define variable $step-breakpoint-image      = $step-breakpoint-pattern;
define variable $test-breakpoint-image      = $test-breakpoint-pattern;
define variable $enabled-tracepoint-image   = $enabled-tracepoint-pattern;
define variable $disabled-tracepoint-image  = $disabled-tracepoint-pattern;
define variable $profile-point-image        = $profile-point-pattern;
define variable $current-location-image     = $current-location-pattern;
define variable $prompt-image               = $prompt-pattern;
define variable $values-image               = $values-pattern;
define variable $warning-image              = $warning-pattern;
define variable $serious-warning-image      = $serious-warning-pattern;


// The images must be in the order given by $potential-breakpoint, etc.
define variable $standard-images :: <simple-object-vector> = #[];

define function initialize-standard-images ()
  $standard-images
    := vector($potential-breakpoint-image,	// $potential-breakpoint
	      $enabled-breakpoint-image,	// $enabled-breakpoint
	      $disabled-breakpoint-image,	// $disabled-breakpoint
	      $step-breakpoint-image,		// $step-breakpoint
	      $test-breakpoint-image,		// $test-breakpoint
	      $enabled-tracepoint-image,	// $enabled-tracepoint
	      $disabled-tracepoint-image,	// $disabled-tracepoint
	      $profile-point-image,		// $profile-point
	      $current-location-image,		// $current-location
	      $prompt-image,			// $prompt-arrow
	      $values-image,			// $values-arrow
	      $warning-image,			// $warning-arrow
	      $serious-warning-image)		// $serious-warning-arrow
end function initialize-standard-images;

initialize-standard-images();

define method standard-images
    (window :: <deuce-pane>, index :: <integer>) => (image)
  $standard-images[index]
end method standard-images;
