Module:    mini-duim
Synopsis:  Mini-DUIM layout stubs
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Space requirements

define sealed class <space-requirement> (<object>)
  slot space-requirement-width, required-init-keyword: width:;
  slot space-requirement-min-width;
  slot space-requirement-max-width;
  slot space-requirement-height, required-init-keyword: height:;
  slot space-requirement-min-height;
  slot space-requirement-max-height;
end class <space-requirement>;

define method initialize
    (space-req :: <space-requirement>,
     #key width,  min-width = width,   max-width = width,
     	  height, min-height = height, max-height = height)
  next-method();
  space-requirement-min-height(space-req) := min-height;
  space-requirement-max-height(space-req) := max-height;
  space-requirement-min-width(space-req) := min-width;
  space-requirement-max-width(space-req) := max-width
end method initialize;

define method space-requirement-components
    (space-req :: <space-requirement>)
 => (width, min-width, max-width, height, min-height, max-height)
  values(space-requirement-width(space-req),      space-requirement-min-width(space-req),
	 space-requirement-max-width(space-req),  space-requirement-height(space-req),
	 space-requirement-min-height(space-req), space-requirement-max-height(space-req))
end method space-requirement-components;

define constant $fill :: <integer> = 100000;


/// Default methods for layout protocol

define method compose-space
    (pane :: <sheet>, #key width, height) => (space-req :: <space-requirement>)
  do-compose-space(pane, width: width, height: height)
end method compose-space;

define method allocate-space (pane :: <sheet>, width, height) => ()
  do-allocate-space(pane, width, height)
end method allocate-space;

define method do-allocate-space (pane :: <sheet>, width, height) => ()
  ignore(width, height);
  #f
end method do-allocate-space;


/// Placeholder classes

// This is needed for our concrete gadget classes
define open abstract class <leaf-pane> (<basic-sheet>)
end class <leaf-pane>;

// This is needed for our concrete gadget classes
define open abstract class <single-child-composite-pane> (<basic-sheet>)
end class <multiple-child-composite-pane>;

// This is needed for our concrete gadget classes
define open abstract class <multiple-child-composite-pane> (<basic-sheet>)
end class <multiple-child-composite-pane>;


// This is needed for 'frame-wrapper' methods
define sealed class <column-layout> (<basic-sheet>)
end class <column-layout>;
