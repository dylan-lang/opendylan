Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Layout classes tests

define sideways method make-test-instance
    (class :: subclass(<layout>)) => (instance :: <layout>)
  make-test-pane(class)
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<grid-layout>)) => (instance :: <grid-layout>)
  let cell-space-requirement
    = make(<space-requirement>, width: 100, height: 100);
  make-test-pane(<grid-layout>, cell-space-requirement: cell-space-requirement)
end method make-test-instance;

define duim-layouts class-test <layout> ()
  //---*** Fill this in...
end class-test <layout>;

define duim-layouts class-test <basic-composite-pane> ()
  //---*** Fill this in...
end class-test <basic-composite-pane>;

define duim-layouts class-test <drawing-pane> ()
  //---*** Fill this in...
end class-test <drawing-pane>;

define duim-layouts class-test <leaf-pane> ()
  //---*** Fill this in...
end class-test <leaf-pane>;

define duim-layouts class-test <multiple-child-composite-pane> ()
  //---*** Fill this in...
end class-test <multiple-child-composite-pane>;

define duim-layouts class-test <null-pane> ()
  //---*** Fill this in...
end class-test <null-pane>;

define duim-layouts class-test <simple-pane> ()
  //---*** Fill this in...
end class-test <simple-pane>;

define duim-layouts class-test <single-child-composite-pane> ()
  //---*** Fill this in...
end class-test <single-child-composite-pane>;

define duim-layouts class-test <top-level-sheet> ()
  //---*** Fill this in...
end class-test <top-level-sheet>;



/// layout tests

// some default values

define variable *layout-default-width*  = 200;
define variable *layout-default-height* = 300;
define variable *child-explicit-width*  = 200;
define variable *child-explicit-height* = 300;


/// expected-width and expected-height

// I've made these methods horribly complicated so that each test can
// pick and choose as to the simplest place to redefine. It works as
// follows:
//
// 1. expected-named-size 
// This takes the name of the size it is looking for. Redefining on this
// method means you can implement a single method that will do everything.
// 2. expected-named-width/expected-named-height
// This one can also be subclasses, in this case if all of the width
// cases can be handled in one go, for example.
// 3. expected-<xxx>-width/expected-<xxx>-height
// These methods allow redefinition of the methods that produce each
// individual result

define generic expected-default-size
    (name, #key width, height) => (size :: <integer>);

define generic expected-named-size
    (pane, name, #key width, height, #all-keys) => (size :: <integer>);
define generic expected-named-width
    (pane, name, #key width, height, #all-keys) => (size :: <integer>);
define generic expected-named-height
    (pane, name, #key width, height, #all-keys) => (size :: <integer>);
define generic expected-width 
    (pane, #key, #all-keys) => (width :: <integer>);
define generic expected-min-width 
    (pane, #key, #all-keys) => (width :: <integer>);
define generic expected-max-width 
    (pane, #key, #all-keys) => (width :: <integer>);
define generic expected-height
    (pane, #key, #all-keys) => (height :: <integer>);
define generic expected-min-height
    (pane, #key, #all-keys) => (height :: <integer>);
define generic expected-max-height
    (pane, #key, #all-keys) => (height :: <integer>);



define method expected-default-size 
    (name :: <symbol>, #key width, height) => (size :: <integer>)
  select (name)
    #"width"      => width | 100;
    #"min-width"  => 0;
    #"max-width"  => $fill;
    #"height"     => height | 100;
    #"min-height" => 0;
    #"max-height" => $fill;
  end
end method expected-default-size;

define method expected-size-function 
    (name :: <symbol>) => (size-function :: <function>)
  select (name)
    #"width"      => expected-width;
    #"min-width"  => expected-min-width;
    #"max-width"  => expected-max-width;
    #"height"     => expected-height;
    #"min-height" => expected-min-height;
    #"max-height" => expected-max-height;
  end
end method expected-size-function;

define method expected-named-size
    (pane, name, #key width, height) => (size :: <integer>)
  select (name)
    #"width"      => space-requirement-width;
    #"min-width"  => space-requirement-min-width;
    #"max-width"  => space-requirement-max-width;
    #"height"     => space-requirement-height;
    #"min-height" => space-requirement-min-height;
    #"max-height" => space-requirement-max-height;
  end(pane, compose-space(pane, width: width, height: height))
end method expected-named-size;

define method expected-width-given-default
    (pane, #rest args, #key width, #all-keys) => (width :: <integer>)
  apply(expected-constrained-width, pane, width, args)
  /*---*** code was this... is it still correct?
  max(apply(expected-min-width, pane, args),
      min(width, apply(expected-max-width, pane, args)))
  */
end method expected-width-given-default;

define method expected-height-given-default 
    (pane, #rest args, #key height, #all-keys) => (width :: <integer>)
  apply(expected-constrained-height, pane, height, args)
  /*---*** code was this... is it still correct?
  max(apply(expected-min-height, pane, args),
      min(height, apply(expected-max-height, pane, args)))
  */
end method expected-height-given-default;

define method expected-named-width
    (pane, name, #rest args, #key width, height, #all-keys)
 => (width :: <integer>)
  apply(expected-named-size, pane, name, args)
end method expected-named-width;

define method expected-named-height 
    (pane, name, #rest args, #key width, height, #all-keys)
 => (height :: <integer>)
  apply(expected-named-size, pane, name, args)
end method expected-named-height;

define method expected-width 
    (pane, #rest args, #key width, #all-keys)
 => (width :: <integer>)
  ignore(width);
  apply(expected-named-width, pane, #"width", args)
end method expected-width;

define method expected-height 
    (pane, #rest args, #key height, #all-keys)
 => (height :: <integer>)
  ignore(height);
  apply(expected-named-height, pane, #"height", args)
end method expected-height;

define method expected-min-width 
    (pane, #rest args, #key, #all-keys)
 => (min-width :: <integer>)
  apply(expected-named-width, pane, #"min-width", args)
end method expected-min-width;

define method expected-min-height 
    (pane, #rest args, #key, #all-keys)
 => (min-height :: <integer>)
  apply(expected-named-height, pane, #"min-height", args)
end method expected-min-height;

define method expected-max-width 
    (pane, #rest args, #key, #all-keys)
 => (max-width :: <integer>)
  apply(expected-named-width, pane, #"max-width", args)
end method expected-max-width;

define method expected-max-height 
    (pane, #rest args, #key, #all-keys)
 => (max-height :: <integer>)
  apply(expected-named-height, pane, #"max-height", args)
end method expected-max-height;

define method expected-fixed-width? 
    (pane, #rest args, #key, #all-keys)
 => (fixed? :: <boolean>)
  apply(expected-width, pane, args) = apply(expected-max-width, pane, args)
end method expected-fixed-width?;

define method expected-fixed-height? 
    (pane, #rest args, #key, #all-keys)
 => (fixed? :: <boolean>)
  apply(expected-height, pane, args) = apply(expected-max-height, pane, args)
end method expected-fixed-height?;

define method expected-constrained-width
    (pane, width :: <integer>, #rest args, #key, #all-keys)
 => (width :: <integer>)
  min(max(width, apply(expected-min-width, pane, args)),
      apply(expected-max-width, pane, args))
end method expected-constrained-width;

define method expected-constrained-height
    (pane, height :: <integer>, #rest args, #key, #all-keys)
 => (height :: <integer>)
  min(max(height, apply(expected-min-height, pane, args)),
      apply(expected-max-height, pane, args))
end method expected-constrained-height;

define method normalize-max-size
    (size :: <integer>) => (size :: <integer>)
  min(size, $fill)
end method normalize-max-size;


/// check-layout-pane-layout

define method check-default-pane-size
    (pane, name, #rest args, #key, #all-keys) => ()
  invalidate-space-requirements(pane);
  let space = compose-space(pane);
  let (width, min-width, max-width, height, min-height, max-height)
    = space-requirement-components(pane, space);
  check(concatenate(name, " min-width <= default width"), \<=,
        min-width, width);
  check(concatenate(name, " default width <= max-width"), \<=,
        width, max-width);
  check(concatenate(name, " min-height <= default height"), \<=,
        min-height, height);
  check(concatenate(name, " default height <= max-height"), \<=,
        height, max-height);
  check-equal(concatenate(name, " default width"),
              width,
              apply(expected-width, pane, args));
  check-equal(concatenate(name, " default height"),
              height,
              apply(expected-height, pane, args));
  check-equal(concatenate(name, " default min-width"),
              min-width,
              apply(expected-min-width, pane, args));
  check-equal(concatenate(name, " default min-height"),
              min-height,
              apply(expected-min-height, pane, args));
  check-equal(concatenate(name, " default max-width"),
              normalize-max-size(max-width),
              normalize-max-size(apply(expected-max-width, pane, args)));
  check-equal(concatenate(name, " default max-height"),
              normalize-max-size(max-height),
              normalize-max-size(apply(expected-max-height, pane, args)));
end method check-default-pane-size;

define method check-pane-size 
    (pane, name,
     #rest args, 
     #key width: user-width, height: user-height, #all-keys)
 => ()
  invalidate-space-requirements(pane);
  let space = compose-space(pane,
                            width: user-width,
                            height: user-height);
  let (width, min-width, max-width, height, min-height, max-height)
    = space-requirement-components(pane, space);
  check(concatenate(name, " min-width <= width"), \<=,
        min-width, width);
  check(concatenate(name, " width <= max-width"), \<=,
        width, max-width);
  check-equal(concatenate(name, " accepts explicit width"),
              width,
              max(min-width, min(user-width, max-width)));
  check(concatenate(name, " min-height <= height"), \<=,
        min-height, height);
  check(concatenate(name, " height <= max-height"), \<=,
        height, max-height);
  check-equal(concatenate(name, " accepts explicit height"),
              height,
              max(min-height, min(user-height, max-height)));
  check-equal(concatenate(name, " requested width"),  
              width,
              apply(expected-width, pane, args));
  check-equal(concatenate(name, " requested height"),
              height,
              apply(expected-height, pane, args));
  check-equal(concatenate(name, " requested min-width"),
              min-width,
              apply(expected-min-width, pane, args));
  check-equal(concatenate(name, " requested min-height"),
              min-height,
              apply(expected-min-height, pane, args));
  check-equal(concatenate(name, " requested max-width"),
              normalize-max-size(max-width),
              normalize-max-size(apply(expected-max-width, pane, args)));
  check-equal(concatenate(name, " requested max-height"),
              normalize-max-size(max-height),
              normalize-max-size(apply(expected-max-height, pane, args)));
end method check-pane-size;

define generic expected-space-allocation
    (pane, #key, #all-keys)
 => (space-allocation :: false-or(<sequence>));

define method sheet-position-in-parent 
    (sheet :: <sheet>) => (x :: <integer>, y :: <integer>)
  let point = transform-region(sheet-transform(sheet),
                               make(<point>, x: 0, y: 0));
  values(floor(point-x(point)), floor(point-y(point)))
end method sheet-position-in-parent;

define method enforce-fully-within-parent? 
    (sheet :: <sheet>) => (enforce? :: <boolean>)
  #t
end method enforce-fully-within-parent?;

define method check-child-allocations (layout, name, #rest args) => ()
  let child-allocations = apply(expected-space-allocation, layout, args);
  let (layout-width, layout-height) = box-size(sheet-region(layout));
  if (child-allocations)
    for (child in sheet-children(layout),
         allocation in child-allocations,
         count from 1)
      let (x, y) = sheet-position-in-parent(child);
      let (width, height) = box-size(sheet-region(child));
      let child-name = format-to-string("%s child %d", name, count);
      if (enforce-fully-within-parent?(layout))
        check-true(concatenate(child-name, " fully within parent"),
                   x >= 0 & y >= 0
                   & (x + width)  <= layout-width
                   & (y + height) <= layout-height)
      end;
      check-equal(concatenate(child-name, " x"),
                  floor(x), allocation[0]);
      check-equal(concatenate(child-name, " y"),
                  floor(y), allocation[1]);
      check-equal(concatenate(child-name, " width"),
                  width, allocation[2]);
      check-equal(concatenate(child-name, " height"),
                  height, allocation[3]);
      check(concatenate(child-name, " width >= min-width"), \>=,
            width, expected-min-width(child));
      check(concatenate(child-name, " width <= max-width"), \<=,
            width, expected-max-width(child));
      check(concatenate(child-name, " height >= min-height"), \>=,
            height, expected-min-height(child));
      check(concatenate(child-name, " height <= max-height"), \<=,
            height, expected-max-height(child));
    end
  end
end method check-child-allocations;

define method check-space-allocation 
    (pane, name, #rest args, 
     #key width = *layout-default-width*, height = *layout-default-height*,
     #all-keys)
 => ()
  let children = sheet-children(pane);
  check-true(concatenate(name, " children's positions = (0,0)"),
             every?(method (child)
                      let (x, y) = box-position(sheet-region(child));
                      x = 0 & y = 0
                    end,
                    children));
  apply(check-child-allocations, pane, name, 
        width: width, height: height,
        args);
end method check-space-allocation;

define method expected-constrained-size
    (sheet, width :: <integer>, height :: <integer>, 
     #rest args,
     #key, #all-keys)
 => (constrained-width :: <integer>, constrained-height :: <integer>)
  let w = min(max(width, apply(expected-min-width, sheet, args)),
              apply(expected-max-width, sheet, args));
  let h = min(max(height, apply(expected-min-height, sheet, args)),
              apply(expected-max-height, sheet, args));
  values(w, h)
end method expected-constrained-size;

define method check-layout-pane-layout 
    (pane, name,
     #rest args,
     #key x = 0, y = 0, width, height, allocate-space? = #t,
     #all-keys) => ()
  let (w, h) = apply(expected-constrained-size, pane,
                     width  | *layout-default-width*,
                     height | *layout-default-height*,
                     args);
  apply(check-default-pane-size, pane, name, width: #f, height: #f, args);
  apply(check-pane-size, pane, name, width: w, height: h, args);
  if (allocate-space?)
    if (w = 0 | h = 0)
      allocate-space(pane, w, h)
    else
      set-sheet-edges(pane, x, y, w, h);
      let (new-width, new-height) = box-size(pane);
      check-equal(concatenate(name, " width"), new-width, w);
      check-equal(concatenate(name, " height"), new-height, h);
    end
  end;
  apply(check-space-allocation, pane, name, width: w, height: h, args)
end method check-layout-pane-layout;


/// wrapping-layout-pane tests

define method expected-named-size
    (pane :: <wrapping-layout-mixin>, name, #rest args, #key width, height)
 => (size :: <integer>)
  ignore(width, height);
  let child = sheet-child(pane);
  case
    child     => apply(expected-size-function(name), child, args);
    otherwise => apply(expected-default-size, name, args);
  end
end method expected-named-size;

define method expected-space-allocation
    (pane :: <wrapping-layout-mixin>, #key width, height, #all-keys)
 => (space-allocation :: false-or(<vector>))
  let child = sheet-child(pane);
  if (child)
    let space = compose-space(child, width: width, height: height);
    vector(vector(0, 0, 
                  width  | space-requirement-width(child, space),
                  height | space-requirement-height(child, space)))
  end
end method expected-space-allocation;


/// top-level-sheet tests
define method test-top-level-sheet-layout
    (name, child, #rest args, #key) => ()
  let t-l-s = make-test-pane(<top-level-sheet>, child: child);
  apply(check-layout-pane-layout, t-l-s, 
        concatenate(name, " ", gadget-class-name(<top-level-sheet>)),
        args)
end method test-top-level-sheet-layout;

define test top-level-sheet-layouts-test ()
  test-top-level-sheet-layout("empty", #f);
  test-top-level-sheet-layout("fixed", make-test-pane(<test-push-button-pane>));
  test-top-level-sheet-layout("non-fixed", make-test-pane(<test-list-box>));
end test top-level-sheet-layouts-test;


/// Fixed layout tests

define method test-fixed-layout
    (name, children :: <sequence>, #rest args, #key) => ()
  let fixed-layout = make-test-pane(<fixed-layout>, children: children);
  let name = concatenate(name, " ", gadget-class-name(<fixed-layout>));
  apply(check-layout-pane-layout,
        fixed-layout, name,
        args)
end method test-fixed-layout;

// A fixed's children don't have to be within the parent
define method enforce-fully-within-parent?
    (layout :: <fixed-layout>) => (enforce? :: <boolean>)
  #f
end method enforce-fully-within-parent?;

define method expected-space-allocation
    (pane :: <fixed-layout>, #key)
 => (space-allocation :: false-or(<sequence>))
  let children = sheet-children(pane);
  let space-allocations = make(<vector>, size: size(children));
  for (child in children,
       count from 0)
    let (x, y) = sheet-position-in-parent(child);
    let width = expected-width(child);
    let height = expected-height(child);
    space-allocations[count] := vector(x, y, width, height);
  end;
  space-allocations
end method expected-space-allocation;

define duim-layouts class-test <fixed-layout> ()
  test-fixed-layout("empty", #());
  test-fixed-layout("one child",
		    vector(make-test-pane(<test-list-box>, 
					  x: 100, y: 200,
					  width: 200, height: 300)));
  test-fixed-layout("two children",
		    vector(make-test-pane(<test-list-box>, 
					  x: 100, y: 200,
					  width: 200, height: 300),
			   make-test-pane(<test-list-box>, 
					  x: 200, y: 300,
					  width: 400, height: 500)));
  test-layout-child-resizing(<fixed-layout>);
  test-multiple-child-layout-manipulation(<fixed-layout>);
end class-test <fixed-layout>;



/// Pinboard layout tests

define method test-pinboard-layout
    (name, children :: <sequence>, #rest args, #key) => ()
  let pinboard-layout = make-test-pane(<pinboard-layout>, children: children);
  let name = concatenate(name, " ", gadget-class-name(<pinboard-layout>));
  apply(check-layout-pane-layout,
        pinboard-layout, name,
        args)
end method test-pinboard-layout;

// A pinboard's children don't have to be within the parent
define method enforce-fully-within-parent?
    (layout :: <pinboard-layout>) => (enforce? :: <boolean>)
  #f
end method enforce-fully-within-parent?;

define method expected-space-allocation
    (pane :: <pinboard-layout>, #key)
 => (space-allocation :: false-or(<sequence>))
  let children = sheet-children(pane);
  let space-allocations = make(<vector>, size: size(children));
  for (child in children,
       count from 0)
    let (x, y) = sheet-position-in-parent(child);
    let width = expected-width(child);
    let height = expected-height(child);
    space-allocations[count] := vector(x, y, width, height);
  end;
  space-allocations
end method expected-space-allocation;

define duim-layouts class-test <pinboard-layout> ()
  test-pinboard-layout("empty", #());
  test-pinboard-layout("one child",
                       vector(make-test-pane(<test-list-box>, 
					     x: 100, y: 200,
					     width: 200, height: 300)));
  test-pinboard-layout("two children",
                       vector(make-test-pane(<test-list-box>, 
					     x: 100, y: 200,
					     width: 200, height: 300),
                              make-test-pane(<test-list-box>, 
					     x: 200, y: 300,
					     width: 400, height: 500)));
  test-layout-child-resizing(<pinboard-layout>);
  test-multiple-child-layout-manipulation(<pinboard-layout>);
end class-test <pinboard-layout>;


/// Stack layout tests

define method test-stack-layout
    (name, children :: <sequence>, #rest args, #key) => ()
  let stack-layout = make-test-pane(<stack-layout>, children: children);
  let name = concatenate(name, " ", gadget-class-name(<stack-layout>));
  apply(check-layout-pane-layout,
        stack-layout, name,
        args)
end method test-stack-layout;

define method expected-named-size
    (pane :: <stack-layout>, name, #key width, height)
 => (size :: <integer>)
  let size-function = expected-size-function(name);
  let size = 0;
  for (child in sheet-children(pane))
    size := max(size, size-function(child))
  end;
  size + (layout-border(pane) * 2)
end method expected-named-size;

define method expected-space-allocation
    (pane :: <stack-layout>, #key width, height, #all-keys)
 => (space-allocation :: false-or(<sequence>))
  let children = sheet-children(pane);
  let border = layout-border(pane);
  let border*2 = border * 2;
  let space-allocations = make(<vector>, size: size(children));
  let available-width  = width  & (width  - border*2);
  let available-height = height & (height - border*2);
  for (child in children,
       count from 0)
    let width  = expected-width (child, width:  available-width);
    let height = expected-height(child, height: available-height);
    space-allocations[count] := vector(border, border, width, height);
  end;
  space-allocations
end method expected-space-allocation;

define duim-layouts class-test <stack-layout> ()
  test-stack-layout("empty", #());
  test-stack-layout("one child",
		    vector(make-test-pane(<test-list-box>, 
					  x: 100, y: 200,
					  width: 200, height: 300)));
  test-stack-layout("two children",
		    vector(make-test-pane(<test-list-box>, 
					  x: 100, y: 200,
					  width: 200, height: 300),
			   make-test-pane(<test-list-box>, 
					  x: 200, y: 300,
					  width: 400, height: 500)));
  test-layout-child-resizing(<stack-layout>);
  test-multiple-child-layout-manipulation(<stack-layout>);
end class-test <stack-layout>;


/// Table layout tests

// we make this subclass so that we don't lose the contents field
define class <test-table-layout-pane> (<table-layout>)
  constant slot contents :: <sequence> = #(), 
    init-keyword: contents:;
end class <test-table-layout-pane>;

define method class-for-make-pane 
    (framem :: <test-frame-manager>, class == <table-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-table-layout-pane>, #f)
end method class-for-make-pane;

define method table-number-of-rows 
    (table :: <test-table-layout-pane>) => (rows :: <integer>)
  size(contents(table))
end method table-number-of-rows;

define method table-number-of-columns 
    (table :: <test-table-layout-pane>) => (columns :: <integer>)
  let contents = contents(table);
  case
    empty?(contents) => 0;
    otherwise        => size(contents[0]);
  end
end method table-number-of-columns;

define method expected-width
    (table :: <test-table-layout-pane>, #rest args, #key width)
 => (width :: <integer>)
  case
    width     => apply(expected-width-given-default, table, args);
    otherwise => next-method();
  end
end method expected-width;

define method expected-height
    (table :: <test-table-layout-pane>, #rest args, #key height)
 => (height :: <integer>)
  case
    height    => apply(expected-height-given-default, table, args);
    otherwise => next-method();
  end
end method expected-height;

define method expected-named-width 
    (table :: <test-table-layout-pane>, name,
     #rest args, #key width, height, x-spacing = 0)
 => (width :: <integer>)
  let size-function = expected-size-function(name);
  let no-of-columns = table-number-of-columns(table);
  let width = 0;
  if (no-of-columns > 0)
    for (column-index from 0 below no-of-columns)
      let subwidth = 0;
      for (column in contents(table))
	if (column-index < size(column))
	  let child = column[column-index];
	  if (child)
	    subwidth := max(subwidth, size-function(child))
	  end
	end
      end;
      width := width + subwidth
    end;
    width + x-spacing * (no-of-columns - 1)
  else
    apply(expected-default-size, name, args)
  end
end method expected-named-width;

define method expected-named-height 
    (table :: <test-table-layout-pane>, name, 
     #key y-spacing = 0, width, height)
 => (height :: <integer>)
  let size-function = expected-size-function(name);
  let no-of-rows = table-number-of-rows(table);
  let height = 0;
  let rows = contents(table);
  if (~empty?(rows))
    for (row in rows)
      let subheight = 0;
      for (child in row)
	if (child)
	  subheight := max(subheight, size-function(child))
	end
      end;
      height := height + subheight
    end;
    height + y-spacing * (no-of-rows - 1)
  else
    expected-default-size(name)
  end
end method expected-named-height;

define method expected-fixed-column? 
    (table :: <test-table-layout-pane>, column-index)
 => (fixed-column? :: <boolean>)
  every?(method (row)
           let child = column-index >= size(row) & row[column-index];
           if (child)
             expected-fixed-width?(child)
           end
         end,
         contents(table))
end method expected-fixed-column?;

define method expected-fixed-row? 
    (table :: <test-table-layout-pane>, row-index)
 => (fixed-row? :: <boolean>)
  every?(method (child)
           if (child)
             expected-fixed-width?(child)
           end
         end,
         contents(table)[row-index])
end method expected-fixed-row?;

define method expected-column-named-width 
    (table :: <test-table-layout-pane>, name, column-index)
 => (width :: <integer>)
  let size-function = expected-size-function(name);
  let width = 0;
  do(method (row)
       if (column-index < size(row))
         let child = row[column-index];
         if (child)
           max!(width, size-function(child))
         end
       end
     end,
     contents(table));
  width
end method expected-column-named-width;

define method expected-row-named-height
    (table :: <test-table-layout-pane>, name, row-index)
 => (height :: <integer>)
  let size-function = expected-size-function(name);
  let height = 0;
  do(method (child)
       if (child)
         max!(height, size-function(child))
       end
     end,
     contents(table)[row-index]);
  height
end method expected-row-named-height;

define method layout-nth-ratio
    (n :: <integer>, ratios :: false-or(<sequence>)) => (ratio :: <integer>)
  (ratios & n < size(ratios) & ratios[n])
    | 1
end method layout-nth-ratio;

define method layout-fixed-size
    (box :: <layout-pane>, no-of-items :: <integer>,
     size-function :: <function>, fixed-size?-function :: <function>)
 => (fixed-size :: <integer>)
  let size = 0;
  for (count from 0 below no-of-items)
    if (fixed-size?-function(count))
      inc!(size, size-function(count))
    end
  end;
  size
end method layout-fixed-size;

define method layout-ratio-unit 
    (box :: <layout-pane>, no-of-items :: <integer>, total-size :: <integer>,
     size-function :: <function>, fixed-size?-function :: <function>,
     ratios :: false-or(<sequence>))
 => (unit :: <integer>)
  let fixed-size
    = layout-fixed-size(box, no-of-items, size-function, fixed-size?-function);
  let denominator = 0;
  for (count from 0 below no-of-items)
    unless (fixed-size?-function(count))
      inc!(denominator, layout-nth-ratio(count, ratios))
    end
  end;
  if (denominator > 0)
    floor/(total-size - fixed-size, denominator)
  else
    0
  end
end method layout-ratio-unit;

define method expected-space-allocation
    (table :: <test-table-layout-pane>, 
     #key width, height,
          x-spacing = 0, y-spacing = 0,
          x-ratios, y-ratios,
          x-alignment = #"left", y-alignment = #"top")
 => (space-allocation :: false-or(<sequence>))
  let allocation = make(<stretchy-vector>);
  let contents = contents(table);
  let total-x-spacing = x-spacing * (table-number-of-columns(table) - 1);
  let total-y-spacing = y-spacing * (table-number-of-rows(table)    - 1);
  let x-ratio-unit
    = layout-ratio-unit(table, table-number-of-columns(table),
                        width - total-x-spacing,
                        curry(expected-column-named-width, table, #"width"),
                        curry(expected-fixed-column?, table),
                        x-ratios);
  let y-ratio-unit
    = layout-ratio-unit(table, table-number-of-rows(table),
                        height - total-y-spacing,
                        curry(expected-row-named-height, table, #"height"),
                        curry(expected-fixed-row?, table),
                        y-ratios);
  let y = 0;
  for (row in contents,
       row-index from 0)
    let x = 0;
    let y-ratio = layout-nth-ratio(row-index, y-ratios);
    let row-height = if (expected-fixed-row?(table, row-index))
                       expected-row-named-height(table, #"height", row-index)
                     else
                       fix-coordinate(y-ratio-unit * y-ratio)
                     end;
    for (child in row,
         column-index from 0)
      let x-ratio = layout-nth-ratio(column-index, x-ratios);
      let column-width = if (expected-fixed-column?(table, column-index))
			   expected-column-named-width(table, #"width",
						       column-index)
			 else
			   fix-coordinate(x-ratio-unit * x-ratio)
			 end;
      when (child)
	let width = case
		      expected-fixed-width?(child) => expected-width(child);
		      otherwise => column-width;
		    end;
	let height = case
		       expected-fixed-height?(child) => expected-height(child);
		       otherwise => row-height;
		     end;
        let this-x-alignment
          = if (instance?(x-alignment, <sequence>))
              x-alignment[column-index]
            else
              x-alignment
            end;
        let this-y-alignment
          = if (instance?(y-alignment, <sequence>))
              y-alignment[row-index]
            else
              y-alignment
            end;
	let x-adjust = select (this-x-alignment)
			 #"left"  => 0;
			 #"right" => column-width - width;
			 #"centre", #"center" => floor/(column-width - width, 2);
		       end;
	let y-adjust = select (this-y-alignment)
			 #"top" => 0;
			 #"bottom" => row-height - height;
			 #"centre", #"center" => floor/(row-height - height, 2);
		       end;
	add!(allocation, vector(x + x-adjust, y + y-adjust, width, height));
      end;
      inc!(x, column-width + x-spacing)
    end;
    inc!(y, row-height + y-spacing)
  end;
  allocation
end method expected-space-allocation;

define method make-table-layout-pane 
    (children :: <sequence>, no-of-columns :: <integer>, #rest args, #key, #all-keys)
 => (sheet :: <table-layout>)
  let no-of-children = size(children);
  let no-of-rows = ceiling/(no-of-children, no-of-columns);
  let contents = make(<vector>, size: no-of-rows);
  for (i from 0 below no-of-rows)
    let start = i * no-of-columns;
    let end-index = min(start + no-of-columns, no-of-children);
    contents[i] := copy-sequence(children, start: start, end: end-index)
  end;
  apply(make-test-pane, <test-table-layout-pane>, contents: contents, args)
end method make-table-layout-pane;

define method make-default-table-layout-pane 
    (#rest args,
     #key gadget-class = <test-push-button-pane>,
          x-spacing = 0,
          y-spacing = 0)
 => (sheet :: <table-layout>)
  apply(make-table-layout-pane,
	vector(make-test-pane(gadget-class),
	       make-test-pane(gadget-class),
	       make-test-pane(gadget-class),
	       make-test-pane(gadget-class),
	       make-test-pane(gadget-class)),
        2,
	x-spacing: x-spacing, y-spacing: y-spacing,
        args)
end method make-default-table-layout-pane;

define method test-default-table-layout-pane-layout
    (gadget-class :: <class>, name :: <string>, #rest args, #key, #all-keys) => ()
  let gadget-name = gadget-class-name(<table-layout>);
  let table
    = apply(make-default-table-layout-pane,
	    gadget-class: gadget-class,
	    args);
  apply(check-layout-pane-layout, table,
        concatenate(name, " ", gadget-name),
        args)
end method test-default-table-layout-pane-layout;

define method test-table-layout-pane-layout 
    (children :: <sequence>, name, #rest args, #key, #all-keys) => ()
  let gadget-name = gadget-class-name(<table-layout>);
  let table = apply(make-table-layout-pane, children, 2, args);
  apply(check-layout-pane-layout, table, 
        concatenate(name, " ", gadget-name),
        args)
end method test-table-layout-pane-layout;

define method test-mixed-table-layout-pane-layout 
    (name, #rest args, #key, #all-keys) => ()
  let children = vector(make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-list-box>),
                        make-test-pane(<test-list-box>),
                        make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-list-box>),
                        make-test-pane(<test-push-button-pane>));
  apply(test-table-layout-pane-layout, children, name, args);
end method test-mixed-table-layout-pane-layout;

define duim-layouts class-test <table-layout> ()
  let x-spacing = 10;
  let y-spacing = 20;
  test-table-layout-pane-layout(#(), "empty");
  test-default-table-layout-pane-layout
    (<test-push-button-pane>, "fixed",
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-default-table-layout-pane-layout
    (<test-list-box>, "non-fixed",
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-default-table-layout-pane-layout
    (<test-list-box>, "specified ratios",
     x-spacing: x-spacing, y-spacing: y-spacing,
     x-ratios: #(1, 2), y-ratios: #(1, 3));

  test-mixed-table-layout-pane-layout
    ("fixed and non-fixed",
     x-spacing: x-spacing, y-spacing: y-spacing);

  for (alignment in #(#"left", #"center", #"right", #(#"right", #"left")))
    test-mixed-table-layout-pane-layout
      (format-to-string("x %s aligned", alignment),
       x-alignment: alignment);
  end;

  for (alignment in #(#"top", #"center", #"bottom", #(#"bottom", #"top", #"bottom")))
    test-mixed-table-layout-pane-layout
      (format-to-string("y %s aligned", alignment),
       y-alignment: alignment);
  end;

  let children
    = vector(make-default-table-layout-pane(),
	     make-test-pane(<test-list-box>),
	     make-test-pane(<test-list-box>),
	     make-default-table-layout-pane(),
	     make-test-pane(<test-list-box>),
	     make-default-table-layout-pane());
  test-table-layout-pane-layout
    (children, "non-fixed recursive",
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-table-layout-pane-layout
    (vector(#f, #f, #f), "only gaps",
     x-spacing: x-spacing, y-spacing: y-spacing);

  let children
    = vector(#f,
	     make-test-pane(<test-push-button-pane>),
	     make-test-pane(<test-push-button-pane>),
	     #f);
  test-table-layout-pane-layout
    (children, "fixed with gaps",
     x-spacing: x-spacing, y-spacing: y-spacing);

  let children
    = vector(#f,
	     make-test-pane(<test-list-box>),
	     make-test-pane(<test-list-box>),
	     #f);
  test-table-layout-pane-layout
    (children, "non-fixed with gaps",
     x-spacing: x-spacing, y-spacing: y-spacing);
  test-layout-child-resizing(<table-layout>, rows: 1);
  test-multiple-child-layout-manipulation(<table-layout>, columns: 2);
end class-test <table-layout>;


/// Grid layout tests

// we make this subclass so that we don't lose the contents field
define class <test-grid-layout-pane> (<grid-layout>, <test-table-layout-pane>)
end class <test-grid-layout-pane>;

define method class-for-make-pane
    (framem :: <test-frame-manager>, class == <grid-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<test-grid-layout-pane>, #f)
end method class-for-make-pane;

define duim-layouts class-test <grid-layout> ()
  let x-spacing = 10;
  let y-spacing = 20;
  let space-req = make(<space-requirement>, width: 100, height: 100);

/*---*** This needs to make a <grid-layout> somehow!
  test-table-layout-pane-layout(#(), "empty");
  test-default-table-layout-pane-layout
    (<test-push-button-pane>, "fixed",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-default-table-layout-pane-layout
    (<test-list-box>, "non-fixed",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-default-table-layout-pane-layout
    (<test-list-box>, "specified ratios",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing,
     x-ratios: #(1, 2), y-ratios: #(1, 3));

  test-mixed-table-layout-pane-layout
    ("fixed and non-fixed",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  for (alignment in #(#"left", #"center", #"right", #(#"right", #"left")))
    test-mixed-table-layout-pane-layout
      (format-to-string("x %s aligned", alignment),
       cell-space-requirement: space-req,
       x-alignment: alignment);
  end;

  for (alignment in #(#"top", #"center", #"bottom", #(#"bottom", #"top", #"bottom")))
    test-mixed-table-layout-pane-layout
      (format-to-string("y %s aligned", alignment),
       cell-space-requirement: space-req,
       y-alignment: alignment);
  end;

  let children
    = vector(make-default-table-layout-pane(),
	     make-test-pane(<test-list-box>),
	     make-test-pane(<test-list-box>),
	     make-default-table-layout-pane(),
	     make-test-pane(<test-list-box>),
	     make-default-table-layout-pane());
  test-table-layout-pane-layout
    (children, "non-fixed recursive",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  test-table-layout-pane-layout
    (vector(#f, #f, #f), "only gaps",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  let children
    = vector(#f,
	     make-test-pane(<test-push-button-pane>),
	     make-test-pane(<test-push-button-pane>),
	     #f);
  test-table-layout-pane-layout
    (children, "fixed with gaps",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);

  let children
    = vector(#f,
	     make-test-pane(<test-list-box>),
	     make-test-pane(<test-list-box>),
	     #f);
  test-table-layout-pane-layout
    (children, "non-fixed with gaps",
     cell-space-requirement: space-req,
     x-spacing: x-spacing, y-spacing: y-spacing);
  test-layout-child-resizing
     (<grid-layout>, 
      cell-space-requirement: space-req,
      rows: 1);
  test-multiple-child-layout-manipulation
    (<grid-layout>,
      cell-space-requirement: space-req,
     columns: 2);
*/
end class-test <grid-layout>;


/// Box pane tests

define method box-compute-major-size 
    (box :: <box-layout-pane>, space-function :: <function>, 
     #key spacing = 0, #all-keys)
 => (major-size :: <integer>)
  let major-size = 0;
  let children = sheet-children(box);
  for (child in children)
    major-size := major-size + space-function(child)
  end;
  major-size + spacing * (size(children) - 1);
end method box-compute-major-size;

define method box-compute-minor-size
    (box :: <box-layout-pane>, space-function :: <function>)
 => (minor-size :: <integer>)
  let size = 0;
  for (child in sheet-children(box))
    size := max(size, space-function(child))
  end;
  size
end method box-compute-minor-size;

define generic box-compute-width 
    (box :: <layout>, space-function :: <function>, #key, #all-keys)
 => (width :: <integer>);

define generic box-compute-height
    (box :: <layout>, space-function :: <function>, #key, #all-keys)
 => (height :: <integer>);

define method box-compute-width 
    (box :: <row-layout>, space-function :: <function>, #rest args, #key)
 => (width :: <integer>)
  apply(box-compute-major-size, box, space-function, args)
end method box-compute-width;

define method box-compute-height
    (box :: <row-layout>, space-function :: <function>, #rest args, #key)
 => (height :: <integer>)
  box-compute-minor-size(box, space-function)
end method box-compute-height;

define method box-compute-width 
    (box :: <column-layout>, space-function :: <function>, #rest args, #key)
 => (width :: <integer>)
  box-compute-minor-size(box, space-function)
end method box-compute-width;

define method box-compute-height
    (box :: <column-layout>, space-function :: <function>, #rest args, #key)
 => (height :: <integer>)
  apply(box-compute-major-size, box, space-function, args)
end method box-compute-height;

define method expected-width
    (table :: <box-layout-pane>, #rest args, #key width)
 => (width :: <integer>)
  case
    width     => apply(expected-width-given-default, table, args);
    otherwise => next-method();
  end
end method expected-width;

define method expected-height
    (table :: <box-layout-pane>, #rest args, #key height)
 => (height :: <integer>)
  case
    height    => apply(expected-height-given-default, table, args);
    otherwise => next-method();
  end
end method expected-height;

define method expected-named-width
    (box :: <box-layout-pane>, name, #rest args, #key width, height)
 => (width :: <integer>)
  if (~empty?(sheet-children(box)))
    apply(box-compute-width, box, expected-size-function(name), args)
  else
    apply(expected-default-size, name, args)
  end
end method expected-named-width;

define method expected-named-height 
    (box :: <box-layout-pane>, name, #rest args, #key width, height)
 => (height :: <integer>)
  if (~empty?(sheet-children(box)))
    apply(box-compute-height, box, expected-size-function(name), args)
  else
    apply(expected-default-size, name, args)
  end
end method expected-named-height;

define method expected-space-allocation
    (box :: <row-layout>,
     #key spacing = 0,
          y-alignment = #"top",
          x-ratios,
          ratios = x-ratios,
          width, height)
 => (space-allocation :: false-or(<sequence>))
  let children = sheet-children(box);
  let number-of-children = size(children);
  let total-spacing = spacing * (number-of-children - 1);
  let non-fixed-width-unit
    = layout-ratio-unit(box, number-of-children, width - total-spacing,
                        method (count)
                          expected-width(children[count])
                        end,
                        method (count)
                          expected-fixed-width?(children[count])
                        end,
                        ratios);
  let x = 0;
  let child-allocations = make(<stretchy-vector>);
  for (child in children,
       count from 0)
    let ratio = layout-nth-ratio(count, ratios);
    let child-width 
      = if (expected-fixed-width?(child))
          expected-width(child)
        else
          expected-constrained-width
            (child, fix-coordinate(non-fixed-width-unit * ratio))
        end;
    let child-height = expected-constrained-height(child, height);
    let y = select (y-alignment)
              #"top" => 0;
              #"bottom" => height - child-height;
              #"centre", #"center" => floor/(height - child-height, 2);
            end;
    add!(child-allocations, vector(x, y, child-width, child-height));
    inc!(x, child-width + spacing)
  end;
  child-allocations
end method expected-space-allocation;

define method expected-space-allocation
    (box :: <column-layout>, 
     #key spacing = 0,
          x-alignment = #"left",
          y-ratios,
          ratios = y-ratios,
          width, height)
 => (space-allocation :: false-or(<sequence>))
  let children = sheet-children(box);
  let number-of-children = size(children);
  let total-spacing = spacing * (number-of-children - 1);
  let non-fixed-height-unit
    = layout-ratio-unit(box, number-of-children, height - total-spacing,
                        method (count)
                          expected-height(children[count])
                        end,
                        method (count)
                          expected-fixed-height?(children[count])
                        end,
                        ratios);
  let y = 0;
  let child-allocations = make(<stretchy-vector>);
  for (child in children,
       count from 0)
    let ratio = layout-nth-ratio(count, ratios);
    let child-height
      = if (expected-fixed-height?(child))
          expected-height(child)
        else
          expected-constrained-height
            (child, fix-coordinate(non-fixed-height-unit * ratio))
        end;
    let child-width = expected-constrained-width(child, width);
    let x = select (x-alignment)
              #"left"              => 0;
              #"right"             => width - child-width;
              #"center", #"centre" => floor/(width - child-width, 2);
            end;
    add!(child-allocations, vector(x, y, child-width, child-height));
    inc!(y, child-height + spacing)
  end;
  child-allocations
end method expected-space-allocation;

define method test-three-child-layout 
    (class :: <class>, child-class :: <class>,
     #rest args,
     #key name,
          spacing = 10,
          first-child-class = child-class,
     #all-keys) => ()
  let gadget-name = gadget-class-name(class);
  let test-name = if (name)
                    concatenate(name, " ", gadget-name)
                  else
                    gadget-name
                  end;

  let children = vector(make-test-pane(first-child-class),
                        make-test-pane(child-class),
                        make-test-pane(child-class));
  let layout = apply(make-test-pane, class, children: children, spacing: spacing, args);
  apply(check-layout-pane-layout, layout, test-name, spacing: spacing, args);

  let children = vector(make-test-pane(first-child-class, 
                             width:  *child-explicit-width*,
                             height: *child-explicit-height*),
                        make-test-pane(child-class),
                        make-test-pane(child-class));
  let layout = apply(make-test-pane, class, children: children, spacing: spacing, args);
  apply(check-layout-pane-layout, layout, 
        concatenate("explicit child size ", test-name),
        spacing: spacing,
        args);
end method test-three-child-layout;

define method test-box-pane-layout 
    (class :: <class>, #key spacing = 10) => ()
  check-layout-pane-layout(make-test-pane(class),
                           concatenate("empty ", gadget-class-name(class)));
  test-three-child-layout(class, <test-push-button-pane>,
                          spacing: spacing);
  test-three-child-layout(class, <test-list-box>,
                          name: "non-fixed",
                          spacing: spacing);
  test-three-child-layout(class, <test-list-box>,
                          name: "specified ratios",
                          spacing: spacing,
                          ratios: #(1, 2, 3));
  test-three-child-layout(class, <test-list-box>,
                          first-child-class: <test-push-button-pane>,
                          name: "fixed and non-fixed",
                          spacing: spacing);
  test-three-child-layout(class, <test-list-box>,
                          first-child-class: <test-push-button-pane>,
                          name: "centered",
                          x-alignment: #"center",
                          y-alignment: #"center");
  test-three-child-layout(class, <test-list-box>,
                          first-child-class: <test-push-button-pane>,
                          name: "right-aligned",
                          x-alignment: #"right",
                          y-alignment: #"bottom");
end method test-box-pane-layout;

define method test-nested-layouts
    (outer-class :: <class>, inner-class :: <class>, #key spacing = 10) => ()
  let buttons1 = vector(make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-push-button-pane>));
  let buttons2 = vector(make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-push-button-pane>),
                        make-test-pane(<test-push-button-pane>));
  let column1 = make-test-pane(inner-class, children: buttons1);
  let column2 = make-test-pane(inner-class, children: buttons2);
  let pane = make-test-pane(outer-class, children: vector(column1, column2),
                  spacing: spacing);
  let name = gadget-class-name(outer-class);
  check-layout-pane-layout(pane, concatenate("nested ", name),
                           spacing: spacing);
end method test-nested-layouts;

define duim-layouts class-test <column-layout> ()
  test-box-pane-layout(<column-layout>);
  test-nested-layouts(<column-layout>, <row-layout>);
  test-multiple-child-layout-manipulation(<column-layout>);
  test-layout-child-resizing(<column-layout>);
end class-test <column-layout>;

define duim-layouts class-test <row-layout> ()
  test-box-pane-layout(<row-layout>);
  test-nested-layouts(<row-layout>, <column-layout>);
  test-multiple-child-layout-manipulation(<row-layout>);
  test-layout-child-resizing(<row-layout>);
end class-test <row-layout>;


/// Button box layout tests

define method test-button-box 
    (class :: <class>, #key items = #("red", "green", "blue")) => ()
  let name = gadget-class-name(class);
  let box = make-test-pane(class, items: items, spacing: 0);
  let layout = sheet-child(box);
  check-layout-pane-layout(layout, name)
end test-button-box;

define test button-box-layouts-test ()
  test-button-box(<push-box>);
  test-button-box(<radio-box>);
  test-button-box(<check-box>);
end test button-box-layouts-test;


/// Layout manipulation tests

define method test-single-child-layout-manipulation (class :: <class>) => ()
  let class-name = gadget-class-name(class);
  let button1 = make-test-pane(<test-push-button-pane>);
  let layout = make-test-pane(class, child: button1);
  check-equal(concatenate(class-name, " child's initial parent"),
              sheet-parent(button1), layout);
  let button2 = make-test-pane(<test-radio-button-pane>);
  replace-child(layout, button1, button2);
  check-equal(concatenate(class-name, " replace-child old child unparented"),
              sheet-parent(button1), #f);
  check-equal(concatenate(class-name, " replace-child new child's parent"),
              sheet-parent(button2), layout);
  let button3 = make-test-pane(<test-check-button-pane>);
  sheet-child(layout) := button3;
  let button4 = make-test-pane(<test-push-button-pane>);
  remove-child(layout, button3);
  add-child(layout, button4);
  check-equal(concatenate(class-name, " removed child unparented"),
              sheet-parent(button3), #f);
  check-equal(concatenate(class-name, " added child parent"),
              sheet-parent(button4), layout);
end method test-single-child-layout-manipulation;

define method test-multiple-child-layout-manipulation
    (class :: <class>, #rest class-args) => ()
  let class-name = gadget-class-name(class);
  let button1 = make-test-pane(<test-push-button-pane>);
  let layout = apply(make-test-pane, class, children: vector(button1), class-args);
  check-equal(concatenate(class-name, " child's initial parent"),
              sheet-parent(button1), layout);
  let button2 = make-test-pane(<test-radio-button-pane>);
  replace-child(layout, button1, button2);
  check-equal(concatenate(class-name, " replace-child old child unparented"),
              sheet-parent(button1), #f);
  check-equal(concatenate(class-name, " replace-child new child's parent"),
              sheet-parent(button2), layout);
  let button3 = make-test-pane(<test-check-button-pane>);
  sheet-children(layout) := vector(button3);
  let button4 = make-test-pane(<test-push-button-pane>);
  add-child(layout, button4);
  check-equal(concatenate(class-name, " add-child old child still parented"),
              sheet-parent(button3), layout);
  check-equal(concatenate(class-name, " add-child new child's parent"),
              sheet-parent(button4), layout);
  check-equal(concatenate(class-name, " add-child updates parent's children"),
              sheet-children(layout), vector(button3, button4));
  remove-child(layout, button3);
  check-equal(concatenate(class-name, " removed child unparented"),
              sheet-parent(button3), #f);
end method test-multiple-child-layout-manipulation;

define test layout-manipulation-test ()
  test-single-child-layout-manipulation(<spacing-pane>);
  test-single-child-layout-manipulation(<border-pane>);
  test-single-child-layout-manipulation(<test-viewport>);
end test layout-manipulation-test;


/// Layout resizing

define method test-layout-child-resizing 
    (class :: <class>, #rest args) => ()
  let gadget-name = gadget-class-name(class);
  let child-1 = make-test-pane(<push-button>, label: "Child 1");
  let sub-layout = make-test-pane(<column-layout>, children: vector(child-1));
  let layout 
    = apply(make-test-pane, class,
	    child: sub-layout, contents: vector(vector(sub-layout)),
	    args);
  check-layout-pane-layout(layout,
			   concatenate("Before resizing ", gadget-name));
  let child-2 = make-test-pane(<push-button>, label: "Child 2");
  sheet-children(sub-layout) := vector(child-1, child-2);
  relayout-parent(sub-layout);
  check-layout-pane-layout(layout,
			   concatenate("After resizing ", gadget-name));
end method test-layout-child-resizing;

define test layout-resizing-test ()
  test-layout-child-resizing(<spacing-pane>);
  test-layout-child-resizing(<border-pane>);
  test-layout-child-resizing(<viewport>);
end test layout-resizing-test;


/// Define the layout test suite

define suite duim-layouts-suite ()
  test button-box-layouts-test;
  test top-level-sheet-layouts-test;
  test layout-manipulation-test;
  test layout-resizing-test;
end suite duim-layouts-suite;
