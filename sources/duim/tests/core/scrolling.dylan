Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Viewport tests

// Viewports don't need to keep their children within their bounds
define method enforce-fully-within-parent?
    (pane :: <viewport>) => (enforce? :: <boolean>)
  #f
end method enforce-fully-within-parent?;

define method horizontal-scroll-only? 
    (pane :: <test-viewport>) => (horizontal-only? :: <boolean>)
  let horizontal? = horizontal-scroll-bar(pane);
  let vertical?   = vertical-scroll-bar(pane);
  horizontal? & ~vertical?
end method horizontal-scroll-only?;

define method vertical-scroll-only? 
    (pane :: <test-viewport>) => (horizontal-only? :: <boolean>)
  let horizontal? = horizontal-scroll-bar(pane);
  let vertical?   = vertical-scroll-bar(pane);
  vertical? & ~horizontal?
end method vertical-scroll-only?;

define method expected-width 
    (pane :: <viewport>, #key width) => (width :: <integer>)
  let child = sheet-child(pane);
  if (child)
    if (vertical-scroll-only?(pane))
      if (width)
        expected-constrained-width(child, width)
      else
        expected-width(child)
      end
    else
      width | expected-width(child)
    end
  else
    expected-default-size(#"width", width: width)
  end
end method expected-width;

define method expected-height
    (pane :: <viewport>, #key height) => (height :: <integer>)
  let child = sheet-child(pane);
  if (child)
    if (horizontal-scroll-only?(pane))
      if (height)
        expected-constrained-height(pane, height)
      else
        expected-height(child)
      end
    else
      height | expected-height(child)
    end
  else
    expected-default-size(#"height", height: height)
  end
end method expected-height;

define method expected-min-width 
    (pane :: <viewport>, #key) => (min-width :: <integer>)
  let child = sheet-child(pane);
  if (child & vertical-scroll-only?(pane))
    expected-min-width(child)
  else
    expected-default-size(#"min-width")
  end
end method expected-min-width;

define method expected-min-height 
    (pane :: <viewport>, #key) => (min-height :: <integer>)
  let child = sheet-child(pane);
  if (child & horizontal-scroll-only?(pane))
    expected-min-height(child)
  else
    expected-default-size(#"min-height")
  end
end method expected-min-height;

define method expected-max-width 
    (pane :: <viewport>, #key) => (max-width :: <integer>)
  if (vertical-scroll-only?(pane))
    next-method()
  else
    $fill
  end
end method expected-max-width;

define method expected-max-height 
    (pane :: <viewport>, #key) => (max-height :: <integer>)
  if (horizontal-scroll-only?(pane))
    next-method()
  else
    $fill
  end
end method expected-max-height;

define method expected-space-allocation 
    (pane :: <viewport>, #key width, height)
 => (space :: <sequence>)
  let child = sheet-child(pane);
  if (child)
    //---*** This should really take account of stretchable children
    //---*** which grow to fill the viewport
    let child-width  = expected-width(child);
    let child-height = expected-height(child);
    vector(vector(0, 0, child-width, child-height))
  else
    #()
  end
end method expected-space-allocation;


define method test-viewport-layout 
    (name, class, horizontal?, vertical?,
     #rest args,
     #key width, height, #all-keys)
  let test-name = concatenate(name, " ", gadget-class-name(<viewport>));
  let child = class & apply(make-test-pane, class, args);
  let viewport 
    = make-test-pane(<viewport>, 
		     horizontal-scroll-bar: if (horizontal?) make-test-pane(<scroll-bar>) end, 
		     vertical-scroll-bar:   if (vertical?)   make-test-pane(<scroll-bar>) end,
		     child: child);
  check-layout-pane-layout(viewport, test-name);
  let (new-width, new-height)
    = expected-constrained-size(viewport, 500, 500);
  set-sheet-size(viewport, new-width, new-height);
  check-layout-pane-layout(viewport, concatenate("resized ", test-name));
  viewport
end method test-viewport-layout;

define method test-viewport-layouts (name, class, #rest args)
  apply(test-viewport-layout, concatenate("no-bar ", name), 
        class, #f, #f, args);
  apply(test-viewport-layout, concatenate("vertical ", name),
        class, #f, #t, args);
  apply(test-viewport-layout, concatenate("horizontal ", name),
        class, #t, #f, args);
  apply(test-viewport-layout, concatenate("two-bar ", name), 
        class, #t, #t, args);
end method test-viewport-layouts;

define duim-gadgets class-test <viewport> ()
  test-viewport-layouts("empty", #f);
  test-viewport-layouts("fixed", <push-button>);
  test-viewport-layouts("non-fixed", <list-box>);
  test-viewport-layouts("large child", <list-box>, width: 600, height: 800);
end class-test <viewport>;


/// Scroller tests

define inline function border-thickness*2
    (border :: <border>) => (thickness :: <integer>)
  //---*** Why does this not work
  // border-thickness(border) * 2
  4
end function border-thickness*2;

define method expected-width 
    (pane :: <scroller>, #key width) => (width :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-width;

define method expected-height
    (pane :: <scroller>, #key height) => (height :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-height;

define method expected-min-width 
    (pane :: <scroller>, #key) => (min-width :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-min-width;

define method expected-min-height 
    (pane :: <scroller>, #key) => (min-height :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-min-height;

define method expected-max-width 
    (pane :: <scroller>, #key) => (max-width :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-max-width;

define method expected-max-height 
    (pane :: <scroller>, #key) => (max-height :: <integer>)
  let child = sheet-child(pane);
  if (instance?(child, <border>))
    next-method() + border-thickness*2(child)
  else
    next-method()
  end
end method expected-max-height;

define method expected-space-allocation 
    (pane :: <scroller>, #key width, height)
 => (space :: <sequence>)
  next-method()
end method expected-space-allocation;


define method test-scroller 
    (name :: <string>, class :: false-or(<class>), 
     scroll-bars :: one-of(#"none", #"horizontal", #"vertical", #"both"),
     borders :: <boolean>) => ()
  let child = class & make-test-pane(class);
  let scroller
     = make-test-pane(<scroller>,
		      contents: child,
		      scroll-bars: scroll-bars,
		      border-type: borders & #"sunken");
  check-layout-pane-layout(scroller, name);
  let (new-width, new-height)
    = expected-constrained-size(scroller, 1000, 1000);
  set-sheet-size(scroller, new-width, new-height);
  check-layout-pane-layout(scroller, concatenate("resized ", name));
end method test-scroller;

define method test-scroller-layout 
    (name :: <string>, class :: false-or(<class>))
  let gadget-name = gadget-class-name(<scroller>);
  // Borders
  test-scroller(concatenate(name, " no-bar ", gadget-name), 
                class, #"none", #t);
  test-scroller(concatenate(name, " horizontal ", gadget-name), 
                class, #"horizontal", #t);
  test-scroller(concatenate(name, " vertical ", gadget-name), 
                class, #"vertical", #t);
  test-scroller(concatenate(name, " two-bar ", gadget-name), 
                class, #"both", #t);
  // No borders
  test-scroller(concatenate(name, " no-bar borderless ", gadget-name), 
                class, #"none", #f);
  test-scroller(concatenate(name, " horizontal borderless ", gadget-name), 
                class, #"horizontal", #f);
  test-scroller(concatenate(name, " vertical borderless ", gadget-name), 
                class, #"vertical", #f);
  test-scroller(concatenate(name, " two-bar borderless ", gadget-name), 
                class, #"both", #f);
end method test-scroller-layout;

define duim-gadgets class-test <scroller> ()
  test-scroller-layout("empty", #f);
  test-scroller-layout("fixed", <push-button>);
  test-scroller-layout("non-fixed", <list-box>);
end class-test <scroller>;
