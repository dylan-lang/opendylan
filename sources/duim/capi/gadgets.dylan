Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Abstract gadget handling

define open abstract class <capi-sheet-mixin> (<abstract-sheet>)
end class <capi-sheet-mixin>;

define open abstract class <capi-pane-mixin>
    (<capi-sheet-mixin>,
     <standard-input-mixin>,
     <mirrored-sheet-mixin>)
end class <capi-pane-mixin>;

define open abstract class <capi-gadget-mixin> (<capi-pane-mixin>)
end class <capi-gadget-mixin>;

define open abstract class <capi-collection-gadget-mixin> (<capi-gadget-mixin>)
end class <capi-collection-gadget-mixin>;


/// Value handling

define method capi-mirror-value (gadget :: <capi-mirror>)
  #f
end method capi-mirror-value;

define method capi-new-mirror-value (gadget :: <capi-mirror>)
  capi-mirror-value(gadget)
end method capi-new-mirror-value;

define method capi-sheet-enabled? (gadget :: <gadget>)
  gadget-enabled?(gadget)
end method capi-sheet-enabled?;

define method note-gadget-enabled (client, gadget :: <capi-gadget-mixin>)
  update-capi-gadget-enabled-state(gadget, #t);
  next-method()
end method note-gadget-enabled;

define method note-gadget-disabled
    (client, gadget :: <capi-gadget-mixin>) => ()
  update-capi-gadget-enabled-state(gadget, #f);
  next-method()
end method note-gadget-disabled;

define method update-capi-gadget-enabled-state 
    (gadget :: <capi-gadget-mixin>, state)
  let mirror = sheet-direct-mirror(gadget);
  when (instance?(mirror, <capi-simple-pane>))
    simple-pane-enabled(mirror) := state | #()
  end
end method update-capi-gadget-enabled-state;


/// callback handling

define method mirror-primary-callback (mirror)
  sheet-primary-callback(mirror-sheet(mirror))
end method mirror-primary-callback;

define method sheet-primary-callback (sheet :: <action-gadget-mixin>)
  distribute-activate-callback(sheet)
end method sheet-primary-callback;

define method sheet-primary-callback (gadget :: <value-gadget-mixin>)
  let value = capi-new-mirror-value(sheet-direct-mirror(gadget));
  distribute-value-changed-callback(gadget, value)
end method sheet-primary-callback;


// Return #t if the port generates damage events for each mirrored sheet,
// in effect, handling repaint itself (this is the X model).  Return #f
// if only one damage event comes in for the top level sheet, meaning that
// repainting of child sheets must be done manually (the Mac model).
define method port-handles-repaint?
    (_port :: <capi-port>, sheet :: <capi-sheet-mixin>)
  #f
end method port-handles-repaint?;

// CAPI "panes" are all of the gadget-y things.  They repaint themselves.
define method port-handles-repaint?
    (_port :: <capi-port>, sheet :: <capi-pane-mixin>)
  #t
end method port-handles-repaint?;

// In CAPI, we don't get a repaint event for this, so get DUIM to do it.
define method port-handles-repaint?
    (_port :: <capi-port>, sheet :: <drawing-pane>)
  #f
end method port-handles-repaint?;

define method default-foreground-setter
    (foreground :: <ink>, pane :: <capi-pane-mixin>)
 => (foreground :: <ink>)
  next-method();
  //--- change the foreground of the gadget
  foreground
end method default-foreground-setter;

define method default-background-setter
    (background :: <ink>, pane :: <capi-pane-mixin>)
 => (background :: <ink>)
  next-method();
  //--- change the background of the gadget
  background
end method default-background-setter;

define method capi-mirror-parent (sheet)
  sheet-mirror(sheet)
end method capi-mirror-parent;

define method capi-gadget-constraints (gadget :: <capi-gadget-mixin>)
  let mirror = sheet-direct-mirror(gadget);
  if (mirror)
    let pane = maybe-decoration-pane(mirror, #t);
    let (min-width, min-height, max-width, max-height) = get-constraints(pane);
    local method max-space (min, max)
	    select (max by instance?)
	      <number>  => max;
	      <list>    => $fill;
	      otherwise => min;
	    end
	  end method;
    values(min-width, min-height,
	   max-space(min-width, max-width), max-space(min-height, max-height))
  else
    error("Attempting to query an unmirrored CAPI gadget's constraints: %=", gadget);
    values(100, 100, 100, 100)
  end
end method capi-gadget-constraints;
  
define method do-compose-space
    (gadget :: <capi-gadget-mixin>, #key width, height)
 => (space-req :: <space-requirement>)
  let (mn-w, mn-h, mx-w, mx-h) = capi-gadget-constraints(gadget);
  let best-width  = if (width)  constrain-size(width,  mn-w, mx-w) else mn-w end;
  let best-height = if (height) constrain-size(height, mn-h, mx-h) else mn-h end;
  make(<space-requirement>,
       width:      best-width, height:     best-height,
       min-width:  mn-w,       min-height: mn-h,
       max-width:  mx-w,       max-height: mx-h)
end method do-compose-space;

define method note-capi-gadget-value-changed 
    (sheet :: <capi-sheet-mixin>, mirror, value) => ()
  #f
end method note-capi-gadget-value-changed;

define method note-gadget-value-changed
    (gadget :: <capi-gadget-mixin>) => ()
  next-method();
  note-capi-gadget-value-changed(gadget, sheet-direct-mirror(gadget), gadget-value(gadget))
end method note-gadget-value-changed;

define method capi-gadget-selection 
    (gadget :: <capi-gadget-mixin>) => (selection)
  let selection = gadget-selection(gadget);
  select (gadget-selection-mode(gadget))
    #"none"     => #();
    #"single"   => (~empty?(selection) & selection[0]) | #();
    #"multiple" => as(<list>, selection);
  end
end method capi-gadget-selection;

define method note-gadget-selection-changed 
    (gadget :: <capi-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror & ~instance?(gadget, <combo-box>))
    choice-selection(mirror) := capi-gadget-selection(gadget)
  end
end method note-gadget-selection-changed;

define method gadget-label-setter
    (label, gadget :: <capi-gadget-mixin>) => (label)
  next-method & next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & note-capi-gadget-label-changed(gadget, mirror, label);
  label
end method gadget-label-setter;

define method note-capi-gadget-label-changed 
    (sheet :: <capi-sheet-mixin>, mirror, label)
  #f
end method note-capi-gadget-label-changed;


/// Buttons

define open abstract class <capi-button-mixin>
    (<capi-gadget-mixin>)
end class <capi-button-mixin>;

define method allocate-space
    (pane :: <capi-button-mixin>, width :: <integer>, height :: <integer>)
end method allocate-space;

define method text-or-image-from-gadget-label
    (gadget)
 => (text :: false-or(<string>), image :: false-or(<image>),
     mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>));
  let (label, mnemonic, index)
    = compute-mnemonic-from-label(gadget, gadget-label(gadget), remove-ampersand?: #t);
  case
    instance?(label, <string>) =>
      values(label, #f, mnemonic, index);
    label =>
      values("<image>", #f, mnemonic, index);
    otherwise =>
      values("", #f, mnemonic, index);
  end
end method text-or-image-from-gadget-label;

define method note-capi-gadget-label-changed 
    (sheet :: <capi-button-mixin>, mirror, label)
  let (label, mnemonic, index)
    = compute-mnemonic-from-label(sheet, label, remove-ampersand?: #t);
  ignore(mnemonic, index);
  item-text(mirror) := label;
  invalidate-pane-constraints(mirror);
  relayout-parent(sheet)
end method note-capi-gadget-label-changed;


define sealed class <capi-push-button-pane>
    (<capi-button-mixin>,
     <push-button>,
     <leaf-pane>)
end class <capi-push-button-pane>;

define method sheet-primary-callback (sheet :: <capi-push-button-pane>)
  distribute-activate-callback(sheet)
end method sheet-primary-callback;

define sealed class <capi-push-button-mirror>
    (<capi-mirror>,
     <capi-push-button>)
end class <capi-push-button-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <push-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<capi-push-button-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-push-button-pane>)
  let (text, image, mnemonic)
    = text-or-image-from-gadget-label(sheet);
  make-capi-mirror(_port, sheet, <capi-push-button-mirror>,
                   interaction: no-selection:,
                   text: text,
                   callback-type: item:,
                   callback: mirror-primary-callback);
end method do-make-mirror;


define sealed class <capi-radio-button-pane>
    (<capi-button-mixin>,
     <radio-button>,
     <leaf-pane>)
end class <capi-radio-button-pane>;

define method sheet-primary-callback (gadget :: <capi-radio-button-pane>)
  let value = capi-new-mirror-value(sheet-direct-mirror(gadget));
  distribute-value-changed-callback(gadget, value)
end method sheet-primary-callback;

define sealed class <capi-radio-button-mirror>
    (<capi-mirror>,
     <capi-radio-button>)
end class <capi-radio-button-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <radio-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<capi-radio-button-pane>, #f)
end method class-for-make-pane;

define method capi-mirror-value (gadget :: <capi-radio-button-mirror>)
  lisp-true?(button-selected(gadget))
end method capi-mirror-value;

define method capi-new-mirror-value (gadget :: <capi-radio-button-mirror>)
  lisp-false?(button-selected(gadget))
end method capi-new-mirror-value;

define method note-capi-gadget-value-changed 
    (sheet :: <capi-radio-button-pane>, mirror :: <capi-radio-button-mirror>, value)
  button-selected(mirror) := value | #()
end method note-capi-gadget-value-changed;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-radio-button-pane>)
  let (text, image, mnemonic)
    = text-or-image-from-gadget-label(sheet);
  make-capi-mirror(_port, sheet, <capi-radio-button-mirror>,
                   interaction: single-selection:,
                   text: text,
                   selected: gadget-value(sheet) | #(),
                   callback-type: item:,
                   callback: mirror-primary-callback);
end method do-make-mirror;


define sealed class <capi-check-button-pane>
    (<capi-button-mixin>,
     <check-button>,
     <leaf-pane>)
end class <capi-check-button-pane>;

define method sheet-primary-callback (gadget :: <capi-check-button-pane>)
  let value = capi-new-mirror-value(sheet-direct-mirror(gadget));
  distribute-value-changed-callback(gadget, value)
end method sheet-primary-callback;

define sealed class <capi-check-button-mirror>
    (<capi-mirror>,
     <capi-check-button>)
end class <capi-check-button-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <check-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-check-button-pane>, #f)
end method class-for-make-pane;

define method capi-mirror-value (gadget :: <capi-check-button-mirror>)
  lisp-true?(button-selected(gadget))
end method capi-mirror-value;

define method note-capi-gadget-value-changed 
    (sheet :: <capi-check-button-pane>, mirror :: <capi-check-button-mirror>, value)
  button-selected(mirror) := value | #()
end method note-capi-gadget-value-changed;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-check-button-pane>)
  let (text, image, mnemonic)
    = text-or-image-from-gadget-label(sheet);
  make-capi-mirror(_port, sheet, <capi-check-button-mirror>,
                   interaction: multiple-selection:,
                   text: text,
                   selected: gadget-value(sheet) | #(),
                   callback-type: item:,
                   callback: mirror-primary-callback,
                   retract-callback: mirror-primary-callback)
end method do-make-mirror;


define sealed class <capi-list-box-pane> 
    (<capi-gadget-mixin>,
     <list-box>,
     <leaf-pane>)
end class <capi-list-box-pane>;

define sealed class <capi-list-box-mirror>
    (<capi-mirror>,
     <capi-list-panel>)
end class <capi-list-box-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <list-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-list-box-pane>, #f)
end method class-for-make-pane;

define method capi-selection-mode (gadget :: <sheet>)
  select (gadget-selection-mode(gadget))
    #"none"     => no-selection:;
    #"single"   => single-selection:;
    #"multiple" => extended-selection:;
  end
end method capi-selection-mode;

define method capi-list-box-selection-callback 
    (gadget :: <capi-list-box-pane>) => ()
  let mirror = sheet-direct-mirror(gadget);
  let mirror-selection = choice-selection(mirror);
  let selection
    = if (instance?(mirror-selection, <sequence>))
        mirror-selection
      else
        vector(mirror-selection)
      end;
  distribute-selection-changed-callback(gadget, selection)
end method capi-list-box-selection-callback;

define method capi-list-box-action-callback (sheet :: <capi-list-box-pane>)
  distribute-activate-callback(sheet)
end method capi-list-box-action-callback;

define method capi-horizontal-scroll-value (sheet :: <capi-gadget-mixin>)
  (gadget-scrolling-horizontally?(sheet) & bottom:) | #()
end method capi-horizontal-scroll-value;

define method capi-vertical-scroll-value (sheet :: <capi-gadget-mixin>)
  (gadget-scrolling-vertically?(sheet) & right:) | #()
end method capi-vertical-scroll-value;

define method update-capi-mirror-items (mirror :: <capi-collection>, items)
  collection-items(mirror) := as(<simple-vector>, items)
end method update-capi-mirror-items;

define method note-gadget-items-changed
    (gadget :: <capi-gadget-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-capi-mirror-items(mirror, gadget-items(gadget))
  end
end method note-gadget-items-changed;

define method capi-collection-gadget-items 
    (sheet :: <collection-gadget-mixin>)
  as(<simple-vector>, gadget-items(sheet))
end method capi-collection-gadget-items;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-list-box-pane>)
  let horizontal-bar? = gadget-scrolling-horizontally?(sheet);
  let vertical-bar? = gadget-scrolling-vertically?(sheet);
  make-capi-mirror(_port, sheet, <capi-list-box-mirror>,
                   interaction: capi-selection-mode(sheet),
                   // Coerce to a vector so that we can handle <range>'s
                   items: capi-collection-gadget-items(sheet),
                   selection: capi-gadget-selection(sheet),
                   print-function: method (item)
                                     collection-gadget-item-label(sheet, item)
                                   end,
                   horizontal-scroll: capi-horizontal-scroll-value(sheet),
                   vertical-scroll:   capi-vertical-scroll-value(sheet),
                   min-width:  (~horizontal-bar? & text-width:) | #(),
                   min-height: (~vertical-bar? & text-height:) | #(),
                   callback-type: none:,
                   selection-callback:
		     method ()
		       capi-list-box-selection-callback(sheet)
		     end,
                   retract-callback:
		     method ()
		       capi-list-box-selection-callback(sheet)
		     end,
                   extend-callback:
		     method ()
		       capi-list-box-selection-callback(sheet)
		     end,
                   action-callback: 
		     method ()
		       distribute-activate-callback(sheet)
		     end);
end method do-make-mirror;


define sealed class <capi-option-box-pane> 
    (<capi-gadget-mixin>,
     <option-box>,
     <leaf-pane>)
end class <capi-option-box-pane>;

define sealed class <capi-option-box-mirror>
    (<capi-mirror>,
     <capi-option-pane>)
end class <capi-option-box-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <option-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-option-box-pane>, #f)
end method class-for-make-pane;

define method capi-option-box-selection-callback
    (gadget :: <capi-option-box-pane>) => ()
  let mirror = sheet-direct-mirror(gadget);
  let mirror-selection = choice-selection(mirror);
  let selection
    = if (instance?(mirror-selection, <sequence>))
        mirror-selection
      else
        vector(mirror-selection)
      end;
  distribute-selection-changed-callback(gadget, selection)
end method capi-option-box-selection-callback;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-option-box-pane>)
  make-capi-mirror(_port, sheet, <capi-option-box-mirror>,
                   // Coerce to a vector so that we can handle <range>'s
                   items: capi-collection-gadget-items(sheet),
                   print-function: method (item)
                                     collection-gadget-item-label(sheet, item)
                                   end,
                   selection: capi-gadget-selection(sheet),
                   callback-type: none:,
                   selection-callback: method ()
                                         capi-option-box-selection-callback
                                           (sheet)
                                       end);
end method do-make-mirror;


/// Viewports

define sealed class <capi-viewport>
    (<capi-sheet-mixin>,
     <viewport>,
     <permanent-medium-mixin>,
     <mirrored-sheet-mixin>,
     <single-child-composite-pane>)
end class <capi-viewport>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <viewport>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-viewport>, #f)
end method class-for-make-pane;

//--- Make the CAPI viewport four pixels bigger than it should be to
//--- account for the border pane.
define method do-compose-space
    (pane :: <capi-viewport>, #key width, height)
 => (space-req :: <space-requirement>)
  next-method()
  // let space
  //   = next-method(pane, 
  //                 width: width & width - 2,
  //                 height: height & height - 2);
  // let thickness = 4;
  // space-requirement+(space, pane,
  //                    width: thickness, height: thickness)
end method do-compose-space;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-viewport>)
  // next-method();
  make-capi-mirror(_port, sheet, <capi-mirror-pane>,
                   has-motif-border: #"none")
end method do-make-mirror;


/// Scroll bars

define sealed class <capi-scroll-bar-pane>
    (<capi-gadget-mixin>,
     <scroll-bar>,
     <leaf-pane>)
  sealed slot scroll-bar-scale :: <float> = 1000.0;
end class <capi-scroll-bar-pane>;

define sealed class <capi-scroll-bar-mirror>
    (<capi-mirror>,
     <capi-scroll-bar>)
end class <capi-scroll-bar-mirror>;

//--- This is a grubby hack to make the scroll-bars be the same size
//--- as the rest of LispWorks... why is CAPI returning the wrong size?
define method capi-gadget-constraints (gadget :: <capi-scroll-bar-pane>)
  let (min-width, min-height, max-width, max-height) = next-method();
  ignore(max-width, max-height);
  select (gadget-orientation(gadget))
    #"horizontal" => values(min-width, 20,  $fill, 20);
    #"vertical" =>   values(20, min-height, 20, $fill);
  end
end method capi-gadget-constraints;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <scroll-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-scroll-bar-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-scroll-bar-pane>)
  let orientation = select (gadget-orientation(sheet))	//--- damned emulator
                      #"horizontal" => horizontal:;
                      #"vertical" => vertical:;
                    end;
  make-capi-mirror(_port, sheet, <capi-scroll-bar-mirror>,
		   callback: capi-scroll-bar-callback,
                   orientation: orientation);
end method do-make-mirror;

define method initialize (mirror :: <capi-scroll-bar-mirror>, #key)
  next-method();
  let gadget = mirror-sheet(mirror);
  update-capi-scroll-bar-mirror-range(mirror, gadget-value-range(gadget));
  update-capi-scroll-bar-mirror-values(mirror,
				       slug-size: gadget-slug-size(gadget),
				       value: gadget-value(gadget))
end method initialize;

define method update-capi-scroll-bar-mirror-range
    (mirror :: <capi-scroll-bar-mirror>, range :: <range>)
  let gadget = mirror-sheet(mirror);
  let scale :: <float> = scroll-bar-scale(gadget);
  range-start(mirror) := floor(range[0] * scale);
  range-end(mirror)   := floor(range[size(range) - 1] * scale);
  //---*** How do we really work out the line size and page size?
  //scroll-bar-line-size(mirror) := 16;
  //scroll-bar-page-size(mirror) := floor/(capi-min - capi-max, 10);
end method update-capi-scroll-bar-mirror-range;

define method capi-scroll-bar-value
    (gadget :: <capi-scroll-bar-pane>, value)
  let range = gadget-value-range(gadget);
  range[floor/(value, scroll-bar-scale(gadget))]
end method capi-scroll-bar-value;

define method capi-scroll-bar-callback
    (interface, mirror :: <capi-scroll-bar-mirror>, how, where)
  let gadget = mirror-sheet(mirror);
  select (how)
    #"page" =>
      case
        where >= 0 => scroll-down-page(gadget);
        otherwise  => scroll-up-page(gadget);
      end;
    #"line" =>
      case
        where >= 0 => scroll-down-line(gadget);
        otherwise  => scroll-up-line(gadget);
      end;
    #"move" =>
      gadget-value(gadget, do-callback?: #t)
        := capi-scroll-bar-value(gadget, range-slug-start(mirror));
    #"drag" =>
      gadget-value(gadget, do-callback?: #t)
        := capi-scroll-bar-value(gadget, where);
  end
end method capi-scroll-bar-callback;


define method note-gadget-value-range-changed
    (gadget :: <capi-scroll-bar-pane>) => ()
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-capi-scroll-bar-mirror-range(mirror, gadget-value-range(gadget))
  end
end method note-gadget-value-range-changed;

define method update-capi-scroll-bar-mirror-values
    (gadget :: <capi-scroll-bar-pane>, #key slug-size, value)
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-capi-scroll-bar-mirror-values(mirror, slug-size: slug-size, value: value)
  end
end method update-capi-scroll-bar-mirror-values;

define method note-capi-gadget-value-changed 
    (gadget :: <capi-scroll-bar-pane>, mirror, value) => ()
  update-capi-scroll-bar-mirror-values(gadget, value: value)
end method note-capi-gadget-value-changed;

define method note-gadget-slug-size-changed
    (gadget :: <capi-scroll-bar-pane>) => ()
  next-method();
  update-capi-scroll-bar-mirror-values(gadget, slug-size: gadget-slug-size(gadget))
end method note-gadget-slug-size-changed;

define method note-scroll-bar-changed
    (gadget :: <capi-scroll-bar-pane>) => ()
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    update-capi-scroll-bar-mirror-range(mirror, gadget-value-range(gadget));
    update-capi-scroll-bar-mirror-values(mirror,
					 slug-size: gadget-slug-size(gadget),
					 value: gadget-value(gadget))
  end
end method note-scroll-bar-changed;

define method update-capi-scroll-bar-mirror-values
    (mirror :: <capi-scroll-bar-mirror>, #key slug-size, value)
  let gadget = mirror-sheet(mirror);
  let scale :: <float> = scroll-bar-scale(gadget);
  let slug-start = floor(scale * (value | gadget-value(gadget)));
  let slug-size  = floor(scale * (slug-size | gadget-slug-size(gadget)));
  range-slug-start(mirror)     := slug-start;
  range-slug-end(mirror)       := slug-start + slug-size;
  scroll-bar-page-size(mirror) := slug-size;
end method update-capi-scroll-bar-mirror-values;


/// Sliders

define sealed class <capi-slider-pane>
    (<capi-gadget-mixin>,
     <slider>,
     <leaf-pane>)
end class <capi-slider-pane>;

define sealed class <capi-slider-mirror>
    (<capi-mirror>,
     <capi-slider>)
end class <capi-slider-mirror>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <slider>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-slider-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, slider :: <capi-slider-pane>)
  let range = gadget-value-range(slider);
  let value = gadget-value(slider);
  make-capi-mirror(_port, slider, <capi-slider-mirror>,
                   start: mirror-slider-value(slider, range[0]),
                   end:   mirror-slider-value(slider, range[size(range) - 1]),
		   slug-start: mirror-slider-value(slider, value),
                   callback-type: item:,
                   callback: capi-slider-callback);
end method do-make-mirror;

define method capi-slider-callback
    (interface :: <capi-interface>, mirror :: <capi-slider-mirror>, value)
  let gadget = mirror-sheet(mirror);
  let mirror-value = mirror-slider-value(gadget, value);
  distribute-value-changed-callback(gadget, mirror-value)
end method capi-slider-callback;

define method mirror-slider-value (gadget :: <capi-slider-pane>, value)
  let range = gadget-value-range(gadget);
  let scale = range[1] - range[0];
  floor(value * scale)
end method mirror-slider-value;

define method note-capi-gadget-value-changed
    (gadget :: <capi-slider-pane>, mirror :: <capi-slider-mirror>, value) => ()
  range-slug-start(mirror) := mirror-slider-value(gadget, value)
end method note-capi-gadget-value-changed;


/// "Decorations"

define sealed class <capi-label-pane>
    (<capi-gadget-mixin>,
     <label>,
     <leaf-pane>)
end class <capi-label-pane>;

define sealed class <capi-label-mirror>
    (<capi-mirror>,
     <capi-title-pane>)
end class <capi-label-mirror>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <label>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-label-pane>, #f)
end method class-for-make-pane;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-label-pane>)
  let (text, image, mnemonic)
    = text-or-image-from-gadget-label(sheet);
  make-capi-mirror(_port, sheet, <capi-label-mirror>,
                   text: text);
end method do-make-mirror;

define method note-gadget-label-changed (gadget :: <capi-label-pane>) => ()
  let mirror = sheet-direct-mirror(gadget);
  when (mirror)
    let (text, image, mnemonic)
      = text-or-image-from-gadget-label(gadget);
    title-pane-text(mirror) := text
  end
end method note-gadget-label-changed;



/// Text fields

define open abstract class <capi-text-field-mixin>
    (<capi-gadget-mixin>)
end class <capi-text-field-mixin>;

define open abstract class <capi-text-field-mirror-mixin>
    (<capi-mirror>)
end class <capi-text-field-mirror-mixin>;

define method capi-mirror-value (mirror :: <capi-text-field-mirror-mixin>)
  text-input-pane-text(mirror)
end method capi-mirror-value;

define method note-gadget-text-changed
    (gadget :: <capi-text-field-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  if (mirror)
    let new-text = gadget-text(gadget);
    unless (new-text = text-input-pane-text(mirror))
      text-input-pane-text(mirror) := new-text
    end
  end
end method note-gadget-text-changed;

define method note-capi-gadget-value-changed 
    (sheet :: <capi-text-field-mixin>, mirror :: <capi-text-field-mirror-mixin>, value)
  #f
end method note-capi-gadget-value-changed;

define sealed method text-caret-position
    (gadget :: <capi-text-field-mixin>)
 => (position :: false-or(<integer>))
  //---*** Do this
  0
end method text-caret-position;

define sealed method text-caret-position-setter
    (position :: false-or(<integer>), gadget :: <capi-text-field-mixin>)
 => (position :: false-or(<integer>))
  //---*** Do this
  position
end method text-caret-position-setter;

define method selected-text
    (gadget :: <capi-text-field-mixin>) => (string :: false-or(<string>))
  //---*** Do this
  #f
end method selected-text;

define sealed method text-selection
    (gadget :: <capi-text-field-mixin>)
 => (range :: type-union(<text-range>, one-of(#f)))
  //---*** Do this
  #f
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), gadget :: <capi-text-field-mixin>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  //---*** Do this
  range
end method text-selection-setter;


define sealed class <capi-text-field-mirror>
    (<capi-text-field-mirror-mixin>,
     <capi-text-input-pane>)
end class <capi-text-field-mirror>;

define method do-make-mirror 
    (_port :: <capi-port>, gadget :: <capi-text-field-mixin>)
  make-capi-mirror(_port, gadget, <capi-text-field-mirror>,
		   text: gadget-text(gadget),
		   callback-type: item:,
		   callback: method (mirror)
			       let text = text-input-pane-text(mirror);
			       distribute-text-changed-callback(gadget, text);
			       distribute-activate-callback(gadget)
			     end,
		   change-callback: method (text)
				      if (text ~= gadget-value(gadget))
					distribute-text-changing-callback(gadget, text)
				      end
				    end,
		   change-callback-type: data:);
end method do-make-mirror;


define sealed class <capi-text-field-pane>
    (<capi-text-field-mixin>,
     <text-field>,
     <leaf-pane>)
end class <capi-text-field-pane>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <text-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-text-field-pane>, #f)
end method class-for-make-pane;


define sealed class <capi-combo-box-pane>
    (<capi-text-field-mixin>,
     <combo-box>,
     <leaf-pane>)
end class <capi-combo-box-pane>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <combo-box>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-combo-box-pane>, #f)
end method class-for-make-pane;

//--- Ignore the items since we don't have a way to show them
define method update-capi-mirror-items
    (mirror :: <capi-text-field-mirror>, items :: <sequence>) => ()
  #f
end method update-capi-mirror-items;


define sealed class <capi-password-field-pane>
    (<capi-text-field-mixin>,
     <password-field>,
     <leaf-pane>)
end class <capi-password-field-pane>;

define sealed class <capi-password-field-mirror>
    (<capi-text-field-mirror-mixin>,
     <capi-password-pane>)
end class <capi-password-field-mirror>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <password-field>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-password-field-pane>, #f)
end method class-for-make-pane;

define method capi-mirror-value (mirror :: <capi-password-field-mirror>)
  text-input-pane-text(mirror)
end method capi-mirror-value;

define method do-make-mirror 
    (_port :: <capi-port>, gadget :: <capi-password-field-pane>)
  make-capi-mirror(_port, gadget, <capi-password-field-mirror>,
		   text: gadget-value(gadget),
		   callback-type: item:,
		   change-callback: method (text)
				      if (text ~= gadget-value(gadget))
					distribute-text-changing-callback(gadget, text)
				      end
				    end,
		   change-callback-type: data:,
		   callback: method (mirror)
			       let value = text-input-pane-text(mirror);
			       distribute-value-changed-callback(gadget, value);
			       distribute-activate-callback(gadget)
			     end);
end method do-make-mirror;


define sealed class <capi-text-editor-pane>
    (<capi-gadget-mixin>,
     <text-editor>,
     <leaf-pane>)
end class <capi-text-editor-pane>;

define sealed class <capi-text-editor-mirror>
    (<capi-mirror>,
     <capi-editor-pane>)
end class <capi-text-editor-mirror>;

define method class-for-make-pane
    (framem :: <capi-frame-manager>, class == <text-editor>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-text-editor-pane>, #f)
end method class-for-make-pane;

define method do-compose-space
    (gadget :: <capi-text-editor-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let nlines = gadget-lines(gadget);
  let ncols  = gadget-columns(gadget);
  let scroll-bar-slop = 20;
  when (nlines & ~height)
    height := (nlines + 1) * font-height(text-style, _port) + scroll-bar-slop
  end;
  when (ncols  & ~width)
    width  := (ncols  + 1) * font-width(text-style, _port)  + scroll-bar-slop
  end;
  let (mn-w, mn-h, mx-w, mx-h) = capi-gadget-constraints(gadget);
  let best-width  = if (width)  max(mn-w, width)  else mn-w end;
  let best-height = if (height) max(mn-h, height) else mn-h end;
  make(<space-requirement>,
       width:      best-width, height:     best-height,
       min-width:  mn-w,       min-height: mn-h,
       max-width:  mx-w,       max-height: mx-h)
end method do-compose-space;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-text-editor-pane>)
  make-capi-mirror(_port, sheet, <capi-text-editor-mirror>,
                   text: gadget-value(sheet) | "",
                   horizontal-scroll: capi-horizontal-scroll-value(sheet),
                   vertical-scroll:   capi-vertical-scroll-value(sheet));
end method do-make-mirror;

define method note-gadget-text-changed
    (gadget :: <capi-text-editor-pane>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & (editor-pane-text(mirror) := gadget-text(gadget) | "")
end method note-gadget-text-changed;

define method note-capi-gadget-value-changed 
    (sheet :: <capi-text-editor-pane>, mirror :: <capi-text-editor-mirror>, value)
  #f
end method note-capi-gadget-value-changed;
