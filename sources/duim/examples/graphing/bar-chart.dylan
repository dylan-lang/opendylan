Module:       duim-graphing-internals
Author:       Scott McKay
Synopsis:     Simple bar charts for DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple bar charts

// A bar chart consists of a collection gadget (i.e., the bar chart itself)
// and a set of bars that act as the collection gadget's items.  Bars are
// subclassable so that client programmers can associate application data
// with them.


/// Bar charts

define open abstract class <bar-chart>
    (<scrolling-gadget-mixin>,
     <action-gadget-mixin>,
     <popup-menu-gadget-mixin>,
     <oriented-gadget-mixin>,
     <choice-gadget>)
  // For a chart with vertical bars, the scaling determines the bar heights
  // For a chart with horizontal bars, the scaling determines the bar widths
  slot bar-scaling :: <real> = 1.0,
    init-keyword: scaling:;
  // For a chart with vertical bars, the minor size gives the bar width
  // For a chart with horizontal bars, the minor size gives the bar height
  slot bar-minor-size :: <integer> = 5,
    init-keyword: minor-size:;
  slot bar-spacing :: <integer> = 5,
    init-keyword: spacing:;
end class <bar-chart>;

// This is the class that clients should subclass
define open abstract class <bar-chart-pane>
    (<standard-input-mixin>,
     <standard-repainting-mixin>,
     <permanent-medium-mixin>,
     <bar-chart>,
     <layout-pane>)
end class <bar-chart-pane>;

define sealed class <simple-bar-chart-pane>
    (<bar-chart-pane>)
end class <simple-bar-chart-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <bar-chart>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<simple-bar-chart-pane>, #f)
end method class-for-make-pane;


define method do-compose-space
    (chart :: <bar-chart-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  default-space-requirements(chart, width: width, height: height)
end method do-compose-space;

define method do-allocate-space
    (chart :: <bar-chart-pane>, width :: <integer>, height :: <integer>) => ()
  ignore(width);
  select (gadget-orientation(chart))
    #"horizontal" =>
      let x :: <integer> = bar-spacing(chart);
      for (bar :: <bar-pane> in sheet-children(chart))
	unless (sheet-withdrawn?(bar))
	  let space-req = compose-space(bar);
	  let (w, w-, w+, h, h-, h+) = space-requirement-components(bar, space-req);
	  ignore(w-, w+, h-, h+);
	  set-sheet-edges(bar, x, height - 2 - h, x + w, height - 2);
	  x := x + w + bar-spacing(bar)
	end
      end;
    #"vertical" =>
      //---*** Do this
      #f;
  end
end method do-allocate-space;

// The items are the bars, which are the children!
define method note-gadget-items-changed 
    (chart :: <bar-chart-pane>) => ()
  next-method();
  sheet-children(chart) := gadget-items(chart);
  when (sheet-mapped?(chart))
    // Hack to get the screen to redisplay
    clear-box*(chart, sheet-region(chart));
    // Force relayout of all items, then notify up the sheet tree
    // if anything changed
    relayout-parent(chart);
    // Ensure that all kids are mapped
    sheet-mapped?(chart, do-repaint?: #f) := #t;
    repaint-sheet(chart, $everywhere);
    force-display(chart)
  end
end method note-gadget-items-changed;

define method note-gadget-selection-changed
    (box :: <bar-chart-pane>) => ()
  next-method();
  let items     = gadget-items(chart);
  let selection = gadget-selection(chart);
  local method set-selection (bar :: <bar-pane>, selection :: <boolean>) => ()
	  bar-selected?(bar) := selection;
	  repaint-sheet(bar, $everywhere)
	end method;
  for (bar in items)
    set-selection(bar, #f)
  end;
  for (index in selection)
    set-selection(items[index], #t)
  end
end method note-gadget-selection-changed;

define variable $chart-pen = make(<standard-pen>, width: 1, dashes: #f);

define method handle-repaint
    (bar :: <bar-chart-pane>, medium :: <medium>, region :: <region>) => ()
  with-drawing-options (medium, pen: $chart-pen)
    let (left, top, right, bottom) = box-edges(bar);
    //--- I bet tick marks would be nice, too
    draw-line(medium, left, top, left, bottom);
    draw-line(medium, left, bottom - 1, right, bottom - 1)
  end
end method handle-repaint;


/// Bars

// Note that the gadget value of a bar must be a real number,
// which will be used to cmopute the size of the bar
define open abstract class <bar>
    (<value-gadget>,
     <action-gadget>)
end class <bar>;

// This is the class that clients should subclass
define open abstract class <bar-pane>
    (<bar>,
     <simple-pane>)
  sealed slot bar-selected? :: <boolean> = #f;
end class <bar-pane>;

define sealed class <simple-bar-pane>
    (<bar-pane>)
end class <simple-bar-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<simple-bar-pane>, #f)
end method class-for-make-pane;


// By default, the scaling comes from the chart
define method bar-scaling
    (bar :: <bar-pane>) => (scaling :: <real>)
  bar-scaling(sheet-parent(bar))
end method bar-scaling;

// By default, the minor size comes from the chart
define method bar-minor-size
    (bar :: <bar-pane>) => (spacing :: <integer>)
  bar-minor-size(sheet-parent(bar))
end method bar-minor-size;

// By default, the spacing comes from the chart
define method bar-spacing
    (bar :: <bar-pane>) => (spacing :: <integer>)
  bar-spacing(sheet-parent(bar))
end method bar-spacing;


define method do-compose-space
    (bar :: <bar-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let chart :: <bar-chart-pane> = sheet-parent(bar);
  select (gadget-orientation(chart))
    #"horizontal" =>
      let bar-width  = bar-minor-size(bar);
      let bar-height = floor(gadget-value(bar) * bar-scaling(bar));
      make(<space-requirement>,
	   width: bar-width, height: bar-height);
    #"vertical" =>
      let bar-height = bar-minor-size(bar);
      let bar-width  = floor(gadget-value(bar) * bar-scaling(bar));
      make(<space-requirement>,
	   width: bar-width, height: bar-height);
  end
end method do-compose-space;

define variable $bar-pen   = $chart-pen;
define variable $bar-brush = make-gray-color(0.80);

define method handle-repaint
    (bar :: <bar-pane>, medium :: <medium>, region :: <region>) => ()
  let (left, top, right, bottom) = box-edges(bar);
  with-drawing-options (medium, brush: $bar-brush)
    draw-rectangle(medium, left, top, right, bottom, filled?: #t);
  end;
  with-drawing-options (medium, pen: $bar-pen, brush: ~bar-selected?(bar) & $bar-brush)
    draw-rectangle(medium, left, top, right, bottom, filled?: #f)
  end
end method handle-repaint;

define method handle-event 
    (bar :: <bar-pane>, event :: <button-press-event>) => ()
  when (gadget-enabled?(bar))
    let chart :: <bar-chart-pane> = set-bar-chart-selection(bar, event);
    select (event-button(event))
      $left-button => #f;
      $middle-button =>
	activate-gadget(chart);
      $right-button =>
	execute-popup-menu-callback
	  (chart, gadget-client(chart), gadget-id(chart), bar,
	   x: event-x(event), y: event-y(event));
    end
  end
end method handle-event;

define method handle-event 
    (bar :: <bar-pane>, event :: <double-click-event>) => ()
  when (gadget-enabled?(bar)
	& event-button(event) == $left-button)
    // Set the selection and activate
    let chart :: <bar-chart-pane> = set-bar-chart-selection(bar, event);
    activate-gadget(chart)
  end
end method handle-event;

define method set-bar-chart-selection
    (bar :: <bar-pane>, event) => (chart :: <bar-chart-pane>)
  let chart :: <bar-chart-pane> = sheet-parent(bar);
  let items     = gadget-items(chart);
  let selection = gadget-selection(chart);
  let index     = position(items, bar);
  local method set-selection (bar :: <bar-pane>, selection :: <boolean>) => ()
	  bar-selected?(bar) := selection;
	  repaint-sheet(bar, $everywhere)
	end method;
  select (gadget-selection-mode(chart))
    #"none"   => #f;
    #"single" =>
      for (index in selection)
        set-selection(items[index], #f)
      end;
      gadget-selection(chart, do-callback?: #t) := vector(index);
      set-selection(items[index], #t);
    #"multiple" =>
      if (event-modifier-state(event) == $shift-key)
	// Adding to or removing from selection
	if (member?(index, selection))
	  gadget-selection(chart, do-callback?: #t) := remove(selection, index);
	  set-selection(items[index], #f)
	else
	  gadget-selection(chart, do-callback?: #t) := add(selection, index);
	  set-selection(items[index], #t)
	end
      else
        for (index in selection)
	  set-selection(items[index], #f)
	end;
	gadget-selection(chart, do-callback?: #t) := vector(index);
	set-selection(items[index], #t);
      end;
  end;
  chart
end method set-bar-chart-selection;


/*
chart := contain(make(<bar-chart>, width: 200, height: 100));

gadget-items(chart) := vector(make(<bar>, value: 20),
			      make(<bar>, value: 10),
			      make(<bar>, value: 30));

gadget-value(chart) & gadget-value(gadget-value(chart));
*/
