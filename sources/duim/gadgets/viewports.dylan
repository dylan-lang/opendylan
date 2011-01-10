Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Viewports, for scrolling

// The single child of the viewport is the pane being scrolled.
// Usually mirrored at the back-end
//--- This should be split into an abstract and concrete class...
define open abstract class <viewport> 
    (<basic-gadget>,
     <standard-input-mixin>,		// so we can pass on events to the child...
     <client-overridability-mixin>,
     <scrolling-sheet-mixin>,
     <single-child-mixin>)
  // This describes the region that we are displaying
  sealed slot viewport-region :: false-or(<region>) = #f;
end class <viewport>;

define protocol-predicate viewport;

define protocol <<viewport-protocol>> ()
  function sheet-viewport
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>))
 => (viewport :: false-or(<viewport>));
  function sheet-viewport-region
    (sheet :: type-union(<abstract-sheet>, <abstract-gadget>))
 => (region :: <region>);
  function scroll-viewport
    (viewport :: <viewport>, x :: <integer>, y :: <integer>,
     #key update-scroll-bars?) => ();
  function scroll-viewport-child
    (viewport :: <viewport>, sheet :: <abstract-sheet>, x :: <integer>, y :: <integer>,
     #key update-scroll-bars?) => ();
  function shift-visible-region
    (sheet :: <abstract-sheet>,
     oleft :: <integer>, otop :: <integer>, oright :: <integer>, obottom :: <integer>,
     nleft :: <integer>, ntop :: <integer>, nright :: <integer>, nbottom :: <integer>) => ();
  // Users can define methods on the next two to do things such as
  // linking two scrollable panes together
  function note-viewport-position-changed
    (frame, sheet :: <abstract-sheet>, x, y) => ();
  function note-viewport-region-changed
    (sheet :: <abstract-sheet>, viewport :: <viewport>) => ();
  // These two ask of a sheet, are you capable of scrolling in this direction?
  // The default methods just query the containing viewport as to whether there
  // is a scroll bar in that direction
  function sheet-scrolls-horizontally?
    (sheet :: <abstract-sheet>) => (true? :: <boolean>);
  function sheet-scrolls-vertically?
    (sheet :: <abstract-sheet>) => (true? :: <boolean>);
end protocol <<viewport-protocol>>;

define method initialize 
    (viewport :: <viewport>, #key) => ()
  next-method();
  let (width, height) = box-size(viewport);
  viewport-region(viewport) := make-bounding-box(0, 0, width, height)
end method initialize;

// This is where we pass on events to the right place -- the child
//---*** Maybe this should only happen when the child is unmirrored.
//---*** Also, couldn't it behave the same as <drawing-pane>?
define method handle-event
    (viewport :: <viewport>, event :: <event>) => ()
  let child = sheet-child(viewport);
  when (child)
    handle-event(event-handler(child), event)
  end
end method handle-event;

// The input focus of a viewport gets redirected to the sheet the
// viewport is looking at
define method sheet-input-focus (sheet :: <viewport>) => (focus :: <sheet>)
  sheet-child(sheet)
end method sheet-input-focus;


// The idea here is that we allow the viewport to be infinitely
// stretchable and shrinkable.  If no size is specified, we make the 
// viewport large enough to contain the entire child.
// We also allow scrolling in only one direction, in which case the
// other direction just picks up everything from the child.
define method do-compose-space
    (viewport :: <viewport>, #key width, height)
 => (space-req :: <space-requirement>)
  let child = sheet-child(viewport);
  let horizontal? = sheet-scrolls-horizontally?(child);
  let vertical?   = sheet-scrolls-vertically?(child);
  let vertical-only?   = vertical?   & ~horizontal?;
  let horizontal-only? = horizontal? & ~vertical?;
  let default-space-req
    = default-space-requirement(viewport, width: width, height: height);
  let space-req
    = if (child)
	compose-space(child, width: width, height: height);
      else
	default-space-req;
      end;
  let ( w,  w-,  w+,  h,  h-,  h+)
    = space-requirement-components(child | viewport, space-req);
  let (dw, dw-, dw+, dh, dh-, dh+)
    = space-requirement-components(viewport, default-space-req);
  let min-width  :: <integer> = if (vertical-only?)   w- else dw- end;
  let max-width  :: <integer> = if (vertical-only?)   w+ else dw+ end;
  let min-height :: <integer> = if (horizontal-only?) h- else dh- end;
  let max-height :: <integer> = if (horizontal-only?) h+ else dh+ end;
  //--- I'm not sure about this.  This says if no size is specified then
  //--- we make it the size of the child... but the child could be huge
  //--- so the initial window comes up huge (bounded only by the size of
  //--- the screen).  An alternative might be to make it some reasonable
  //--- minimum by default (100 pixels), but this could be just as wrong.
  let best-width  :: <integer>
    = constrain-size(width  | if (horizontal?) w else dw end, min-width, max-width);
  let best-height :: <integer>
    = constrain-size(height | if (vertical?)   h else dh end, min-height, max-height);
  make(<space-requirement>,
       width:  best-width,  min-width:  min-width,  max-width:  max-width,
       height: best-height, min-height: min-height, max-height: max-height)
end method do-compose-space;

// Yes, we really do mean to put this wrapper on 'allocate-space', not
// on 'DO-allocate-space'.  The idea is that subclasses of <viewport> might
// want their own 'do-allocate-space' method, but the stuff here should
// be done no matter what.
define method allocate-space
    (viewport :: <viewport>, width :: <integer>, height :: <integer>) => ()
  next-method();
  // Now reposition the viewport
  let contents = sheet-child(viewport);		// i.e., the scrollable contents
  when (contents)
    let (owidth, oheight) = box-size(viewport-region(viewport));
    viewport-region(viewport)			// might cons a new region...
      := set-box-size(viewport-region(viewport), width, height);
    // If previously it was too small to display the entire contents
    // but now it is large enough, scroll the window
    //--- Rethink this sometime...
    let (ox, oy) = box-position(viewport-region(viewport));
    let (cleft, ctop, cright, cbottom) = box-edges(contents);
    let cw = cright - cleft;
    let ch = cbottom - ctop;
    let x = ox;
    let y = oy;
    when (owidth < cw & cw <= width)
      x := cleft
    end;
    when (oheight < ch & ch <= height)
      y := ctop
    end;
    if (x ~= ox | y ~= oy)
      scroll-viewport(viewport, x, y)
    else
      update-scroll-bars(viewport);
      note-viewport-region-changed(sheet-child(viewport), viewport)
    end
  end
end method allocate-space;

define method do-allocate-space
    (viewport :: <viewport>, width :: <integer>, height :: <integer>) => ()
  // Resize the child if it is happy to take on the new size
  let child = sheet-child(viewport);
  let horizontal? = (sheet-horizontal-scroll-bar(viewport) ~== #f);
  let vertical?   = (sheet-vertical-scroll-bar(viewport) ~== #f);
  when (child)
    let (old-width, old-height) = box-size(child);
    let width  = if (horizontal?) max(width,  old-width)  else width  end;
    let height = if (vertical?)   max(height, old-height) else height end;
    let space-req = compose-space(child, width:  width, height: height);
    let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
    ignore(w-, w+, h-, h+);
    let child-width  :: <integer> = w;
    let child-height :: <integer> = h;
    unless (child-width = old-width & child-height = old-height)
      set-sheet-size(child, child-width, child-height)
    end
  end
end method do-allocate-space;


/// Back end and user hooks

define method note-viewport-region-changed
    (sheet :: <sheet>, viewport :: <viewport>) => ()
  #f
end method note-viewport-region-changed;

// Note that the frame might be #f, which is why we don't specialize on it
define method note-viewport-position-changed
    (frame, sheet :: <sheet>, x, y) => ()
  ignore(frame, sheet, x, y);
  #f
end method note-viewport-position-changed;


/// User-level control of scrolling

// Returns the viewport of the pane, if there is one
define method sheet-viewport
    (sheet :: <sheet>) => (viewport :: false-or(<viewport>))
  let parent = sheet-parent(sheet);
  when (parent)
    case
      viewport?(parent) => parent;
      viewport-fencepost?(parent) => #f;
      otherwise => sheet-viewport(parent)
    end
  end
end method sheet-viewport;

// Given a sheet, return the region of it's viewport.  If the sheet is not
// being scrolled, the viewport region is effectively just the sheet region.
define method sheet-viewport-region
    (sheet :: <sheet>) => (region :: <region>)
  let viewport = sheet-viewport(sheet);
  (viewport & viewport-region(viewport))
  // Not a scrolling pane, so the sheet's region is the viewport region
  | sheet-region(sheet)
end method sheet-viewport-region;

define method scroll-position
    (sheet :: <sheet>) => (x :: <integer>, y :: <integer>)
  box-position(sheet-viewport-region(sheet))
end method scroll-position;

define method set-scroll-position
    (sheet :: <sheet>, x :: <integer>, y :: <integer>) => ()
  let viewport = sheet-viewport(sheet);
  when (viewport)
    scroll-viewport(viewport, x, y)
  end
end method set-scroll-position;


define method sheet-scrolls-horizontally?
    (sheet :: <sheet>) => (true? :: <boolean>)
  let viewport = sheet-viewport(sheet);
  viewport & sheet-horizontal-scroll-bar(viewport) ~== #f
end method sheet-scrolls-horizontally?;

define method sheet-scrolls-vertically?
    (sheet :: <sheet>) => (true? :: <boolean>)
  let viewport = sheet-viewport(sheet);
  viewport & sheet-vertical-scroll-bar(viewport) ~== #f
end method sheet-scrolls-vertically?;


// We need to handle this specially for viewports, because viewports don't
// change size when their kid changes size.  We need to (1) be sure the kid
// really changes size, and (2) update the scroll bars.
//--- Andy still doesn't buy this, and SWM doesn't know for sure
define method relayout-parent 
    (viewport :: <viewport>, #key width, height) => (did-layout? :: <boolean>)
  when (sheet-attached?(viewport))	// be forgiving
    let child = sheet-child(viewport);
    when (child)
      let space-req = compose-space(child, width: width, height: height);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(child, space-req);
      ignore(w-, w+, h-, h+);
      let new-width  :: <integer> = w;
      let new-height :: <integer> = h;
      unless (sheet-layed-out-to-size?(child, new-width, new-height))
	set-sheet-size(child, new-width, new-height)
      end;
      update-scroll-bars(viewport);
      #t
    end
  end
end method relayout-parent;

/*
//--- This is closer to what we really want, but it doesn't manage to
//--- propagate geometry changes to the scroll bars themselves.  Why not?
define method relayout-parent 
    (viewport :: <viewport>, #key width, height) => (did-layout? :: <boolean>)
  ignore(width, height);
  when (sheet-attached?(viewport))	// be forgiving
    block ()
      next-method()
    cleanup
      update-scroll-bars(viewport)
    end
  end
end method relayout-parent;
*/


/// Scrolling protocol

define method sheet-scroll-range
    (viewport :: <viewport>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  box-edges(sheet-child(viewport))
end method sheet-scroll-range;

define method sheet-scroll-range
    (sheet :: <sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  box-edges(sheet)
end method sheet-scroll-range;

define method sheet-visible-range
    (viewport :: <viewport>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  box-edges(viewport-region(viewport))
end method sheet-visible-range;

define method sheet-visible-range
    (sheet :: <sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  box-edges(sheet)
end method sheet-visible-range;

define method set-sheet-visible-range
    (viewport :: <viewport>, 
     left :: <real>, top :: <real>, right :: <real>, bottom :: <real>) => ()
  ignore(right, bottom);
  let x :: <integer> = truncate(left);
  let y :: <integer> = truncate(top);
  scroll-viewport(viewport, x, y, update-scroll-bars?: #f)
end method set-sheet-visible-range;


define method line-scroll-amount
    (viewport :: <viewport>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  line-scroll-amount(sheet-child(viewport), orientation) | next-method()
end method line-scroll-amount;

define method page-scroll-amount
    (viewport :: <viewport>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  page-scroll-amount(sheet-child(viewport), orientation) | next-method()
end method page-scroll-amount;


/// BITBLT

define method scroll-viewport
    (viewport :: <viewport>, x :: <integer>, y :: <integer>,
     #key update-scroll-bars? = #t) => ()
  let sheet = sheet-child(viewport);
  when (sheet)
    scroll-viewport-child(viewport, sheet, x, y,
			  update-scroll-bars?: update-scroll-bars?)
  end
end method scroll-viewport;

// For non-mirrored children, we have to do BITBLT and repaint by hand
define method scroll-viewport-child
    (viewport :: <viewport>, sheet :: <sheet>, x :: <integer>, y :: <integer>,
     #key update-scroll-bars? = #t) => ()
  let (left, top, right, bottom) = box-edges(sheet-viewport-region(sheet));
  // Optimize this case, since the rest of this code can be quite
  // expensive, especially on servers that require 'copy-area' to
  // be synchronous
  unless (x = left & y = top)
    //--- This could actually bash the sheet-transform
    sheet-transform(sheet) := make-translation-transform(-x, -y);
    viewport-region(viewport)		// might cons a new region...
      := set-box-position(viewport-region(viewport), x, y);
    // Must go after 'set-box-position'
    when (update-scroll-bars?)
      update-scroll-bars(viewport)
    end;
    // When we scroll, we've got to move all mirrored children
    update-all-mirror-positions(sheet);
    let (nleft, ntop, nright, nbottom) = box-edges(sheet-viewport-region(sheet));
    case
      // If some of the stuff that was previously on display is still on
      // display, BITBLT it into the proper place and redraw the rest
      ltrb-intersects-ltrb?(left, top, right, bottom,
			    nleft, ntop, nright, nbottom) =>
	// Move the old stuff to the new position
	shift-visible-region(sheet, left, top, right, bottom,
				    nleft, ntop, nright, nbottom);
	let rectangles
	  = ltrb-difference(nleft, ntop, nright, nbottom,
			    left, top, right, bottom);
	with-sheet-medium (medium = sheet)
	  for (region in rectangles)
	    with-clipping-region (medium, region)
	      clear-box*(medium, region);
	      repaint-sheet(sheet, region)
	    end
	  end
	end;
      // Otherwise, just redraw the whole visible viewport.
      // Adjust for the left and top margins by hand so 'clear-box'
      // doesn't erase the margin components.
      otherwise =>
	let region = viewport-region(viewport);
	with-sheet-medium (medium = sheet)
	  clear-box*(medium, region)
	end;
	repaint-sheet(sheet, region)
    end;
    let frame = sheet-frame(sheet);
    when ((left ~= x | top ~= y) & frame)
      note-viewport-position-changed(frame, sheet, x, y)
    end
  end
end method scroll-viewport-child;

// For mirrored children, the act of setting the sheet's transform is
// typically enough to trigger repainting, etc.  Back-ends are, of course,
// free to specialize this behavior further
define method scroll-viewport-child
    (viewport :: <viewport>, sheet :: <mirrored-sheet-mixin>, x :: <integer>, y :: <integer>,
     #key update-scroll-bars? = #t) => ()
  let (left, top, right, bottom) = box-edges(sheet-viewport-region(sheet));
  ignore(right, bottom);
  unless (x = left & y = top)
    sheet-transform(sheet) := make-translation-transform(-x, -y);
    viewport-region(viewport)
      := set-box-position(viewport-region(viewport), x, y);
    when (update-scroll-bars?)
      update-scroll-bars(viewport)
    end;
    update-all-mirror-positions(sheet);
    let frame = sheet-frame(sheet);
    when ((left ~= x | top ~= y) & frame)
      note-viewport-position-changed(frame, sheet, x, y)
    end
  end
end method scroll-viewport-child;

// This shifts a region of the "host screen" that's visible to some other visible
// location using 'copy-area'.  It does _not_ do any cleaning up after itself.
// It does not side-effect the output history of an output-recording stream.
// Back-ends can specialize this themselves based on the concrete gadget
// class.  For example, Windows uses 'ScrollWindowEx' to do this.
define method shift-visible-region
    (sheet :: <sheet>,
     oleft :: <integer>, otop :: <integer>, oright :: <integer>, obottom :: <integer>,
     nleft :: <integer>, ntop :: <integer>, nright :: <integer>, nbottom :: <integer>) => ()
  ignore(oright, obottom, nright, nbottom);
  let dx = oleft - nleft;
  let dy = otop  - ntop;
  let (sheet-width, sheet-height) = box-size(sheet-viewport-region(sheet));
  let from-x = #f;
  let from-y = #f;
  case
    dx >= 0 & dy >= 0 =>	// shifting down and to the right
      from-x := 0;
      from-y := 0;
    dx >= 0 & dy <= 0 =>	// shifting up and to the right
      from-x := 0;
      from-y := -dy;
    dy >= 0 =>			// shifting down and to the left
      from-x := -dx;
      from-y := 0;
    otherwise =>		// shifting up and to the left
      from-x := -dx;
      from-y := -dy
  end;
  let width  = sheet-width  - abs(dx);
  let height = sheet-height - abs(dy);
  let transform = sheet-transform(sheet);
  let (to-x, to-y)
    = untransform-position(transform, from-x + dx, from-y + dy);
  let (width, height) = untransform-distance(transform, width, height);
  let (from-x, from-y) = untransform-position(transform, from-x, from-y);
  copy-area(sheet, from-x, from-y, width, height, to-x, to-y)
end method shift-visible-region;
