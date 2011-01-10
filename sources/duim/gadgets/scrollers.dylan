Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Abstract scroller pane

// Scroller panes just serve to lay out all of the things that make up
// a scrollable pane -- the viewport, the pane being scrolled, and the
// viewport into the pane being scrolled.
define open abstract class <scroller> 
    (<scrolling-gadget-mixin>, <basic-gadget>)
  sealed slot scroller-sheet :: false-or(<scrolling-sheet-mixin>) = #f,
    setter: %sheet-setter;
  sealed slot scroller-gadget-supplies-scroll-bars? = #f;
end class <scroller>;

define open generic make-scrolling-layout
    (framem :: <frame-manager>, sheet :: false-or(<sheet>),
     #key scroller, horizontal?, vertical?,
          border-type, foreground, background)
 => (layout :: <sheet>);

// Options can be any of the pane sizing options
define macro scrolling
  { scrolling (#rest ?options:expression)
      ?contents:body
    end }
    => { begin
           let _scrolled-sheet = #f;
           let ?=scrolled-sheet = method (p) _scrolled-sheet := p end;
           let _contents = ?contents;	// contents is a single expression
           ignore(?=scrolled-sheet);
           // Separate the "contents" from the "scrolled pane" so that folks
           // can do things like wrap margins around the scrolled pane.
	   // "Contents" is the child of the scroller and serves to control
	   // layout, whereas "scrolled-sheet" is the sheet being controlled
	   // by the scroll bars.
           make(<scroller>,
		contents: _contents,
		scrolled-sheet: _scrolled-sheet,
                ?options)
         end }
end macro scrolling;


/// Concrete scroller panes

define sealed class <scroller-pane>
    (<scroller>, <single-child-wrapping-pane>)
end class <scroller-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <scroller>, #key)
 => (class :: <class>, options :: false-or(<sequence>));
  values(<scroller-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<scroller-pane>));
define sealed domain initialize (<scroller-pane>);

define constant $vertical-scroll-bar-values
    = #[#t, #"both", #"vertical", #"dynamic"];
define constant $horizontal-scroll-bar-values
     = #[#t, #"both", #"horizontal", #"dynamic"];

// We could use a toolkit scrolling window, but that's actually a lot harder
// and we give up flexibility, too.  This mimics the look and feel properly
// anyway, so it's no big deal.
define sealed method initialize
    (scroller :: <scroller-pane>,
     #key contents, scrolled-sheet, frame-manager: framem,
          child-width, child-height, border-type: borders = #"sunken") => ()
  next-method();
  let scroll-bars = gadget-scroll-bars(scroller);
  let scrolled-sheet = scrolled-sheet | contents;
  let gadget-scroller?
    = instance?(scrolled-sheet, <gadget>)
      & gadget-supplies-scroll-bars?
          (framem, scrolled-sheet, scroll-bars: scroll-bars);
  if (gadget-scroller?)
    add-child(scroller, contents);
    scroller-gadget-supplies-scroll-bars?(scroller) := #t
  else
    check-type(scroll-bars, <scroll-bar-type>);
    let foreground = default-foreground(scroller);
    let background = default-background(scroller);
    let horizontal? = member?(scroll-bars, $horizontal-scroll-bar-values);
    let vertical?   = member?(scroll-bars, $vertical-scroll-bar-values);
    // 'contents' will be the sheet that gets put into the layout with
    // scroll bars etc, and 'scrolled-sheet' is the sheet that the scroll
    // bars are attached to.
    let (contents, scrolled-sheet)
      = if (instance?(scrolled-sheet, <scrolling-sheet-mixin>))
          values(contents, scrolled-sheet)
        else
          let viewport
            = make-sheet-viewport(scrolled-sheet,
                                  width: child-width,
                                  height: child-height);
          // The children of the scroller pane are the contents, which may
          // be a "superset" of the pane we are actually scrolling.
          values(if (contents == scrolled-sheet) viewport else contents end,
                 viewport)
        end;
    let layout
      = make-scrolling-layout
	  (framem, contents,
	   scroller: scroller,
	   horizontal?: horizontal?, vertical?: vertical?,
	   border-type: borders,
	   foreground: foreground, background: background);
    scroller.%sheet := scrolled-sheet;
    add-child(scroller, layout)
  end;
  initialize-scrolling(scroller, scrolled-sheet)
end method initialize;

define open generic initialize-scrolling
    (scroller :: <scroller>, scrolled-sheet :: false-or(<abstract-sheet>)) => ();

define method initialize-scrolling
    (scroller :: <scroller>, scrolled-sheet :: false-or(<sheet>)) => ()
  #f
end method initialize-scrolling;

define function make-sheet-viewport
    (sheet :: false-or(<sheet>), #key width, height)
 => (viewport :: <viewport>)
  let viewport = make(<viewport>, width: width, height: height);
  // Splice the viewport between the pane we are scrolling and its parent
  when (sheet)
    let parent = sheet-parent(sheet);
    when (parent)
      replace-child(parent, sheet, viewport)
    end;
    add-child(viewport, sheet)
  end;
  viewport
end function make-sheet-viewport;

define method make-scrolling-layout
    (framem :: <frame-manager>, sheet :: false-or(<sheet>),
     #key scroller,
	  horizontal? = #t, vertical? = #t,
          border-type: borders = #"sunken", foreground, background)
 => (layout :: <sheet>)
  with-frame-manager (framem)
    let vertical-bar
      = vertical? & make(<scroll-bar>,
			 orientation: #"vertical",
			 id: #"vertical",
			 client: scroller,
			 background: background,
			 foreground: foreground);
    let horizontal-bar
      = horizontal? & make(<scroll-bar>,
			   orientation: #"horizontal",
			   id: #"horizontal",
			   client: scroller,
			   background: background,
			   foreground: foreground);
    sheet-horizontal-scroll-bar(sheet) := horizontal-bar;
    sheet-vertical-scroll-bar(sheet)   := vertical-bar;
    let layout
      = case
	  horizontal? & vertical? =>
	    make(<table-layout>,
		 contents: vector(vector(sheet, vertical-bar),
				  vector(horizontal-bar, #f)));
	  vertical? =>
	    make(<row-layout>,
		 children: vector(sheet, vertical-bar));
	  horizontal? =>
	    make(<column-layout>, 
		 children: vector(sheet, horizontal-bar));
	  otherwise =>
	    sheet;
	end;
    if (borders)
      with-border (type: borders) layout end
    else
      layout
    end
  end
end method make-scrolling-layout;
