Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// top level sheet

define variable *top-level-sheet-border* = 8;

define sealed class <capi-top-level-sheet>
    (<standard-repainting-mixin>,
     <capi-pane-mixin>,
     <top-level-sheet>)
end class <capi-top-level-sheet>;

define method class-for-make-pane 
    (framem :: <capi-frame-manager>, class == <top-level-sheet>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<capi-top-level-sheet>, #f)
end method class-for-make-pane;

define method do-compose-space
    (pane :: <capi-top-level-sheet>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let space-req = next-method();
  let (width, min-width, max-width, height, min-height, max-height)
    = space-requirement-components(pane, space-req);
  let (display-width, display-height) = box-size(display(pane));
  when (min-width  >= $fill) min-width  := 1 end;
  when (min-height >= $fill) min-height := 1 end;
  when (max-width  >= $fill) max-width  := display-width  end;
  when (max-height >= $fill) max-height := display-height end;
  min!(min-width,  display-width);
  min!(min-height, display-height);
  min!(max-width,  display-width);
  min!(max-height, display-height);
  make(<space-requirement>,
       width:  width,  min-width:  min-width,  max-width:  max-width,
       height: height, min-height: min-height, max-height: max-height)
end method do-compose-space;


// This class wraps up the real window system object
define sealed class <capi-top-level-mirror> (<capi-mirror>, <capi-interface>)
  sealed slot mirror-sheet, init-keyword: sheet:;
end class <capi-top-level-mirror>;

define method top-level-interface-best-geometry 
    (mirror :: <capi-top-level-mirror>)
  let (width, height) = box-size(mirror-sheet(mirror));
  values(#(), #(), width, height)
end method top-level-interface-best-geometry;

define method do-make-mirror 
    (_port :: <capi-port>, sheet :: <capi-top-level-sheet>)
  make-top-level-mirror(_port, sheet-frame(sheet), sheet)
end method do-make-mirror;

define method destroy-mirror
    (port :: <capi-port>, sheet :: <capi-top-level-sheet>, mirror)
  let rep = representation(mirror);
  if (~instance?(rep, <list>))
    destroy-representation(rep)
  end;
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method quit-capi-frame (mirror :: <capi-top-level-mirror>) => ()
  let sheet = mirror-sheet(mirror);
  let frame = sheet-frame(sheet);
  exit-frame(frame, destroy?: #t)
end method quit-capi-frame;

//--- We need to subvert the DUIM mirroring mechanism and mirror all of
//--- the menus before the top-level gets created, as the CAPI is much
//--- happier creating the menu bar in one piece rather than creating
//--- an empty menu bar and then populating it a piece at a time.
define method mirror-top-level-menus
    (frame :: <simple-frame>, top-level :: <capi-top-level-sheet>) => ()
  let port = port(top-level);
  let menu-bar = frame-menu-bar(frame);
  if (menu-bar)
    do-sheet-tree(method (sheet)
                    if (instance?(sheet, <mirrored-sheet-mixin>))
                      make-mirror(port, sheet)
                    end
                  end,
                  menu-bar)
  end
end method mirror-top-level-menus;

define method make-top-level-mirror 
    (_port :: <capi-port>, frame :: <simple-frame>, sheet :: <capi-top-level-sheet>)
  let mirror
    = make-capi-mirror(_port, sheet, <capi-top-level-mirror>,
                       title: frame-title(sheet-frame(sheet)) | "Unnamed frame",
                       internal-border: *top-level-sheet-border*,
                       parent: convert-to-screen(),
                       confirm-destroy-function: quit-capi-frame,
                       create?: #f);
  sheet-direct-mirror(sheet) := mirror;
  //--- This doesn't work, unfortunately
  // mirror-top-level-menus(frame, sheet);
  ensure-pane-created(mirror);
  mirror
end method make-top-level-mirror;

define method map-mirror
    (_port :: <capi-port>, sheet :: <top-level-sheet>, mirror) => ()
  map-top-level-mirror(mirror)
end method map-mirror;

define method raise-mirror 
    (_port :: <capi-port>, sheet :: <top-level-sheet>, mirror,
     #key activate? = #t) => ()
  ignore(activate?);
  // Uniconize it, and then raise it
  show-representation(representation(mirror));
  raise-interface(mirror)
end method raise-mirror;

define method lower-mirror
    (_port :: <capi-port>, sheet :: <top-level-sheet>, mirror) => ()
  lower-interface(mirror)
end method lower-mirror;

define method update-mirror-bounds
    (mirror :: <capi-top-level-mirror>,
     min-width, min-height, max-width, max-height)
  update-representation-bounds(representation(mirror),
                               min-width, min-height,
                               max-width, max-height)
end method update-mirror-bounds;

define method map-top-level-mirror (mirror :: <capi-top-level-mirror>)
  let sheet = mirror-sheet(mirror);
  let space-req = compose-space(sheet);
  let (w, w-, w+, h, h-, h+) = space-requirement-components(sheet, space-req);
  ignore(w, h);
  update-mirror-bounds(mirror, w-, h-, w+, h+);
  run-capi-post-actions(mirror);
  display-representation(representation(mirror));
  run-capi-top-level-function(mirror);
end method map-top-level-mirror;

define method set-mirror-edges 
    (_port :: <capi-port>, sheet :: <capi-top-level-sheet>, mirror,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  let width  = right  - left;
  let height = bottom - top;
  maybe-change-geometry(mirror,
                        #(), #(), width, height)
end method set-mirror-edges;

define method contact-shell-mapped? (mirror :: <capi-top-level-mirror>)
  let contact = representation(mirror);
  contact-state(contact-parent(contact)) = #"mapped"
end method contact-shell-mapped?;

define method tk-resize-top-level 
    (mirror :: <capi-top-level-mirror>, contact, width, height)
  if (contact-shell-mapped?(mirror))
    let sheet = mirror-sheet(mirror);
    let (left, top, right, bottom) = mirror-edges(port(sheet), sheet, mirror);
    let new-region = make-bounding-box(left, top, right, bottom);
    trace-sheet-event(mirror, "resized to %=x%= at %=,%=",
		      right - left, bottom - top, left, top);
    distribute-capi-event(mirror,
			  make(<window-configuration-event>,
			       sheet: sheet,
			       region: new-region))
  end
end method tk-resize-top-level;


define method note-frame-title-changed 
    (framem :: <capi-frame-manager>, frame :: <simple-frame>)
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let mirror = sheet-direct-mirror(top-sheet);
    when (mirror)
      interface-title(mirror) := as(<string>, frame-title(frame) | "Unnamed frame")
    end
  end
end method note-frame-title-changed;

define method note-frame-icon-changed 
    (framem :: <capi-frame-manager>, frame :: <simple-frame>)
  #f
end method note-frame-icon-changed;


/// Top level drawing pane:

// this pane wraps up the frame-layout so that layout can happen
// more easily and so that drawing doesn't get done on the top-level window.

define sealed class <capi-top-level-drawing-pane> 
    (<permanent-medium-mixin>,
     <capi-pane-mixin>,
     <column-layout>)
end class <capi-top-level-drawing-pane>;

define method do-make-mirror
    (_port :: <capi-port>, sheet :: <capi-top-level-drawing-pane>)
  make-capi-mirror(_port, sheet, <capi-mirror-pane>,
                   visible-border: no:);
end method do-make-mirror;

define method set-mirror-edges 
    (_port :: <capi-port>, sheet :: <capi-top-level-drawing-pane>, mirror,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  let width = right - left;
  let height = bottom - top;
  maybe-change-geometry(sheet-direct-mirror-decoration(sheet),
                        #(), #(), width, height)
end method set-mirror-edges;

define method frame-wrapper
    (framem :: <capi-frame-manager>, frame :: <simple-frame>, layout)
  with-frame-manager (framem)
    let children :: <stretchy-object-vector> = make(<stretchy-vector>);
    let menu-bar = frame-menu-bar(frame) | make(<menu-bar>);
    when (menu-bar) add!(children, menu-bar) end;
    let drawing-children = frame-main-children(framem, frame, layout);
    if (drawing-children)
      add!(children, make-capi-frame-drawing-area(framem, drawing-children))
    end;
    make(<column-layout>, children: children, spacing: 2)
  end
end method frame-wrapper;

define method frame-main-children
    (framem :: <frame-manager>, frame :: <simple-frame>, layout)
 => (children :: false-or(<sequence>))
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  let drawing-children :: <stretchy-object-vector> = make(<stretchy-vector>);
  when (tool-bar)   add!(drawing-children, tool-bar)   end;
  when (layout)     add!(drawing-children, layout)     end;
  when (status-bar) add!(drawing-children, status-bar) end;
  unless (empty?(drawing-children))
    drawing-children
  end
end method frame-main-children;

define method make-capi-frame-drawing-area
    (framem :: <capi-frame-manager>, children :: <sequence>)
 => (layout :: <sheet>)
  with-frame-manager (framem)
    make(<spacing>,
	 child: make(<capi-top-level-drawing-pane>,
		     spacing: 2,
		     children: children),
	 thickness: *top-level-sheet-border*)
  end
end method make-capi-frame-drawing-area;

define method frame-top-level-sheet-size
    (framem :: <capi-frame-manager>, frame :: <frame>,
     width :: false-or(<integer>), height :: false-or(<integer>))
 => (width :: <integer>, height :: <integer>)
  ignore(width, height);
  let (width,  height)  = next-method();
  let (gwidth, gheight) = box-size(display(frame));
  // The fudge factor is most useful on X ports...
  let fudge-factor = 0.9;
  width  := min(width  | 0, floor(gwidth  * fudge-factor));
  height := min(height | 0, floor(gheight * fudge-factor));
  values(width, height)
end method frame-top-level-sheet-size;

define method update-frame-layout
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let old-layout = *old-layout*;
    let new-layout = frame-layout(frame);
    let layout-parent = sheet-parent(old-layout);
    if (old-layout)
      if (new-layout)
	replace-child(layout-parent, old-layout, new-layout);
	relayout-children(layout-parent);
	relayout-parent(new-layout);
	sheet-mapped?(new-layout, clear?: #t, repaint?: #t) := sheet-mapped?(layout-parent)
      else
	remove-child(layout-parent, old-layout);
	relayout-parent(layout-parent)
      end
    else
      error("Adding a new layout into a frame not yet implemented")
    end
  end
end method update-frame-layout;

define sealed method update-frame-wrapper
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  if (top-sheet)
    let children = frame-main-children(framem, frame, frame-layout(frame));
    let drawing-area = ensure-frame-drawing-area(framem, frame);
    let child = sheet-child(drawing-area);
    sheet-children(child) := children | #[];
    update-frame-menu-bar(framem, frame);
    relayout-parent(top-sheet);
  end  
end method update-frame-wrapper;

define method ensure-frame-drawing-area
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  if (top-sheet)
    let column = sheet-child(top-sheet);
    let children = sheet-children(column);
    case
      size(children) = 2 =>
        children[1];
      size(children) = 1 & ~instance?(children[0], <menu-bar>) =>
        children[0];
      otherwise =>
        let drawing-area = make-capi-frame-drawing-area(framem, #());
        add-child(column, drawing-area);
        drawing-area
    end;
  end  
end method ensure-frame-drawing-area;

define method update-frame-menu-bar 
    (framem :: <capi-frame-manager>, frame :: <simple-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  let column = sheet-child(top-sheet);
  let old-menu-bar = sheet-children(column)[0];
  let menu-bar = frame-menu-bar(frame);
  unless (menu-bar = old-menu-bar)
    case
      menu-bar =>
        replace-child(column, old-menu-bar, menu-bar);
        relayout-parent(menu-bar);
      otherwise =>
        //---*** Need to be able to remove a menu-bar too...
        #f;
    end;
  end;
end method update-frame-menu-bar;

define method remove-capi-mirror (mirror :: <capi-top-level-mirror>, parent)
  #f
end method remove-capi-mirror;
