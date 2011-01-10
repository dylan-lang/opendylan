module: tiles
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Methods for call-in from the GUI.

define constant $menu-actions 
  = #[#"set-square-tiling",           // Code 0
      #"set-random-tiling",           // Code 1
      #"set-semi-random-tiling",      // Code 2
      #"set-pentagonal-tiling",       // Code 3
      #"set-octagonal/square-tiling", // Code 4
      #"set-tile-size",               // Code 5
      #"set-tile-crossing",           // Code 6
      #"tile-flicker",                // Code 7
      #"tile-show-grid",              // Code 8
      #"tile-show-dual"];             // Code 9

define constant $tame-menu-event = make(<menu-event>);

define method make-tiling-view (representation, width, height)
  make(<tiling-view>, 
       representation: representation,
       extent:         point(width, height))
end method;

define generic do-menu-action (view, code, value);

define method do-menu-action (view :: <view>, code, value)
  let id = element($menu-actions, code, default: #f);
  do-menu-action-by-name(view, id, value);
end method;

define method do-menu-action-by-name (view :: <view>, id, value)
  do-event(view, $tame-menu-event, id);
end method;

define method do-menu-action-by-name 
    (view :: <view>, id == #"set-tile-size", value)
  reset-tile-size(view, value);
end method;

define method do-menu-action-by-name 
    (view :: <view>, id == #"set-tile-crossing", value)
  reset-tile-crossing(view, value);
end method;

define method do-menu-action-by-name 
    (view :: <view>, id == #"tile-show-grid", value)
  view.show-grid? := ~view.show-grid?;
  unless (view.show-grid?)
    view.show-dual? := #t;
  end unless;
  invalidate-all(view);
end method;

define method do-menu-action-by-name 
    (view :: <view>, id == #"tile-show-dual", value)
  view.show-dual? := ~view.show-dual?;
  unless (view.show-dual?)
    view.show-grid? := #t;
  end unless;
  invalidate-all(view);
end method;

define variable *last-cell* = #f;

define method do-mouse-action (view :: <view>, x, y)
  let cell = point-to-cell(view, point(x, y));
  *last-cell* := cell;
  if (cell)
    set-next-tiling-state(cell)
  end;
end method;

define method do-mouse-drag-action (view :: <view>, x, y)
  let new-cell = point-to-cell(view, point(x, y));
  unless (*last-cell* & *last-cell* == new-cell)
    *last-cell* := new-cell;
     if (new-cell)
       set-next-tiling-state(new-cell);
     end if;
  end unless;
end method;
