Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// CAPI mirrors

// This class wraps up the real window system object
define open abstract class <capi-mirror> (<capi-drawable>, <mirror>)
  sealed slot mirror-sheet, init-keyword: sheet:;
end class <capi-mirror>;

define open abstract class <capi-mirror-pane>
    (<capi-mirror>, <capi-event-handling-pane>)
end class <capi-mirror-pane>;

define method ensure-sheet-pane-created (sheet :: <sheet>, pane)
  ensure-pane-created(pane)
end method ensure-sheet-pane-created;

define method ensure-sheet-pane-created (sheet :: <menu>, pane)
end method ensure-sheet-pane-created;

define method ensure-sheet-pane-created (sheet :: <menu-box>, pane)
end method ensure-sheet-pane-created;

define method ensure-sheet-pane-created (sheet :: <menu-button>, pane)
end method ensure-sheet-pane-created;

define method ensure-representation (pane)
  ensure-pane-created(pane);
  representation(pane)
end method ensure-representation;

define method capi-sheet-enabled? (pane :: <sheet>)
  #t
end method capi-sheet-enabled?;

define method do-make-mirror
    (_port :: <capi-port>, sheet :: <sheet>) => (mirror)
  make-capi-mirror(_port, sheet, <capi-mirror-pane>)
end method do-make-mirror;

define method make-capi-mirror 
    (_port :: <capi-port>, sheet, class,
     #rest args, #key parent, enabled, create? = #t, #all-keys)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  let parent
    = parent
      | capi-mirror-parent(sheet)
      | error("Failed to mirror - no known parent for sheet %=", sheet);
  let parent-rep = representation(convert-to-screen());
  let mirror
    = apply(make, class,
            sheet: sheet,
            enabled: enabled | capi-sheet-enabled?(sheet) | #(),
            region: make-bounding-box(left, top, right, bottom),
            parent: parent,
	    foreground: capi-mirror-foreground(_port, sheet, parent-rep) | #(),
            background: capi-mirror-background(_port, sheet, parent-rep) | #(),
            font: capi-mirror-text-style(_port, sheet, parent-rep) | #(),
            args);
  if (create?)
    ensure-pane-created(mirror)
  end;
  let _port = port(sheet);
  sheet-direct-mirror(sheet) := mirror;
  //--- CAPI makes things mapped, so we unmap it immediately to make the
  //--- code behave more like a proper backend.
  unmap-mirror(_port, sheet, mirror);
  mirror
end method make-capi-mirror;

define method capi-mirror-foreground
    (_port :: <capi-port>, sheet :: <sheet>, contact)
  let foreground = get-default-foreground(_port, sheet, default: #f);
  foreground & color->capi-color(contact, foreground)
end method capi-mirror-foreground;

define method capi-mirror-background
    (_port :: <capi-port>, sheet :: <sheet>, contact)
  let background = get-default-background(_port, sheet, default: #f);
  background & color->capi-color(contact, background)
end method capi-mirror-background;

define method capi-mirror-text-style
    (_port :: <capi-port>, sheet :: <sheet>, contact)
  let text-style = get-default-text-style(_port, sheet, default: #f);
  text-style
  & fully-merged-text-style?(text-style)
  & text-style-mapping(port(sheet), text-style)
end method capi-mirror-text-style;

/*
//--- What we're trying to do here is to update the various mirror styles,
//--- which we can't do at creation time because all the various pointers
//--- up the sheet/mirror tree aren't yet in place.
define method make-mirror
    (_port :: <capi-port>, sheet :: <mirrored-sheet-mixin>) => (mirror)
  let mirror = next-method();
  let port = port(sheet);
  let rep = representation(mirror);
  update-representation-foreground
    (rep, color->capi-color(rep, get-default-foreground(port, sheet)));
  update-representation-background
    (rep, color->capi-color(rep, get-default-background(port, sheet)));
  update-representation-font
    (rep, text-style-mapping(port, get-default-text-style(port, sheet)));
  mirror
end method do-make-mirror;
*/

define method remove-capi-mirror (mirror :: <capi-mirror>, parent)
  if (sheet-mapped?(mirror-sheet(parent)))
    remove-element(mirror, parent);
  end
  //--- deallocate all window system resources
  // let rep = representation(mirror);
  // if (rep)
  //   destroy-representation(rep)
  // end;
end method remove-capi-mirror;

define method destroy-mirror
    (_port :: <capi-port>, sheet :: <sheet>, mirror) => ()
  if (instance?(mirror, <capi-element>)
      & instance?(mirror, <capi-mirror>))
    remove-capi-mirror(mirror, element-parent(mirror))
  end;
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

// We don't need to map anything but the top level sheet, because
// CAPI will do most of the work for us
define method map-mirror
    (_port :: <capi-port>, sheet :: <sheet>, mirror) => ()
  let rep = representation(mirror);
  restore-representation(mirror, rep)
end method map-mirror;

define method unmap-mirror
    (_port :: <capi-port>, sheet :: <sheet>, mirror) => ()
  when (instance?(mirror, <capi-element>)
      & instance?(mirror, <capi-mirror>))
    let rep = representation(mirror);
    unless (lisp-false?(rep))
      withdraw-representation(mirror, rep)
    end
  end
end method unmap-mirror;

define method raise-mirror
    (_port :: <capi-port>, sheet :: <sheet>, mirror,
     #key activate? = #t) => ()
  ignore(activate?);
  //--- do it
end method raise-mirror;

define method lower-mirror
    (_port :: <capi-port>, sheet :: <sheet>, mirror) => ()
  //--- do it
end method lower-mirror;

define method mirror-visible? (_port :: <capi-port>, sheet :: <sheet>, mirror)
  let rep = mirror & ensure-representation(mirror);
  let shell = rep & capi-shell(rep);
  shell & (contact-state(shell) == mapped:)
end method mirror-visible?;

define method set-mirror-edges
    (_port :: <capi-port>, sheet :: <mirrored-sheet-mixin>, mirror,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  let extra-borders = 0;
  let double-borders = extra-borders * 2;
  let x = floor(left - extra-borders);
  let y = floor(top  - extra-borders);
  let width  = floor(right  - left + double-borders);
  let height = floor(bottom - top  + double-borders);
  maybe-change-geometry(sheet-direct-mirror-decoration(sheet),
                        x, y, width, height);
end method set-mirror-edges;

define method mirror-edges
    (_port :: <capi-port>, sheet :: <mirrored-sheet-mixin>, mirror)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let rep = ensure-representation(mirror);
  if (rep & ~instance?(rep, <list>))	// wierd test because of Lisp NIL
    let (x, y, width, height) = representation-geometry(rep);
    values(x, y, x + width, y + height)
  else
    // We might get here when someone asks after a pull-down menu
    // that is not mapped, so return something useful
    //--- We need this because of the wierdness of pull-down menus
    sheet-device-edges(sheet)
  end
end method mirror-edges;

define method maybe-change-geometry (self, x, y, width, height)
  let rep = ensure-representation(self);
  when (rep & ~instance?(rep, <list>))	// wierd test because of Lisp NIL
    let (old-x, old-y, old-width, old-height) = representation-geometry(rep);
    unless (x = old-x
            & y = old-y
            & width = old-width
            & height = old-height)
      change-geometry(self, width, height, x, y)
    end
  end
end method maybe-change-geometry;

define method sheet-direct-mirror-decoration (sheet)
  let mirror = sheet-direct-mirror(sheet);
  maybe-decoration-pane(mirror, #t)
end method sheet-direct-mirror-decoration;
