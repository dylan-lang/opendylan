Module:       Dylan-User
Synopsis:     DUIM back-end for GTK
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module gtk-duim
  use dylan;
  use operating-system;

  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;        //---*** until we've got all native gadgets in

  use c-ffi;

  use glib;
  use gobject;
  use gobject-glue;
  use pango;
  use cairo;
  use gdk;
  use gtk;
  use gtk-properties;

  // Basic classes
  export <gtk-port>,
         <gtk-medium>,
         <gtk-frame-manager>;

  // Gadget creation protocols
  export <gtk-mirror>,
         <widget-mirror>,
         <gadget-mirror>,
         make-gtk-mirror,
         install-event-handlers,
         install-named-handlers,
         update-mirror-attributes,
         set-mirror-parent,
         move-mirror,
         size-mirror;

  // These can be used by someone who wants to import their own GTK gadget
  export \event-handler-definer,
         <gtk-pane-mixin>,
         <gtk-gadget-mixin>,
         <gtk-text-gadget-mixin>,
         <gtk-top-level-sheet-mixin>,
         handle-gtk-destroy-event,
         handle-gtk-delete-event,
         handle-gtk-motion-event,
         handle-gtk-button-press-event,
         handle-gtk-button-release-event,
         handle-gtk-key-press-event,
         handle-gtk-key-release-event,
         handle-gtk-configure-event,
         handle-gtk-expose-event,
         handle-gtk-enter-event,
         handle-gtk-leave-event,
         handle-gtk-clicked-event,
         handle-gtk-select-row-event,
         handle-gtk-click-column-event,
         handle-gtk-resize-column-event;

  // Other possibly useful exports
  export shutdown-gtk-duim;

end module gtk-duim;
