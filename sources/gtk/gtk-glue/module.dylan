Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
Copyright: (C) 2007,.  All rights reserved.

define module gtk-support
  use common-dylan;
  use c-ffi;
  use gtk-internal, export: all;
  use finalization;
  use dylan-primitives;
  use dylan-extensions, import: { debug-name, integer-as-raw, raw-as-integer };

  export g-signal-connect, initialize-gtk,
    gtk-widget-get-window,
    gtk-widget-get-state,
    gtk-widget-get-allocation,
    gtk-dialog-get-vbox,
    gtk-dialog-get-action-area,
    gtk-menu-shell-set-ignore-enter,
    popup-gtk-menu,
    gtk-set-button-time,
    g-value-nullify,
    g-value-to-dylan,
    g-value-set-value,
    property-getter-definer,
    property-setter-definer,
    \with-gdk-lock;
end;  
