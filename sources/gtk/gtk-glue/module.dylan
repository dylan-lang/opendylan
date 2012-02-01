Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
copyright: Original Code is Copyright (c) 2007 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define module gtk-support
  use common-dylan;
  use c-ffi;
  use gtk-internal, export: all;
  use finalization;
  use dylan-primitives;
  use dylan-extensions, import: { debug-name, integer-as-raw, raw-as-integer };

  export $G-TYPE-INVALID, $G-TYPE-NONE,
    $G-TYPE-CHAR, $G-TYPE-UCHAR, $G-TYPE-BOOLEAN,
    $G-TYPE-INT, $G-TYPE-UINT, $G-TYPE-LONG,
    $G-TYPE-ULONG, $G-TYPE-INT64, $G-TYPE-UINT64,
    $G-TYPE-ENUM, $G-TYPE-FLAGS, $G-TYPE-FLOAT,
    $G-TYPE-DOUBLE, $G-TYPE-STRING, $G-TYPE-POINTER,
    $G-TYPE-BOXED, $G-TYPE-PARAM, $G-TYPE-OBJECT;

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
