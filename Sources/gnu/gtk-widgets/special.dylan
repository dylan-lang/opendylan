Module:    gtk-widgets
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Type coercion functions

define gtk-type-cast-function GTK-LABEL         => <GtkLabel*>;
define gtk-type-cast-function GTK-BUTTON        => <GtkButton*>;
define gtk-type-cast-function GTK-TOGGLE-BUTTON => <GtkToggleButton*>;
define gtk-type-cast-function GTK-CHECK-BUTTON  => <GtkCheckButton*>;
define gtk-type-cast-function GTK-RADIO-BUTTON  => <GtkRadioButton*>;
define gtk-type-cast-function GTK-ENTRY         => <GtkEntry*>;
define gtk-type-cast-function GTK-TEXT          => <GtkText*>;
define gtk-type-cast-function GTK-CLIST         => <GtkCList*>;
define gtk-type-cast-function GTK-HSCALE        => <GtkHScale*>;
define gtk-type-cast-function GTK-VSCALE        => <GtkVScale*>;
define gtk-type-cast-function GTK-HSCROLLBAR    => <GtkHScrollbar*>;
define gtk-type-cast-function GTK-VSCROLLBAR    => <GtkVScrollbar*>;
define gtk-type-cast-function GTK-MENU          => <GtkMenu*>;
define gtk-type-cast-function GTK-MENU-BAR      => <GtkMenuBar*>;
define gtk-type-cast-function GTK-MENU-ITEM     => <GtkMenuItem*>;


/// Opaque structures

// Since it doesn't matter what an opaque structure is, make it an int
define inline constant <C-opaque-structure> = <C-signed-int>;

// These are opaque GTK structures
define inline constant <_GtkLabelWord> = <C-opaque-structure>;
define inline constant <_GtkTextFont>  = <C-opaque-structure>;
