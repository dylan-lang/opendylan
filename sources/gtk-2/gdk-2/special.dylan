Module:        GDK-2
Synopsis:      Manually coded additions to the automatic translation.
Copyright:     Copyright (C) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-License:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <C-raw-int**> => <C-raw-int*>;
define C-pointer-type <GdkTimeCoord***> => <GdkTimeCoord**>;

define opaque-structure <_GdkDeviceClass>;
define opaque-structure <_GdkDisplayManager>;
define opaque-structure <_GdkPangoRendererPrivate>;
define opaque-structure <_GdkRegion>;
define opaque-structure <_GdkVisualClass>;

// Slightly weird stuff going on here.
define opaque-structure <_GdkAtom>;
define C-pointer-type <GdkAtom> => <_GdkAtom>;
define C-pointer-type <GdkAtom*> => <GdkAtom>;
define C-pointer-type <GdkAtom**> => <GdkAtom*>;

// We're defining this one manually because the translation
// script will choke on function pointer arguments.
define inline-only C-function gdk-window-invalidate-maybe-recurse
  parameter window1     :: <GdkWindow*>;
  parameter region2     :: <GdkRegion*>;
  parameter child_func3 :: <C-function-pointer>;
  parameter user_data4  :: <gpointer>;
  c-name: "gdk_window_invalidate_maybe_recurse";
end;

// This one has output-parameters, so we define it manually:
define inline-only C-function gdk-window-get-pointer
  input  parameter window1              :: <GdkWindow*>;
  output parameter x2            :: <gint*>;
  output parameter y3            :: <gint*>;
  output parameter mask4         :: <GdkModifierType*>;
  result value                   :: <GdkWindow*>;
  c-name: "gdk_window_get_pointer";
end;

