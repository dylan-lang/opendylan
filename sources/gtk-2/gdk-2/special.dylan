Module:        GDK-2
Synopsis:      Manually coded additions to the automatic translation.
Copyright:     Copyright (C) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-License:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <C-int**> => <C-int*>;
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
