Module:        GLib-2
Synopsis:      Manually coded additions to the automatic translation.
Copyright:     Copyright (C) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-License:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <C-string**> => <C-string*>;
define C-pointer-type <gchar***> => <gchar**>;
define inline constant <size-t> = <gsize>;

// These are defined in a strange way that doesn't parse.
define inline constant $GLIB-SYSDEF-POLLIN	= 1;
define inline constant $GLIB-SYSDEF-POLLOUT	= 4;
define inline constant $GLIB-SYSDEF-POLLPRI	= 2;
define inline constant $GLIB-SYSDEF-POLLERR	= 8;
define inline constant $GLIB-SYSDEF-POLLHUP	= 16;
define inline constant $GLIB-SYSDEF-POLLNVAL	= 32;


// The following types are opaque structs whose definitions
// do not appear in the header files.  We need to handle
// these specially or they'll end up being undefined.

define macro opaque-structure-definer
  { define opaque-structure ?:name }
    // It doesn't matter what type an opaque structure is,
    // because it's never supposed to be used directly.
    => { define inline constant ?name = <C-signed-int> }
end macro opaque-structure-definer;

define opaque-structure <_GAllocator>;
// define opaque-structure <_GCache>;
define opaque-structure <_GCond>;
define opaque-structure <_GData>;
define opaque-structure <_GHashTable>;
define opaque-structure <_GMainContext>;
define opaque-structure <_GMainLoop>;
define opaque-structure <_GMemChunk>;
define opaque-structure <_GModule>;
define opaque-structure <_GMutex>;
define opaque-structure <_GOptionContext>;
define opaque-structure <_GOptionGroup>;
define opaque-structure <_GPrivate>;
// define opaque-structure <_GRelation>;
define opaque-structure <_GStringChunk>;
// define opaque-structure <_GTimer>;
define opaque-structure <_GTree>;

// Slightly weird stuff going on here.
define opaque-structure <_GIConv>;
define C-pointer-type <GIConv> => <_GIConv>;
define C-pointer-type <GIConv*> => <GIConv>;
