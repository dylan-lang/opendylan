Module:    glib
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//---*** Hand code these, since they aren't generally useful
define C-pointer-type <C-string**> => <C-string*>;
define C-pointer-type <gchar***> => <gchar**>;

//---*** Hack until I can really get these to parse!
define inline constant $GLIB-SYSDEF-POLLIN	= 1;
define inline constant $GLIB-SYSDEF-POLLOUT	= 4;
define inline constant $GLIB-SYSDEF-POLLPRI	= 2;
define inline constant $GLIB-SYSDEF-POLLERR	= 8;
define inline constant $GLIB-SYSDEF-POLLHUP	= 16;
define inline constant $GLIB-SYSDEF-POLLNVAL	= 32;

//---*** Not sure how best to treat these...
define inline constant <GAllocator*>   = <gpointer>;
define inline constant <GCache*>       = <gpointer>;
define inline constant <GCond*>        = <gpointer>;
define inline constant <GData*>        = <gpointer>;
define inline constant <GHashTable*>   = <gpointer>;
define inline constant <GMainLoop*>    = <gpointer>;
define inline constant <GMemChunk*>    = <gpointer>;
define inline constant <GModule*>      = <gpointer>;
define inline constant <GMutex*>       = <gpointer>;
define inline constant <GPrivate*>     = <gpointer>;
define inline constant <GRelation*>    = <gpointer>;
define inline constant <GStringChunk*> = <gpointer>;
define inline constant <GTimer*>       = <gpointer>;
define inline constant <GTree*>        = <gpointer>;

define C-pointer-type <GAllocator**>   => <GAllocator*>;
define C-pointer-type <GCache**>       => <GCache*>;
define C-pointer-type <GCond**>        => <GCond*>;
define C-pointer-type <GData**>        => <GData*>;
define C-pointer-type <GHashTable**>   => <GHashTable*>;
define C-pointer-type <GMainLoop**>    => <GMainLoop*>;
define C-pointer-type <GMemChunk**>    => <GMemChunk*>;
define C-pointer-type <GMutex**>       => <GMutex*>;
define C-pointer-type <GModule**>      => <GModule*>;
define C-pointer-type <GPrivate**>     => <GPrivate*>;
define C-pointer-type <GRelation**>    => <GRelation*>;
define C-pointer-type <GStringChunk**> => <GStringChunk*>;
define C-pointer-type <GTimer**>       => <GTimer*>;
define C-pointer-type <GTree**>        => <GTree*>;
