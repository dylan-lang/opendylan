Module:    gdk
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Since it doesn't matter what an opaque structure is, make it an int
define inline constant <C-opaque-structure> = <C-signed-int>;

// These are opaque GDK structures
define inline constant <_GdkIC>            = <C-opaque-structure>;
define inline constant <_GdkDevicePrivate> = <C-opaque-structure>;
