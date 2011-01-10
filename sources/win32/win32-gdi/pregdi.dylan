Module:    Win32-GDI
Synopsis:  Declarations to load first.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Need these to be open generics because methods are also
// defined in the "ole-controls" library:
define open-accessor xExt-value;
define open-accessor yExt-value;
define open-accessor dwType-value;

// Also defined in "win32-dialog":
define open-accessor lStructSize-value;
