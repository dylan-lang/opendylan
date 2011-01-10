module: harp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// The definition of a vanilla GC class

define primary class <garbage-collector> (<object>)
end;

define constant plain-garbage-collector = make(<garbage-collector>);

