Module:    gtk-common
Synopsis:  Additional declarations to be loaded before the automatically
	   converted files.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The following slot accessor functions need to be declared as open generics
// because they have methods defined in more than one library. 

define open-accessor accel-group-value;
define open-accessor callback-value;
define open-accessor callback-data-value;
define open-accessor changed-value;
define open-accessor child-value;
define open-accessor draw-slider-value;
