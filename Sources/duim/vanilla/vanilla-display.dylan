Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	      Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method initialize-display 
    (_port :: <vanilla-port>, _display :: <display>) => ()
  //--- Mirror the display, set its region, and set its characteristics
  ignoring("initialize-display")
end method initialize-display;
