Module:    gtk-widgets
Synopsis:  Manually coded additions to the automatic translation.
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro button-press-callback-definer
  { define button-press-callback ?new:name = ?old:name }
 => { define gtk-callback (?new, <GdkEventButton*>) = ?old }
end macro button-press-callback-definer;
