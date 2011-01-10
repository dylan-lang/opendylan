Author:    Andy Armstrong
Synopsis:  A Dylan application to play around in
Module:    dylan-playground
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main ()
  format-out("The Dylan playground.\n\n"
	       "If you see this message you have probably started the "
	       "dylan-playground project unintentionally.  Use the "
	       "Open Playground command in the Help menu to interact "
	       "with the dylan-playground instead.");
end method main;

main();
