Module:       win32-scribble
Author:       Scott McKay
Synopsis:     Win32 version of the DUIM scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Run the Scribble application as a top-level application

define method scribble 
    (#key container, container-region)
 => (frame :: <scribble-frame>)
  let frame
    = make(<scribble-frame>,
	   container: container,
	   container-region: container-region,
	   title: "Scribble");
  //--- Do we need any other initializations?
  start-frame(frame);
  frame
end method scribble;

scribble();
