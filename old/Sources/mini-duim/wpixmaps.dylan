Module:    win32-duim
Synopsis:  Win32 pixmap implementation
Author:    David Gray, Scott McKay, Andrew Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <win32-pixmap> (<pixmap>)
  slot %pixmap,
    init-keyword: pixmap:;
  slot %medium :: <medium>, 
    init-keyword: medium:;
end class <win32-pixmap>;

//--- A little wierd that this is called 'medium-drawable', but what the heck
define method medium-drawable
    (pixmap :: <win32-pixmap>) => (drawable)
  pixmap.%pixmap
end method medium-drawable;

define method image-width 
    (pixmap :: <win32-pixmap>) => (width :: <integer>)
  //---*** do it
  20
end method image-width;

define method image-height 
    (pixmap :: <win32-pixmap>) => (height :: <integer>)
  //---*** do it
  20
end method image-height;

define method image-depth 
    (pixmap :: <win32-pixmap>) => (depth :: <integer>)
  //---*** do it
  1
end method image-depth;

//--- Should all pixmaps just keep a track of their medium anyway?
define method port 
    (pixmap :: <win32-pixmap>) => (port :: <port>)
  port(pixmap.%medium)
end method port;


define method do-make-pixmap
    (_port :: <win32-port>, medium :: <win32-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <win32-pixmap>)
  //---*** do this properly!
  make(<win32-pixmap>)
end method do-make-pixmap;

define method do-destroy-pixmap
    (_port :: <win32-port>, pixmap :: <win32-pixmap>) => ()
  //---*** do this properly
  #f
end method do-destroy-pixmap;


define sealed class <win32-pixmap-medium>
    (<win32-medium>, 
     <basic-pixmap-medium>)
  slot %pixmap, init-keyword: pixmap:;
  slot %medium, init-keyword: medium:;
end class <win32-pixmap-medium>;

define method make-pixmap-medium
    (_port :: <win32-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <win32-pixmap-medium>)
  //---*** do it
  error("make-pixmap-medium not implemented yet!")
end method make-pixmap-medium;


define method do-copy-area
    (from :: <win32-pixmap>, from-x, from-y, width, height,
     to :: <win32-pixmap>,   to-x,   to-y,   function) => ()
  //---*** do it
end method do-copy-area;

define method do-copy-area
    (from :: <win32-pixmap>, from-x, from-y, width, height,
     to :: <win32-medium>,   to-x,   to-y,   function) => ()
  //---*** do it
end method do-copy-area;

define method do-copy-area
    (from :: <win32-medium>, from-x, from-y, width, height,
     to :: <win32-pixmap>, to-x, to-y, function) => ()
  //---*** do it
end method do-copy-area;

define method do-copy-area
    (from :: <win32-medium>, from-x, from-y, width, height,
     to :: <win32-medium>, to-x, to-y, function) => ()
  //---*** do it
end method do-copy-area;

