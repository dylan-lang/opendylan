Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <vanilla-pixmap> (<pixmap>)
  sealed slot %pixmap,
    init-keyword: pixmap:;
  sealed slot %medium :: <medium>, 
    init-keyword: medium:;
end class <vanilla-pixmap>;

//--- A little wierd that this is called 'medium-drawable', but what the heck
define method medium-drawable
    (pixmap :: <vanilla-pixmap>) => (drawable)
  pixmap.%pixmap
end method medium-drawable;

define method image-width 
    (pixmap :: <vanilla-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-width;

define method image-height 
    (pixmap :: <vanilla-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-height;

define method image-depth 
    (pixmap :: <vanilla-pixmap>) => (width :: <integer>)
  //--- Do it
end method image-depth;

//---*** Should all pixmaps just keep a track of their medium anyway?
define method port 
    (pixmap :: <vanilla-pixmap>) => (port :: <port>)
  port(pixmap.%medium)
end method port;


define method do-make-mirror
    (_port :: <vanilla-port>, sheet :: <pixmap-sheet>)
 => (mirror :: <vanilla-mirror>)
  //--- Do it, or maybe you don't need to do anything
end method do-make-mirror;


define method do-make-pixmap
    (_port :: <vanilla-port>, medium :: <vanilla-medium>, 
     width :: <integer>, height :: <integer>)
 => (pixmap :: <vanilla-pixmap>)
  //--- Do it
end method do-make-pixmap;

define method destroy-pixmap
    (pixmap :: <vanilla-pixmap>) => ()
  //--- Do it
end method destroy-pixmap;


define sealed class <vanilla-pixmap-medium>
    (<vanilla-medium>, 
     <basic-pixmap-medium>)
  sealed slot %pixmap, init-keyword: pixmap:;
  sealed slot %medium, init-keyword: medium:;
end class <vanilla-pixmap-medium>;

define method make-pixmap-medium
    (_port :: <vanilla-port>, sheet :: <sheet>, #key width, height)
 => (medium :: <vanilla-pixmap-medium>)
  //--- Do it
end method make-pixmap-medium;


define method do-copy-area
    (from :: <vanilla-pixmap>, from-x, from-y, width, height,
     to :: <vanilla-pixmap>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <vanilla-pixmap>, from-x, from-y, width, height,
     to :: <vanilla-medium>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <vanilla-medium>, from-x, from-y, width, height,
     to :: <vanilla-pixmap>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;

define method do-copy-area
    (from :: <vanilla-medium>, from-x, from-y, width, height,
     to :: <vanilla-medium>, to-x, to-y,
     #key function = $boole-1) => ()
  //--- Do it
end method do-copy-area;
