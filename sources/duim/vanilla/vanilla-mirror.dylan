Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//--- This class wraps up the real window system object
define sealed class <vanilla-mirror> (<mirror>)
  sealed slot mirror-sheet :: <sheet>,
    init-keyword: sheet:;
  sealed slot %ink-cache :: <object-table> = make(<table>);
  sealed slot %region :: <region>, 
    init-keyword: region:;
end class <vanilla-mirror>;

define method do-make-mirror
    (_port :: <vanilla-port>, sheet :: <sheet>) => (mirror)
  let (left, top, right, bottom) = sheet-native-edges(sheet);
  //--- Call compute-default-foreground/background/text-style to
  //--- figure out what characteristics the mirror should have
  let mirror = make(<vanilla-mirror>,
		    sheet: sheet,
		    region: make-bounding-box(left, top, right, bottom));
  //--- Initialize the mirror here
  mirror
end method do-make-mirror;


define method destroy-mirror 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>) => ()
  //--- Deallocate all window system resources
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method map-mirror 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>) => ()
  //--- Do it
end method map-mirror;

define method unmap-mirror 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>) => ()
  //--- Do it
end method unmap-mirror;

define method raise-mirror 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>,
     #key activate? = #t) => ()
  //--- Do it
end method raise-mirror;

define method lower-mirror 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>) => ()
  //--- Do it
end method lower-mirror;

define method mirror-visible? 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>)
 => (visible? :: <boolean>)
  //--- Do it
  #t
end method mirror-visible?;


// Returns the edges of the mirror in its parent's coordinate space
define method mirror-edges 
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(mirror.%region)
end method mirror-edges;

// Sets the edges of the mirror in its parent's coordinate space
define method set-mirror-edges
    (_port :: <vanilla-port>, sheet :: <sheet>, mirror :: <vanilla-mirror>
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  mirror.%region := set-box-edges(mirror.%region, left, top, right, bottom)
end method set-mirror-edges;
