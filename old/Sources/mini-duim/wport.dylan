Module:    win32-duim
Synopsis:  Win32 port implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 ports

define sealed class <win32-port> (<basic-port>)
  slot port-metrics :: <win32-metrics>;
  slot %cursor-cache = make(<table>);
end class <win32-port>;

define method initialize (_port :: <win32-port>, #key) => ()
  next-method();
  register-window-classes();
  _port.port-metrics := make-win32-metrics();
end method initialize;

define sideways method class-for-make-port
    (type == #"win32", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<win32-port>, concatenate(initargs, #(event-processor-type:, #"n")))
end method class-for-make-port;

define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"win32", initargs)
end method class-for-make-port;

define method port-type (_port :: <win32-port>) => (type :: <symbol>)
  #"win32"
end method port-type;

define method port-name (_port :: <win32-port>) => (name :: false-or(<string>))
  #f
end method port-name;


/// Port metrics

define method port-metrics
    (client) => (metrics :: <win32-metrics>)
  let _port = port(client);
  if (_port)
    port-metrics(_port)
  else
    error("Port metrics called for unattached %=", client)
  end
end method port-metrics;


// Useful trampolines
define method win32-dialog-base-units
    (client) => (units :: <integer>)
  win32-dialog-base-units(port-metrics(client))
end method win32-dialog-base-units;

define method win32-mouse-buttons
    (client) => (units :: <integer>)
  win32-mouse-buttons(port-metrics(client))
end method win32-mouse-buttons;

define method win32-pixels-per-inch
    (client) => (units :: <integer>)
  win32-pixels-per-inch(port-metrics(client))
end method win32-pixels-per-inch;


/// Pointer position hacking

define method do-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <coordinate>, y :: <coordinate>)
  let (dx, dy) = sheet-screen-position(_port, sheet);
  with-stack-structure (point :: <PPOINT>)
    //---*** Not yet exported: GetCursorPos(point);
    values(point.x-value - dx, point.y-value - dy)
  end
end method do-pointer-position;

define method do-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <coordinate>, y :: <coordinate>)
  with-stack-structure (point :: <PPOINT>)
    //---*** Not yet exported: GetCursorPos(point);
    values(point.x-value, point.y-value)
  end
end method do-pointer-position;

define method do-set-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <coordinate>, y :: <coordinate>) => ()
  let (dx, dy) = sheet-screen-position(_port, sheet);
  //---*** Not yet exported: SetCursorPos(fix-coordinate(x + dx), fix-coordinate(y + dy))
end method do-set-pointer-position;

define method do-set-pointer-position
    (_port :: <win32-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <coordinate>, y :: <coordinate>) => ()
  //---*** Not yet exported: SetCursorPos(fix-coordinate(x), fix-coordinate(y))
end method do-set-pointer-position;


/// Pointer cursor hacking

define table $cursor-table :: <table>
  #"default"           => $IDC-ARROW;
  #"vertical-scroll"   => $IDC-SIZENS;
  #"scroll-up"         => $IDC-ARROW;
  #"scroll-down"       => $IDC-ARROW;
  #"horizontal-scroll" => $IDC-SIZEWE;
  #"scroll-left"       => $IDC-ARROW;
  #"scroll-right"      => $IDC-ARROW;
  #"busy"              => $IDC-WAIT;
  #"upper-left"        => $IDC-SIZENWSE;
  #"upper-right"       => $IDC-SIZENESW;
  #"lower-left"        => $IDC-SIZENESW;
  #"lower-right"       => $IDC-SIZENWSE;
  #"vertical-thumb"    => $IDC-ARROW;
  #"horizontal-thumb"  => $IDC-ARROW;
  #"button"            => $IDC-ARROW;
  #"prompt"            => $IDC-ARROW;
  #"move"              => $IDC-CROSS;
  #"position"          => $IDC-CROSS;
  #"i-beam"            => $IDC-IBEAM;
end table $cursor-table;

define method do-set-pointer-cursor
    (_port :: <win32-port>, pointer :: <pointer>, cursor) => ()
  let hCursor :: <HCURSOR> = realize-cursor(_port, cursor);
  //---*** Not yet exported: SetCursor(hCursor)
end method do-set-pointer-cursor;

define method do-set-sheet-pointer-cursor
    (_port :: <win32-port>, sheet :: <sheet>, cursor) => ()
  //---*** What does the "sheet cursor" mean in Windows?
  let hCursor :: <HCURSOR> = realize-cursor(_port, cursor);
  //---*** Not yet exported: SetCursor(hCursor)
end method do-set-sheet-pointer-cursor;

define method realize-cursor
    (_port :: <win32-port>, cursor :: <symbol>) => (hCursor :: <HCURSOR>)
  realize-cursor(_port, gethash($cursor-table, cursor) | $IDC-ARROW)
end method realize-cursor;

define method realize-cursor
    (_port :: <win32-port>, cursor :: <integer>) => (hCursor :: <HCURSOR>)
  gethash(_port.%cursor-cache, cursor)
  | begin
      let hCursor = LoadCursor($null-hInstance, cursor);
      if (null-handle?(hCursor))
	hCursor := LoadCursor($null-hInstance, $IDC-ARROW)
      end;
      gethash(_port.%cursor-cache, cursor) := hCursor;
      hCursor
    end
end method realize-cursor;
