Module:    midi
Synopsis:  Raw MIDI interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <midi-device> (<object>) end;

define open abstract class <midi-channel> (<object>) end;

define open generic default-midi-device () => (md :: <midi-device>);

define open generic open-midi-device (md :: <midi-device>) => ();
define open generic reset-midi-device (md :: <midi-device>) => ();
define open generic close-midi-device (md :: <midi-device>) => ();

define inline method do-with-open-midi-device (f :: <function>, md :: <midi-device>)
  let open? = #f;
  block ()
    open-midi-device(md);
    open? := #t;
    f();
  cleanup
    if (open?)
      close-midi-device(md);
    end;
  end;
end method;

define macro with-open-midi-device 
  { with-open-midi-device (?md:expression) ?:body end }
    => { do-with-open-midi-device(method () ?body end, ?md) }
end macro;

define open generic select-midi-instrument 
    (md :: <midi-device>, mc :: <midi-channel>, mv :: <midi-instrument>) => ();

// eof
