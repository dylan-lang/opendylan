Module:    midi
Synopsis:  Raw midi interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed concrete class <win32-midi-device> (<midi-device>)
  constant slot midi-id :: <integer>,
    required-init-keyword: id:;
  constant slot midi-channels :: <simple-object-vector>,
    required-init-keyword: channels:;
  slot midi-open? :: <boolean> = #f;
  slot midi-handle :: <HMIDIOUT>;
end class;

define sealed concrete class <win32-midi-channel> (<midi-channel>)
  constant slot midi-number :: <integer>,
    required-init-keyword: number:;
  slot midi-current-instrument :: <midi-instrument>,
    required-init-keyword: current-instrument:;
end class;

define constant $default-instrument = find-midi-instrument("Acoustic Grand Piano");

define constant $midi-mapper-device
  = make(<win32-midi-device>, 
         id: as(<integer>, $MIDIMAPPER), 
         channels: map-as(<simple-object-vector>,
                          method (number) 
                            make(<win32-midi-channel>, 
                                 number: number,
                                 current-instrument:  $default-instrument)
                          end,
                          range(from: 0, below: 16)));

define sealed method default-midi-device () => (md :: <win32-midi-device>)
  $midi-mapper-device
end method;

define sealed method open-midi-device (md :: <win32-midi-device>) => ()
  if (midi-open?(md))
    error("The MIDI device %= cannot be opened because it is already open.", md);
  end;
  let (code, handle) = midiOutOpen(midi-id(md), 0, 0, 0);
  if (code ~== 0)
    error("Failed to open midi device %=: %s", md, midi-error-string(code));
  end;
  midi-handle(md) := handle;
  midi-open?(md)  := #t;
end method;

define sealed method reset-midi-device (md :: <win32-midi-device>) => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot be reset because it is not open.", md);
  end;
  midiOutReset(midi-handle(md));
end method;

define sealed method close-midi-device (md :: <win32-midi-device>) => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot be closed because it is not open.", md);
  end;
  midi-open?(md) := #f;
  midiOutClose(midi-handle(md));
end method;

define sealed method select-midi-instrument
    (md :: <win32-midi-device>, mc :: <win32-midi-channel>, mv :: <midi-instrument>) => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot used because it is not open.", md);
  end;
  send-midi-message
    (midi-handle(md), $MIDI-patch, midi-number(mc), midi-number(mv), 0);
end method;

define sealed method midi-on 
    (md :: <win32-midi-device>, mc :: <win32-midi-channel>, 
       pitch :: <midi-pitch>, velocity :: <midi-velocity>) 
 => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot used because it is not open.", md);
  end;
  send-midi-message
    (midi-handle(md), $MIDI-on, midi-number(mc), pitch, velocity);
end method;

define sealed method midi-off
    (md :: <win32-midi-device>, mc :: <win32-midi-channel>, 
       pitch :: <midi-pitch>)
 => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot used because it is not open.", md);
  end;
  send-midi-message
    (midi-handle(md), $MIDI-off, midi-number(mc), pitch, 0);
end method;

define sealed method midi-pitch-bend
    (md :: <win32-midi-device>, mc :: <win32-midi-channel>, 
       bend :: <integer>)
 => ()
  if (~midi-open?(md))
    error("The MIDI device %= cannot used because it is not open.", md);
  end;
  let lo = logand(bend, #x7f);
  let hi = ash(bend, -7);
  send-midi-message
    (midi-handle(md), $MIDI-pitch-bend, midi-number(mc), lo, hi);
end method;

//// Utilities

define constant $max-error-string-size = 255;

define method midi-error-string (id :: <integer>) => (message :: <byte-string>)
  with-stack-structure (buf :: <C-string>, element-count: $max-error-string-size)
    midiOutGetErrorText(id, buf, $max-error-string-size);
    as(<byte-string>, buf);
  end;
end method;

define method send-midi-message 
    (handle :: <HMIDIOUT>, status :: <integer>, channel :: <integer>, 
       data1 :: <integer>, data2 :: <integer>) 
 => ()
  let code 
    = midiOutShortMsg(handle, logior(status, channel, ash(data1, 8), ash(data2, 16)));
  if (code ~== 0)
    error("Error sending MIDI message: %s.", midi-error-string(code));
  end;
  code
end method;

define method send-midi-message 
    (md :: <midi-device>, status :: <integer>, channel :: <midi-channel>, 
       data1 :: <integer>, data2 :: <integer>) 
 => ()
  send-midi-message
    (midi-handle(md), status, midi-number(channel), data1, data2);
end method;

// eof

