Module:    dylan-user
Synopsis:  Raw MIDI interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module midi
  use dylan;
  use win32-common;
  use win32-multimedia;
  use c-ffi;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use simple-random;

  // Raw MIDI spec

  export
    $MIDI-off, 
    $MIDI-on, 
    $MIDI-key-pressure, 
    $MIDI-parameter, 
    $MIDI-patch, 
    $MIDI-channel-pressure, 
    $MIDI-pitch-bend,

    send-midi-message,

    <midi-pitch>, 
      $midi-min-pitch,
      $midi-middle-c-pitch,
      $midi-max-pitch,

    <midi-velocity>,
      $midi-min-velocity,
      $midi-max-velocity,

    <midi-pitch-bend>,
      $midi-pitch-bend-full-up,
      $midi-pitch-bend-central,
      $midi-pitch-bend-full-down,

    $midi-balance-parameter,

    <midi-balance>,
      $midi-balance-full-left,
      $midi-balance-central,
      $midi-balance-full-right,

    <midi-instrument>, 
      midi-name, midi-number, do-midi-instruments,

    <midi-instrument-family>,
      midi-name, midi-members,
      do-midi-instrument-families,
      do-midi-instrument-family-members,
    
    find-midi-instrument,

    <midi-percussion>,
      midi-name, midi-number, do-midi-percussion,

    find-midi-percussion;

  // Higher level MIDI protocol

  export
    <midi-device>, <midi-channel>,
      default-midi-device,
      midi-id,
      open-midi-device, reset-midi-device, close-midi-device, 
        with-open-midi-device,
      midi-channels,
      select-midi-instrument;

  export
    midi-on, midi-off, midi-pitch-bend;

end module midi;
