Module:    midi
Synopsis:  Raw midi interface
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// This file defines types and constants corresponding to the control
//// values documented in the basic MIDI spec, plus the General MIDI
//// instruments.

// TODO: This is very incomplete, and what is there is based on my novice
// understanding about MIDI! 

// TODO: MIDI numbering is often one-based in speech, but zero-based in
// implementation: we should probably do something to make it clear 
// which is being used. The parameters below define the zero-based
// implementation numberings.

//// MIDI status codes

define constant $MIDI-off              :: <integer> = #x80;
define constant $MIDI-on               :: <integer> = #x90;
define constant $MIDI-key-pressure     :: <integer> = #xA0;
define constant $MIDI-parameter        :: <integer> = #xB0;
define constant $MIDI-patch            :: <integer> = #xC0;
define constant $MIDI-channel-pressure :: <integer> = #xD0;
define constant $MIDI-pitch-bend       :: <integer> = #xE0;

//// MIDI pitch values

define constant $midi-min-pitch      = 0;
define constant $midi-middle-c-pitch = 60;
define constant $midi-max-pitch      = 127;

define constant <midi-pitch>
  = limited(<integer>,
            min: $midi-min-pitch,
            max: $midi-max-pitch);

//// MIDI velocity values

define constant $midi-min-velocity = 0;
define constant $midi-max-velocity = 127;

define constant <midi-velocity> 
  = limited(<integer>, 
            min: $midi-min-velocity, 
            max: $midi-max-velocity);

//// MIDI pitch bend values

define constant $midi-pitch-bend-full-down = 1;
define constant $midi-pitch-bend-central   = #x2000;
define constant $midi-pitch-bend-full-up   = #x3FFF;

define constant <midi-pitch-bend> 
  = limited(<integer>, 
            min: $midi-pitch-bend-full-down, 
            max: $midi-pitch-bend-full-up);

//// MIDI parameters

define constant $midi-balance-parameter = 10;

define constant $midi-balance-full-left = 0;
define constant $midi-balance-central = 64;
define constant $midi-balance-full-right = 127;


define constant <midi-balance>
  = limited(<integer>,
            min: $midi-balance-full-left,
            max: $midi-balance-full-right);

//// MIDI instruments

define sealed concrete class <midi-instrument> (<object>)
  constant slot midi-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot midi-number :: <integer>,
    required-init-keyword: number:;
end class;

define sealed concrete class <midi-instrument-family> (<object>)
  constant slot midi-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot midi-members :: <simple-object-vector>,
    required-init-keyword: members:;
end class;

define method do-midi-instrument-family-members 
    (f :: <function>, mvf :: <midi-instrument-family>)
  do(f, midi-members(mvf))
end method;

define constant *midi-instrument-families* :: <deque> = make(<deque>);

define method do-midi-instrument-families (f :: <function>)
  do(f, *midi-instrument-families*)
end method;

define constant *midi-instruments* :: <deque> = make(<deque>);

define method do-midi-instruments (f :: <function>)
  do(f, *midi-instruments*)
end method;

define method find-midi-instrument 
    (instrument-number :: <integer>, #key default = #f) => (maybe-instrument)
  element(*midi-instruments*, instrument-number, default: #f)
    | default
end method;

define constant *midi-instrument-table* :: <object-table> = make(<object-table>);

define method find-midi-instrument 
    (instrument-name :: <byte-string>, #key default = #f) => (maybe-instrument)
  element(*midi-instrument-table*, as(<symbol>, instrument-name), default: #f)
    | default
end method;

define method do-define-midi-instrument-family 
    (family-name :: <byte-string>, first-instrument-number :: <integer>,
       #rest member-names)
  let members = map-as(<simple-object-vector>,
                       method (name, number) 
                         let instrument = make(<midi-instrument>, name: name, number: number);
                         push-last(*midi-instruments*, instrument);
                         element(*midi-instrument-table*, as(<symbol>, name)) := instrument;
                         instrument
                       end,
                       member-names,
                       range(from: first-instrument-number));
  let family = make(<midi-instrument-family>, name: family-name, members: members);
  push-last(*midi-instrument-families*, family);
end method;

define macro midi-instrument-family-definer
  { define midi-instrument-family 
        ?family-name:expression from ?first-instrument-number:expression
      ?members:*
     end }
     => { do-define-midi-instrument-family(?family-name, ?first-instrument-number, ?members) }
members:
  { }
    => { }
  { instrument ?instrument-name:expression; ... }
    => { ?instrument-name, ... }
end macro;

define midi-instrument-family "Piano" from 0
  instrument "Acoustic Grand Piano";
  instrument "Bright Acoustic Piano";
  instrument "Electric Grand Piano";
  instrument "Honky-tonk Piano";
  instrument "Rhodes Piano";
  instrument "Chorused Piano";
  instrument "Harpsichord";
  instrument "Clavinet";
end midi-instrument-family;

define midi-instrument-family "Chromatic Percussion" from 8
  instrument "Celesta";
  instrument "Glockenspiel";
  instrument "Music box";
  instrument "Vibraphone";
  instrument "Marimba";
  instrument "Xylophone";
  instrument "Tubular Bells";
  instrument "Dulcimer";
end midi-instrument-family;

define midi-instrument-family "Organ" from 16
  instrument "Hammond Organ";
  instrument "Percussive Organ";
  instrument "Rock Organ";
  instrument "Church Organ";
  instrument "Reed Organ";
  instrument "Accordian";
  instrument "Harmonica";
  instrument "Tango Accordian";
end midi-instrument-family;

define midi-instrument-family "Guitar" from 24
  instrument "Acoustic Guitar (nylon)";
  instrument "Acoustic Guitar (steel)";
  instrument "Electric Guitar (jazz)";
  instrument "Electric Guitar (clean)";
  instrument "Electric Guitar (muted)";
  instrument "Overdriven Guitar";
  instrument "Distortion Guitar";
  instrument "Guitar Harmonics";
end midi-instrument-family;

define midi-instrument-family "Bass" from 32
  instrument "Acoustic Bass";
  instrument "Electric Bass (finger)";
  instrument "Electric Bass (pick)";
  instrument "Fretless Bass";
  instrument "Slap Bass 1";
  instrument "Slap Bass 2";
  instrument "Synth Bass 1";
  instrument "Synth Bass 2";
end midi-instrument-family;

define midi-instrument-family "Strings" from 40
  instrument "Violin";
  instrument "Viola";
  instrument "Cello";
  instrument "Contrabass";
  instrument "Tremolo Strings";
  instrument "Pizzicato Strings";
  instrument "Orchestral Harp";
  instrument "Timpani";
end midi-instrument-family;

define midi-instrument-family "Ensemble" from 48
  instrument "String Ensemble 1";
  instrument "String Ensemble 2";
  instrument "Synth Strings 1";
  instrument "Synth Strings 2";
  instrument "Choir Aahs";
  instrument "Instrument Oohs";
  instrument "Synth Instrument";
  instrument "Orchestra Hit";
end midi-instrument-family;

define midi-instrument-family "Brass" from 56
  instrument "Trumpet";
  instrument "Trombone";
  instrument "Tuba";
  instrument "Muted Trumpet";
  instrument "French Horn";
  instrument "Brass Section";
  instrument "Synth Brass 1";
  instrument "Synth Brass 2";
end midi-instrument-family;

define midi-instrument-family "Reed" from 64
  instrument "Soprano Sax";
  instrument "Alto Sax";
  instrument "Tenor Sax";
  instrument "Baritone Sax";
  instrument "Oboe";
  instrument "English Horn";
  instrument "Bassoon";
  instrument "Clarinet";
end midi-instrument-family;

define midi-instrument-family "Pipe" from 72
  instrument "Piccolo";
  instrument "Flute";
  instrument "Recorder";
  instrument "Pan Flute";
  instrument "Bottle Blow";
  instrument "Shakuhachi";
  instrument "Whistle";
  instrument "Ocarina";
end midi-instrument-family;

define midi-instrument-family "Synth Lead" from 80
  instrument "Lead 1 (square)";
  instrument "Lead 2 (sawtooth)";
  instrument "Lead 3 (caliope lead)";
  instrument "Lead 4 (chiff lead)";
  instrument "Lead 5 (charang)";
  instrument "Lead 6 (instrument)";
  instrument "Lead 7 (fifths)";
  instrument "Lead 8 (brass + lead)";
end midi-instrument-family;

define midi-instrument-family "Synth Pad" from 88
  instrument "Pad 1 (new age)";
  instrument "Pad 2 (warm)";
  instrument "Pad 3 (polysynth)";
  instrument "Pad 4 (choir)";
  instrument "Pad 5 (bowed)";
  instrument "Pad 6 (metallic)";
  instrument "Pad 7 (halo)";
  instrument "Pad 8 (sweep)";
end midi-instrument-family;

define midi-instrument-family "Synth Effects" from 96
  instrument "FX 1 (rain)";
  instrument "FX 2 (soundtrack)";
  instrument "FX 3 (crystal)";
  instrument "FX 4 (atmosphere)";
  instrument "FX 5 (brightness)";
  instrument "FX 6 (goblins)";
  instrument "FX 7 (echoes)";
  instrument "FX 8 (sci-fi)";
end midi-instrument-family;

define midi-instrument-family "Ethnic" from 104
  instrument "Sitar";
  instrument "Banjo";
  instrument "Shamisen";
  instrument "Koto";
  instrument "Kalimba";
  instrument "Bagpipe";
  instrument "Fiddle";
  instrument "Shanai";
end midi-instrument-family;

define midi-instrument-family "Percussive" from 112
  instrument "Tinkle Bell";
  instrument "Agogo";
  instrument "Steel Drums";
  instrument "Woodblock";
  instrument "Taiko Drum";
  instrument "Melodic Tom";
  instrument "Synth Drum";
  instrument "Reverse Cymbal";
end midi-instrument-family;

define midi-instrument-family "Sound Effects" from 120
  instrument "Guitar Fret Noise";
  instrument "Breath Noise";
  instrument "Seashore";
  instrument "Bird Tweet";
  instrument "Telephone Ring";
  instrument "Helicopter";
  instrument "Applause";
  instrument "Gunshot";
end midi-instrument-family;

//// MIDI percussion

define sealed concrete class <midi-percussion> (<object>)
  constant slot midi-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot midi-number :: <integer>,
    required-init-keyword: number:;
end class;

define constant *midi-percussion* :: <deque> 
  = make(<deque>);

define constant *midi-percussion-table* :: <object-table> 
  = make(<object-table>);

define method do-define-midi-percussion 
    (first-number :: <integer>, #rest sound-names) => ()
  for (number from first-number, sound-name in sound-names)
    let object = make(<midi-percussion>, 
                      name:   sound-name,
                      number: number);
    push-last(*midi-percussion*, object);
    *midi-percussion-table*[as(<symbol>, sound-name)] := object;
  end;
end method;

define method find-midi-percussion
    (sound-number :: <integer>, #key default = #f) => (maybe-sound)
  element(*midi-percussion*, sound-number, default: #f)
    | default
end method;

define method find-midi-percussion
    (sound-name :: <byte-string>, #key default = #f) => (maybe-sound)
  element(*midi-percussion-table*, as(<symbol>, sound-name), default: #f)
    | default
end method;

define method do-midi-percussion (f :: <function>)
  do(f, *midi-percussion*);
end method;


define macro midi-percussion-definer
  { define midi-percussion from ?first-number:expression ?sounds:* end }
    => { do-define-midi-percussion(?first-number, ?sounds) }
sounds:
  { }
    => { }
  { sound ?sound-name:expression; ... }
    => { ?sound-name, ... }
end macro;

define midi-percussion from 35
  sound "Acoustic Bass Drum"; 
  sound "Bass Drum 1";        
  sound "Side Stick";         
  sound "Acoustic Snare";     
  sound "Hand Clap";          
  sound "Electric Snare";     
  sound "Low Floor Tom";      
  sound "Closed Hi-Hat";      
  sound "High Floor Tom";     
  sound "Pedal Hi-Hat";       
  sound "Low Tom";            
  sound "Open Hi-Hat";        
  sound "Low-Mid Tom";        
  sound "Hi-Mid Tom";         
  sound "Crash Cymbal 1";     
  sound "High Tom";           
  sound "Ride Cymbal 1";      
  sound "Chinese Cymbal";     
  sound "Ride Bell";          
  sound "Tambourine";         
  sound "Splash Cymbal";      
  sound "Cowbell";            
  sound "Crash Cymbal 2";     
  sound "Vibraslap";
  sound "Ride Cymbal 2";
  sound "Hi Bongo";
  sound "Low Bongo";
  sound "Mute Hi Conga";
  sound "Open Hi Conga";
  sound "Low Conga";
  sound "High Timbale";
  sound "Low Timbale";
  sound "High Agogo";
  sound "Low Agogo";
  sound "Cabasa";
  sound "Maracas";
  sound "Short Whistle";
  sound "Long Whistle";
  sound "Short Guiro";
  sound "Long Guiro";
  sound "Claves";
  sound "Hi Wood Block";
  sound "Low Wood Block";
  sound "Mute Cuica";
  sound "Open Cuica";
  sound "Mute Triangle";
  sound "Open Triangle"; 
end midi-percussion;

// eof
