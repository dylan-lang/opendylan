Module:    plonker
Synopsis:  Play with MIDI
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// A hack at some scale utilities.

define method cycle (val, #rest vals) => (cycle :: <list>)
  let l = pair(val, as(<list>, vals));
  for (cursor = l then cursor.tail, until: cursor.tail == #())
  finally
    cursor.tail := l;
  end;
  l
end method;

define method scale-to-naturals 
    (scale :: <sequence>) => (naturals :: <sequence>)
  let naturals = make(<simple-object-vector>, size: 7, fill: #());
  for (i from 0 below 7)
    let prev-i = modulo(i - 1, 7);
    let step = modulo(scale[i] - scale[prev-i], 12);
    if (step > 1)
      naturals[i]      := pair(#"flat", naturals[i]);
      naturals[prev-i] := pair(#"sharp", naturals[prev-i]);
    end;
  end;
  naturals
end method;

//// The major pentatonic (I think!)

define constant $major-scale
  = #[0, 2, 4, 5, 7, 9, 11, 12];

define constant $major-scale-naturals
  = scale-to-naturals($major-scale);

define constant $major-scale-naturals-cycle
  = apply(cycle, $major-scale-naturals);
