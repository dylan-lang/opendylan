module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <byte-character> = <character>;

define method key-sequence (sequence :: <sequence>)
  range(from: 0, up-to: sequence.size);
end method;

define abstract class <flat-sequence> (<sequence>)
end;

define method initial-state (sequence :: <flat-sequence>)
  if (empty?(sequence))
    #f;
  else
    0;
  end if;
end method;

define method final-state (sequence :: <flat-sequence>)
  if (empty?(sequence))
    #f;
  else
    size(sequence) - 1;
  end if;
end method;

define method next-state (sequence :: <flat-sequence>, state :: <integer>)
  if (state = (size(sequence) - 1))
    #f;
  else
    state + 1;
  end if;
end method;

define method previous-state (sequence :: <flat-sequence>, state :: <integer>)
  if (state = 0)
    #f;
  else
    state - 1;
  end if;
end method;

define method copy-state (sequence :: <flat-sequence>, state :: <integer>)
  state;
end method;

define method current-element (sequence :: <flat-sequence>, state :: <integer>)
  sequence[state];
end method;

define method current-key (sequence :: <flat-sequence>, state :: <integer>)
  state;
end method;
