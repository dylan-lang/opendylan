module: dfmc-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define compilation-pass allocate-registers,
//   visit: functions,
//   mandatory?: #t,
//   after: analyze-environments,
//   check-after?: #t;

define method allocate-registers (f :: <&lambda>) => ();
  let e :: <lambda-lexical-environment> = f.environment;
  let offset = 0;
  let number-parameters = size(parameters(f));
  for (tmp in e.temporaries, i :: <integer> from 0)
    if (i <= number-parameters | used?(tmp))
      tmp.frame-offset := offset;
      offset           := offset + 1;
    else
      tmp.frame-offset := 0;
    end if;
  end for;
  e.frame-size := offset + 1;
end method;

// eof
