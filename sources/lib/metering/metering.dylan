Module: metering
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Stub implementation.

define macro metering-set-definer
  { define metering-set ?:name ?specs:* end }
    => { define constant ?name = #f;
         ignore(?name)}
end macro;

define function make-metering-set (#rest places) #f end;

define macro with-metering 
  { with-metering (?set:expression, #rest ?options:*) ?:body end }
    => { ?body }
end macro;

define function do-with-metering (set, body) body() end;

// eof
