module: dood
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline method dood-compute-instance-size
    (dood :: <dood>, object == <machine-word>) => (address :: <address>)
  1 + 1 // 32 bits
end method;

//define constant dood-read-machine-word     = dood-read-word;
define constant dood-read-machine-word-at  = dood-read-word-at;
define constant dood-write-machine-word    = dood-write-word;
//define constant dood-write-machine-word-at = dood-write-word-at;

// eof
