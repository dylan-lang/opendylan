module:    test-profiler
synopsis:  Represeting the different types of counts maintained by the pofiler
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This class is used to represent the different types of counts maintained
// by the profiler (at the moment there are only two). Each count is
// maintained using a counter group. An instance of this class is created for
// each thread in the target application.
//
define class <profiler-counter-groups> (<object>)
  slot seen-on-stack :: <counter-group>,
       required-init-keyword: seen-on-stack:;

  slot top-of-stack :: <counter-group>,
       required-init-keyword: top-of-stack:;
end;


// Update the different types of profiler counts using the data in the
// profile snapshot provided.
//
define method update (counter-groups :: <profiler-counter-groups>,
                      profile-snapshot :: <profile-snapshot>)
                  => ()

  let ips = instruction-pointers (profile-snapshot);
  let weight = weight (profile-snapshot);

  // First, the top of stack counts
  if (size (ips) > 0)
    let counter = find-counter (counter-groups.top-of-stack, ips[0]);
    increment (counter);
  end if;

  // Now, the seen on stack counts
  for (ip in ips)
    let counter = find-counter (counter-groups.seen-on-stack, ip);
    increment (counter);
  end for;
end method;
