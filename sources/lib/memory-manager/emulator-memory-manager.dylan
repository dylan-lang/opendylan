module: memory-manager
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

import-cl-functions
  (clean-down-1.sys(as: lisp/clean-down),
   mark-and-sweep.sys(as: lisp/mark-and-sweep),
   room-report.dylan(as: lisp/room),
   lisp/block-promotion.dylan(as: lisp/block-promotion));

define method room ()
  lisp/room()
end method;

define method collect-garbage (#key print-stats?)
  lisp/clean-down();
  room();
end method;

define method mark-garbage ()
  lisp/mark-and-sweep(3);
end method;

define method block-promotion ()
  lisp/block-promotion();
end method;


define macro with-ramp-allocation
  { with-ramp-allocation ?:body end }
    => {
	?body;
	}
end macro with-ramp-allocation;
