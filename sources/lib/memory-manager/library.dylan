Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library memory-manager
  use dylan;
  export memory-manager;
end library;

define module memory-manager
  use dylan;
  use dylan-primitives;
  use dylan-extensions;
  use finalization;
  export
    <garbage-collection>,
    garbage-collection-info,
    garbage-collection-stats,
    maybe-collect-garbage,
    collect-garbage?, collect-garbage!,
    collect-garbage, 
    mark-garbage, block-promotion,
    room,
    enable-gc-messages,
    \with-ramp-allocation;
end module;

// eof
