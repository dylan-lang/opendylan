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
  use translator;
  export
    maybe-collect-garbage, collect-garbage, 
    mark-garbage, block-promotion,
    room;
end module;
