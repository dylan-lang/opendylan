Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integers proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

/// NOTE: In the Emulator, arithmetic is already generic so this library is basically a NOP.

define library big-integers
  use dylan;
  use generic-arithmetic;
  export big-integers;
end library big-integers;

define module big-integers
  use generic-arithmetic,
    export: all;
end module big-integers;
