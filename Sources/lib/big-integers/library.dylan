Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integers proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

define library big-integers
  use functional-dylan;
  use generic-arithmetic;
  export big-integers;
end library big-integers;

define module big-integers
  use generic-arithmetic, export: all;
end module big-integers;

define module big-integers-internal
  use dylan-excluding-arithmetic;
  use dylan-arithmetic,
    prefix: "dylan/";
  use machine-word-lowlevel;
  use big-integers;
  use dylan-primitives;
  use dylan-extensions,
    rename: {generic-binary-logior => binary-logior,
	     generic-binary-logxor => binary-logxor,
	     generic-binary-logand => binary-logand};
  use functional-extensions,
    import: {number-to-string, machine-word-to-string};
end module big-integers-internal;
