Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

define library generic-arithmetic
  use functional-dylan,
    export: {dylan-excluding-arithmetic,
	     dylan-arithmetic};
  use common-dylan,
    import: {common-extensions};
  export generic-arithmetic,
	 generic-arithmetic-dylan,
	 generic-arithmetic-common-dylan,
	 generic-arithmetic-functional-dylan;
end library generic-arithmetic;

define module generic-arithmetic
  use dylan-excluding-arithmetic;
  use dylan-extensions,
    import: {<abstract-integer> => <integer>,
	     generic-logior => logior,
	     generic-logxor => logxor,
	     generic-logand => logand},
    export: all;
  create $minimum-integer, $maximum-integer,
	 range, <range>,
	 \+, \-, \*, \/,
	 negative,
	 floor,  ceiling,  round,  truncate,
	 floor/, ceiling/, round/, truncate/,
	 modulo, remainder,
	 \^,
	 abs,
	 lognot,
	 logbit?,
	 ash, lsh,
	 lcm, gcd,
	 \for;
end module generic-arithmetic;

define module generic-arithmetic-dylan
  use dylan-excluding-arithmetic,
    export: all;
  use generic-arithmetic,
    export: all;
end module generic-arithmetic-dylan;

define module generic-arithmetic-common-dylan
  use generic-arithmetic-dylan,
    export: all;
  use common-extensions,
    export: all;
end module generic-arithmetic-common-dylan;

define module generic-arithmetic-functional-dylan
  use generic-arithmetic-dylan,
    export: all;
  use functional-extensions,
    export: all;
end module generic-arithmetic-functional-dylan;

define module generic-arithmetic-internal
  use generic-arithmetic-dylan;
  use dylan-arithmetic,
    prefix: "dylan/";
  use dylan-extensions,
    import: {false-or,
	     generic-binary-logior => binary-logior,
	     generic-binary-logxor => binary-logxor,
	     generic-binary-logand => binary-logand};
end module generic-arithmetic-internal;
