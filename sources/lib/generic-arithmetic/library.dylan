Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

define library generic-arithmetic
  use dylan;
  use common-dylan, import: { common-extensions };

  export generic-arithmetic,
	 generic-arithmetic-dylan,
	 generic-arithmetic-common-dylan;
end library generic-arithmetic;

define module generic-arithmetic
  use dylan-excluding-arithmetic;
  use dylan-extensions,
    import: { <abstract-integer> => <integer>,
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
