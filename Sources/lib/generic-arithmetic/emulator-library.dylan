Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

/// NOTE: In the Emulator, arithmetic is already generic so this library is basically a NOP.

define library generic-arithmetic
  use functional-dylan;
  export dylan-excluding-arithmetic,
	 dylan-arithmetic,
	 generic-arithmetic,
	 generic-arithmetic-dylan,
	 generic-arithmetic-functional-dylan;
end library generic-arithmetic;

define module dylan-excluding-arithmetic
  use dylan,
    exclude: {<integer>,
	      $minimum-integer, $maximum-integer,
	      range, <range>,
	      \+, \-, \*, \/,
	      negative,
	      floor,  ceiling,  round,  truncate,
	      floor/, ceiling/, round/, truncate/,
	      modulo, remainder,
	      \^,
	      abs,
	      logior, logxor, logand, lognot,
	      logbit?,
	      ash, lsh,
	      lcm, gcd,
	      \for},
    export: all;
  //---*** Anything from dylan-extensions?
end module dylan-exluding-arithmetic;

define module dylan-arithmetic
  use dylan,
    import: {<integer>,
	     $minimum-integer, $maximum-integer,
	     range, <range>,
	     \+, \-, \*, \/,
	     negative,
	     floor,  ceiling,  round,  truncate,
	     floor/, ceiling/, round/, truncate/,
	     modulo, remainder,
	     \^,
	     abs,
	     logior, logxor, logand, lognot,
	     logbit?,
	     ash, lsh,
	     lcm, gcd,
	     \for},
    export: all;
  //---*** Anything from dylan-extensions?
end module dylan-arithmetic;

define module generic-arithmetic
  use dylan-excluding-arithmetic;
  use dylan-arithmetic,
    export: all;
end module generic-arithmetic;

define module generic-arithmetic-dylan
  use dylan-excluding-arithmetic,
    export: all;
  use generic-arithmetic,
    export: all;
end module generic-arithmetic-dylan;

define module generic-arithmetic-functional-dylan
  use generic-arithmetic-dylan,
    export: all;
  use functional-extensions,
    export: all;
end module generic-arithmetic-functional-dylan;
