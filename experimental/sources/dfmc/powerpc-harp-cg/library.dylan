Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the POWERPC-HARP-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-powerpc-harp-cg

 use functional-dylan;
 use io;

 use powerpc-harp;
 use dfmc-harp-cg;
 use dfmc-harp-cg-linker;

 export dfmc-powerpc-harp-cg;

end library dfmc-powerpc-harp-cg;


define module dfmc-powerpc-harp-cg

 use functional-dylan;
 use streams-internals;

 use powerpc-harp,
    export: { <powerpc-back-end>,
	      <linux-powerpc-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-harp-cg-linker;

 export
    current-powerpc-back-end, current-powerpc-back-end-setter;

end module dfmc-powerpc-harp-cg;
