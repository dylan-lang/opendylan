Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the PENTIUM-HARP-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-pentium-harp-cg

 use common-dylan;
 use io;

 use pentium-harp;
 use dfmc-harp-cg;
 use dfmc-native-harp-cg;
 use dfmc-harp-cg-linker;

 export dfmc-pentium-harp-cg;

end library dfmc-pentium-harp-cg;


define module dfmc-pentium-harp-cg

 use common-dylan, exclude: { format-to-string };
 use streams-internals;

 use pentium-harp,
    export: { <pentium-back-end>,
              <pentium-unix-back-end>,
	      <pentium-linux-back-end>,
              <pentium-freebsd-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-native-harp-cg,
    export: { current-native-back-end, current-native-back-end-setter };
 use dfmc-harp-cg-linker;

end module dfmc-pentium-harp-cg;
