Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the HARP-X86-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-harp-x86-cg

 use common-dylan;
 use io;

 use harp-x86;
 use dfmc-harp-cg;
 use dfmc-harp-native-cg;
 use dfmc-harp-cg-linker;

 export dfmc-harp-x86-cg;

end library dfmc-harp-x86-cg;


define module dfmc-harp-x86-cg

 use common-dylan;
 use streams-internals;

 use harp-x86,
    export: { <harp-x86-back-end>,
              <harp-x86-unix-back-end>,
              <harp-x86-linux-back-end>,
              <harp-x86-freebsd-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-harp-native-cg,
    export: { current-native-back-end, current-native-back-end-setter };
 use dfmc-harp-cg-linker;

end module dfmc-harp-x86-cg;
