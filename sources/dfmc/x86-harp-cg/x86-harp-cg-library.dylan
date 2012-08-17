Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the X86-HARP-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-x86-harp-cg

 use common-dylan;
 use io;

 use x86-harp;
 use dfmc-harp-cg;
 use dfmc-native-harp-cg;
 use dfmc-harp-cg-linker;

 export dfmc-x86-harp-cg;

end library dfmc-x86-harp-cg;


define module dfmc-x86-harp-cg

 use common-dylan, exclude: { format-to-string };
 use streams-internals;

 use x86-harp,
    export: { <harp-x86-back-end>,
              <harp-x86-unix-back-end>,
              <harp-x86-linux-back-end>,
              <harp-x86-freebsd-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-native-harp-cg,
    export: { current-native-back-end, current-native-back-end-setter };
 use dfmc-harp-cg-linker;

end module dfmc-x86-harp-cg;
