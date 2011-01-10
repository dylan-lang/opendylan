Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the NATIVE-HARP-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-native-harp-cg

 use functional-dylan;
 use io;

 use native-harp;
 use dfmc-harp-cg;
 use dfmc-harp-cg-linker;

 export dfmc-native-harp-cg;

end library dfmc-native-harp-cg;


define module dfmc-native-harp-cg

 use functional-dylan;
 use streams-internals;

 use native-harp,
    export: { <harp-native-back-end>,
              <native-unix-back-end>,
	      <native-linux-back-end>,
              <native-freebsd-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-harp-cg-linker;

end module dfmc-native-harp-cg;
