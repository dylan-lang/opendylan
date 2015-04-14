Module: dylan-user
language: infix-dylan
Synopsis:  The module definition for the NATIVE-HARP-CG module
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-harp-native-cg

 use common-dylan;
 use io;

 use harp-native;
 use dfmc-harp-cg;
 use dfmc-harp-cg-linker;

 export dfmc-harp-native-cg;

end library dfmc-harp-native-cg;


define module dfmc-harp-native-cg

 use common-dylan;
 use streams-internals;

 use harp-native,
    export: { <harp-native-back-end>,
              <harp-native-unix-back-end>,
	      <harp-native-linux-back-end>,
              <harp-native-freebsd-back-end> };
 use dfmc-harp-cg, export: {*trace-harp?*};
 use dfmc-harp-cg-linker;

end module dfmc-harp-native-cg;
