Module: dfmc-pentium-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


current-native-back-end() := select ($os-name)
			       #"win32"  => <pentium-back-end>;
			       #"linux"  => <pentium-linux-back-end>;
			       otherwise => <pentium-back-end>;
			     end;
