module:    dylan-user
Synopsis:  The library definition for the HARP-X86-UNIX-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp-x86-unix-rtg
  use common-dylan;
  use harp-x86;
  use harp-native-rtg;
  use harp-unix-rtg;
  use harp-x86-rtg;

  export harp-x86-unix-rtg;
end library;
