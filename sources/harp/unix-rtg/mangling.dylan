module:    harp-unix-rtg
Synopsis:  Support for name mangling for the Dylan Unix runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




///// Mangling functions


define sideways method c-mangle 
    (be :: <native-unix-back-end>, name :: <byte-string>)
     => (mangled :: <byte-string>)
  name
end method;
