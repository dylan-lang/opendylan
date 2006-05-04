module:    linux-rtg
Synopsis:  Support for name mangling for the Dylan Linux runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




///// Mangling functions


define sideways method c-mangle 
    (be :: <native-unix-back-end>, name :: <byte-string>)
     => (mangled :: <byte-string>)
  name
end method;
