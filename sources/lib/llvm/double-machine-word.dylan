Module:       llvm-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <double-machine-word> (<object>)
  constant slot double-machine-word-low :: <machine-word>,
    required-init-keyword: low:;
  constant slot double-machine-word-high :: <machine-word>,
    required-init-keyword: high:;
end class;

define sealed inline method \=
    (x :: <double-machine-word>, y :: <double-machine-word>)
 => (result :: <boolean>)
  double-machine-word-low(x) = double-machine-word-low(y)
  & double-machine-word-high(x) = double-machine-word-high(y)
end method;

