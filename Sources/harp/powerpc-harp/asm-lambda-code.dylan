module:    powerpc-harp
Synopsis:  PowerPC Generation of <compiled-lambda> objects
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method code-vector-class(backend :: <powerpc-back-end>)
 => (class == <simple-integer-vector>)
  <simple-integer-vector>
end method;

define compiled-lambda-referenced-data-method
  (<powerpc-back-end>, <simple-integer-vector>, 2, 4);

