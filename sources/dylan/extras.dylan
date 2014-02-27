Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function as-object (x :: <machine-word>)
  primitive-cast-raw-as-pointer
    (primitive-unwrap-machine-word(x))
end function;
