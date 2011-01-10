Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method \= (x :: <vector>, y :: <vector>) => (result :: <boolean>)
  let x-size :: <integer> = x.size;
  x-size = y.size
  & iterate grovel (i :: <integer> = 0)
      if (i < x-size)
        (x[i] = y[i]) & grovel(i + 1)
      else
        #t
      end if
    end iterate
end method;

/*
define copy-down-method \=
    (x :: <byte-string>, y :: <byte-string>) => (result :: <boolean>);
*/
