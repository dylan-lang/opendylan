Module:       common-dylan-internals
Synopsis:     Transcendentals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define binary-transcendental hypot (x, y);

define macro hypot-method-definer
  { define hypot-method (?type:name, ?one:expression) end }
    => { define method hypot (y :: ?type, x :: ?type) => (result :: ?type)
           let x = abs(x);
           let y = abs(y);
           let (a, b)
             = if (y > x)
                 values(y, x)
               else
                 values(x, y)
               end if;
           if (zero?(a))
             b
           else
             let t = b / a;
             a * sqrt(?one + t * t)
           end if
         end method }
end macro;

define hypot-method (<single-float>, 1.0s0) end;
define hypot-method (<double-float>, 1.0d0) end;
