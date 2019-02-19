Module:       common-dylan-internals
Synopsis:     Transcendentals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define binary-transcendental hypot (x, y);

define sealed may-inline method hypot (x :: <single-float>, y :: <single-float>)
 => (z :: <single-float>)
  primitive-raw-as-single-float
    (%call-c-function("hypotf")
         (x :: <raw-single-float>, y :: <raw-single-float>)
      => (z :: <raw-single-float>)
         (primitive-single-float-as-raw(x),
          primitive-single-float-as-raw(y))
     end)
end method hypot;

define sealed may-inline method hypot (x :: <double-float>, y :: <double-float>)
 => (z :: <double-float>)
  primitive-raw-as-double-float
    (%call-c-function("hypot")
         (x :: <raw-double-float>, y :: <raw-double-float>)
      => (z :: <raw-double-float>)
         (primitive-double-float-as-raw(x),
          primitive-double-float-as-raw(y))
     end)
end method hypot;
