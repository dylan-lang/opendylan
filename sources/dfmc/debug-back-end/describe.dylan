Module: dfmc-debug-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Describe.

define compiler-sideways method describe* (o*) => ()
  for (o in o*)
    describe(o);
    format-out("--\n");
  end;
end method;

define compiler-sideways method describe (o) => ()
  format-out("%= is an instance of %=.\n", o, &object-class(o));
  // for-instance-slot-value (value described-by desc in o)
  for-layout-fixed-slot-value (value described-by desc in o)
    describe-slot(o, desc, value);
  end;
  let repeated = o.&object-class.^repeated-slot-descriptor;
  if (repeated)
    describe-repeated-slot(o, repeated);
  end;
end method;

define method describe-slot (o, desc :: <&slot-descriptor>, value)
  format-out("%s: %=\n", ^debug-name(^slot-getter(desc)), value);
end method;

define method describe-slot (o, desc :: <&virtual-slot-descriptor>, value)
  format-out("(virtual) %s: %=\n", ^debug-name(^slot-getter(desc)), value);
end method;

define method describe-repeated-slot (o, desc :: <&repeated-slot-descriptor>)
  let repeated-size = ^slot-value(o, ^size-slot-descriptor(desc));
  let repeated-name = ^debug-name(^slot-getter(desc));
  for (i from 0 below repeated-size)
    format-out("%s[%=]: %=\n", 
               repeated-name, i, ^repeated-slot-value(o, desc, i));
  end;
end method;
