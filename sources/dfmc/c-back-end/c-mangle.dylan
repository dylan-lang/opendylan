module: dfmc-c-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method c-global-mangle (name)
  global-mangle(*c-back-end*, name);
end method;

define method c-local-mangle (name)
  local-mangle(*c-back-end*, name);
end method;

define method c-raw-mangle (name)
  raw-mangle(*c-back-end*, name);
end method;

define method c-type-name (o :: <&class>)
  $dylan-type-string
end method;

define method c-type-name (o :: <&raw-type>)
  o.raw-type-c-name
end method;

define method c-repeated-type-name (o)
  c-type-name(o)
end method;

define method c-repeated-type-name (o :: <&class>)
  if (o == dylan-value(#"<byte-character>"))
    "char"
  else
    c-type-name(o)
  end if
end method;

/// !@#$ hack for when slot-type is not yet filled in

define method c-type-name (o)
  $dylan-type-string
end method;

// eof
