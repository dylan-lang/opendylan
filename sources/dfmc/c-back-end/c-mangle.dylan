module: dfmc-c-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dummy-c-back-end = make(<c-back-end>);

define method c-global-mangle (name)
  global-mangle(current-back-end() | $dummy-c-back-end, name);
end method;

define method c-local-mangle (name)
  local-mangle(current-back-end() | $dummy-c-back-end, name);
end method;

define method c-raw-mangle (name)
  raw-mangle(current-back-end() | $dummy-c-back-end, name);
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
