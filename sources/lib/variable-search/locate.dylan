Module: variable-search
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method locate-variable (o :: <object>)
  error("Failed to locate a variable binding for %=", o);
end method;

define method locate-variable (o :: <function>)
  values(o.debug-name, #"dylan", #"dylan");
end method;

define method locate-variable (o :: <class>)
  values(o.debug-name, #"dylan", #"dylan");
end method;
