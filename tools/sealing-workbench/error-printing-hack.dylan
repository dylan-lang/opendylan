module:   sealing-workbench
author:   Paul Haahr
synopsis: Hack around error not using the right version of format.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method error(condition, #rest args)
  apply(dylan/error, condition, args)
end method error;

define method error(string :: <string>, #rest args)
  dylan/error("~A", apply(format-to-string, string, args))
end method error;

define method signal(condition, #rest args)
  apply(dylan/signal, condition, args)
end method error;

define method signal(string :: <string>, #rest args)
  dylan/signal("~A", apply(format-to-string, string, args))
end method error;
