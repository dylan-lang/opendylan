Module:       io-internals
Synopsis:     The format out library
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method format-out (format-string :: <string>, #rest args) => ()
  apply(format, *standard-output*, format-string, args)
end method;

// eof
