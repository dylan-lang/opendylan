module: locator-internals
author: Tim McNerney
revised: 13-Feb-96 mf
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <locator-error> (<simple-error>)
end;

define class <locator-print-error> (<locator-error>)
end;

define class <locator-parse-error> (<locator-error>)
end;

define class <locator-translation-error> (<locator-error>)
end;

define class <locator-merge-error> (<locator-error>)
end;

define method locator-error (error-class :: subclass (<locator-error>),
			     format-string :: <string>,
			     #rest format-args)
  signal(make(error-class,
	      format-string: format-string,
	      format-arguments: format-args));
end method;
