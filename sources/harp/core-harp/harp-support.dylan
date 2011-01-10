module:    base-harp
Synopsis:  Runtime support for the HARP backend.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Runtime support for the HARP backend
//// Started by Tony (8 Apr 94)


/// Needed at runtime ...

define method harp-error (#rest args)
  apply(error, args);
end;



define open generic harp-warning (be :: <harp-back-end>, #rest args);

define method harp-warning (be :: <harp-back-end>, #rest args)
  apply(format-out, args);
  format-out("\n");
end;


define open generic harp-message (be :: <harp-back-end>, #rest args);

define method harp-message (be :: <harp-back-end>, #rest args)
  apply(format-out, args);
end;



define method pattern-error (#rest args)
  harp-error("*** Ran out of patterns, with arguments %=.\n", args);
end;








