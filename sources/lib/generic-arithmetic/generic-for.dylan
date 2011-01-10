Module:    generic-arithmetic-internal
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///---*** TEMPORARY KLUDGE!

define macro for
  { for (?header:*) ?fbody:* end }
     => { dylan/for (?header) ?fbody end }
end macro for;

