Module:    dw
Synopsis:  Batch mode compilation handling
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function current-pentium-back-end-setter (new-value) => (new-value)
  new-value
end function current-pentium-back-end-setter;

define variable *trace-harp?* = #f;

define constant <linux-pentium-back-end> = #f;
