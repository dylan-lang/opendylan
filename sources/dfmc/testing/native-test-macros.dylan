Module:   tests
Author:   Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro compiler-test-definer
  { define compiler-test ?:name ?:body end }
    => { define constant ?name ## "-test" = method () ?body end }
end macro;
