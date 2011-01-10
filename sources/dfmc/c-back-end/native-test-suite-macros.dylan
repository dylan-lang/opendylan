Module:   dfm
Author:   Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro compiler-test
  { compiler-test ?:name ?:body end }
    => { begin ?body end }
end macro;

define macro compiler-test-suite-definer
  { define compiler-test-suite ?expressions end }
  => { ?expressions }
 expressions:
  { ?:expression; }
  => { ?expression }
  { ?:expression; ... }
  => { ?expression & ... }
end macro;
