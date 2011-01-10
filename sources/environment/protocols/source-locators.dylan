Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Location modelling

define class <source-location> (<object>)
  slot source-location-file = #f,
    init-keyword: file:;
  slot source-location-start-line = #f,
    init-keyword: start-line:;
  slot source-location-end-line = #f,
    init-keyword: end-line:;
end class <source-location>;
