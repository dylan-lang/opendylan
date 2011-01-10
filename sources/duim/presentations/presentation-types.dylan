Module:       DUIM-Presentations-Internals
Synopsis:     DUIM presentation system
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presentation types

define generic do-accept (...) => (...);
define generic do-present (...) => (...);
define generic do-describe (...) => (...);
define generic do-highlight (...) => (...);
define generic do-position-test (...) => (...);


/// Standard types

<real>
  <integer>
    limited(<integer>)
  <float>

<string>

<keyword>

<boolean>

<sequence>
  <enumeration>

<member>
<subset>
