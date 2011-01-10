module:      access-path-implementation
synopsis:    A dummy implementation of locators
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <locator> = <string>;

define method as (to == <locator>, s :: <string>)
      s;
end method;
