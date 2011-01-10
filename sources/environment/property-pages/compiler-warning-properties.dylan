Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Compiler warning property pages

define sideways method frame-property-types
    (frame :: <environment-frame>,
     class :: subclass(<compiler-warning-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"source"))
end method frame-property-types;
