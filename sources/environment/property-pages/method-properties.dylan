Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Method pages

define sideways method frame-property-types
    (frame :: <environment-frame>,
     method-class :: subclass(<method-object>))
 => (types :: <list>)
  next-method()
/*---*** This is too confusing
  remove-duplicates
    (concatenate(next-method(),
                 frame-property-types(frame, <generic-function-object>)))
*/
end method frame-property-types;

define method frame-generic-function-methods
    (frame :: <environment-frame>, function :: <method-object>)
 => (methods :: <sequence>)
  let gf = method-generic-function(frame.ensure-frame-project, function);
  frame-generic-function-methods(frame, gf)
end method frame-generic-function-methods;

define sideways method make-frame-property-page
    (frame :: <environment-frame>,
     method-class :: subclass(<method-object>),
     type :: <symbol>)
 => (page :: <property-page>)
  make-frame-property-page(frame, <generic-function-object>, type)
end method make-frame-property-page;

