Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<duim-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"hierarchy"))
end method frame-property-types;

define method duim-object-children-generator
    (frame :: <environment-frame>, object :: <duim-object>)
 => (objects :: <sequence>)
  let project = frame.ensure-frame-project;
  let children = duim-object-children(project, object);
  //---*** Should we sort them? Probably not by default
  children
end method duim-object-children-generator;

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<duim-object>),
     type == #"hierarchy")
 => (label :: <string>, displayer :: <tree-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<tree-control-displayer>,
	   element-label: "object",
	   information-available?-function: curry(application-tethered?, project),
	   transaction-function: curry(perform-application-transaction, project),
	   children-generator: curry(duim-object-children-generator, frame),
	   label-key: curry(frame-default-object-name, frame));
  values("Hierarchy", displayer)
end method make-frame-property-page-displayer;
