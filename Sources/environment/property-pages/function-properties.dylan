Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function property pages

define sideways method frame-property-types
    (frame :: <environment-frame>,
     generic-function :: subclass(<generic-function-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"methods"))
end method frame-property-types;


/// Function General property types

define method general-property-types
    (object :: subclass(<dylan-function-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"parameters", #"values"))
end method general-property-types;


/// Generic function methods

define method frame-generic-function-methods
    (frame :: <environment-frame>, function :: <generic-function-object>)
 => (methods :: <sequence>)
  let project = frame.ensure-frame-project;
  generic-function-object-methods(project, function)
end method frame-generic-function-methods;

define method frame-sort-generic-function-methods
    (frame :: <environment-frame>, methods :: <sequence>,
     order :: <symbol>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  select (order)
    #"method" =>
      frame-sort-items(frame, methods);
    #"library" =>
      frame-sort-items(frame, methods,
		       key: curry(environment-object-library, project));
  end
end method frame-sort-generic-function-methods;

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>,
     class :: subclass(<generic-function-object>),
     type == #"methods")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
	   element-label: "method",
	   children-generator: curry(frame-generic-function-methods, frame),
	   headings: #["Method", "Library"],
	   widths:   #[300, 150],
	   generators: vector(identity, 
			      curry(environment-object-library, project)),
	   sort-orders: #[#"method", #"library"],
	   sort-order: #"method",
	   sort-function: curry(frame-sort-generic-function-methods, frame),
	   label-key: curry(frame-default-object-name, frame));
  values("Methods", displayer)
end method make-frame-property-page-displayer;
