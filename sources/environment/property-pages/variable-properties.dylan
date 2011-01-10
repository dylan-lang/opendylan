Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variable properties

define sideways method frame-property-types
    (frame :: <environment-frame>,
     class :: subclass(<variable-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"values"))
end method frame-property-types;


/// Variable General property types

define method general-property-types
    (object :: subclass(<variable-object>))
 => (types :: <list>)
  let types = next-method();
  select (object by instance?)
    <thread-variable-object> =>
      types;
    otherwise =>
      concatenate(types, #(#"value"));
  end
end method general-property-types;


/// Values property page

define constant <variable-thread-type> 
  = type-union(one-of(#"all"), <thread-object>);

define sealed class <thread-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-thread :: <variable-thread-type>,
    required-init-keyword: thread:;
end class <thread-wrapper>;

define sealed domain make (singleton(<thread-wrapper>));
define sealed domain initialize (<thread-wrapper>);

define method frame-sort-variable-values
    (frame :: <environment-frame>, values :: <sequence>,
     order :: <symbol>)
 => (values :: <sequence>)
  select (order)
    #"thread" =>
      frame-sort-items(frame, values,
		       key: wrapper-thread,
		       label-key: curry(variable-value-label, frame));
    #"value" =>
      frame-sort-items(frame, values,
		       key: wrapper-object);
  end
end method frame-sort-variable-values;

define method frame-variable-values
    (frame :: <environment-frame>,
     variable :: <variable-object>)
 => (values :: <sequence>)
  let project = frame.ensure-frame-project;
  let value = variable-value(project, variable);
  if (value)
    vector(make(<thread-wrapper>, 
		thread: #"all",
		object: value))
  else
    #[]
  end
end method frame-variable-values;

define method frame-variable-values
    (frame :: <environment-frame>,
     variable :: <thread-variable-object>)
 => (values :: <sequence>)
  let project = frame.ensure-frame-project;
  let application = project.project-application;
  if (application)
    let threads = application-threads(application);
    let values = make(<vector>, size: threads.size);
    for (thread in threads,
	 i from 0)
      values[i]
	:= make(<thread-wrapper>, 
		thread: thread,
		object: variable-value(project, variable, thread: thread))
    end;
    values
  else
    #[]
  end
end method frame-variable-values;

define constant $all-threads-label = "All threads";

define method variable-value-label
    (frame :: <environment-fixed-project-frame>, object == #"all")
 => (label :: <string>)
  $all-threads-label
end method variable-value-label;

define method variable-value-label
    (frame :: <environment-fixed-project-frame>, object :: <environment-object>)
 => (label :: <string>)
  frame-default-object-name(frame, object)
end method variable-value-label;

define sideways method make-frame-property-page-displayer
    (frame :: <environment-fixed-project-frame>, 
     class :: subclass(<variable-object>),
     type == #"values")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
	   element-label: "value",
	   information-available?-function: curry(application-tethered?, project),
	   transaction-function: curry(perform-application-transaction, project),
	   children-generator: curry(frame-variable-values, frame),
	   headings: #["Thread", "Value"],
	   widths:   #[200, 800],
	   sort-orders: #[#"thread", #"value"],
	   sort-function: curry(frame-sort-variable-values, frame),
	   generators: vector(wrapper-thread, wrapper-object),
	   label-key: curry(variable-value-label, frame));
  values("Values", displayer)
end method make-frame-property-page-displayer;
