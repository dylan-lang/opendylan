Module:    environment-property-pages
Synopsis:  Application Properties
Author:    Andy Armstrong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Application property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, library :: subclass(<application>))
 => (types :: <list>)
  concatenate(next-method(),
              #[#"threads", #"components"])
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, class :: subclass(<application>))
 => (type :: false-or(<symbol>))
  #"threads"
end method frame-default-property-type;


/// Application threads property page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, class :: subclass(<application>),
     type == #"threads")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
           element-label: "thread",
           headings: #["Thread", "State"],
           widths:   #[400, 100],
           information-available?-function: curry(application-tethered?, project),
           transaction-function: curry(perform-application-transaction, project),
           children-generator: application-threads,
           sort-orders: #[#"thread", #"state"],
           sort-function: curry(frame-sort-application-threads, frame),
           generators: vector(identity, curry(thread-state, project)),
           label-key: curry(thread-label, frame));
  values("Threads", displayer)
end method make-frame-property-page-displayer;

define method frame-sort-application-threads
    (frame :: <environment-frame>, threads :: <sequence>,
     order :: <symbol>)
 => (threads :: <sequence>)
  select (order)
    #"thread" =>
      frame-sort-items(frame, threads);
    #"state" =>
      frame-sort-items
        (frame, threads,
         label-key: method (thread :: <thread-object>)
                      let project = frame.ensure-frame-project;
                      let state = thread-state(project, thread);
                      as(<string>, state)
                    end);
  end
end method frame-sort-application-threads;

define method thread-label
    (frame :: <environment-frame>, thread :: <thread-object>)
 => (label :: <string>)
  frame-default-object-name(frame, thread)
end method thread-label;

define method thread-label
    (frame :: <environment-frame>, state :: <symbol>)
 => (label :: <string>)
  let project = frame.ensure-frame-project;
  thread-state-label(project, state)
end method thread-label;


/// Application components property page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, class :: subclass(<application>),
     type == #"components")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
           element-label: "DLL",
           headings: #["DLL", "Version", "File"],
           widths:   #[200, 100, 500],
           information-available?-function: curry(application-tethered?, project),
           transaction-function: curry(perform-application-transaction, project),
           children-generator: application-components,
           sort-orders: #[#"component", #"version", #"file"],
           sort-order:  #"component",
           sort-function: curry(frame-sort-application-components, frame),
           generators: vector(identity,
                              curry(component-version-string, project),
                              curry(component-image-filename, project)),
           label-key: curry(frame-default-object-name, frame));
  values("DLLs", displayer)
end method make-frame-property-page-displayer;

define method frame-sort-application-components
    (frame :: <environment-frame>, components :: <sequence>,
     order :: <symbol>)
 => (threads :: <sequence>)
  let project = frame.ensure-frame-project;
  select (order)
    #"component" =>
      frame-sort-items(frame, components);
    #"version" =>
      frame-sort-items
        (frame, components,
         label-key: curry(component-version-string, project));
    #"file" =>
      frame-sort-items
        (frame, components,
         label-key: method (component :: <component-object>)
                      let filename
                        = component-image-filename(project, component);
                      if (filename) as(<string>, filename) else "" end
                    end);
  end
end method frame-sort-application-components;
