Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Composite object

define abstract class <composite-object> (<application-object>)
end class <composite-object>;

define open generic composite-object-size
    (server :: <server>, object :: <composite-object>, #key inherited?)
 => (size :: false-or(<integer>));

define open generic composite-object-contents
    (server :: <server>, object :: <composite-object>, #key inherited?)
 => (names :: <sequence>, values :: <sequence>);


/// Project dispatching methods

define method composite-object-size
    (project :: <project-object>, object :: <composite-object>,
     #key inherited?)
 => (size :: false-or(<integer>))
  let application = ensure-application-server(project, object);
  application & composite-object-size(application, object)
end method composite-object-size;

define method composite-object-contents
    (project :: <project-object>, object :: <composite-object>, 
     #key inherited?)
 => (names :: <sequence>, values :: <sequence>)
  let application = ensure-application-server(project, object);
  if (application)
    composite-object-contents(application, object)
  else
    values(#[], #[])
  end
end method composite-object-contents;
