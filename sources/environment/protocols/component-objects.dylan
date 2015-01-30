module:      environment-protocols
synopsis:    The <component-object> class, and its associated protocols.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <COMPONENT-OBJECT>
//    A subclass of <APPLICATION-OBJECT>.
//    Represents a runtime "component" - ie. a DLL/EXE file, or a shared
//    object file.

//    APPLICATION-OBJECT-ADDRESS can be called on objects of this class,
//    and the result is interpreted as the "base address" of the component.

//    ENVIRONMENT-OBJECT-PRIMITIVE-NAME for this class returns the name
//    of the component as stripped of all platform-specific pathname
//    and extension strings. Therefore, there is no COMPONENT-NAME
//    protocol.

define class <component-object> (<application-object>)
end class;


///// COMPONENT-IMAGE-FILENAME
//    Synopsis: Locates the binary image file (on disk) associated with
//              the component.
//    Inputs:
//      server     - The dispatching context.
//      component  - The <component-object> in question.
//    Results:
//      file       - A qualified filename for the binary image.

define open generic component-image-filename
    (server :: <server>, component :: <component-object>)
 => (file :: false-or(<file-locator>));

define method component-image-filename
    (project :: <project-object>, component :: <component-object>)
 => (file :: false-or(<file-locator>))
  let server = choose-server(project, component);
  server & component-image-filename(server, component)
end method component-image-filename;


///// COMPONENT-VERSION
//    Synopsis: Returns the version number for a component. This is assumed
//              to be both meaninful and obtainable on all platforms.
//    Inputs:
//      server     - The dispatching context.
//      component  - The <component-object> in question.
//    Results:
//      major-version-index  - The major version number of the component.
//      minor-version-index  - The minor version number of the component.

define open generic component-version
    (server :: <server>, component :: <component-object>)
 => (major-version-index :: <integer>, minor-version-index :: <integer>);

define method component-version
    (project :: <project-object>, component :: <component-object>)
 => (major-version-index :: <integer>, minor-version-index :: <integer>)
  let server = choose-server(project, component);
  if (server)
    component-version(server, component)
  else
    values(0, 0)
  end if
end method;

define function component-version-string
    (server :: <server>, component :: <component-object>)
 => (version-string :: <string>)
  let (major-version, minor-version) = component-version(server, component);
  format-to-string("%d.%d", major-version, minor-version)
end function component-version-string;


///// LOOKUP-COMPONENT-BY-NAME
//    Synopsis: Attempts to find a <component-object> with a given name.

define open generic lookup-component-by-name
    (server :: <server>, name :: <string>)
 => (component :: false-or(<component-object>));

define method lookup-component-by-name
    (project :: <project-object>, name :: <string>)
 => (component :: false-or(<component-object>))
  let server = project.project-application;
  server & lookup-component-by-name(server, name);
end method;


///// DO-APPLICATION-COMPONENTS
//    Synopsis: Iterates over the components currently loaded into an
//              application.
//    Inputs:
//      f          - The iterator. A function of signature:
//                   (<COMPONENT-OBJECT>) => ().
//      server     - The dispatching context.
//    Outputs:
//      None.

define open generic do-application-components
    (f :: <function>, server :: <server>) => ();

define method do-application-components
    (f :: <function>, project :: <project-object>) => ()
  let server = project.project-application;
  if (server)
    do-application-components(f, server)
  end if
end method;


/// Some convenience functions built on these protocols

define function application-components
    (server :: <server>) => (components :: <sequence>)
  let results = make(<stretchy-vector>);
  do-application-components
    (method (component :: <component-object>)
       add!(results, component)
     end,
     server);
  results
end function application-components;
