Module:    environment-manager
Synopsis:  Environment Manager
Author:    Andy Armstrong, Hugh Greene, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tool handling (i.e., functions provided by other env tools)

//---*** andrewa: this is now badly named...
define open generic find-deuce-frame
    (#rest initargs, #key, #all-keys) => ();

define open generic find-and-call-in-deuce-frame
    (function :: <function>, #rest initargs, #key, #all-keys) => ();

define open generic new-project-wizard
    (#key frame, title, #all-keys)
 => (project-locations :: <sequence> /* of: <locator> */);

define open generic show-definition-summary
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<definition-object>))
 => (success? :: <boolean>);

define open generic show-documentation
    (name :: <string>,
     project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     object :: false-or(<definition-object>))
 => (success? :: <boolean>);

// ---*** Jason: how get passed #f?
define open generic browse-object
    (project :: <project-object>, object :: false-or(<environment-object>), #key page)
 => (success? :: <boolean>);

define open generic browse-object-type
    (project :: <project-object>, object :: <environment-object>, #key page)
 => (success? :: <boolean>);

define open generic browse-object-generic-function
    (project :: <project-object>, object :: <environment-object>, #key page)
 => (success? :: <boolean>);

define open generic show-properties
    (project :: <project-object>, object :: <environment-object>, #key page) => ();

define open generic edit-object-definitions
    (project :: <project-object>, object :: <environment-object>) => ();

define open generic compile-project-location
    (project :: <project-object>, location :: <source-location>,
     scope :: <symbol>) => ();

define open generic environment-open-file
    (filename :: <file-locator>)
 => (success? :: <boolean>);
