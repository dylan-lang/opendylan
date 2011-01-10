Module:    environment-framework
Synopsis:  Environment Framework library
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Name handling

define open generic frame-object-name
    (frame :: <frame>, object) => (name :: <string>);

define variable *print-length* :: <integer> = 10;
define variable *print-depth* :: <integer> = 10;


/// Default method
define method frame-object-name
    (frame :: <frame>, object :: <environment-object>)
 => (name :: <string>)
  ignore(frame, object);
  "{environment object}"
end method frame-object-name;


/// Find named objects

define open generic find-named-object
    (frame :: <frame>, name :: <string>, #key type)
 => (object :: false-or(<environment-object>));


// Default method
define method find-named-object
    (frame :: <frame>, name :: <string>, #key type)
 => (object :: false-or(<environment-object>))
  #f
end method find-named-object;
