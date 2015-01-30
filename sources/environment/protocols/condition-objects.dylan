Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Condition objects

define user-object-class <condition-object> (<user-object>)
  binding <condition>, module: dylan, library: dylan;
end user-object-class <condition-object>;

define user-object-class <format-string-condition-object> (<condition-object>)
  binding <format-string-condition>, module: dylan-extensions, library: dylan;
end user-object-class <format-string-condition-object>;

define constant $format-condition-string-id
  = make(<definition-id>,
         name: "format-condition-string",
         module: $dylan-extensions-module-id);

define constant $format-condition-arguments-id
  = make(<definition-id>,
         name: "format-condition-arguments",
         module: $dylan-extensions-module-id);


/// Printing support

define method environment-object-type-name
    (condition :: <condition-object>) => (name :: <string>)
  "Condition"
end method environment-object-type-name;

define method condition-format-options
    (server :: <server>, condition :: <condition-object>)
 => (string :: false-or(<string-object>),
     arguments :: false-or(<collection-object>))
  values(user-object-slot-value
           (server, condition, $format-condition-string-id),
         user-object-slot-value
           (server, condition, $format-condition-arguments-id))
end method condition-format-options;
