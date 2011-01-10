Module:    scepter-error-implementation
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// RESTARTS

define open class <idl-condition-restart> (<simple-restart>)
  keyword format-string: = "Try to continue from IDL error";
end class;

/// CONDITIONS

define open abstract class <idl-condition> (<condition>)
  constant slot idl-condition-string :: <string> = "", init-keyword: string:; // each-subclass
  constant slot idl-condition-source :: false-or(<scepter-source>),
                  init-keyword: source:,
                  init-function: compose(scepter-source, get-scepter);
end class;

define open generic idl-condition-source (condition :: <idl-condition>) => (source :: false-or(<scepter-source>));
define open generic idl-condition-recoverable? (condition :: <idl-condition>) => (recoverable? :: <boolean>);
define open generic idl-condition-serious? (condition :: <idl-condition>) => (serious? :: <boolean>);
define open generic idl-condition-title (stream :: <stream>, condition :: <idl-condition>) => ();
define open generic idl-condition-body (stream :: <stream>, condition :: <idl-condition>) => ();

define method idl-condition-recoverable? (condition :: <idl-condition>)
 => (recoverable? :: <boolean>)
  #t;
end method;

define method idl-condition-serious? (condition :: <idl-condition>)
 => (serious? :: <boolean>)
  #t;
end method;

define method idl-condition-title (stream :: <stream>, condition :: <idl-condition>)
 => ()
  format(stream, "%s", condition.idl-condition-string);
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-condition>)
 => ()
end method;


define open abstract class <idl-error> (<idl-condition>, <error>)
end class;

// unused
// 
// define class <idl-ok> (<idl-error>)
//   inherited slot condition-string = "all is fine ";
// end class;

define open generic idl-condition-similar? (condition1 :: <condition>, condition2 :: <condition>) => (similar? :: <boolean>);

define method idl-condition-similar? (condition1 :: <condition>, condition2 :: <condition>)
 => (similar? :: <boolean>)
  condition1.object-class == condition2.object-class;
end method;

