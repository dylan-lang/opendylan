Module:    emulator-environment-backend 
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Application handling

define class <emulator-application> (<application>)
  slot user-object-class-mapping-table :: <table> = make(<table>);
end class <emulator-application>;

define method application-threads
    (application :: <emulator-application>, #key client)
 => (threads :: <sequence>)
  let threads = all-processes();
  map(curry(ensure-server-object, application), threads)
end method application-threads;

//--- They are identical in the emulator!
define method find-application-proxy
    (application :: <emulator-application>, compiler-proxy)
 => (application-proxy)
  compiler-proxy
end method find-application-proxy;

define method run-application
    (application :: <emulator-application>,
     #key debug?, filename, arguments)
  #f
end method run-application;


/// Compiler database handling

define class <emulator-database> (<compiler-database>)
end class <emulator-database>;

//--- They are identical in the emulator!
define method find-compiler-proxy
    (database :: <emulator-database>, application-proxy)
 => (compiler-proxy)
  application-proxy
end method find-compiler-proxy;


/// Threads

define method get-environment-object-primitive-name
    (application :: <emulator-application>, object :: <thread-object>)
 => (name :: <string>)
  process-name(application-object-proxy(object))
end method get-environment-object-primitive-name;
