Module:    dfmc-application
Synopsis:  tracking of runtime definitions that don't require registration
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// FIND-APPLICATION-PROXY (Environment Protocol Method)
//    Attempts to find an application proxy from its unique definition
//    ID, in the context of the application server.

define method find-application-proxy
    (application :: <dfmc-application>, id :: <definition-id>)
 => (proxy)
  let target = application.application-target-app;
  let (binding-name, module-name, library-name)
    = definition-id-to-string-triple(id);
  let proxy = #f;
  perform-debugger-transaction
    (target,
     method ()
       proxy := 
         application-name-to-runtime-proxy(application,
                                           binding-name,
                                           module-name,
                                           library-name)
     end method);
  proxy;
end method;


///// APPLICATION-PROXY-ID (Environment Protocol Method)
//    Given an application proxy, returns the associated ID if there is
//    one.

define method application-proxy-id
    (application :: <dfmc-application>, proxy)
  => (id :: false-or(<definition-id>))
  let factory = application.application-proxy-factory;
  element(factory.proxy-factory-proxy-to-id-mappings, proxy, default: #f);
end method;


