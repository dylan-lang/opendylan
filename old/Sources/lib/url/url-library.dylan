Module: dylan-user
Author: James Casey, January 1997
Synopsis: Simple url parser
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library url
  use functional-dylan;
  use operating-system;
  use streams;
  use format;
  use format-out;

  export url-internals;
  export url;
  export url-debug;
end library url;

define module url
  create <url>, url-scheme, 
    url-server, url-server-setter,
    url-path, url-path-setter,
    url-query, url-query-setter,
    url-fragment, url-fragment-setter,
    url-host, url-host-setter,
    url-port, url-port-setter,
    url-user, url-user-setter,
    url-password, url-password-setter;
  create url-default-port, url-string, url-is-relative?;
end module url;

define module url-internals
  use functional-dylan;
  use format;

  // Interface
  use url, export: all;
  export url-scheme-as-class;
end module url-internals;

define module url-debug
  use functional-dylan;
  
  use format-out;
  
  use url-internals;
end module url-debug;