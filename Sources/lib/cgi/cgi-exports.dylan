Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library cgi         
  use common-dylan;
  use io;
  use system;
  use collection-extensions;
  export forms, environment-variables;
end;

define module forms        
  use common-dylan;
  use streams;
  use print;
  use standard-io; // in standard-io;
  use subseq;
  use format;
  use format-out;
  use operating-system, 
    rename: { environment-variable => getenv }; // HQN
  export main, cgi-main, dispatch-cgi-function, do-dispatch-cgi-function;
  export list-to-string;
end;

//define module cgi
//  use forms, rename: {cgi-main => main}, export: all;
//end;

define module environment-variables
  use dylan;
  use operating-system, 
    rename: { environment-variable => getenv }; // HQN
  export 
    $remote-host,
    $remote-address,
    $server-name,
    $server-port,
    $server-protocol,
    $gateway-interface,
    $query-string,
    $content-type,
    $content-length,
    $http-referer,
    $http-user-agent,
    $http-accept;
end;

// eof
