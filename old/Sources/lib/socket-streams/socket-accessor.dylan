Module:     socket-streams-internals
Author:     James Casey
Synopsis:   Abstracts out the functionality of a tcp-accessor
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <socket-accessor> (<external-stream-accessor>)
  slot socket-descriptor, 
    required-init-keyword: locator:; //, init-value: #f;
  slot socket-connection-eof? = #f;
end class <socket-accessor>;

