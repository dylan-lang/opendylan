Module:    dfmc-back-end-protocol
Author:    Jonathan Bachrach, Keith Playford
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define abstract open class <back-end> (<object>) 
  constant slot mangler = make(<mangler>);
end class;

define abstract open class <lambda-compiled-data> (<object>) 
end class;

define abstract open class <local-variable> (<object>) 
end class;
