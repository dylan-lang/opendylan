Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class corba/<serverrequest> (corba/<pseudo-object>)
end class;

define open generic corba/serverrequest/operation (request :: corba/<serverrequest>)
 => (identifier :: corba/<identifier>);

define open generic corba/serverrequest/arguments (request :: corba/<serverrequest>, nv :: corba/<nvlist>)
 => (nv :: corba/<nvlist>);

define open generic corba/serverrequest/ctx (request :: corba/<serverrequest>)
 => (context :: corba/<context>);

define open generic corba/serverrequest/set-result (request :: corba/<serverrequest>, val :: corba/<any>)
 => ();

define open generic corba/serverrequest/set-exception (request :: corba/<serverrequest>, val :: corba/<any>)
 => ();

