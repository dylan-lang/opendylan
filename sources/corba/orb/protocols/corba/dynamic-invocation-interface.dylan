Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// CONSTS

define constant corba/$arg-in :: corba/<long> = 1;
define constant corba/$arg-out :: corba/<long> = 2;
define constant corba/$arg-inout :: corba/<long> = 4;
define constant corba/$inv-no-response :: corba/<long> = 8;

/// REQUESTS

define open abstract class corba/<request> (corba/<pseudo-object>)
end class;

/// NB not in standard but needed
define open generic corba/request/add-exception 
    (request :: corba/<request>,
    exception-type :: <typecode>)
 => ();

/// NB not in standard but needed
define open generic corba/request/add-context
    (request :: corba/<request>,
    propname :: corba/<identifier>)
 => ();

define open generic corba/request/add-arg
    (request :: corba/<request>,
     name :: corba/<Identifier>,
     arg-type :: <typecode>,
     value :: <object>,
     len :: corba/<long>,
     arg-flags :: corba/<Flags>)
 => ();

define open generic corba/request/invoke
    (request :: corba/<request>,
     invoke-flags :: corba/<Flags>)
 => ();

define open generic corba/request/delete
    (request :: corba/<request>)
 => ();

define open generic corba/request/send
    (request :: corba/<request>,
     invoke-flags :: corba/<Flags>)
 => ();

define open generic corba/request/get-response
    (request :: corba/<request>,
     response-flags :: corba/<Flags>)
 => ();
    
/// CONTEXTS

define open abstract class corba/<Context> (corba/<pseudo-object>)
end class;

define open generic corba/context/context-name (object :: corba/<context>)
 => (name :: corba/<Identifier>);

define open generic corba/context/parent (object :: corba/<context>)
 => (parent :: false-or(corba/<context>));

define open generic corba/context/create-child
    (object :: corba/<context>, child-ctx-name :: corba/<identifier>)
 => (child-ctx :: corba/<context>);

define open generic corba/context/set-one-value
    (object :: corba/<context>, propname :: corba/<identifier>, propvalue :: corba/<any>)
 => ();

define open generic corba/context/set-values
    (object :: corba/<context>, values :: corba/<NVList>)
 => ();

define open generic corba/context/delete-values
    (object :: corba/<context>, propname :: corba/<identifier>)
 => ();

define open generic corba/context/get-values
    (object :: corba/<context>,
     start-scope :: corba/<Identifier>,
     op-flags :: corba/<Flags>,
     pattern :: corba/<Identifier>)
 => (value :: corba/<NVList>);

define constant corba/$ctx-restrict-scope = 1;
