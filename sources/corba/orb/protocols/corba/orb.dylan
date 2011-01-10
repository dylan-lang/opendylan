Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ORB

define open abstract class corba/<orb> (corba/<pseudo-object>)
end class;

define open generic corba/orb/object-to-string
    (orb :: corba/<orb>, object :: corba/<object>)
 => (str :: corba/<string>);

define open generic corba/orb/string-to-object
    (orb :: corba/<orb>, str :: corba/<string>)
 => (object :: corba/<object>);

define open generic corba/orb/object-to-file
    (orb :: corba/<orb>, file :: corba/<string>, object :: corba/<object>)
 => ();

define open generic corba/orb/file-to-object
    (orb :: corba/<orb>, file :: corba/<string>)
 => (object :: corba/<object>);
       
define open generic corba/orb/create-list
    (orb :: corba/<orb>, count :: corba/<long>)
 => (new-list :: corba/<NVList>);

define open generic corba/orb/get-default-context
    (orb :: corba/<orb>)
 => (ctx :: corba/<context>);

define open generic corba/orb/work-pending
    (orb :: corba/<orb>)
 => (work? :: corba/<boolean>);

define open generic corba/orb/perform-work
    (orb :: corba/<orb>)
 => ();

define open generic corba/orb/shutdown
    (orb :: corba/<orb>, wait-for-completion :: <boolean>)
 => ();

define open generic corba/orb/run
    (orb :: corba/<orb>)
 => ();

define constant corba/orb/<objectid> = corba/<string>;

define constant corba/orb/<objectidlist> = limited(corba/<sequence>, of: corba/orb/<objectid>);

define sealed class corba/orb/<invalidname> (corba/<user-exception>)
end class;

define sealed domain make (singleton(corba/orb/<invalidname>));
define sealed domain initialize (corba/orb/<invalidname>);

define constant corba/orb/$invalid-name-typecode =
    make(<exception-typecode>,
         type: corba/orb/<invalidname>,
         name: "InvalidName",
         repository-id: "IDL:omg.org/CORBA/ORB/InvalidName:1.0",
         members: vector());

define method class-typecode (class == corba/orb/<invalidname>)
    => (typecode :: <typecode>)
  corba/orb/$invalid-name-typecode
end method;
         
define method object-typecode (object :: corba/orb/<invalidname>)
    => (typecode :: <typecode>)
  corba/orb/$invalid-name-typecode
end method;
         

define open generic corba/orb/list-initial-services (orb :: corba/<orb>)
 => (list :: corba/orb/<objectidlist>);

define open generic corba/orb/resolve-initial-references (orb :: corba/<orb>, objectid :: corba/orb/<objectid>)
 => (object :: corba/<object>);

/// ORB-INIT

define constant corba/<orbid> = corba/<string>;

define constant corba/<arg-list> = limited(corba/<sequence>, of: corba/<string>);

define open generic corba/orb-init
    (argv :: corba/<arg-list>,
     orb-identifier :: corba/<orbid>)
 => (orb :: corba/<orb>,
     argv :: corba/<arg-list>);


/// OBJECT

define open abstract class corba/<object> (<object>)
end class;

define open generic corba/object/is-nil (object :: false-or(<object>))
 => (result :: corba/<boolean>);

define open generic corba/object/duplicate (object :: <object>)
 => (result :: corba/<object>);

define open generic corba/object/release (object :: <object>)
 => ();

define open generic corba/object/is-a (object :: <object>, logical-type-id :: corba/<string>)
 => (result :: corba/<boolean>);

define open generic corba/object/non-existent (object :: <object>)
 => (result :: corba/<boolean>);

define open generic corba/object/is-equivalent (object :: <object>, other-object :: corba/<object>)
 => (result :: corba/<boolean>);

define open generic corba/object/hash (object :: <object>, maximum :: corba/<unsigned-long>)
 => (result :: corba/<unsigned-long>);

define open generic corba/object/create-request
    (object :: <object>,
     ctx :: corba/<Context>,
     operation :: corba/<Identifier>,
     arg-list :: corba/<NVList>,
     result :: corba/<NamedValue>,
     req-flags :: corba/<Flags>)
 => (result :: corba/<NamedValue>, request :: corba/<Request>);

/// INTERNALS

define open generic corba/object/ior (object :: <object>, #key direct?)
 => (ior);

define open generic corba/object/ior-setter (ior, object :: <object>)
 => (ior);

define open generic make-nil (class :: <class>)
 => (object :: corba/<object>);

/// PSEUDO-OBJECT
/// Reify this class in case its useful

define abstract class corba/<pseudo-object> (<object>)
end class;

/// CURRENT
///
/// NB corba/<current> can be returned from corba/orb/resolve-initial-references
/// which is typed to return corba/<object> so it must be a subclass.

define open abstract class corba/<current> (corba/<object>)
end class;
