Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// COMPLETION-STATUS

define constant corba/<completion-status> = one-of(#"COMPLETED-YES", #"COMPLETED-NO", #"COMPLETED-MAYBE");

define constant corba/$completion-status-typecode =
  make(<enum-typecode>,
       name: "completion_status",
       repository-id: "IDL:omg.org/CORBA/completion_status:1.0",
       members: vector(#"COMPLETED-YES", #"COMPLETED-NO", #"COMPLETED-MAYBE"));

define method class-typecode (class == corba/<completion-status>)
 => (typecode :: <typecode>)
  corba/$completion-status-typecode
end method;

/// EXCEPTION-TYPE

define constant corba/<exception-type> = one-of(#"NO-EXCEPTION", #"USER-EXCEPTION", #"SYSTEM-EXCEPTION");

define constant corba/$exception-type-typecode =
  make(<enum-typecode>,
       name: "exception_type",
       repository-id: "IDL:omg.org/CORBA/exception_type:1.0",
       members: vector(#"NO-EXCEPTION", #"USER-EXCEPTION", #"SYSTEM-EXCEPTION"));

define method class-typecode (class == corba/<exception-type>)
 => (typecode :: <typecode>)
  corba/$exception-type-typecode
end method;

/// EXCEPTIONS

define sealed class corba/<UNKNOWN> (corba/<system-exception>)
  sealed slot corba/UNKNOWN/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/UNKNOWN/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<UNKNOWN>));
define sealed domain initialize (corba/<UNKNOWN>);

define constant corba/$UNKNOWN-typecode =
  make(<exception-typecode>,
       type: corba/<UNKNOWN>,
       name: "UNKNOWN",
       repository-id: "IDL:omg.org/CORBA/UNKNOWN:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/UNKNOWN/minor,
			    setter: corba/UNKNOWN/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/UNKNOWN/completed,
			    setter: corba/UNKNOWN/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<UNKNOWN>)
 => (typecode :: <typecode>)
  corba/$UNKNOWN-typecode
end method;

define sealed method class-typecode (class == corba/<UNKNOWN>)
 => (typecode :: <typecode>)
  corba/$UNKNOWN-typecode
end method;

define sealed class corba/<BAD-PARAM> (corba/<system-exception>)
  sealed slot corba/BAD-PARAM/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/BAD-PARAM/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<BAD-PARAM>));
define sealed domain initialize (corba/<BAD-PARAM>);

define constant corba/$BAD-PARAM-typecode =
  make(<exception-typecode>,
       name: "BAD_PARAM",
       type: corba/<BAD-PARAM>,
       repository-id: "IDL:omg.org/CORBA/BAD_PARAM:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/BAD-PARAM/minor,
			    setter: corba/BAD-PARAM/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/BAD-PARAM/completed,
			    setter: corba/BAD-PARAM/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<BAD-PARAM>)
 => (typecode :: <typecode>)
  corba/$BAD-PARAM-typecode
end method;

define sealed method class-typecode (class == corba/<BAD-PARAM>)
 => (typecode :: <typecode>)
  corba/$BAD-PARAM-typecode
end method;

define sealed class corba/<NO-MEMORY> (corba/<system-exception>)
  sealed slot corba/NO-MEMORY/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/NO-MEMORY/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<NO-MEMORY>));
define sealed domain initialize (corba/<NO-MEMORY>);

define constant corba/$NO-MEMORY-typecode =
  make(<exception-typecode>,
       name: "NO_MEMORY",
       type: corba/<NO-MEMORY>,
       repository-id: "IDL:omg.org/CORBA/NO_MEMORY:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/NO-MEMORY/minor,
			    setter: corba/NO-MEMORY/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/NO-MEMORY/completed,
			    setter: corba/NO-MEMORY/completed-setter,
			    init-keyword: completed:)
			 ));

define sealed method object-typecode (object :: corba/<NO-MEMORY>)
 => (typecode :: <typecode>)
  corba/$NO-MEMORY-typecode
end method;

define sealed method class-typecode (class == corba/<NO-MEMORY>)
 => (typecode :: <typecode>)
  corba/$NO-MEMORY-typecode
end method;

define sealed class corba/<IMP-LIMIT> (corba/<system-exception>)
  sealed slot corba/IMP-LIMIT/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/IMP-LIMIT/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<IMP-LIMIT>));
define sealed domain initialize (corba/<IMP-LIMIT>);

define constant corba/$IMP-LIMIT-typecode =
  make(<exception-typecode>,
       name: "IMP_LIMIT",
       type: corba/<IMP-LIMIT>,
       repository-id: "IDL:omg.org/CORBA/IMP_LIMIT:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/IMP-LIMIT/minor,
			    setter: corba/IMP-LIMIT/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/IMP-LIMIT/completed,
			    setter: corba/IMP-LIMIT/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<IMP-LIMIT>)
 => (typecode :: <typecode>)
  corba/$IMP-LIMIT-typecode
end method;

define sealed method class-typecode (class == corba/<IMP-LIMIT>)
 => (typecode :: <typecode>)
  corba/$IMP-LIMIT-typecode
end method;

define sealed class corba/<COMM-FAILURE> (corba/<system-exception>)
  sealed slot corba/COMM-FAILURE/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/COMM-FAILURE/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<COMM-FAILURE>));
define sealed domain initialize (corba/<COMM-FAILURE>);

define constant corba/$COMM-FAILURE-typecode =
  make(<exception-typecode>,
       name: "COMM_FAILURE",
       type: corba/<COMM-FAILURE>,
       repository-id: "IDL:omg.org/CORBA/COMM_FAILURE:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/COMM-FAILURE/minor,
			    setter: corba/COMM-FAILURE/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/COMM-FAILURE/completed,
			    setter: corba/COMM-FAILURE/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<COMM-FAILURE>)
 => (typecode :: <typecode>)
  corba/$COMM-FAILURE-typecode
end method;

define sealed method class-typecode (class == corba/<COMM-FAILURE>)
 => (typecode :: <typecode>)
  corba/$COMM-FAILURE-typecode
end method;

define sealed class corba/<INV-OBJREF> (corba/<system-exception>)
  sealed slot corba/INV-OBJREF/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INV-OBJREF/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INV-OBJREF>));
define sealed domain initialize (corba/<INV-OBJREF>);

define constant corba/$INV-OBJREF-typecode =
  make(<exception-typecode>,
       name: "INV_OBJREF",
       type: corba/<INV-OBJREF>,
       repository-id: "IDL:omg.org/CORBA/INV_OBJREF:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INV-OBJREF/minor,
			    setter: corba/INV-OBJREF/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INV-OBJREF/completed,
			    setter: corba/INV-OBJREF/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INV-OBJREF>)
 => (typecode :: <typecode>)
  corba/$INV-OBJREF-typecode
end method;

define sealed method class-typecode (class == corba/<INV-OBJREF>)
 => (typecode :: <typecode>)
  corba/$INV-OBJREF-typecode
end method;

define sealed class corba/<NO-PERMISSION> (corba/<system-exception>)
  sealed slot corba/NO-PERMISSION/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/NO-PERMISSION/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<NO-PERMISSION>));
define sealed domain initialize (corba/<NO-PERMISSION>);

define constant corba/$NO-PERMISSION-typecode =
  make(<exception-typecode>,
       name: "NO_PERMISSION",
       type: corba/<NO-PERMISSION>,
       repository-id: "IDL:omg.org/CORBA/NO_PERMISSION:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/NO-PERMISSION/minor,
			    setter: corba/NO-PERMISSION/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/NO-PERMISSION/completed,
			    setter: corba/NO-PERMISSION/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<NO-PERMISSION>)
 => (typecode :: <typecode>)
  corba/$NO-PERMISSION-typecode
end method;

define sealed method class-typecode (class == corba/<NO-PERMISSION>)
 => (typecode :: <typecode>)
  corba/$NO-PERMISSION-typecode
end method;

define sealed class corba/<INTERNAL> (corba/<system-exception>)
  sealed slot corba/INTERNAL/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INTERNAL/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INTERNAL>));
define sealed domain initialize (corba/<INTERNAL>);

define constant corba/$INTERNAL-typecode =
  make(<exception-typecode>,
       name: "INTERNAL",
       type: corba/<INTERNAL>,
       repository-id: "IDL:omg.org/CORBA/INTERNAL:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INTERNAL/minor,
			    setter: corba/INTERNAL/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INTERNAL/completed,
			    setter: corba/INTERNAL/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INTERNAL>)
 => (typecode :: <typecode>)
  corba/$INTERNAL-typecode
end method;

define sealed method class-typecode (class == corba/<INTERNAL>)
 => (typecode :: <typecode>)
  corba/$INTERNAL-typecode
end method;

define sealed class corba/<MARSHAL> (corba/<system-exception>)
  sealed slot corba/MARSHAL/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/MARSHAL/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<MARSHAL>));
define sealed domain initialize (corba/<MARSHAL>);

define constant corba/$MARSHAL-typecode =
  make(<exception-typecode>,
       name: "MARSHAL",
       type: corba/<MARSHAL>,
       repository-id: "IDL:omg.org/CORBA/MARSHAL:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/MARSHAL/minor,
			    setter: corba/MARSHAL/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/MARSHAL/completed,
			    setter: corba/MARSHAL/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<MARSHAL>)
 => (typecode :: <typecode>)
  corba/$MARSHAL-typecode
end method;

define sealed method class-typecode (class == corba/<MARSHAL>)
 => (typecode :: <typecode>)
  corba/$MARSHAL-typecode
end method;

define sealed class corba/<INITIALIZE> (corba/<system-exception>)
  sealed slot corba/INITIALIZE/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INITIALIZE/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INITIALIZE>));
define sealed domain initialize (corba/<INITIALIZE>);

define constant corba/$INITIALIZE-typecode =
  make(<exception-typecode>,
       name: "INITIALIZE",
       type: corba/<INITIALIZE>,
       repository-id: "IDL:omg.org/CORBA/INITIALIZE:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INITIALIZE/minor,
			    setter: corba/INITIALIZE/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INITIALIZE/completed,
			    setter: corba/INITIALIZE/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INITIALIZE>)
 => (typecode :: <typecode>)
  corba/$INITIALIZE-typecode
end method;

define sealed method class-typecode (class == corba/<INITIALIZE>)
 => (typecode :: <typecode>)
  corba/$INITIALIZE-typecode
end method;

define sealed class corba/<NO-IMPLEMENT> (corba/<system-exception>)
  sealed slot corba/NO-IMPLEMENT/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/NO-IMPLEMENT/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<NO-IMPLEMENT>));
define sealed domain initialize (corba/<NO-IMPLEMENT>);

define constant corba/$NO-IMPLEMENT-typecode =
  make(<exception-typecode>,
       name: "NO_IMPLEMENT",
       type: corba/<NO-IMPLEMENT>,
       repository-id: "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/NO-IMPLEMENT/minor,
			    setter: corba/NO-IMPLEMENT/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/NO-IMPLEMENT/completed,
			    setter: corba/NO-IMPLEMENT/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<NO-IMPLEMENT>)
 => (typecode :: <typecode>)
  corba/$NO-IMPLEMENT-typecode
end method;

define sealed method class-typecode (class == corba/<NO-IMPLEMENT>)
 => (typecode :: <typecode>)
  corba/$NO-IMPLEMENT-typecode
end method;

define sealed class corba/<BAD-TYPECODE> (corba/<system-exception>)
  sealed slot corba/BAD-TYPECODE/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/BAD-TYPECODE/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<BAD-TYPECODE>));
define sealed domain initialize (corba/<BAD-TYPECODE>);

define constant corba/$BAD-TYPECODE-typecode =
  make(<exception-typecode>,
       name: "BAD_TYPECODE",
       type: corba/<BAD-TYPECODE>,
       repository-id: "IDL:omg.org/CORBA/BAD_TYPECODE:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/BAD-TYPECODE/minor,
			    setter: corba/BAD-TYPECODE/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/BAD-TYPECODE/completed,
			    setter: corba/BAD-TYPECODE/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<BAD-TYPECODE>)
 => (typecode :: <typecode>)
  corba/$BAD-TYPECODE-typecode
end method;

define sealed method class-typecode (class == corba/<BAD-TYPECODE>)
 => (typecode :: <typecode>)
  corba/$BAD-TYPECODE-typecode
end method;

define sealed class corba/<BAD-OPERATION> (corba/<system-exception>)
  sealed slot corba/BAD-OPERATION/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/BAD-OPERATION/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<BAD-OPERATION>));
define sealed domain initialize (corba/<BAD-OPERATION>);

define constant corba/$BAD-OPERATION-typecode =
  make(<exception-typecode>,
       name: "BAD_OPERATION",
       type: corba/<BAD-OPERATION>,
       repository-id: "IDL:omg.org/CORBA/BAD_OPERATION:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/BAD-OPERATION/minor,
			    setter: corba/BAD-OPERATION/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/BAD-OPERATION/completed,
			    setter: corba/BAD-OPERATION/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<BAD-OPERATION>)
 => (typecode :: <typecode>)
  corba/$BAD-OPERATION-typecode
end method;

define sealed method class-typecode (class == corba/<BAD-OPERATION>)
 => (typecode :: <typecode>)
  corba/$BAD-OPERATION-typecode
end method;

define sealed class corba/<NO-RESOURCES> (corba/<system-exception>)
  sealed slot corba/NO-RESOURCES/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/NO-RESOURCES/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<NO-RESOURCES>));
define sealed domain initialize (corba/<NO-RESOURCES>);

define constant corba/$NO-RESOURCES-typecode =
  make(<exception-typecode>,
       name: "NO_RESOURCES",
       type: corba/<NO-RESOURCES>,
       repository-id: "IDL:omg.org/CORBA/NO_RESOURCES:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/NO-RESOURCES/minor,
			    setter: corba/NO-RESOURCES/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/NO-RESOURCES/completed,
			    setter: corba/NO-RESOURCES/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<NO-RESOURCES>)
 => (typecode :: <typecode>)
  corba/$NO-RESOURCES-typecode
end method;

define sealed method class-typecode (class == corba/<NO-RESOURCES>)
 => (typecode :: <typecode>)
  corba/$NO-RESOURCES-typecode
end method;

define sealed class corba/<NO-RESPONSE> (corba/<system-exception>)
  sealed slot corba/NO-RESPONSE/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/NO-RESPONSE/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<NO-RESPONSE>));
define sealed domain initialize (corba/<NO-RESPONSE>);

define constant corba/$NO-RESPONSE-typecode =
  make(<exception-typecode>,
       name: "NO_RESPONSE",
       type: corba/<NO-RESPONSE>,
       repository-id: "IDL:omg.org/CORBA/NO_RESPONSE:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/NO-RESPONSE/minor,
			    setter: corba/NO-RESPONSE/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/NO-RESPONSE/completed,
			    setter: corba/NO-RESPONSE/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<NO-RESPONSE>)
 => (typecode :: <typecode>)
  corba/$NO-RESPONSE-typecode
end method;

define sealed method class-typecode (class == corba/<NO-RESPONSE>)
 => (typecode :: <typecode>)
  corba/$NO-RESPONSE-typecode
end method;

define sealed class corba/<PERSIST-STORE> (corba/<system-exception>)
  sealed slot corba/PERSIST-STORE/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/PERSIST-STORE/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<PERSIST-STORE>));
define sealed domain initialize (corba/<PERSIST-STORE>);

define constant corba/$PERSIST-STORE-typecode =
  make(<exception-typecode>,
       name: "PERSIST_STORE",
       type: corba/<PERSIST-STORE>,
       repository-id: "IDL:omg.org/CORBA/PERSIST_STORE:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/PERSIST-STORE/minor,
			    setter: corba/PERSIST-STORE/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/PERSIST-STORE/completed,
			    setter: corba/PERSIST-STORE/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<PERSIST-STORE>)
 => (typecode :: <typecode>)
  corba/$PERSIST-STORE-typecode
end method;

define sealed method class-typecode (class == corba/<PERSIST-STORE>)
 => (typecode :: <typecode>)
  corba/$PERSIST-STORE-typecode
end method;

define sealed class corba/<BAD-INV-ORDER> (corba/<system-exception>)
  sealed slot corba/BAD-INV-ORDER/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/BAD-INV-ORDER/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<BAD-INV-ORDER>));
define sealed domain initialize (corba/<BAD-INV-ORDER>);

define constant corba/$BAD-INV-ORDER-typecode =
  make(<exception-typecode>,
       name: "BAD_INV_ORDER",
       type: corba/<BAD-INV-ORDER>,
       repository-id: "IDL:omg.org/CORBA/BAD_INV_ORDER:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/BAD-INV-ORDER/minor,
			    setter: corba/BAD-INV-ORDER/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/BAD-INV-ORDER/completed,
			    setter: corba/BAD-INV-ORDER/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<BAD-INV-ORDER>)
 => (typecode :: <typecode>)
  corba/$BAD-INV-ORDER-typecode
end method;

define sealed method class-typecode (class == corba/<BAD-INV-ORDER>)
 => (typecode :: <typecode>)
  corba/$BAD-INV-ORDER-typecode
end method;

define sealed class corba/<TRANSIENT> (corba/<system-exception>)
  sealed slot corba/TRANSIENT/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/TRANSIENT/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<TRANSIENT>));
define sealed domain initialize (corba/<TRANSIENT>);

define constant corba/$TRANSIENT-typecode =
  make(<exception-typecode>,
       name: "TRANSIENT",
       type: corba/<TRANSIENT>,
       repository-id: "IDL:omg.org/CORBA/TRANSIENT:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/TRANSIENT/minor,
			    setter: corba/TRANSIENT/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/TRANSIENT/completed,
			    setter: corba/TRANSIENT/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<TRANSIENT>)
 => (typecode :: <typecode>)
  corba/$TRANSIENT-typecode
end method;

define sealed method class-typecode (class == corba/<TRANSIENT>)
 => (typecode :: <typecode>)
  corba/$TRANSIENT-typecode
end method;

define sealed class corba/<FREE-MEM> (corba/<system-exception>)
  sealed slot corba/FREE-MEM/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/FREE-MEM/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<FREE-MEM>));
define sealed domain initialize (corba/<FREE-MEM>);

define constant corba/$FREE-MEM-typecode =
  make(<exception-typecode>,
       name: "FREE_MEM",
       type: corba/<FREE-MEM>,
       repository-id: "IDL:omg.org/CORBA/FREE_MEM:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/FREE-MEM/minor,
			    setter: corba/FREE-MEM/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/FREE-MEM/completed,
			    setter: corba/FREE-MEM/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<FREE-MEM>)
 => (typecode :: <typecode>)
  corba/$FREE-MEM-typecode
end method;

define sealed method class-typecode (class == corba/<FREE-MEM>)
 => (typecode :: <typecode>)
  corba/$FREE-MEM-typecode
end method;

define sealed class corba/<INV-IDENT> (corba/<system-exception>)
  sealed slot corba/INV-IDENT/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INV-IDENT/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INV-IDENT>));
define sealed domain initialize (corba/<INV-IDENT>);

define constant corba/$INV-IDENT-typecode =
  make(<exception-typecode>,
       name: "INV_IDENT",
       type: corba/<INV-IDENT>,
       repository-id: "IDL:omg.org/CORBA/INV_IDENT:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INV-IDENT/minor,
			    setter: corba/INV-IDENT/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INV-IDENT/completed,
			    setter: corba/INV-IDENT/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INV-IDENT>)
 => (typecode :: <typecode>)
  corba/$INV-IDENT-typecode
end method;

define sealed method class-typecode (class == corba/<INV-IDENT>)
 => (typecode :: <typecode>)
  corba/$INV-IDENT-typecode
end method;

define sealed class corba/<INV-FLAG> (corba/<system-exception>)
  sealed slot corba/INV-FLAG/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INV-FLAG/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INV-FLAG>));
define sealed domain initialize (corba/<INV-FLAG>);

define constant corba/$INV-FLAG-typecode =
  make(<exception-typecode>,
       name: "INV_FLAG",
       type: corba/<INV-FLAG>,
       repository-id: "IDL:omg.org/CORBA/INV_FLAG:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INV-FLAG/minor,
			    setter: corba/INV-FLAG/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INV-FLAG/completed,
			    setter: corba/INV-FLAG/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INV-FLAG>)
 => (typecode :: <typecode>)
  corba/$INV-FLAG-typecode
end method;

define sealed method class-typecode (class == corba/<INV-FLAG>)
 => (typecode :: <typecode>)
  corba/$INV-FLAG-typecode
end method;

define sealed class corba/<INTF-REPOS> (corba/<system-exception>)
  sealed slot corba/INTF-REPOS/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INTF-REPOS/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INTF-REPOS>));
define sealed domain initialize (corba/<INTF-REPOS>);

define constant corba/$INTF-REPOS-typecode =
  make(<exception-typecode>,
       name: "INTF_REPOS",
       type: corba/<INTF-REPOS>,
       repository-id: "IDL:omg.org/CORBA/INTF_REPOS:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INTF-REPOS/minor,
			    setter: corba/INTF-REPOS/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INTF-REPOS/completed,
			    setter: corba/INTF-REPOS/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INTF-REPOS>)
 => (typecode :: <typecode>)
  corba/$INTF-REPOS-typecode
end method;

define sealed method class-typecode (class == corba/<INTF-REPOS>)
 => (typecode :: <typecode>)
  corba/$INTF-REPOS-typecode
end method;

define sealed class corba/<BAD-CONTEXT> (corba/<system-exception>)
  sealed slot corba/BAD-CONTEXT/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/BAD-CONTEXT/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<BAD-CONTEXT>));
define sealed domain initialize (corba/<BAD-CONTEXT>);

define constant corba/$BAD-CONTEXT-typecode =
  make(<exception-typecode>,
       name: "BAD_CONTEXT",
       type: corba/<BAD-CONTEXT>,
       repository-id: "IDL:omg.org/CORBA/BAD_CONTEXT:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/BAD-CONTEXT/minor,
			    setter: corba/BAD-CONTEXT/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/BAD-CONTEXT/completed,
			    setter: corba/BAD-CONTEXT/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<BAD-CONTEXT>)
 => (typecode :: <typecode>)
  corba/$BAD-CONTEXT-typecode
end method;

define sealed method class-typecode (class == corba/<BAD-CONTEXT>)
 => (typecode :: <typecode>)
  corba/$BAD-CONTEXT-typecode
end method;

define sealed class corba/<OBJ-ADAPTER> (corba/<system-exception>)
  sealed slot corba/OBJ-ADAPTER/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/OBJ-ADAPTER/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<OBJ-ADAPTER>));
define sealed domain initialize (corba/<OBJ-ADAPTER>);

define constant corba/$OBJ-ADAPTER-typecode =
  make(<exception-typecode>,
       name: "OBJ_ADAPTER",
       type: corba/<OBJ-ADAPTER>,
       repository-id: "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/OBJ-ADAPTER/minor,
			    setter: corba/OBJ-ADAPTER/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/OBJ-ADAPTER/completed,
			    setter: corba/OBJ-ADAPTER/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<OBJ-ADAPTER>)
 => (typecode :: <typecode>)
  corba/$OBJ-ADAPTER-typecode
end method;

define sealed method class-typecode (class == corba/<OBJ-ADAPTER>)
 => (typecode :: <typecode>)
  corba/$OBJ-ADAPTER-typecode
end method;

define sealed class corba/<DATA-CONVERSION> (corba/<system-exception>)
  sealed slot corba/DATA-CONVERSION/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/DATA-CONVERSION/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<DATA-CONVERSION>));
define sealed domain initialize (corba/<DATA-CONVERSION>);

define constant corba/$DATA-CONVERSION-typecode =
  make(<exception-typecode>,
       name: "DATA_CONVERSION",
       type: corba/<DATA-CONVERSION>,
       repository-id: "IDL:omg.org/CORBA/DATA_CONVERSION:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/DATA-CONVERSION/minor,
			    setter: corba/DATA-CONVERSION/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/DATA-CONVERSION/completed,
			    setter: corba/DATA-CONVERSION/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<DATA-CONVERSION>)
 => (typecode :: <typecode>)
  corba/$DATA-CONVERSION-typecode
end method;

define sealed method class-typecode (class == corba/<DATA-CONVERSION>)
 => (typecode :: <typecode>)
  corba/$DATA-CONVERSION-typecode
end method;

define sealed class corba/<OBJECT-NOT-EXIST> (corba/<system-exception>)
  sealed slot corba/OBJECT-NOT-EXIST/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/OBJECT-NOT-EXIST/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<OBJECT-NOT-EXIST>));
define sealed domain initialize (corba/<OBJECT-NOT-EXIST>);

define constant corba/$OBJECT-NOT-EXIST-typecode =
  make(<exception-typecode>,
       name: "OBJECT_NOT_EXIST",
       type: corba/<OBJECT-NOT-EXIST>,
       repository-id: "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/OBJECT-NOT-EXIST/minor,
			    setter: corba/OBJECT-NOT-EXIST/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/OBJECT-NOT-EXIST/completed,
			    setter: corba/OBJECT-NOT-EXIST/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<OBJECT-NOT-EXIST>)
 => (typecode :: <typecode>)
  corba/$OBJECT-NOT-EXIST-typecode
end method;

define sealed method class-typecode (class == corba/<OBJECT-NOT-EXIST>)
 => (typecode :: <typecode>)
  corba/$OBJECT-NOT-EXIST-typecode
end method;

define sealed class corba/<TRANSACTION-REQUIRED> (corba/<system-exception>)
  sealed slot corba/TRANSACTION-REQUIRED/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/TRANSACTION-REQUIRED/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<TRANSACTION-REQUIRED>));
define sealed domain initialize (corba/<TRANSACTION-REQUIRED>);

define constant corba/$TRANSACTION-REQUIRED-typecode =
  make(<exception-typecode>,
       name: "TRANSACTION_REQUIRED",
       type: corba/<TRANSACTION-REQUIRED>,
       repository-id: "IDL:omg.org/CORBA/TRANSACTION_REQUIRED:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/TRANSACTION-REQUIRED/minor,
			    setter: corba/TRANSACTION-REQUIRED/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/TRANSACTION-REQUIRED/completed,
			    setter: corba/TRANSACTION-REQUIRED/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<TRANSACTION-REQUIRED>)
 => (typecode :: <typecode>)
  corba/$TRANSACTION-REQUIRED-typecode
end method;

define sealed method class-typecode (class == corba/<TRANSACTION-REQUIRED>)
 => (typecode :: <typecode>)
  corba/$TRANSACTION-REQUIRED-typecode
end method;

define sealed class corba/<TRANSACTION-ROLLEDBACK> (corba/<system-exception>)
  sealed slot corba/TRANSACTION-ROLLEDBACK/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/TRANSACTION-ROLLEDBACK/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<TRANSACTION-ROLLEDBACK>));
define sealed domain initialize (corba/<TRANSACTION-ROLLEDBACK>);

define constant corba/$TRANSACTION-ROLLEDBACK-typecode =
  make(<exception-typecode>,
       name: "TRANSACTION_ROLLEDBACK",
       type: corba/<TRANSACTION-ROLLEDBACK>,
       repository-id: "IDL:omg.org/CORBA/TRANSACTION_ROLLEDBACK:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/TRANSACTION-ROLLEDBACK/minor,
			    setter: corba/TRANSACTION-ROLLEDBACK/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/TRANSACTION-ROLLEDBACK/completed,
			    setter: corba/TRANSACTION-ROLLEDBACK/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<TRANSACTION-ROLLEDBACK>)
 => (typecode :: <typecode>)
  corba/$TRANSACTION-ROLLEDBACK-typecode
end method;

define sealed method class-typecode (class == corba/<TRANSACTION-ROLLEDBACK>)
 => (typecode :: <typecode>)
  corba/$TRANSACTION-ROLLEDBACK-typecode
end method;

define sealed class corba/<INVALID-TRANSACTION> (corba/<system-exception>)
  sealed slot corba/INVALID-TRANSACTION/minor :: corba/<unsigned-long>, required-init-keyword: minor:;
  sealed slot corba/INVALID-TRANSACTION/completed :: corba/<completion-status>, required-init-keyword: completed:;
end class;

define sealed domain make (singleton(corba/<INVALID-TRANSACTION>));
define sealed domain initialize (corba/<INVALID-TRANSACTION>);

define constant corba/$INVALID-TRANSACTION-typecode =
  make(<exception-typecode>,
       name: "INVALID_TRANSACTION",
       type: corba/<INVALID-TRANSACTION>,
       repository-id: "IDL:omg.org/CORBA/INVALID_TRANSACTION:1.0",
       members: vector(
		       make(<typecode-member>,
			    name: "minor",
			    typecode: corba/$unsigned-long-typecode,
			    getter: corba/INVALID-TRANSACTION/minor,
			    setter: corba/INVALID-TRANSACTION/minor-setter,
			    init-keyword: minor:),
		       make(<typecode-member>,
			    name: "completed",
			    typecode: corba/$completion-status-typecode,
			    getter: corba/INVALID-TRANSACTION/completed,
			    setter: corba/INVALID-TRANSACTION/completed-setter,
			    init-keyword: completed:)
			 ));		       

define sealed method object-typecode (object :: corba/<INVALID-TRANSACTION>)
 => (typecode :: <typecode>)
  corba/$INVALID-TRANSACTION-typecode
end method;

define sealed method class-typecode (class == corba/<INVALID-TRANSACTION>)
 => (typecode :: <typecode>)
  corba/$INVALID-TRANSACTION-typecode
end method;


/// SYSTEM-EXCEPTIONS

define method system-exceptions ()
  vector(
	 corba/<UNKNOWN>,
	 corba/<NO-MEMORY>,
	 corba/<IMP-LIMIT>,
	 corba/<COMM-FAILURE>,
	 corba/<INV-OBJREF>,
	 corba/<NO-PERMISSION>,
	 corba/<INTERNAL>,
	 corba/<MARSHAL>,
	 corba/<INITIALIZE>,
	 corba/<NO-IMPLEMENT>,
	 corba/<BAD-TYPECODE>,
	 corba/<BAD-OPERATION>,
	 corba/<NO-RESOURCES>,
	 corba/<NO-RESPONSE>,
	 corba/<PERSIST-STORE>,
	 corba/<BAD-INV-ORDER>,
	 corba/<TRANSIENT>,
	 corba/<FREE-MEM>,
	 corba/<INV-IDENT>,
	 corba/<INV-FLAG>,
	 corba/<INTF-REPOS>,
	 corba/<BAD-CONTEXT>,
	 corba/<OBJ-ADAPTER>,
	 corba/<DATA-CONVERSION>,
	 corba/<OBJECT-NOT-EXIST>,
	 corba/<TRANSACTION-REQUIRED>,
	 corba/<TRANSACTION-ROLLEDBACK>,
	 corba/<INVALID-TRANSACTION>
	   );
end method;

	 
