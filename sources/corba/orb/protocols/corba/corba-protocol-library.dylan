Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-protocol
  use corba-dylan;
  export corba-protocol-externals;
  export corba-protocol;
end library;

define module corba-protocol-externals
  create
    corba/<null>,
    corba/<void>,
    corba/<long>,
    corba/<short>,
    corba/<unsigned-long>,
    corba/<unsigned-short>,
    corba/<float>,
    corba/<double>,
    corba/<octet>,
    corba/<char>,
    corba/<boolean>,
    corba/<string>,
    corba/<sequence>,
    corba/<array>,
    corba/<struct>,
    corba/<exception>,
    corba/<user-exception>,
    corba/<system-exception>,
    corba/<any>,
    corba/<union>,
    corba/<encapsulation>;

  create
    corba/any/type, corba/any/type-setter,
    corba/any/value, corba/any/value-setter,
    corba/union/discriminator, corba/union/discriminator-setter,
    corba/union/value, corba/union/value-setter;

  create
    <unknown-type-any-coercion-error>,
    <unknown-type-any-coercion-restart>,
    <incompatible-type-any-coercion-error>;

  create
    corba/<Identifier>,
    corba/<Flags>,
    corba/<NamedValue>,
    corba/NamedValue/name,
    corba/NamedValue/argument,
    corba/NamedValue/len,
    corba/NamedValue/arg-modes,
    corba/<NVList>,
    corba/$arg-in,
    corba/$arg-out,
    corba/$arg-inout,
    corba/$inv-no-response;

  create
    corba/<object>,
    corba/object/is-nil,
    corba/object/is-a,
    corba/object/non-existent,
    corba/object/duplicate,
    corba/object/release,
    corba/object/is-equivalent,
    corba/object/hash,
    corba/object/create-request,
    make-nil;

  create
    corba/<completion-status>,
    corba/<exception-type>,
    corba/<UNKNOWN>,
    corba/UNKNOWN/minor, corba/UNKNOWN/minor-setter,
    corba/UNKNOWN/completed, corba/UNKNOWN/completed-setter,
    corba/<BAD-PARAM>,
    corba/BAD-PARAM/minor, corba/BAD-PARAM/minor-setter,
    corba/BAD-PARAM/completed, corba/BAD-PARAM/completed-setter,
    corba/<NO-MEMORY>,
    corba/NO-MEMORY/minor, corba/NO-MEMORY/minor-setter,
    corba/NO-MEMORY/completed, corba/NO-MEMORY/completed-setter,
    corba/<IMP-LIMIT>,
    corba/IMP-LIMIT/minor, corba/IMP-LIMIT/minor-setter,
    corba/IMP-LIMIT/completed, corba/IMP-LIMIT/completed-setter,
    corba/<COMM-FAILURE>,
    corba/COMM-FAILURE/minor, corba/COMM-FAILURE/minor-setter,
    corba/COMM-FAILURE/completed, corba/COMM-FAILURE/completed-setter,
    corba/<INV-OBJREF>,
    corba/INV-OBJREF/minor, corba/INV-OBJREF/minor-setter,
    corba/INV-OBJREF/completed, corba/INV-OBJREF/completed-setter,
    corba/<NO-PERMISSION>,
    corba/NO-PERMISSION/minor, corba/NO-PERMISSION/minor-setter,
    corba/NO-PERMISSION/completed, corba/NO-PERMISSION/completed-setter,
    corba/<INTERNAL>,
    corba/INTERNAL/minor, corba/INTERNAL/minor-setter,
    corba/INTERNAL/completed, corba/INTERNAL/completed-setter,
    corba/<MARSHAL>,
    corba/MARSHAL/minor, corba/MARSHAL/minor-setter,
    corba/MARSHAL/completed, corba/MARSHAL/completed-setter,
    corba/<INITIALIZE>,
    corba/INITIALIZE/minor, corba/INITIALIZE/minor-setter,
    corba/INITIALIZE/completed, corba/INITIALIZE/completed-setter,
    corba/<NO-IMPLEMENT>,
    corba/NO-IMPLEMENT/minor, corba/NO-IMPLEMENT/minor-setter,
    corba/NO-IMPLEMENT/completed, corba/NO-IMPLEMENT/completed-setter,
    corba/<BAD-TYPECODE>,
    corba/BAD-TYPECODE/minor, corba/BAD-TYPECODE/minor-setter,
    corba/BAD-TYPECODE/completed, corba/BAD-TYPECODE/completed-setter,
    corba/<BAD-OPERATION>,
    corba/BAD-OPERATION/minor, corba/BAD-OPERATION/minor-setter,
    corba/BAD-OPERATION/completed, corba/BAD-OPERATION/completed-setter,
    corba/<NO-RESOURCES>,
    corba/NO-RESOURCES/minor, corba/NO-RESOURCES/minor-setter,
    corba/NO-RESOURCES/completed, corba/NO-RESOURCES/completed-setter,
    corba/<NO-RESPONSE>,
    corba/NO-RESPONSE/minor, corba/NO-RESPONSE/minor-setter,
    corba/NO-RESPONSE/completed, corba/NO-RESPONSE/completed-setter,
    corba/<PERSIST-STORE>,
    corba/PERSIST-STORE/minor, corba/PERSIST-STORE/minor-setter,
    corba/PERSIST-STORE/completed, corba/PERSIST-STORE/completed-setter,
    corba/<BAD-INV-ORDER>,
    corba/BAD-INV-ORDER/minor, corba/BAD-INV-ORDER/minor-setter,
    corba/BAD-INV-ORDER/completed, corba/BAD-INV-ORDER/completed-setter,
    corba/<TRANSIENT>,
    corba/TRANSIENT/minor, corba/TRANSIENT/minor-setter,
    corba/TRANSIENT/completed, corba/TRANSIENT/completed-setter,
    corba/<FREE-MEM>,
    corba/FREE-MEM/minor, corba/FREE-MEM/minor-setter,
    corba/FREE-MEM/completed, corba/FREE-MEM/completed-setter,
    corba/<INV-IDENT>,
    corba/INV-IDENT/minor, corba/INV-IDENT/minor-setter,
    corba/INV-IDENT/completed, corba/INV-IDENT/completed-setter,
    corba/<INV-FLAG>,
    corba/INV-FLAG/minor, corba/INV-FLAG/minor-setter,
    corba/INV-FLAG/completed, corba/INV-FLAG/completed-setter,
    corba/<INTF-REPOS>,
    corba/INTF-REPOS/minor, corba/INTF-REPOS/minor-setter,
    corba/INTF-REPOS/completed, corba/INTF-REPOS/completed-setter,
    corba/<BAD-CONTEXT>,
    corba/BAD-CONTEXT/minor, corba/BAD-CONTEXT/minor-setter,
    corba/BAD-CONTEXT/completed, corba/BAD-CONTEXT/completed-setter,
    corba/<OBJ-ADAPTER>,
    corba/OBJ-ADAPTER/minor, corba/OBJ-ADAPTER/minor-setter,
    corba/OBJ-ADAPTER/completed, corba/OBJ-ADAPTER/completed-setter,
    corba/<DATA-CONVERSION>,
    corba/DATA-CONVERSION/minor, corba/DATA-CONVERSION/minor-setter,
    corba/DATA-CONVERSION/completed, corba/DATA-CONVERSION/completed-setter,
    corba/<OBJECT-NOT-EXIST>,
    corba/OBJECT-NOT-EXIST/minor, corba/OBJECT-NOT-EXIST/minor-setter,
    corba/OBJECT-NOT-EXIST/completed, corba/OBJECT-NOT-EXIST/completed-setter,
    corba/<TRANSACTION-REQUIRED>,
    corba/TRANSACTION-REQUIRED/minor, corba/TRANSACTION-REQUIRED/minor-setter,
    corba/TRANSACTION-REQUIRED/completed, corba/TRANSACTION-REQUIRED/completed-setter,
    corba/<TRANSACTION-ROLLEDBACK>,
    corba/TRANSACTION-ROLLEDBACK/minor, corba/TRANSACTION-ROLLEDBACK/minor-setter,
    corba/TRANSACTION-ROLLEDBACK/completed, corba/TRANSACTION-ROLLEDBACK/completed-setter,
    corba/<INVALID-TRANSACTION>,
    corba/INVALID-TRANSACTION/minor, corba/INVALID-TRANSACTION/minor-setter,
    corba/INVALID-TRANSACTION/completed, corba/INVALID-TRANSACTION/completed-setter;

  create
    corba/<Context>,
    corba/context/context-name,
    corba/context/parent,
    corba/context/create-child,
    corba/context/set-one-value,
    corba/context/set-values,
    corba/context/delete-values,
    corba/context/get-values,
    corba/$ctx-restrict-scope;

  create
    corba/<orb>,
    corba/orb/object-to-string,
    corba/orb/string-to-object,
    corba/orb/object-to-file, // NB non-standard
    corba/orb/file-to-object, // NB non-standard
    corba/orb/create-list,
    corba/orb/get-default-context,
    corba/orb/<objectid>,
    corba/orb/<objectidlist>,
    corba/orb/<invalidname>,
    corba/orb/list-initial-services,
    corba/orb/resolve-initial-references,
    corba/orb/work-pending,
    corba/orb/perform-work,
    corba/orb/shutdown,
    corba/orb/run,
    corba/<orbid>,
    corba/<arg-list>,
    corba/orb-init;

  create
    corba/<request>,
    corba/request/add-arg,
    corba/request/add-exception, // NB non-standard
    corba/request/add-context, // NB non-standard
    corba/request/invoke,
    corba/request/delete,
    corba/request/send,
    corba/request/get-response;

  create
    corba/<serverrequest>,
    corba/serverrequest/operation,
    corba/serverrequest/arguments,
    corba/serverrequest/ctx,
    corba/serverrequest/set-result,
    corba/serverrequest/set-exception;

  create
    corba/<current>;

  create
    CORBA/<RepositoryId>,
    CORBA/<TCKind>,
    CORBA/TCKind/successor,
    CORBA/TCKind/predecessor,
    CORBA/TCKind/>,
    CORBA/TCKind/<,
    CORBA/$TCKind-typecode,
    CORBA/<TypeCode>,
    CORBA/$TypeCode-typecode,
    CORBA/TypeCode/<Bounds>,
    CORBA/TypeCode/$Bounds-typecode,
    CORBA/TypeCode/<BadKind>,
    CORBA/TypeCode/$BadKind-typecode,
    CORBA/TypeCode/equal,
    CORBA/TypeCode/kind,
    CORBA/TypeCode/id,
    CORBA/TypeCode/name,
    CORBA/TypeCode/member-count,
    CORBA/TypeCode/member-name,
    CORBA/TypeCode/member-type,
    CORBA/TypeCode/member-label,
    CORBA/TypeCode/discriminator-type,
    CORBA/TypeCode/default-index,
    CORBA/TypeCode/length,
    CORBA/TypeCode/content-type;

  create
    object-typecode,
    class-typecode,
    corba/$null-typecode,
    corba/$void-typecode,
    corba/$short-typecode,
    corba/$long-typecode,
    corba/$unsigned-short-typecode,
    corba/$unsigned-long-typecode,
    corba/$float-typecode,
    corba/$double-typecode,
    corba/$boolean-typecode,
    corba/$char-typecode,
    corba/$octet-typecode,
    corba/$any-typecode,
    corba/$typecode-typecode,
    corba/$principal-typecode,
    corba/$string-typecode;

end module;

define module corba-protocol
  use corba-dylan;
  use corba-protocol-externals, export: all;

  export
    <anonymous-object>,
    <anonymous-struct>,
    <anonymous-exception>,
    anonymous-object-properties, anonymous-object-properties-setter;

  export
    corba/object/ior, corba/object/ior-setter,
    system-exceptions,
    ensure-any-typecode-consistent,
    ensure-any-native-type-consistent;

  export
    <typecode>,
    <empty-typecode>,
    <simple-typecode>,
    <complex-typecode>,
    <type-typecode>,
    <membered-typecode>,
    <null-typecode>,
    <void-typecode>,
    <short-typecode>,
    <long-typecode>,
    <unsigned-short-typecode>,
    <unsigned-long-typecode>,
    <float-typecode>,
    <double-typecode>,
    <boolean-typecode>,
    <char-typecode>,
    <octet-typecode>,
    <any-typecode>,
    <typecode-typecode>,
    <typecode-typecode>,
    <object-reference-typecode>,
    <struct-typecode>,
    <typecode-member>,
    <union-typecode>,
    <typecode-branch>,
    <enum-typecode>,
    <string-typecode>,
    <sequence-typecode>,
    <array-typecode>,
    <alias-typecode>,
    <exception-typecode>,
    <indirection-typecode>;

  export
    typecode-kind,
    typecode-code,
    typecode-alignment,
    typecode-native-type, typecode-native-type-setter,
    typecode-anonymous-native-type,
    typecode-repository-id, typecode-repository-id-setter,
    typecode-name, typecode-name-setter,
    typecode-members, typecode-members-setter,
    typecode-count,
    typecode-singleton,
    typecode-member-name, typecode-member-name-setter,
    typecode-member-typecode, typecode-member-typecode-setter,
    typecode-member-getter, typecode-member-getter-setter,
    typecode-member-setter, typecode-member-setter-setter,
    typecode-member-init-keyword, typecode-member-init-keyword-setter,
    typecode-member-native-type, typecode-member-native-type-setter,
    typecode-discriminator-typecode, typecode-discriminator-typecode-setter,
    typecode-default-used, typecode-default-used-setter,
    typecode-label-value, typecode-label-value-setter,
    typecode-branch-tags, typecode-branch-tags-setter,
    typecode-symbol-index,
    typecode-max-length, typecode-max-length-setter,
    typecode-element-typecode, typecode-element-typecode-setter,
    typecode-max-length, typecode-max-length-setter,
    typecode-length, typecode-length-setter,
    typecode-aliased, typecode-aliased-setter,
    typecode-offset, typecode-offset-setter,
    typecode-nesting, typecode-nesting-setter;
    
end module;
