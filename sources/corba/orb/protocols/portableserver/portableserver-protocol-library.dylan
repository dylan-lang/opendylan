Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library portableserver-protocol
  use corba-dylan;
  use corba-protocol;
  export portableserver-protocol;
end library;

define module portableserver-protocol
  use corba-dylan;
  use corba-protocol;

  create
    portableserver/<servant>,
    portableserver/<dynamic-servant>,
    portableserver/<poamanager>,
    portableserver/<adapteractivator>,
    portableserver/<servantmanager>,
    portableserver/<servantactivator>,
    portableserver/<servantlocator>,
    portableserver/<poa>,
    portableserver/poa/the-name,
    portableserver/poa/the-parent,
    portableserver/poa/the-poamanager,
    portableserver/poa/the-activator, portableserver/poa/the-activator-setter,
    portableserver/adapteractivator/unknown-adapter,
    portableserver/servantactivator/incarnate,
    portableserver/servantactivator/etherealize,
    portableserver/servantlocator/preinvoke,
    portableserver/servantlocator/postinvoke,
    portableserver/poa/<adapteralreadyexists>,
    portableserver/poa/<adapternonexistent>,
    portableserver/poa/<noservant>,
    portableserver/poa/<objectalreadyactive>,
    portableserver/poa/<objectnotactive>,
    portableserver/poa/<servantalreadyactive>,
    portableserver/poa/<servantnotactive>,
    portableserver/poa/<wrongadapter>,
    portableserver/poa/<wrongpolicy>,
    portableserver/poamanager/<adapterinactive>,
    portableserver/poa/<invalidpolicy>,
    portableserver/<forwardrequest>,
    portableserver/poa/create-poa,
    portableserver/poa/destroy,
    portableserver/poa/find-poa,
    portableserver/poa/get-servant-manager,
    portableserver/poa/set-servant-manager,
    portableserver/poa/get-servant,
    portableserver/poa/set-servant,
    portableserver/poa/activate-object-with-id,
    portableserver/poa/activate-object,
    portableserver/poa/deactivate-object,
    portableserver/poa/create-reference,
    portableserver/poa/create-reference-with-id,
    portableserver/poa/servant-to-id,
    portableserver/poa/servant-to-reference,
    portableserver/poa/id-to-servant,
    portableserver/poa/id-to-reference,
    portableserver/poa/reference-to-servant,
    portableserver/poa/reference-to-id,
    portableserver/poamanager/activate,
    portableserver/poamanager/deactivate,
    portableserver/poamanager/hold-requests,
    portableserver/poamanager/discard-requests,
    portableserver/forwardrequest/forward-reference,
    portableserver/forwardrequest/forward-reference-setter,
    portableserver/poa/invalidpolicy/index,
    portableserver/poa/invalidpolicy/index-setter;

  create
    corba/serverrequest/invoke,
    corba/serverrequest/forward,
    portableserver/servant/primary-interface,
    invoke-operation;

  create
    *optimize-collocation?*,
    maybe-collocated-invoke;

  create
    portableserver/<current>,
    portableserver/current/<nocontext>,
    portableserver/current/get-poa,
    portableserver/current/get-object-id;

end module;

