Module:    OLE-Control-Framework
Synopsis:  main class for representing an OLE Control
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <fixnum> = <integer>; // formerly known as <small-integer>

define open COM-interface <ole-control-framework>
  ( <ole-in-process-server>, <Simple-Component-Object> )

  // interfaces implemented:
  slot server-IPersistStream :: <CPersistStreamInit>;
  slot server-info :: <CProvideClassInfo>;
  slot server-IOleControl :: <COleControl>;

  // state variables:
  slot freeze-events? :: <boolean> = #f;
  slot hatch-when-UI-active? :: <boolean> = #t;
  slot freeze-UI? :: <boolean> = #f;
  slot OLE-util-locale :: <integer> = $LOCALE-USER-DEFAULT;

  // interfaces provided by the container:
  slot container-IOleControlSite :: <interface> = $null-interface;
  // interface for getting ambient properties from the container:
  slot container-IDispatch :: <interface> = $null-interface;

end <ole-control-framework>;

define method initialize ( obj :: <ole-control-framework>,
			   #rest args, #key) => ();
  next-method();

  obj.dll-active-interface-counter := obj.dll-active-interface-counter + 1;
  obj.server-info :=
    make(if ( null?(obj.object-default-source-interface) )
	   <CProvideClassInfo> // no source interface
	 else
	   <CProvideClassInfo2> // support GetGUID
	 end if,
	 server-framework: obj, controlling-unknown: obj);

  obj.server-IPersistStream := make(<CPersistStreamInit>,
				    server-framework: obj,
				    controlling-unknown: obj);
  obj.server-IOleControl := make(<COleControl>, server-framework: obj,
				 controlling-unknown: obj);
  values()
end method initialize;

define method terminate (obj :: <ole-control-framework>) => ();
  Output-Debug-String("terminate(<ole-control-framework>)\r\n");
  next-method();
  Release(obj.container-IOleControlSite);
  obj.container-IOleControlSite := $null-interface;
  Release(obj.container-IDispatch);
  obj.container-IDispatch := $null-interface;
  obj.dll-active-interface-counter := obj.dll-active-interface-counter - 1;
  values();
end method terminate;




define COM-interface <CProvideClassInfo> ( <IProvideClassInfo> )

  constant slot get-obj :: <ole-control-framework>,
		required-init-keyword: server-framework: ;
end;

define COM-interface <CProvideClassInfo2> ( <IProvideClassInfo2>,
					    <CProvideClassInfo> )
end;

define method IProvideClassInfo/GetClassInfo (this :: <CProvideClassInfo>)
 => (status :: <HRESULT>, pTI :: <LPTYPEINFO>);

  let typeinfo = this.get-obj.object-typeinfo;
  AddRef(typeinfo);
  values($S-OK, typeinfo)
end IProvideClassInfo/GetClassInfo;

define method IProvideClassInfo2/GetGUID (this :: <CProvideClassInfo2>,
					  kind :: <fixnum>,
					  pGUID :: <REFGUID>)
 => (status :: <HRESULT>);
  
  select ( kind )
    $GUIDKIND-DEFAULT-SOURCE-DISP-IID =>
      // outgoing dispinterface, labeled [source, default].
      
      let typeinfo = this.get-obj.object-default-source-interface;
      let ID = typeinfo.guid-value;
      %memcpy(pGUID, ID, size-of(<CLSID>));
      $S-OK;
    otherwise =>
      $E-INVALIDARG;
  end select
end IProvideClassInfo2/GetGUID;

