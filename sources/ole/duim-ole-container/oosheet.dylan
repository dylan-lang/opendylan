Module:    DUIM-OLE-Container
Synopsis:  OLE object sheet -- class and methods for an embedded document.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open primary class <ole-gadget> ( duim/<gadget>,
					 duim/<space-requirement-mixin>,
					 duim/<leaf-layout-mixin>,
					 duim/<mirrored-sheet-mixin>,
					 duim/<basic-sheet> )
  sealed slot sheet-contained-object :: false-or(<duim-contained-object>) = #f,
		init-keyword: contained-object:;

  // The following three values are used only to create an OLE object if
  // one has not already been supplied above.
  sealed slot sheet-ole-class-id :: <REFCLSID> = null-pointer(<REFCLSID>);
  sealed slot sheet-ole-file-name :: false-or(<string>) = #f;
  sealed slot sheet-insert-options :: <sequence> = #[],
    init-keyword: insert-options:;

  slot %documentation :: false-or(<string>) = #f,
    init-keyword: documentation:,
    setter: duim/gadget-documentation-setter;
end <ole-gadget>;

define method initialize (sheet :: <ole-gadget>,
			  #rest args,
			  #key contained-object,
                            class-id, file-name, #all-keys) => ();
  let obj = contained-object;
  if ( obj )
    obj.object-sheet := sheet;
  else
    if ( file-name & ~ empty?(file-name) )
      sheet.sheet-ole-file-name := file-name;
    elseif ( class-id )
      sheet.sheet-ole-class-id := as(<REFCLSID>, class-id); // not freed ???
    else
      error("make(%=) must specify either class-id: or file-name:",
	    object-class(sheet));
    end if;
  end if;
  next-method();
end method initialize;

define method gadget-ole-class-id ( sheet :: <ole-gadget> )
 => (class-id :: false-or(<REFCLSID>));
  let obj = sheet.sheet-contained-object;
  if ( obj )
    obj.document-class-id // what it is
  else
    sheet.sheet-ole-class-id // what it will be
  end if
end gadget-ole-class-id;

define method gadget-ole-file-name ( sheet :: <ole-gadget> )
 => (file-name :: false-or(<string>));
  let obj = sheet.sheet-contained-object;
  if ( obj )
    obj.document-file-name
  else
    sheet.sheet-ole-file-name
  end if
end gadget-ole-file-name;

define method duim/port-handles-repaint?
    (_port :: duim/<win32-port>, sheet :: <ole-gadget>)
 => (port-handles? :: <boolean>)
  #f
end method duim/port-handles-repaint?;

define method duim/repaint-sheet
    (sheet :: <ole-gadget>, region :: duim/<region>,
     #key medium, force?) => ()
  ignore(force?);
  let obj = sheet.sheet-contained-object;
  if ( obj & obj.alive? )
    paint-contained-document
      (obj, duim/get-DC(medium |
			  sheet.duim/sheet-parent.duim/sheet-medium));
  end if;
end method;

define method duim/gadget-enabled?
    (gadget :: <ole-gadget>) => (enabled? :: <boolean>)
  let obj = gadget.sheet-contained-object;
  obj & obj.document-in-place-active?
end method duim/gadget-enabled?;

define method duim/gadget-enabled?-setter
    (enabled? :: <boolean>, gadget :: <ole-gadget>) => (enabled? :: <boolean>)
  let obj = gadget.sheet-contained-object;
  if ( obj )
    if ( enabled? ~= obj.document-in-place-active? )
      if ( enabled? )
	container-do-verb(obj, $OLEIVERB-PRIMARY, null-pointer(w/<LPMSG>));
      else
	container-UI-deactivate(obj);
      end if;
    end if;
  elseif ( enabled? )
    error("Can't enable %= yet", gadget); // ???
  end if;
  enabled?
end method duim/gadget-enabled?-setter;

define method duim/gadget-documentation (gadget :: <ole-gadget>)
 => ( documentation :: false-or(<string>) )
  gadget.%documentation |
  begin
    let obj = gadget.sheet-contained-object;
    obj & (gadget.duim/gadget-documentation := 
	     get-server-name(obj, $USERCLASSTYPE-SHORT))
  end
end;
