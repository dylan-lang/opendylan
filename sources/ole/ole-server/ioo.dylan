Module:    OLE-Server
Synopsis:  Methods for class <COleObject>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// This corresponds to these registration entries:
//	HKEY_CLASSES_ROOT\...\protocol\StdFileEditing\verb\1 = &Open
//	HKEY_CLASSES_ROOT\CLSID\{...}\Verb\1 = &Open,0,2
define constant $VERB-OPEN = 1;


// Called to notify the object of it's client site.

define method IOleObject/SetClientSite(this :: <COleObject>,
				       client-site :: <LPOLECLIENTSITE>)
	=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/SetClientSite\r\n");

  // AddRef it so it doesn't go away.
  AddRef(client-site);

  let obj = this.get-obj;

  // if we already have a client site, release it.
  Release(obj.container-IOleClientSite);

  // store copy of the client site.
  obj.container-IOleClientSite := client-site;

  /* return */ $S-OK

end method IOleObject/SetClientSite;


// Called to set up an advise on the OLE object.

define method IOleObject/Advise(this :: <COleObject>,
				advise-sink :: <LPADVISESINK>)
	=> (status :: <HRESULT>, connection-ID :: <integer>);

  Output-Debug-String("IOleObject/Advise\r\n");

  let obj :: <basic-ole-server> = this.get-obj;

  // if we haven't made an OleAdviseHolder yet, make one.
  if ( null?(obj.container-IOleAdviseHolder) )
    let ( status, holder ) = CreateOleAdviseHolder();
    check-ole-status(status, "CreateOleAdviseHolder", this);
    obj.container-IOleAdviseHolder := holder;
  end if;

  // pass this call onto the OleAdviseHolder.
  /* return */
  IOleAdviseHolder/Advise(obj.container-IOleAdviseHolder, advise-sink)
end method IOleObject/Advise;

// Called to pass strings for Window titles.

define method IOleObject/SetHostNames(this :: <COleObject>,
				      container-app-name :: <LPCOLESTR>,
				      container-obj-name :: <LPCOLESTR>)
		=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/SetHostNames\r\n");

  this.container-application-name := container-app-name;
  unless ( null-pointer?(container-obj-name) )
    this.container-document-name := container-obj-name;
  end unless;

  /* return */ $S-OK
end method IOleObject/SetHostNames;

define method OLE-util-container-name ( obj :: <basic-ole-server> )
 => ( app-name :: false-or(<string>), doc-name :: false-or(<string>) );
  let oo = obj.server-IOleObject;
  if ( null?(oo) )
    values(#f, #f)
  else
    values( oo.container-application-name, oo.container-document-name )
  end if
end method;


// Called by the container application to invoke a verb.

define method IOleObject/DoVerb(this :: <COleObject>,
				verb :: <integer>, // verb to be invoked
   // Don't bother specializing on arguments that are not directly used here.
				lpmsg /* :: <LPMSG> */, // window message
				active-site /* :: <LPOLECLIENTSITE> */,
				lindex /* :: <signed-long> */,//extended layout
				hwndParent /* :: <HWND> */,// containing window
				rect /* :: <LPCRECT> */ // space in container
				)
	=> status :: <HRESULT>;

 block()
  let status :: <HRESULT> = $S-OK;
	
  Output-Debug-String("IOleObject/DoVerb OLEIVERB-");

  let obj = this.get-obj;
  select ( verb )
		
    $OLEIVERB-SHOW, $OLEIVERB-PRIMARY => 
      Output-Debug-String("SHOW\r\n");

      if ( this.open-as-separate-window )
	SetFocus(this.open-as-separate-window); 
      elseif ( do-in-place-activate(obj, verb) = #f )
	let app-window = open-edit(this, active-site);
	if ( null?(app-window) )
	  status := $E-FAIL;
	else
	  this.open-as-separate-window := app-window;
	end if;
      end if;

    $OLEIVERB-UIACTIVATE,
    $OLEIVERB-INPLACEACTIVATE => 
      // See "Inside OLE", 2nd ed., page 1019 for the difference between these.
      Output-Debug-String(if ( verb = $OLEIVERB-INPLACEACTIVATE )
			    "INPLACEACTIVATE\r\n"
			      else
			    "UIACTIVATE\r\n"
			  end if);
      if ( this.open-as-separate-window )
	status := $E-FAIL;
      end if;

      // inplace activate
      unless ( do-in-place-activate(obj, verb) )
	status := $E-FAIL;
      end unless;

    $OLEIVERB-DISCARDUNDOSTATE => 
      Output-Debug-String("DISCARDUNDOSTATE\r\n");
      // don't have to worry about this situation as we don't
      // support an undo state.
      unless ( obj.in-place-active? )
	status := $OLE-E-NOT-INPLACEACTIVE;
      end unless;

    $OLEIVERB-HIDE => 
      Output-Debug-String("HIDE\r\n");
      // if in-place active, do an "in-place" hide, otherwise
      // just hide the app window.
      if ( obj.in-place-active? )
	OLE-part-in-place-deactivated(obj); // notify application before change
	remove-frame-level-UI(obj);
	do-in-place-hide(obj);
      else
	OLE-part-hide(obj);
      end if;

    $OLEIVERB-OPEN, $VERB-OPEN => 
      Output-Debug-String("OPEN\r\n");
      // if inplace active, deactivate
      if ( obj.in-place-active? )
	IOleInPlaceObject/InPlaceDeactivate(obj.server-IOleInPlaceObject);
      end if;
       
      // open into another window.
      let app-window = open-edit(this, active-site);
      if ( null?(app-window) )
	status := $E-FAIL;
      else
	this.open-as-separate-window := app-window;
      end if;

    otherwise =>
      debug-out(" %=\n", verb);
      if ( verb < 0 )
	status := $E-NOTIMPL;
      else
	status := $OLEOBJ-S-INVALIDVERB;
      end if;
  end select;
  status
 exception (<abort>)
   $E-ABORT
 end block
end method IOleObject/DoVerb;


// Used by IOleObject/DoVerb above to open the object into a separate window.

define method open-edit(this :: <COleObject>, active-site :: <LPOLECLIENTSITE>)
 => window :: <HWND>; 

  let obj = this.get-obj;
  // Don't know why it uses this instead of the argument which is not used.
  let site = obj.container-IOleClientSite;
  unless( null?( site ) )
    IOleClientSite/ShowObject(site);
    // tell the site we are opening so the object can be hatched out.
    IOleClientSite/OnShowWindow(site, #t);
  end unless;
  hide-hatch-window(obj);

  let window = OLE-part-open-out(obj);

  unless ( null?(window) ) // unless activation failed
    // Need to raise the new window above the container window:
    unless ( SetForegroundWindow(window) )
      debug-out("SetForegroundWindow error %d\n", GetLastError());
    end unless;
    if ( null?( SetFocus(window) ) )
      debug-out("SetFocus error %d\n", GetLastError());
    end if;
  end unless;
  window
end method open-edit;

// default method, may be overridden:
define method OLE-part-hide (obj :: <basic-ole-server>) => ();
  let oo :: <COleObject> = server-IOleObject(obj);
  let hwnd = oo.open-as-separate-window;
  unless ( null?(hwnd) )
    OLE-util-part-hidden(obj);
    ShowWindow(hwnd, $SW-HIDE);
  end unless;
  values()
end method;

// This users should call this in their implementation of OLE-part-hide
// if they don't use the default method.
define method OLE-util-part-hidden (obj :: <basic-ole-server>) => ()
  let oo :: <COleObject> = server-IOleObject(obj);
  oo.open-as-separate-window := #f;
  let site = container-IOleClientSite(obj);
  if (~null-pointer?(site))
    IOleClientSite/OnShowWindow(site, #f);
  end;
end;


// Returns the extent of the object.

define method IOleObject/GetExtent(this :: <COleObject>,
			draw-aspect :: <unsigned-fixnum>,
			lpsizel :: <LPSIZEL>) => status :: <HRESULT>;

  Output-Debug-String("IOleObject/GetExtent\r\n");

  // Only DVASPECT_CONTENT is supported....
  if ( draw-aspect = $DVASPECT-CONTENT )

    // return the correct size in HIMETRIC...
    let ( width, height ) = OLE-util-HIMETRIC-size(this.get-obj);
    lpsizel.cx-value := width;
    lpsizel.cy-value := height;
    $S-OK
  else
    $E-FAIL
  end if
end method IOleObject/GetExtent;


define method OLE-util-HIMETRIC-size ( this :: <basic-ole-server> )
 => ( width :: <integer>, height :: <integer> );

  // return the correct size in HIMETRIC...
  let ( width, height ) = OLE-util-current-size(this);
  pixels-to-himetric(width, height)
end method;


// Called to get the most up to date data

define method IOleObject/Update(this :: <COleObject>) => status :: <HRESULT>;

  Output-Debug-String("IOleObject/Update\r\n");

  // force an update
  OLE-util-send-view-change(this.get-obj);

  /* return */ $S-OK
end method IOleObject/Update;


// Called when the OLE object needs to be closed
define method IOleObject/Close(this :: <COleObject>,
			       save-option-flags :: <unsigned-fixnum>)
 => status :: <HRESULT>;

  Output-Debug-String("IOleObject/Close\r\n");

  OLE-util-close-server(this.get-obj, save: save-option-flags)
end method IOleObject/Close;


// Breaks down an OLE advise that has been set up on this object.

define method IOleObject/Unadvise(this :: <COleObject>,
				  connection :: <unsigned-fixnum>)
 => status :: <HRESULT>;
	
  Output-Debug-String("IOleObject/Unadvise\r\n");

  // pass on to OleAdviseHolder.
  /* return */ 
  IOleAdviseHolder/Unadvise(this.get-obj.container-IOleAdviseHolder,
			    connection)
end method IOleObject/Unadvise;

// Enumerates the verbs associated with this object.
// This version obtains the information from the registry for this server.
// (Not using $OLE-S-USEREG because that won't work in a .DLL.)
// The data needs to be in the registry instead of here because the default
// verbs are only needed when the server is not active, in which case this 
// method isn't used anyway.
define method IOleObject/EnumVerbs(this :: <COleObject>)
	=> ( status :: <HRESULT>, enumerator :: <LPENUMOLEVERB> );
  Output-Debug-String("IOleObject/EnumVerbs\r\n");
  OleRegEnumVerbs(this.get-obj.obj-class-ID)
end method IOleObject/EnumVerbs;

// Called to get the current client site of the object.
define method IOleObject/GetClientSite(this :: <COleObject>)
	=> ( status :: <HRESULT>, client-site :: <Interface>);

  Output-Debug-String("IOleObject/GetClientSite\r\n");
  let site = this.get-obj.container-IOleClientSite;
  AddRef(site);
  /* return */ values( $S-OK, site )
end method IOleObject/GetClientSite;


// Used to set the object's moniker.
// Note: this function doesn't seem to be used yet (apparently just for
// linked objects), and doesn't look right anyway -- neither argument is used.
define method IOleObject/SetMoniker(this :: <COleObject>,
			 which-moniker :: <unsigned-fixnum>,
			 pmk :: <LPMONIKER>) => status :: <HRESULT>;

  block(return)
    Output-Debug-String("IOleObject/SetMoniker\r\n");

    let obj = this.get-obj;
    let site = obj.container-IOleClientSite;
    if ( null?(site) )
      return($E-FAIL);
    end if;

    let ( status :: <HRESULT>, lpmk :: <Interface> ) =
      IOleClientSite/GetMoniker(site, $OLEGETMONIKER-ONLYIFTHERE,
				$OLEWHICHMK-OBJFULL);
    if ( FAILED?(status) )
      return(status);
    end if;

    unless ( null?(container-IOleAdviseHolder(obj)) )
      IOleAdviseHolder/SendOnRename(container-IOleAdviseHolder(obj), lpmk);
    end unless;

    let ( status :: <HRESULT>, ROT :: <LPRUNNINGOBJECTTABLE> ) =
      GetRunningObjectTable(0);
    if ( status = $NOERROR )
		
      if ( obj.ROT-registration ~= 0 )
	IRunningObjectTable/Revoke(ROT, obj.ROT-registration);
      end if;

      let (  status2 :: <HRESULT>, x :: <unsigned-fixnum> ) =
	IRunningObjectTable/Register(ROT, 0, obj, lpmk);
      obj.ROT-registration := x;
      Release(ROT);
    end if;

    return( $S-OK );
  end block;
end method IOleObject/SetMoniker;


// This doesn't seem to be used yet, and may not be right.
define method IOleObject/GetMoniker(this :: <COleObject>,
				    assignment :: <unsigned-fixnum>,
				    which-moniker :: <unsigned-fixnum>)
		=> ( status :: <HRESULT> , ppmk :: <LPMONIKER> );

  Output-Debug-String("IOleObject/GetMoniker\r\n");

  /* return */
  IOleClientSite/GetMoniker(container-IOleClientSite(this.get-obj),
			    $OLEGETMONIKER-ONLYIFTHERE, $OLEWHICHMK-OBJFULL)
end method IOleObject/GetMoniker;

// Initialize the object from the passed data transfer object.
// Returns $S-FALSE to indicate that this is being ignored.
define method IOleObject/InitFromData(this :: <COleObject>,
				      data-object :: <LPDATAOBJECT>,
				      creation? :: <boolean>,
				      reserved)
		=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/InitFromData\r\n");

  /* return */ $S-FALSE
end method IOleObject/InitFromData;


// Returns an IDataObject that is the same as doing an OleSetClipboard
// Optional, not implemented here.
define method IOleObject/GetClipboardData(this :: <COleObject>, reserved)
 => (status :: <HRESULT>, data-object :: <Interface>)

  Output-Debug-String("IOleObject/GetClipboardData\r\n");
  values( $E-NOTIMPL , $NULL-interface )
end method IOleObject/GetClipboardData;

// Determine if object is up to date.
//      Our embedded object is always up to date.  This function is
//      particularly useful in linking situations.

define method IOleObject/IsUpToDate(this :: <COleObject>)
	=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/IsUpToDate\r\n");
  $S-OK
end method IOleObject/IsUpToDate;


// Return the application's class ID

define method IOleObject/GetUserClassID(this :: <COleObject>,
					pClsid /* :: <LPCLSID> */ )
	=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/GetUserClassID\r\n");
  copy-class-ID(this.get-obj, pClsid)
end method IOleObject/GetUserClassID;


// Get a user presentable ID for this object
// This is currently deferred to the registry.

define method IOleObject/GetUserType(this :: <COleObject>,
				     form-of-type /* :: <unsigned-fixnum> */ )
	=> (status :: <HRESULT>, user-type-name :: <LPOLESTR>);

  OleRegGetUserType(this.get-obj.obj-class-ID, form-of-type)
end method IOleObject/GetUserType;


// Called to set the extent of the object.

define method IOleObject/SetExtent(this :: <COleObject>,
				   draw-aspect :: <unsigned-fixnum>,
				   p-sizel :: <LPSIZEL>)
	=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/SetExtent\r\n");

  if ( draw-aspect ~= $DVASPECT-CONTENT )
    $E-NOTIMPL // don't yet support thumbnail or icon representation
  else
    let obj :: <basic-ole-server> = this.get-obj;
    let ( new-width :: <fixnum>, new-height :: <fixnum> ) =
      himetric-to-pixels(p-sizel.cx-value, p-sizel.cy-value);
    let ( old-width, old-height ) = OLE-util-current-size(obj);
    if ( new-width = old-width & new-height = old-height )
      $S-OK
    elseif ( ~ obj.in-place-active? )
      $OLE-E-NOTRUNNING
    else
      debug-out("calling OLE-part-change-size; old = %d,%d; new = %d,%d\n",
		 old-width, old-height, new-width, new-height);
      if ( OLE-part-change-size(obj, new-width, new-height) )
	obj.embedded-width  := new-width;
	obj.embedded-height := new-height;
	with-stack-structure( rect :: <LPRECT> )
	  GetClientRect(OLE-part-doc-window(obj), rect);
	  rect.right-value := rect.left-value + new-width;
	  rect.bottom-value := rect.top-value + new-height;
	  set-window-sizes(obj, rect, rect, obj.in-place-visible?,
			   obj.using-hatch-window?, set-position?: #f)
	  // above returns $S-OK
	end with-stack-structure
      else
	$E-FAIL  // change refused
      end if
    end if
  end if
end method IOleObject/SetExtent;


// Return enumerator for the outstanding advises associated with this object.

define method IOleObject/EnumAdvise(this :: <COleObject>)
	=> ( status :: <HRESULT>,  enumerator :: <Interface>);
	
  Output-Debug-String("IOleObject/EnumAdvise\r\n");

  // pass on to the OLE Advise holder.
  IOleAdviseHolder/EnumAdvise(this.get-obj.container-IOleAdviseHolder)
end method IOleObject/EnumAdvise;


// Return status information about the object.
// This currently defers to the registry.

define method IOleObject/GetMiscStatus(this :: <COleObject>,
				       aspect :: <unsigned-fixnum>)
	=> ( status :: <HRESULT>, status-bits :: <unsigned-fixnum> );

  Output-Debug-String("IOleObject/GetMiscStatus\r\n");

  OleRegGetMiscStatus(this.get-obj.obj-class-ID, aspect)
end method IOleObject/GetMiscStatus;


// Used to set the palette for the object to use.
// Currently just ignored.  Not called by most containers anyway.
// But should it be returning $E-NOTIMPL instead of $S-OK ?

define method IOleObject/SetColorScheme(this :: <COleObject>,
					palette :: <LPLOGPALETTE>)
	=> status :: <HRESULT>;

  Output-Debug-String("IOleObject/SetColorScheme\r\n");
  $S-OK
end method IOleObject/SetColorScheme;

