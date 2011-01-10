Module:    OLE-Control-Framework
Synopsis:  Implementation of the "IOleControl" interface, 
	   including keyboard mnemonics and ambient properties.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define COM-interface <COleControl> ( <IOleControl> )

  constant slot get-obj :: <ole-control-framework>,
		required-init-keyword: server-framework: ;
  slot ole-control-mnemonics-haccel :: false-or(<HACCEL>) = #f;
  slot ole-control-mnemonics-hglobal :: false-or(<HANDLE>) = #f;
  slot ole-control-mnemonics-flags :: <fixnum> = 0;
  slot ole-control-mnemonics-count :: <fixnum> = 0;
end <COleControl>;

//  -------------------------------
//	mnemonics
//  -------------------------------

define method terminate ( this :: <COleControl> ) => ();
  next-method();
  free-haccel(this.ole-control-mnemonics-hglobal);
  this.ole-control-mnemonics-hglobal := #f;
end method terminate;


// The container calls this to ask the control what keystroke events
// it wants to handle.  This concerns "mnemonic" keys that the container
// will pass to the control even when it is not already active, unlike the
// "accelerator" keys for an active embedded document.
define method IOleControl/GetControlInfo ( this :: <COleControl>,
					   pCI :: <LPCONTROLINFO>)
 => (status :: <HRESULT>)
  Output-Debug-String("IOleControl/GetControlInfo\r\n");
  if ( null-pointer?(pCI) )
    $E-POINTER
  else
    let obj = this.get-obj;
    if ( null-pointer?(obj.container-IOleControlSite) )
      let client-site = obj.container-IOleClientSite;
      unless ( null?(client-site) )
	let ( status, control-site ) =
	  QueryInterface(client-site, $IID-IOleControlSite);
	obj.container-IOleControlSite := control-site;
      end unless;
    end if;
    let status :: <HRESULT> = $E-NOTIMPL;
    let flags :: <fixnum> = 0;
    let num-accel :: <fixnum> = 0;
    let haccel :: <HACCEL> = this.ole-control-mnemonics-hglobal |
      this.ole-control-mnemonics-haccel |
      OLE-part-mnemonics(obj) | null-handle(<HACCEL>);
    unless ( null-handle?(haccel) )
      // fetch the number of entries in the table
      num-accel := CopyAcceleratorTable(haccel, null-pointer(<LPACCEL>), #xFF);
      unless ( zero?(num-accel) )
        if ( haccel == this.ole-control-mnemonics-hglobal |
	      haccel == this.ole-control-mnemonics-haccel )
	  flags := this.ole-control-mnemonics-flags;
	else
	  // We have a problem here in that the "Patron" example from 
	  // "Inside OLE" has incompatible expectations from MFC.
	  // (The documentation, of course, is too vague to be able to say
	  // for sure that either one is wrong.)
	  // Use the fact that MFC initializes the size, while Patron
	  // does not, to try to distinguish the two cases.
	  let global-handle :: false-or(<HANDLE>) = #f;
	  if ( this.ole-control-mnemonics-hglobal | zero?(pCI.cb-value) )
	    // Patron wants a global handle.
	    global-handle := GlobalAlloc($GHND, size-of(<ACCEL>) * num-accel);
	    check-win32-result("GlobalAlloc", global-handle);
	  end if;
	  let copied-table :: <LPACCEL> =
	    if ( global-handle )
	      pointer-cast(<LPACCEL>, 
			   check-win32-result("GlobalLock",
					      GlobalLock(global-handle)))
	    else
	      make(<LPACCEL>, element-count: num-accel)
	    end if;
	  CopyAcceleratorTable(haccel, copied-table, num-accel);
	  for ( i :: <fixnum> from 0 below num-accel )
	    let accel = pointer-value-address(copied-table, index: i);
	    if ( zero?(logand(accel.fVirt-value,
			      logior($FSHIFT, $FCONTROL, $FALT))) )
	      let key = accel.key-value;
	      if ( key = $VK-RETURN )
		// Control wants to handle the Return key.
		flags := logior(flags, $CTRLINFO-EATS-RETURN);
	      elseif ( key = $VK-ESCAPE )
		// Control wants to handle the Escape key.
		flags := logior(flags, $CTRLINFO-EATS-ESCAPE);
	      end if;
	    end if;
	  end for;
	  this.ole-control-mnemonics-haccel := haccel;
	  if ( global-handle )
	    GlobalUnlock(global-handle);
	    haccel := pointer-cast(<HACCEL>, global-handle);
	    this.ole-control-mnemonics-hglobal := haccel;
	  else
	    destroy(copied-table);
	  end if;
	  this.ole-control-mnemonics-flags := flags;
	  this.ole-control-mnemonics-count := num-accel;
	end if;
      end unless;
      status := $S-OK;
    end unless;
    if(this.ole-control-mnemonics-haccel)
      pCI.cb-value := size-of(<CONTROLINFO>);
    end if;
    pCI.hAccel-value  := haccel;    // handle of array of mnemonics
    pCI.cAccel-value  := num-accel; // number of mnemonics
    pCI.dwFlags-value := flags;
    status
  end if
end method;

// ``If a control changes the contents of its CONTROLINFO structure, 
//   it must notify its container by calling
//   IOleControlSite::OnControlInfoChanged.'' (MSDN SDK)
define method OLE-util-key-change ( obj :: <ole-control-framework> ) => ();
  Output-Debug-String("OLE-util-key-change\r\n");
  let this :: <COleControl> = obj.server-IOleControl;
  let old-mnemonics = this.ole-control-mnemonics-hglobal;
  this.ole-control-mnemonics-hglobal := #f;
  this.ole-control-mnemonics-haccel := #f;
  let control-site = obj.container-IOleControlSite;
  unless( null?(control-site) )
    Output-Debug-String(" calling OnControlInfoChanged\r\n");
    IOleControlSite/OnControlInfoChanged(control-site);
  end unless;
  free-haccel(old-mnemonics);
  values()
end;

define function free-haccel ( haccel :: false-or(<HACCEL>) ) => ();
  if ( haccel & ~ null-handle?(haccel) )
    let return = GlobalFree(haccel);
    unless ( null-handle?(return) )
      report-win32-error("GlobalFree");
    end unless;
  end if;
end free-haccel;

define open generic OLE-part-mnemonics ( obj ) => table :: false-or(<HACCEL>);

// default method:
define method OLE-part-mnemonics ( obj :: <IUnknown> )
 => table :: false-or(<HACCEL>);
  #f
end;


// The container calls this to forward a keystroke event to the control
// that handles it.
define method IOleControl/OnMnemonic ( this :: <COleControl>,
				       pMsg :: <LPMSG>)
 => (status :: <HRESULT>)
  Output-Debug-String("IOleControl/OnMnemonic\r\n");
  if ( zero?(this.ole-control-mnemonics-count) )
    // We don't support any mnemonics, so this shouldn't have been called.
    $E-NOTIMPL
  else
    // Nowhere is it documented what kind of message should be expected here,
    // but the available examples of containers send either WM_CHAR,
    // WM_KEYDOWN, or WM_SYSKEYDOWN.
    let message = pMsg.message-value;
    if ( message >= $WM-KEYFIRST & message <= $WM-KEYLAST )
      let key = LOWORD(pMsg.wParam-value);
      debug-out(" calling OLE-part-on-mnemonic for key #x%x\n", key);
      block()
	OLE-part-on-mnemonic(this.get-obj, key, pMsg);
	$S-OK
      exception (<abort>)
	$E-ABORT
      end block
    else // unexpected input, report failure.
      debug-out(" unexpected message #x%x\n", message);
      $E-INVALIDARG
    end if
  end if
end method;

define open generic OLE-part-on-mnemonic ( obj, vkey, pMsg ) => ();

// default method; user can override.
define method OLE-part-on-mnemonic ( obj :: <ole-control-framework>,
				     vkey, pMsg :: <LPMSG> ) => ();
  ignore(vkey);
  let window = OLE-part-command-window(obj);
  let this :: <COleControl> = obj.server-IOleControl;
  let result =
    TranslateAccelerator(window, this.ole-control-mnemonics-haccel, pMsg);
  if ( zero?(result) )
    debug-out(" TranslateAccelerator error %d\n", GetLastError());
  else
    Output-Debug-String(" TranslateAccelerator succeeded\r\n");
  end if;
  values()
end method;

// Should call OleControlSite::OnFocus when focus is received or lost, but
// appears to matter only if the control has mnemonic for Return or Escape or
// needs to modify its appearance when it is the default button.

define method OLE-util-on-focus ( obj :: <ole-control-framework>,
				  got-focus? :: <boolean> ) => ();
  Output-Debug-String("OLE-util-on-focus");
  Output-Debug-Boolean(got-focus?);
  if ( ~ got-focus? )
    OLE-util-flush-view-change(obj);
  end if;
  let control-site = obj.container-IOleControlSite;
  unless( null?(control-site) )
    IOleControlSite/OnFocus(control-site, got-focus?)
  end unless;
  values()
end;

//  -------------------------------
//	event control
//  -------------------------------

// The container uses this to tell the control when it will be not be
// available to process events.
define method IOleControl/FreezeEvents ( this :: <COleControl>,
					 freeze? :: <boolean>)
 => (status :: <HRESULT>)
  debug-out("IOleControl/FreezeEvents %=\n", freeze?);
  this.get-obj.freeze-events? := freeze?;
  // This operation should always report success even when ignored.
  $S-OK
end method;


//  -------------------------------
//	ambient properties
//  -------------------------------

// This code was originally patterned after Brockschmidt's example in:
//  \INOLE\CODE\CHAP24\POLYLINE\POLYLINE.CPP 


// The container uses this to notify the control of a change in the
// ambient properties.
define method IOleControl/OnAmbientPropertyChange ( this :: <COleControl>,
						    disp-ID :: <fixnum> )
 => (status :: <HRESULT>)
  debug-out("IOleControl/OnAmbientPropertyChange %=\n", disp-ID);
  
  let obj = this.get-obj;
  if ( disp-ID = $DISPID-UNKNOWN )
    update-all-ambient-properties(obj);
  elseif ( member?(disp-ID, interesting-properties) |
	   member?(disp-ID, OLE-part-ambient-properties(obj)) )
    update-ambient-property(obj, disp-ID);
  end if;
  // This operation should always report success even when ignored.
  $S-OK 
end method;

define open COM-interface <OCX-OleObject> ( <COleObject> )
end;

define method after-initialize ( obj :: <ole-control-framework> ) => ();

  obj.server-IOleObject := make(<OCX-OleObject>, server-framework: obj,
				controlling-unknown: obj);
  next-method();
end method;

define method IOleObject/SetClientSite(this :: <OCX-OleObject>,
				       client-site :: <LPOLECLIENTSITE>)
	=> status :: <HRESULT>;

  let result = next-method();
  unless ( null-pointer?(client-site) )
    let ( status, interface ) = QueryInterface(client-site, $IID-IDispatch);
    if ( SUCCEEDED?(status) )
      let obj :: <ole-control-framework> = this.controlling-unknown;
      obj.container-IDispatch :=
	pointer-cast(<LPDISPATCH>, interface);
      Output-Debug-String(" initializing ambient properties\r\n");
      update-all-ambient-properties(obj);
    end if;
  end unless;
  result
end method IOleObject/SetClientSite;

// A dispatch property will never return a Dylan function, so we can
// use one as the default value instead of needing to cons a unique object.
define constant $unknown-value = null?;

define method update-ambient-property( obj :: <ole-control-framework>,
				       disp-id :: <integer>) => ();
  let idisp = obj.container-IDispatch;
  unless ( null?(idisp) )
    block ()
      let value = get-property(idisp, disp-id, default: $unknown-value);
      unless ( value == $unknown-value )
	debug-out(" set ambient property, ID = %=, value = %=\n",
		  disp-id, value);
	OLE-part-set-ambient-property(obj, disp-id, value);
      end unless;
    exception (<abort>)
    end block;
  end unless;
end method;

// ambient properties directly supported in this library:
define constant interesting-properties :: <vector> =
  vector($DISPID-AMBIENT-SHOWHATCHING, $DISPID-AMBIENT-UIDEAD,
	 $DISPID-AMBIENT-LOCALEID);

define method update-all-ambient-properties( obj :: <ole-control-framework> )
 => ();
  let properties = union(interesting-properties,
			 OLE-part-ambient-properties(obj));
  for ( property in properties )
    update-ambient-property(obj, property);
  end for;
end method;


define open generic OLE-part-ambient-properties ( obj )
 => ( properties :: <sequence> );

// default method:
define method OLE-part-ambient-properties ( obj :: <ole-control-framework> )
 => ( properties :: <sequence> );
  #[]
end method;

define open generic OLE-part-set-ambient-property (obj, disp-id, value);

define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>,
      disp-id == $DISPID-AMBIENT-SHOWHATCHING,
      show-hatch? :: <boolean> ) => ();

  // if obj.OLE-util-UI-active?, ought to update the display to add or
  // remove the hatch window, but that would be more trouble than it
  // seems worth right now.   -- DNG 9/24/97			???

  obj.hatch-when-UI-active? := show-hatch?;
end method;

define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>,
      disp-id == $DISPID-AMBIENT-UIDEAD,
      dead? :: <boolean> ) => ();
    
  // This should affect the detection of mouse clicks.
  obj.freeze-UI? := dead?;
end method;

define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>,
      disp-id == $DISPID-AMBIENT-LOCALEID,
      lcid ) => ();
  obj.OLE-util-locale := lcid;
end method;

/*
define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>,
      disp-id == $DISPID-AMBIENT-BACKCOLOR,
      value :: <object> ) => ();

  let cr = OLE-util-translate-color(obj, value);
  unless ( cr = obj.m-pl.rgbBackground )
    obj.m-pl.rgbBackground := cr;
    InvalidateRect(obj.m-hWnd, $NULL-RECT, #t);
    UpdateWindow(obj.m-hWnd);
  end unless;
end method;

define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>,
      disp-id == $DISPID-AMBIENT-FORECOLOR,
      value :: <object> ) => ();

  let cr = OLE-util-translate-color(obj, value);
  unless ( cr = obj.m-pl.rgbLine )
    obj.m-pl.rgbLine := cr;
    InvalidateRect(obj.m-hWnd, $NULL-RECT, #t);
    UpdateWindow(obj.m-hWnd);
  end unless;
end method;
*/

// default method does nothing
define method OLE-part-set-ambient-property
    ( obj :: <ole-control-framework>, disp-id :: <integer>, value :: <object> )
 => ();
  values()
end method;

// Convert ambient property value to a color:
define method OLE-util-translate-color(obj :: <interface>, value)
 => (RGB-color :: <object>);

  // Note: theoretically, we should be using `OleTranslateColor' here,
  //   but it wants a palette handle which we don't have here, and it
  //   appears from the documentation that the only thing it provides 
  //   that we aren't doing here is validating the index of a
  //   palette-relative color.
  ignore(obj);
  if ( negative?(value) )
    GetSysColor(LOWORD(value))
  else
    value
  end if
end OLE-util-translate-color;

