Module:    OLE-Server
Synopsis:  Implement the IViewObject[2] interface for drawing 
	   in-process OLE servers (including OLE controls).
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This code originated by following the "Inside OLE" example code in file
// "\inole\code\chap23\polyline\IVIEWOBJ.CPP" dated 05/03/95.


// Note that since <IViewObject2> is a subclass of <IViewObject>, this class
// will respond to QueryInterface for either $IID-IViewObject or
// $IID-IViewObject2.

define COM-interface <CViewObject> ( <IViewObject2> ) 
  constant slot get-obj :: <ole-in-process-server>,
		required-init-keyword: server-framework: ;
  slot default-IViewObject :: <LPVIEWOBJECT2> = $null-interface;

  slot view-advise-sink    :: <LPADVISESINK> = $null-interface; 
//  slot view-frozen-aspects :: <fixnum> = 0;
  slot view-advise-aspects :: <fixnum> = 0;
  slot view-advise-flags   :: <fixnum> = 0;

end <CViewObject>;


define method initialize (this :: <CViewObject>, #rest ignore, #key ) => ()

  Output-Debug-String("initialize(<CViewObject>)\r\n");

  next-method();
  this.default-IViewObject :=
    pointer-cast(<LPVIEWOBJECT2>,
		 query-cache-interface(this.get-obj, $IID-IViewObject2));
end method initialize;


define method terminate (this :: <CViewObject>) => ()
  Output-Debug-String("terminate(<CViewObject>)\r\n");
  unless ( null-pointer?(this.default-IViewObject) )
    AddRef(this.get-obj); // because following Release will decrement it.
    Release(this.default-IViewObject);
    this.default-IViewObject := $null-interface;
  end unless;
  Release(this.view-advise-sink);
  this.view-advise-sink := $null-interface;
  next-method();
end method terminate;


// like in Common Lisp
define inline function logtest (a :: <fixnum>, b :: <fixnum>)
				=> (result :: <boolean>)
  ~ zero?(logand(a,b))
end logtest;

define open generic OLE-part-draw(obj, hDC, pRectBounds);

/*
 * CImpIViewObject::Draw
 *
 * Purpose:
 *  Draws the object on the given hDC specifically for the requested
 *  aspect, device, and within the appropriate bounds.
 *
 * Parameters:
 *  dwAspect        DWORD aspect to draw.
 *  lindex          LONG index of the piece to draw.
 *  pvAspect        LPVOID for extra information, always NULL.
 *  ptd             DVTARGETDEVICE * containing device
 *                  information.
 *  hICDev          HDC containing the IC for the device.
 *  hDC             HDC on which to draw.
 *  pRectBounds     LPCRECTL describing the rectangle in which
 *                  to draw.
 *  pRectWBounds    LPCRECTL describing the placement rectangle
 *                  if part of what you draw is another metafile.
 *  pfnContinue     Function to call periodically during
 *                  long repaints.
 *  dwContinue      DWORD extra information to pass to the
 *                  pfnContinue.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */

// maybe later should be: logior($DVASPECT-CONTENT, $DVASPECT-THUMBNAIL)
define inline constant $supported-aspects = $DVASPECT-CONTENT;

define method IViewObject/Draw (this :: <CViewObject>,
				aspect :: <fixnum>,
				piece-index /* :: <fixnum> */,
				aspect-info /* :: <LPVOID> */,
				device-info /* :: <DVTARGETDEVICE> */,
				hICDev :: <HDC>,
				hDC :: <HDC>,
				pRectBounds :: <LPCRECTL>,
				pRectWBounds /* :: <LPCRECTL> */,
				continue-function /* :: <PROC> */,
				continue-arg /* :: <LPARAM> */)
 => (status :: <HRESULT>)

  debug-out("IViewObject/Draw aspect=%d\n", aspect);
    
  // Delegate iconic, thumbnail, and printed representations.
  if ( ~ logtest(aspect, $supported-aspects) )
    
    /* return */
    IViewObject/Draw(this.default-IViewObject, aspect, piece-index,
		     aspect-info, device-info, hICDev, hDC, pRectBounds,
		     pRectWBounds, continue-function, continue-arg)
  elseif ( null-pointer?(pRectBounds) )
    $E-INVALIDARG
  else
    let saved-DC = SaveDC(hDC);

    // If we supported freezing, and if we are asked to draw a frozen aspect,
    // then at this point we should use the data from a copy we made in
    // IViewObject/Freeze.  Otherwise use the current data.

    block()
      let background-brush =
	// If we're going to a printer, check if it's color capable.
	// if not, then use black on white for this figure.
	if ( (~ null-pointer?(hICDev))
	      & GetDeviceCaps(hICDev, $NUMCOLORS) <= 2 )
	  GetStockObject($WHITE-BRUSH)
	  // and what about setting foreground to black???
	else
	  CreateSolidBrush(GetSysColor($COLOR-WINDOW))
	end if;

      // clear the rectangle:
      // (If the user wants a non-default background color, they will have to
      //  fill the background themselves in their OLE-part-draw method.)
      FillRect(hDC, pRectBounds, background-brush);
      DeleteObject(background-brush);

      // defer to user-supplied code to draw the image:
      OLE-part-draw(this.get-obj, hDC, pRectBounds) | $S-OK

    cleanup
      RestoreDC(hDC, saved-DC);
    exception (<abort>)
      $E-ABORT
    end block
  end if
end method IViewObject/Draw;




/*
 * CImpIViewObject::GetColorSet
 *
 * Purpose:
 *  Retrieves the color palette used by the object.
 *
 * Parameters:
 *  dwAspect        DWORD aspect of interest.
 *  lindex          LONG piece of interest.
 *  pvAspect        LPVOID with extra information, always NULL.
 *  ptd             DVTARGETDEVICE * containing device info.
 *  hICDev          HDC containing the IC for the device.
 *  ppColorSet      LPLOGPALETTE * into which to return the
 *                  pointer to the palette in this color set.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */


define method IViewObject/GetColorSet (this :: <CViewObject>,
				       aspect /* :: <fixnum> */,
				       piece-index /* :: <fixnum> */,
				       aspect-info /* :: <LPVOID> */,
				       device-info /* :: <DVTARGETDEVICE> */,
				       hICDev /* :: <HDC> */)
 => (status :: <HRESULT>, color-set :: <LPLOGPALETTE>)

  Output-Debug-String("IViewObject/GetColorSet\r\n");
  //---- not yet implemented ???
  values($E-NOTIMPL, null-pointer(<LPLOGPALETTE>))
end method IViewObject/GetColorSet;


/*
 * CImpIViewObject::Freeze
 *
 * Purpose:
 *  Freezes the view of a particular aspect such that data
 *  changes do not affect the view.
 *
 * Parameters:
 *  dwAspect        DWORD aspect to freeze.
 *  lindex          LONG piece index under consideration.
 *  pvAspect        LPVOID for further information, always NULL.
 *  pdwFreeze       LPDWORD in which to return the key.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */

define method IViewObject/Freeze (this :: <CViewObject>,
				  aspect /* :: <fixnum> */,
				  piece-index /* :: <fixnum> */,
				  aspect-info /* :: <LPVOID> */)
 => (status :: <HRESULT>, freeze-key :: <fixnum>)

  Output-Debug-String("IViewObject/Freeze\r\n");    

  //---- freezing is not yet implemented.		???
  values( $E-NOTIMPL, 0 )
end method IViewObject/Freeze;


/*
 * CImpIViewObject::Unfreeze
 *
 * Purpose:
 *  Thaws an aspect frozen in ::Freeze.  We expect that a container
 *  will redraw us after freezing if necessary, so we don't send
 *  any sort of notification here.
 *
 * Parameters:
 *  dwFreeze        DWORD key returned from ::Freeze.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */


define method IViewObject/Unfreeze (this :: <CViewObject>,
				    freeze-key /* :: <fixnum> */)
 => (status :: <HRESULT>)

  Output-Debug-String("IViewObject/Unfreeze\r\n");    
  //---- freezing is not yet implemented.
  values( $E-NOTIMPL )
end method IViewObject/Unfreeze;


/*
 * CImpIViewObject::SetAdvise
 *
 * Purpose:
 *  Provides an advise sink to the view object enabling
 *  notifications for a specific aspect.
 *
 * Parameters:
 *  aspects       DWORD describing the aspects of interest.
 *  dwAdvf          DWORD containing advise flags.
 *  pIAdviseSink    LPADVISESINK to notify.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */


define method IViewObject/SetAdvise(this :: <CViewObject>,
				    aspects :: <fixnum>,
				    advise-flags :: <fixnum>,
				    advise-sink :: <LPADVISESINK>)
 => (status :: <HRESULT>)

  Output-Debug-String("IViewObject/SetAdvise\r\n");
  // Pass anything with DVASPECT_ICON or _DOCPRINT to the handler.
  if ( ~ logtest($supported-aspects, aspects) )
    IViewObject/SetAdvise(this.default-IViewObject, aspects,
			  advise-flags, advise-sink);
  end if;
  // We continue because aspects may have more than one in it.

  Release(this.view-advise-sink);
  this.view-advise-sink := advise-sink;
  this.view-advise-aspects := aspects;
  this.view-advise-flags := advise-flags;
  AddRef(advise-sink);
			
  $S-OK
end method IViewObject/SetAdvise;




/*
 * CImpIViewObject::GetAdvise
 *
 * Purpose:
 *  Returns the last known IAdviseSink seen by ::SetAdvise.
 *
 * Parameters:
 *  pdwAspects      LPDWORD in which to store the last
 *                  requested aspects.
 *  pdwAdvf         LPDWORD in which to store the last
 *                  requested flags.
 *  ppIAdvSink      LPADVISESINK * in which to store the
 *                  IAdviseSink.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */

/* // Can't do this until optional output parameters supported. (Bug 641)
define method IViewObject/GetAdvise(this :: <CViewObject>)
 => (status :: <HRESULT>, aspects :: <integer>,
     advise-flags :: <integer>, advise-sink :: <LPADVISESINK>)

  Output-Debug-String("IViewObject/GetAdvise\r\n");
  let advise-sink = this.view-advise-sink;
  AddRef(advise-sink);
  values($S-OK, this.view-advise-aspects, this.view-advise-flags, advise-sink)
end method IViewObject/GetAdvise;
*/

define method IViewObject/GetAdvise(this :: <CViewObject>,
				    pAspects :: <LPDWORD>,
				    pAdvf :: <LPDWORD>,
				    ppAdvSink :: <C-pointer>)
		=> (status :: <HRESULT>);
  Output-Debug-String("IViewObject/GetAdvise\r\n");
  unless ( null-pointer?(pAspects) )
    pointer-value(pAspects) := this.view-advise-aspects;
  end;
  unless ( null-pointer?(pAdvf) )
    pointer-value(pAdvf) := this.view-advise-flags;
  end;
  unless ( null-pointer?(ppAdvSink) )
    let advise-sink = this.view-advise-sink;
    AddRef(advise-sink);
    pointer-value(ppAdvSink) := advise-sink;
  end;
  $S-OK
end;


define method OLE-util-send-view-change(obj :: <ole-in-process-server>) => ();
  next-method(); // notify IDataAdviseHolder
  // Notify container of changed view (ref "Inside OLE" 2nd ed. p. 550)
  let this :: <CViewObject> = obj.server-IViewObject;
  let advise-sink = this.view-advise-sink;
  unless ( null?(advise-sink) )
    let reported-aspects :: <fixnum> =
      logand(this.view-advise-aspects, $supported-aspects);
    unless ( zero?(reported-aspects) )
      IAdviseSink/OnViewChange(advise-sink, reported-aspects, 0);
    end unless;
  end unless;
  values()
end OLE-util-send-view-change;


/*
 * CImpIViewObject::GetExtent
 *
 * Purpose:
 *  Retrieves the extents of the object's display.
 *
 * Parameters:
 *  dwAspect        DWORD of the aspect of interest.
 *  lindex          LONG index of the piece of interest.
 *  ptd             DVTARGETDEVICE * with device information.
 *  pszl            LPSIZEL to the structure in which to return
 *                  the extents.
 *
 * Return Value:
 *  HRESULT         NOERROR or a general error value.
 */


define method IViewObject2/GetExtent(this :: <CViewObject>,
				     aspect :: <fixnum>,
				     piece-index /* :: <fixnum> */,
				     device-info /* :: <DVTARGETDEVICE> */,
				     extent-ptr :: <LPSIZEL>)
 => (status :: <HRESULT>)

  debug-out("IViewObject2/GetExtent aspect=%d\n", aspect);
    
  if ( ~ logtest($DVASPECT-CONTENT, aspect) )
    IViewObject2/GetExtent(this.default-IViewObject,
			   aspect, piece-index, device-info, extent-ptr)
  else
    // return the correct size in HIMETRIC:
    let ( width, height ) = OLE-util-HIMETRIC-size(this.get-obj);
    extent-ptr.cx-value := width;
    extent-ptr.cy-value := height;
    $S-OK
  end if
end method IViewObject2/GetExtent;
