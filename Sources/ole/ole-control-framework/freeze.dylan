Module:    OLE-Control-Framework
Synopsis:  Support for viewing frozen aspects.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Code extracted from original version of iviewobj.dylan converted
// from the Polyline example before freeze support was removed.
// Unfinished and not yet used.

define COM-interface <CViewObject> ( <IViewObject2> ) 
  slot get-obj :: <ole-server-framework>,
		required-init-keyword: server-framework: ;
  slot frozen-aspects :: <fixnum> = 0;
  slot default-iunknown :: <Interface> = $null-interface;
  slot default-IViewObject :: <Interface> = $null-interface;
end <CViewObject>;


define method IViewObject/Draw (this :: <CViewObject>,
				aspect :: <unsigned-fixnum>,
				piece-index :: <fixnum>,
				aspect-info :: <LPVOID>,
				device-info :: <DVTARGETDEVICE>,
				hICDev :: <HDC>,
				hDC :: <HDC>,
				pRectBounds :: <LPCRECTL>,
				pRectWBounds :: <LPCRECTL>,
				continue-function /* :: <PROC> */,
				continue-arg /* :: <LPARAM> */)
 => (status :: <HRESULT>)

  Output-Debug-String("IViewObject/Draw\r\n");
    
  // Delegate iconic and printed representations.
  if ( ~ logtest(logior($DVASPECT-CONTENT, $DVASPECT-THUMBNAIL),
		 aspect) )
    
    /* return */
    IViewObject/Draw(this.default-IViewObject, aspect, piece-index,
		     aspect-info, device-info, hICDev, hDC, pRectBounds,
		     pRectWBounds, continue-function, continue-arg)
  else
    let pl :: <POLYLINEDATA>;
    let obj = this.get-obj;
    let ppl :: <PPOLYLINEDATA> = %addr obj.m-pl;
  
    /*
     * If we're asked to draw a frozen aspect, use the data from
     * a copy we made in IViewObject::Freeze.  Otherwise use the
     * current data.
     */ 
    if ( logtest(aspect, this.frozen-aspects) )
	
      // Point to the data to actually use.
      if ( $DVASPECT-CONTENT = aspect )
	ppl := %addr obj.m-plContent; 
      else
	ppl := %addr obj.m-plThumbnail;
      end if;
    end if;


    // Make a copy so we can modify it (and not worry about threads)
    memcpy(%addr pl, ppl, CBPOLYLINEDATA);

    /*
     * If we're going to a printer, check if it's color capable.
     * if not, then use black on white for this figure.
     */ 
    unless ( null-pointer?(hICDev) )
      if ( GetDeviceCaps(hICDev, $NUMCOLORS) <= 2 )
	pl.rgbBackground := RGB(255, 255, 255);
	pl.rgbLine := RGB(0, 0, 0);
      end if;
    end unless;

    Draw(obj, hDC, #f, #t, pRectBounds, %addr pl);
    /* return */
    $S-OK
  end if
end method IViewObject/Draw;


define constant $FREEZE-KEY-OFFSET = #x3000; // arbitrary value

define method IViewObject/Freeze (this :: <CViewObject>,
				  aspect :: <unsigned-fixnum>,
				  piece-index :: <fixnum>,
				  aspect-info :: <LPVOID>)
 => (status :: <HRESULT> :: freeze-key :: <fixnum>)

  Output-Debug-String("IViewObject/Freeze\r\n");    

  // Delegate anything for ICON or DOCPRINT aspects
  if ( ~ logtest(logior($DVASPECT-CONTENT, $DVASPECT-THUMBNAIL), aspect) )
    IViewObject/Freeze(this.default-IViewObject, aspect, piece-index, aspect-info)
  elseif ( logtest(aspect, this.frozen-aspects) )
    values($VIEW-S-ALREADY-FROZEN, aspect + $FREEZE-KEY-OFFSET)
  else

    this.frozen-aspects := logior(this.frozen-aspects, aspect);

    /*
     * For whatever aspects become frozen, make a copy of the data.
     * Later when drawing, if such a frozen aspect is requested,
     * we'll draw from this data rather than from our current data.
     */ 
    let obj = this.get-obj;
    if ( logtest($DVASPECT-CONTENT, aspect) )
      memcpy(%addr obj.m-plContent, %addr obj.m-pl, CBPOLYLINEDATA);
    end if;

    if ( logtest($DVASPECT-THUMBNAIL, aspect) )
      memcpy(%addr obj.m-plThumbnail, %addr obj.m-pl, CBPOLYLINEDATA);
    end if;

    values($S-OK, aspect + $FREEZE-KEY-OFFSET)
  end if

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
				    freeze-key :: <fixnum>)
 => (status :: <HRESULT>)

  Output-Debug-String("IViewObject/Unfreeze\r\n");    
  let aspect :: <unsigned-fixnum> = freeze-key - $FREEZE-KEY-OFFSET;

  // Delegate anything for ICON or DOCPRINT aspects
  if ( ~ logtest(logior($DVASPECT-CONTENT, $DVASPECT-THUMBNAIL), aspect) )
    // [Uh, this can't work because we don't know what convention is
    //  used by the key returned by the default Freeze.  -- DNG 10/24/96]
    IViewObject/Unfreeze(this.default-IViewObject, freeze-key);
  end if;

  // The aspect to unfreeze is in the key.
  this.frozen-aspects := logand(this.frozen-aspects, lognot(aspect));

    /*
     * Since we always kept our current data up to date, we don't
     * have to do anything thing here like requesting data again.
     * Because we removed aspect from frozen-aspects, Draw
     * will again use the current data.
     */ 

  $S-OK
end method IViewObject/Unfreeze;

