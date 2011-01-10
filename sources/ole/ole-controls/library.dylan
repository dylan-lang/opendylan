module:    dylan-user	
Synopsis:  Windows OLE Controls API (OLECTL.H and OLEPRO32.DLL)
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */



define library OLE-Controls
  use functional-dylan;
  use C-FFI;
  use Win32-Common;
  use Win32-User;
  use Win32-GDI;
  use COM;
  use OLE;
  use OLE-Automation;
  export OLE-Controls;
end;

define module OLE-Controls
  use functional-dylan;
  use C-FFI;
  use machine-words;
  use Win32-common, // need:  <BOOL>, <COLORREF>, <DWORD>, $FFFFFFFF,
		    //	      <HACCEL>, <HBITMAP>, <HCURSOR>, <HINSTANCE>,
		    //	      <HMETAFILE>, <HPALETTE>, <HWND>, <LONG>,
		    //	      <LPVOID>, <SHORT>, <SIZE>, <UINT>, <ULONG>,
		    //	      <USHORT>
    // slot accessors used in both libraries:
    export: { x-value, x-value-setter, y-value, y-value-setter,
	      cbSize-value, cbSize-value-setter,
	      dwFlags-value, dwFlags-value-setter };

  use Win32-GDI,
    // import: { <TEXTMETRICW> },
    export: { xExt-value, xExt-value-setter, yExt-value, yExt-value-setter,
	      dwType-value, dwType-value-setter };

  use Win32-User,
    import: { <LPMSG>, hicon-value, hicon-value-setter },
    export: { hicon-value, hicon-value-setter,
	      pszTitle-value, pszTitle-value-setter };

  use COM, // need: $FACILITY-CONTROL, $FACILITY-ITF, <C-COM-vtbl>,
	   //	<C-HRESULT>, <C-interface>, <CLSID>, <GUID>, <IClassFactory>,
	   //	<IID>, <IPersist>, <IUnknown>, <Interface>, <LPCLSID>,
	   //	<LPCOLESTR>, <LPOLESTR>, <LPSTREAM>, <LPUNKNOWN>,
	   //	<REFIID>, <mapped-interface>, MAKE-SCODE, add-interface
    // slot accessors used in both libraries:
    export: { u-value, u-value-setter, hBitmap-value, hBitmap-value-setter,
	     pUnk-value, pUnk-value-setter,
	     clsid-value, clsid-value-setter };

  use COM-internal;
  use OLE,
    // need: $CLASS-E-NOAGGREGATION, <IViewObject2>, <IOleInPlaceObject>,
    //		<IOleInPlaceSite>, cb-value, cb-value-setter,
    //		hAccel-value, hAccel-value-setter
    export: {cb-value, cb-value-setter,
	     hAccel-value, hAccel-value-setter
		, $OLEMISC-RECOMPOSEONRESIZE, $OLEMISC-ONLYICONIC,
	     $OLEMISC-INSERTNOTREPLACE, $OLEMISC-STATIC,
	     $OLEMISC-CANTLINKINSIDE, $OLEMISC-CANLINKBYOLE1,
	     $OLEMISC-ISLINKOBJECT, $OLEMISC-INSIDEOUT,
	     $OLEMISC-ACTIVATEWHENVISIBLE,
	     $OLEMISC-RENDERINGISDEVICEINDEPENDENT,
	     $OLEMISC-INVISIBLEATRUNTIME, $OLEMISC-ALWAYSRUN,
	     $OLEMISC-ACTSLIKEBUTTON, $OLEMISC-ACTSLIKELABEL,
	     $OLEMISC-NOUIACTIVATE, $OLEMISC-ALIGNABLE, $OLEMISC-SIMPLEFRAME,
	     $OLEMISC-SETCLIENTSITEFIRST, $OLEMISC-IMEMODE,
	     $OLEMISC-IGNOREACTIVATEWHENVISIBLE, $OLEMISC-WANTSTOMENUMERGE,
	     $OLEMISC-SUPPORTSMULTILEVELUNDO
	     };

  use OLE-Automation,
    import: { $VT-BOOL, $VT-DISPATCH, $VT-I2, $VT-I4,
	      <BSTR>, <LPBSTR>, <DISPID>,
	      <DISPPARAMS>, <LPDISPPARAMS>, <EXCEPINFO>, <LPEXCEPINFO>, 
	      <IDispatch>, <LPDISPATCH>, <LPLPDISPATCH>,
	      <LPTYPEINFO>, <LPVARIANT>, <VARIANT>, <VARIANT-BOOL>,
	      <VARTYPE>, <CY>,
	      dwHelpContext-value, dwHelpContext-value-setter,
	      lcid-value, lcid-value-setter,
	      vt-value, vt-value-setter },
    export: { dwHelpContext-value, dwHelpContext-value-setter,
	      lcid-value, lcid-value-setter,
	      vt-value, vt-value-setter, <CY> };


  // from "olectl.h":
  export cbSizeofstruct-value, cbSizeofstruct-value-setter,
	lpstrName-value, lpstrName-value-setter, cySize-value,
	cySize-value-setter, sWeight-value, sWeight-value-setter,
	sCharset-value, sCharset-value-setter, fItalic-value,
	fItalic-value-setter, fUnderline-value, fUnderline-value-setter,
	fStrikethrough-value, fStrikethrough-value-setter, <FONTDESC>,
	<LPFONTDESC>;
  export $PICTYPE-UNINITIALIZED, $PICTYPE-NONE, $PICTYPE-BITMAP,
	$PICTYPE-METAFILE, $PICTYPE-ICON, $PICTYPE-ENHMETAFILE;
  export cbSizeofstruct-value, cbSizeofstruct-value-setter,
	picType-value, picType-value-setter, hpal-value, hpal-value-setter,
	hpal-value, hpal-value-setter, bmp-value, hmeta-value,
	hmeta-value-setter, hmeta-value, hmeta-value-setter, wmf-value,
	icon-value, hemf-value, hemf-value-setter, hemf-value,
	hemf-value-setter, emf-value, <PICTDESC>, <LPPICTDESC>;
  export <OLE-XPOS-PIXELS>, <OLE-YPOS-PIXELS>, <OLE-XSIZE-PIXELS>,
	<OLE-YSIZE-PIXELS>, <OLE-XPOS-CONTAINER>, <OLE-YPOS-CONTAINER>,
	<OLE-XSIZE-CONTAINER>, <OLE-YSIZE-CONTAINER>, $triUnchecked,
	$triChecked, $triGray, <OLE-OPTEXCLUSIVE>, <OLE-CANCELBOOL>,
	<OLE-ENABLEDEFAULTBOOL>;
  export $CTL-E-ILLEGALFUNCTIONCALL, $CTL-E-OVERFLOW,
	$CTL-E-OUTOFMEMORY, $CTL-E-DIVISIONBYZERO, $CTL-E-OUTOFSTRINGSPACE,
	$CTL-E-OUTOFSTACKSPACE, $CTL-E-BADFILENAMEORNUMBER,
	$CTL-E-FILENOTFOUND, $CTL-E-BADFILEMODE, $CTL-E-FILEALREADYOPEN,
	$CTL-E-DEVICEIOERROR, $CTL-E-FILEALREADYEXISTS,
	$CTL-E-BADRECORDLENGTH, $CTL-E-DISKFULL, $CTL-E-BADRECORDNUMBER,
	$CTL-E-BADFILENAME, $CTL-E-TOOMANYFILES, $CTL-E-DEVICEUNAVAILABLE,
	$CTL-E-PERMISSIONDENIED, $CTL-E-DISKNOTREADY,
	$CTL-E-PATHFILEACCESSERROR, $CTL-E-PATHNOTFOUND,
	$CTL-E-INVALIDPATTERNSTRING, $CTL-E-INVALIDUSEOFNULL,
	$CTL-E-INVALIDFILEFORMAT, $CTL-E-INVALIDPROPERTYVALUE,
	$CTL-E-INVALIDPROPERTYARRAYINDEX, $CTL-E-SETNOTSUPPORTEDATRUNTIME,
	$CTL-E-SETNOTSUPPORTED, $CTL-E-NEEDPROPERTYARRAYINDEX,
	$CTL-E-SETNOTPERMITTED, $CTL-E-GETNOTSUPPORTEDATRUNTIME,
	$CTL-E-GETNOTSUPPORTED, $CTL-E-PROPERTYNOTFOUND,
	$CTL-E-INVALIDCLIPBOARDFORMAT, $CTL-E-INVALIDPICTURE,
	$CTL-E-PRINTERERROR, $CTL-E-CANTSAVEFILETOTEMP,
	$CTL-E-SEARCHTEXTNOTFOUND, $CTL-E-REPLACEMENTSTOOLONG,
	$CTL-E-CUSTOM-FIRST;
  export $CONNECT-E-FIRST, $CONNECT-E-LAST, $CONNECT-S-FIRST,
	$CONNECT-S-LAST, $CONNECT-E-NOCONNECTION, $CONNECT-E-ADVISELIMIT,
	$CONNECT-E-CANNOTCONNECT, $CONNECT-E-OVERRIDDEN;
  export $SELFREG-E-FIRST, $SELFREG-E-LAST, $SELFREG-S-FIRST,
	$SELFREG-S-LAST, $SELFREG-E-TYPELIB, $SELFREG-E-CLASS;
  export $PERPROP-E-FIRST, $PERPROP-E-LAST, $PERPROP-S-FIRST,
	$PERPROP-S-LAST, $PERPROP-E-NOPAGEAVAILABLE;
  export $OLEIVERB-PROPERTIES;
  export $VT-STREAMED-PROPSET, $VT-STORED-PROPSET, $VT-BLOB-PROPSET,
	$VT-VERBOSE-ENUM;
  export $VT-COLOR, $VT-XPOS-PIXELS, $VT-YPOS-PIXELS,
	$VT-XSIZE-PIXELS, $VT-YSIZE-PIXELS, $VT-XPOS-HIMETRIC,
	$VT-YPOS-HIMETRIC, $VT-XSIZE-HIMETRIC, $VT-YSIZE-HIMETRIC,
	$VT-TRISTATE, $VT-OPTEXCLUSIVE, $VT-FONT, $VT-PICTURE, $VT-HANDLE;
  export OleLoadPicturePath, OleLoadPictureFile, OleSavePictureFile,
	$LP-DEFAULT, $LP-MONOCHROME, $LP-VGACOLOR, $LP-COLOR;
  export $DISPID-AUTOSIZE, $DISPID-BACKCOLOR, $DISPID-BACKSTYLE,
	$DISPID-BORDERCOLOR, $DISPID-BORDERSTYLE, $DISPID-BORDERWIDTH,
	$DISPID-DRAWMODE, $DISPID-DRAWSTYLE, $DISPID-DRAWWIDTH,
	$DISPID-FILLCOLOR, $DISPID-FILLSTYLE, $DISPID-FONT,
	$DISPID-FORECOLOR, $DISPID-ENABLED, $DISPID-HWND, $DISPID-TABSTOP,
	$DISPID-TEXT, $DISPID-CAPTION, $DISPID-BORDERVISIBLE,
	$DISPID-APPEARANCE, $DISPID-MOUSEPOINTER, $DISPID-MOUSEICON,
	$DISPID-PICTURE, $DISPID-VALID, $DISPID-READYSTATE,
	$DISPID-LISTINDEX, $DISPID-SELECTED, $DISPID-LIST, $DISPID-COLUMN,
	$DISPID-LISTCOUNT, $DISPID-MULTISELECT, $DISPID-MAXLENGTH,
	$DISPID-PASSWORDCHAR, $DISPID-SCROLLBARS, $DISPID-WORDWRAP,
	$DISPID-MULTILINE, $DISPID-NUMBEROFROWS, $DISPID-NUMBEROFCOLUMNS,
	$DISPID-DISPLAYSTYLE, $DISPID-GROUPNAME, $DISPID-IMEMODE,
	$DISPID-ACCELERATOR, $DISPID-ENTERKEYBEHAVIOR,
	$DISPID-TABKEYBEHAVIOR, $DISPID-SELTEXT, $DISPID-SELSTART,
	$DISPID-SELLENGTH, $DISPID-REFRESH, $DISPID-DOCLICK,
	$DISPID-ABOUTBOX, $DISPID-ADDITEM, $DISPID-CLEAR, $DISPID-REMOVEITEM,
	$DISPID-CLICK, $DISPID-DBLCLICK, $DISPID-KEYDOWN, $DISPID-KEYPRESS,
	$DISPID-KEYUP, $DISPID-MOUSEDOWN, $DISPID-MOUSEMOVE, $DISPID-MOUSEUP,
	$DISPID-ERROREVENT, $DISPID-READYSTATECHANGE, $DISPID-CLICK-VALUE,
	$DISPID-RIGHTTOLEFT, $DISPID-TOPTOBOTTOM, $DISPID-THIS,
	$DISPID-AMBIENT-BACKCOLOR, $DISPID-AMBIENT-DISPLAYNAME,
	$DISPID-AMBIENT-FONT, $DISPID-AMBIENT-FORECOLOR,
	$DISPID-AMBIENT-LOCALEID, $DISPID-AMBIENT-MESSAGEREFLECT,
	$DISPID-AMBIENT-SCALEUNITS, $DISPID-AMBIENT-TEXTALIGN,
	$DISPID-AMBIENT-USERMODE, $DISPID-AMBIENT-UIDEAD,
	$DISPID-AMBIENT-SHOWGRABHANDLES, $DISPID-AMBIENT-SHOWHATCHING,
	$DISPID-AMBIENT-DISPLAYASDEFAULT, $DISPID-AMBIENT-SUPPORTSMNEMONICS,
	$DISPID-AMBIENT-AUTOCLIP, $DISPID-AMBIENT-APPEARANCE,
	$DISPID-AMBIENT-CODEPAGE, $DISPID-AMBIENT-PALETTE,
	$DISPID-AMBIENT-CHARSET, $DISPID-AMBIENT-TRANSFERPRIORITY,
	$DISPID-AMBIENT-RIGHTTOLEFT, $DISPID-AMBIENT-TOPTOBOTTOM,
	$DISPID-Name, $DISPID-Delete, $DISPID-Object, $DISPID-Parent;
  export $DISPID-FONT-NAME, $DISPID-FONT-SIZE, $DISPID-FONT-BOLD,
	$DISPID-FONT-ITALIC, $DISPID-FONT-UNDER, $DISPID-FONT-STRIKE,
	$DISPID-FONT-WEIGHT, $DISPID-FONT-CHARSET, $DISPID-FONT-CHANGED,
	$DISPID-PICT-HANDLE, $DISPID-PICT-HPAL, $DISPID-PICT-TYPE,
	$DISPID-PICT-WIDTH, $DISPID-PICT-HEIGHT, $DISPID-PICT-RENDER;


  // from "ocidl.h":
  export $UAS-NORMAL, $UAS-BLOCKED, $UAS-NOPARENTENABLE, $UAS-MASK,
	$READYSTATE-UNINITIALIZED, $READYSTATE-LOADING, $READYSTATE-LOADED,
	$READYSTATE-INTERACTIVE, $READYSTATE-COMPLETE;
  export <LPENUMCONNECTIONS>;
  export dwCookie-value, dwCookie-value-setter, <CONNECTDATA>,
	<LPCONNECTDATA>;
  export <LPCONNECTIONPOINT>;
  export <LPENUMCONNECTIONPOINTS>;
  export <LPCONNECTIONPOINTCONTAINER>;
  export cbLicInfo-value, cbLicInfo-value-setter,
	fRuntimeKeyAvail-value, fRuntimeKeyAvail-value-setter,
	fLicVerified-value, fLicVerified-value-setter, <LICINFO>,
	<LPLICINFO>;
  export <LPPROVIDECLASSINFO>;
  export $GUIDKIND-DEFAULT-SOURCE-DISP-IID;
  export <LPOLECONTROL>;
  export cAccel-value, cAccel-value-setter, <CONTROLINFO>,
	<LPCONTROLINFO>;
  export $CTRLINFO-EATS-RETURN, $CTRLINFO-EATS-ESCAPE;
  export <LPOLECONTROLSITE>;
  export <POINTF>, <LPPOINTF>;
  export $XFORMCOORDS-POSITION, $XFORMCOORDS-SIZE,
	$XFORMCOORDS-HIMETRICTOCONTAINER, $XFORMCOORDS-CONTAINERTOHIMETRIC,
	$XFORMCOORDS-EVENTCOMPAT;
  export <LPPROPERTYPAGE>;
  export size-value, size-value-setter, pszDocString-value,
	pszDocString-value-setter, pszHelpFile-value,
	pszHelpFile-value-setter, <PROPPAGEINFO>, <LPPROPPAGEINFO>;
  export <LPPROPERTYPAGESITE>;
  export $PROPPAGESTATUS-DIRTY, $PROPPAGESTATUS-VALIDATE,
	$PROPPAGESTATUS-CLEAN;
  export <LPSPECIFYPROPERTYPAGES>;
  export cElems-value, cElems-value-setter, pElems-value,
	pElems-value-setter, <CAUUID>, <LPCAUUID>;
  export <LPPERSISTMEMORY>;
  export <LPPERSISTSTREAMINIT>;
  export <LPPERSISTPROPERTYBAG>;
  export <LPSIMPLEFRAMESITE>;
  export <LPFONT>, <TEXTMETRICOLE>, <LPTEXTMETRICOLE>;
  export <LPPICTURE>;
  export $PICTURE-SCALABLE, $PICTURE-TRANSPARENT;
  export <OLE-HANDLE>;
  export <OLE-XPOS-HIMETRIC>;
  export <OLE-YPOS-HIMETRIC>;
  export <OLE-XSIZE-HIMETRIC>;
  export <OLE-YSIZE-HIMETRIC>;
  export <LPFONTDISP>;
  export <LPPICTUREDISP>;
  export $ACTIVATE-WINDOWLESS;
  export $OLEDC-NODRAW, $OLEDC-PAINTBKGND, $OLEDC-OFFSCREEN;
  export $VIEWSTATUS-OPAQUE, $VIEWSTATUS-SOLIDBKGND,
	$VIEWSTATUS-DVASPECTOPAQUE, $VIEWSTATUS-DVASPECTTRANSPARENT,
	$VIEWSTATUS-SURFACE, $VIEWSTATUS-3DSURFACE;
  export $HITRESULT-OUTSIDE, $HITRESULT-TRANSPARENT, $HITRESULT-CLOSE,
	$HITRESULT-HIT;
  export $DVASPECT-OPAQUE, $DVASPECT-TRANSPARENT;
  export dwExtentMode-value, dwExtentMode-value-setter,
	sizelProposed-value, sizelProposed-value-setter, <DVEXTENTINFO>,
	<LPDVEXTENTINFO>;
  export $DVEXTENT-CONTENT, $DVEXTENT-INTEGRAL;
  export $DVASPECTINFOFLAG-CANOPTIMIZE;
  export $POINTERINACTIVE-ACTIVATEONENTRY,
	$POINTERINACTIVE-DEACTIVATEONLEAVE, $POINTERINACTIVE-ACTIVATEONDRAG;
  export <LPERRORLOG>;
  export <LPPROPERTYBAG>;
  export <LPPERPROPERTYBROWSING>;
  export cElems-value, cElems-value-setter, pElems-value,
	pElems-value-setter, <CALPOLESTR>, <LPCALPOLESTR>;
  export cElems-value, cElems-value-setter, pElems-value,
	pElems-value-setter, <CADWORD>, <LPCADWORD>;
  export $PROPBAG2-TYPE-UNDEFINED, $PROPBAG2-TYPE-DATA,
	$PROPBAG2-TYPE-URL, $PROPBAG2-TYPE-OBJECT, $PROPBAG2-TYPE-STREAM,
	$PROPBAG2-TYPE-STORAGE, $PROPBAG2-TYPE-MONIKER;
  export cfType-value, cfType-value-setter, dwHint-value,
	dwHint-value-setter, pstrName-value, pstrName-value-setter,
	<PROPBAG2>, <LPPROPBAG2>;
  export $QACONTAINER-SHOWHATCHING, $QACONTAINER-SHOWGRABHANDLES,
	$QACONTAINER-USERMODE, $QACONTAINER-DISPLAYASDEFAULT,
	$QACONTAINER-UIDEAD, $QACONTAINER-AUTOCLIP,
	$QACONTAINER-MESSAGEREFLECT, $QACONTAINER-SUPPORTSMNEMONICS;
  export <OLE-COLOR>;
  export pClientSite-value, pClientSite-value-setter,
	pAdviseSink-value, pAdviseSink-value-setter,
	pPropertyNotifySink-value, pPropertyNotifySink-value-setter,
	pUnkEventSink-value, pUnkEventSink-value-setter,
	dwAmbientFlags-value, dwAmbientFlags-value-setter, colorFore-value,
	colorFore-value-setter, colorBack-value, colorBack-value-setter,
	pFont-value, pFont-value-setter, pUndoMgr-value,
	pUndoMgr-value-setter, dwAppearance-value, dwAppearance-value-setter,
	hpal-value, hpal-value-setter, pBindHost-value,
	pBindHost-value-setter, pOleControlSite-value,
	pOleControlSite-value-setter, pServiceProvider-value,
	pServiceProvider-value-setter, <QACONTAINER>, <LPQACONTAINER>;
  export dwMiscStatus-value, dwMiscStatus-value-setter,
	dwViewStatus-value, dwViewStatus-value-setter, dwEventCookie-value,
	dwEventCookie-value-setter, dwPropNotifyCookie-value,
	dwPropNotifyCookie-value-setter, dwPointerActivationPolicy-value,
	dwPointerActivationPolicy-value-setter, <QACONTROL>, <LPQACONTROL>;

  export <IEnumConnections>, $IID-IEnumConnections,
		IEnumConnections/Next, IEnumConnections/Skip,
		IEnumConnections/Reset, IEnumConnections/Clone;
  export <IConnectionPoint>, $IID-IConnectionPoint,
		IConnectionPoint/GetConnectionInterface,
		IConnectionPoint/GetConnectionPointContainer,
		IConnectionPoint/Advise, IConnectionPoint/Unadvise,
		IConnectionPoint/EnumConnections;
  export <IEnumConnectionPoints>, $IID-IEnumConnectionPoints,
		IEnumConnectionPoints/Next, IEnumConnectionPoints/Skip,
		IEnumConnectionPoints/Reset, IEnumConnectionPoints/Clone;
  export <IConnectionPointContainer>,
		$IID-IConnectionPointContainer,
		IConnectionPointContainer/EnumConnectionPoints,
		IConnectionPointContainer/FindConnectionPoint;
  export <IClassFactory2>, $IID-IClassFactory2,
		IClassFactory2/GetLicInfo, IClassFactory2/RequestLicKey,
		IClassFactory2/CreateInstanceLic;
  export <IProvideClassInfo>, $IID-IProvideClassInfo,
		IProvideClassInfo/GetClassInfo;
  export <IProvideClassInfo2>, $IID-IProvideClassInfo2,
		IProvideClassInfo2/GetGUID;
  export <IProvideMultipleClassInfo>,
		$IID-IProvideMultipleClassInfo,
		IProvideMultipleClassInfo/GetMultiTypeInfoCount,
		IProvideMultipleClassInfo/GetInfoOfIndex;
  export <IOleControl>, $IID-IOleControl,
		IOleControl/GetControlInfo, IOleControl/OnMnemonic,
		IOleControl/OnAmbientPropertyChange, IOleControl/FreezeEvents;
  export <IOleControlSite>, $IID-IOleControlSite,
		IOleControlSite/OnControlInfoChanged,
		IOleControlSite/LockInPlaceActive,
		IOleControlSite/GetExtendedControl,
		IOleControlSite/TransformCoords,
		IOleControlSite/TranslateAccelerator, IOleControlSite/OnFocus,
		IOleControlSite/ShowPropertyFrame;
  export <IPropertyPage>, $IID-IPropertyPage,
		IPropertyPage/SetPageSite, IPropertyPage/Activate,
		IPropertyPage/Deactivate, IPropertyPage/GetPageInfo,
		IPropertyPage/SetObjects, IPropertyPage/Show,
		IPropertyPage/Move, IPropertyPage/IsPageDirty,
		IPropertyPage/Apply, IPropertyPage/Help,
		IPropertyPage/TranslateAccelerator;
  export <IPropertyPage2>, $IID-IPropertyPage2,
		IPropertyPage2/EditProperty;
  export <IPropertyPageSite>, $IID-IPropertyPageSite,
		IPropertyPageSite/OnStatusChange,
		IPropertyPageSite/GetLocaleID,
		IPropertyPageSite/GetPageContainer,
		IPropertyPageSite/TranslateAccelerator;
  export <IPropertyNotifySink>, $IID-IPropertyNotifySink,
		IPropertyNotifySink/OnChanged,
		IPropertyNotifySink/OnRequestEdit;
  export <ISpecifyPropertyPages>, $IID-ISpecifyPropertyPages,
		ISpecifyPropertyPages/GetPages;
  export <IPersistMemory>, $IID-IPersistMemory,
		IPersistMemory/IsDirty, IPersistMemory/Load,
		IPersistMemory/Save, IPersistMemory/GetSizeMax,
		IPersistMemory/InitNew;
  export <IPersistStreamInit>, $IID-IPersistStreamInit,
		IPersistStreamInit/IsDirty, IPersistStreamInit/Load,
		IPersistStreamInit/Save, IPersistStreamInit/GetSizeMax,
		IPersistStreamInit/InitNew;
  export <IPersistPropertyBag>, $IID-IPersistPropertyBag,
		IPersistPropertyBag/InitNew, IPersistPropertyBag/Load,
		IPersistPropertyBag/Save;
  export <ISimpleFrameSite>, $IID-ISimpleFrameSite,
		ISimpleFrameSite/PreMessageFilter,
		ISimpleFrameSite/PostMessageFilter;
  export <IFont>, $IID-IFont, IFont/get-Name, IFont/put-Name,
		IFont/get-Size, IFont/put-Size, IFont/get-Bold, IFont/put-Bold,
		IFont/get-Italic, IFont/put-Italic, IFont/get-Underline,
		IFont/put-Underline, IFont/get-Strikethrough,
		IFont/put-Strikethrough, IFont/get-Weight, IFont/put-Weight,
		IFont/get-Charset, IFont/put-Charset, IFont/get-hFont,
		IFont/Clone, IFont/IsEqual, IFont/SetRatio,
		IFont/QueryTextMetrics, IFont/AddRefHfont, IFont/ReleaseHfont,
		IFont/SetHdc;
  export <IPicture>, $IID-IPicture, IPicture/get-Handle,
		IPicture/get-hPal, IPicture/get-Type, IPicture/get-Width,
		IPicture/get-Height, IPicture/Render, IPicture/set-hPal,
		IPicture/get-CurDC, IPicture/SelectPicture,
		IPicture/get-KeepOriginalFormat,
		IPicture/put-KeepOriginalFormat, IPicture/PictureChanged,
		IPicture/SaveAsFile, IPicture/get-Attributes;
  export <IFontEventsDisp>, $IID-IFontEventsDisp;
  export <IFontDisp>, $IID-IFontDisp;
  export <IPictureDisp>, $IID-IPictureDisp;
  export <IOleInPlaceObjectWindowless>,
		$IID-IOleInPlaceObjectWindowless,
		IOleInPlaceObjectWindowless/OnWindowMessage,
		IOleInPlaceObjectWindowless/GetDropTarget;
  export <IOleInPlaceSiteEx>, $IID-IOleInPlaceSiteEx,
		IOleInPlaceSiteEx/OnInPlaceActivateEx,
		IOleInPlaceSiteEx/OnInPlaceDeactivateEx,
		IOleInPlaceSiteEx/RequestUIActivate;
  export <IOleInPlaceSiteWindowless>,
		$IID-IOleInPlaceSiteWindowless,
		IOleInPlaceSiteWindowless/CanWindowlessActivate,
		IOleInPlaceSiteWindowless/GetCapture,
		IOleInPlaceSiteWindowless/SetCapture,
		IOleInPlaceSiteWindowless/GetFocus,
		IOleInPlaceSiteWindowless/SetFocus,
		IOleInPlaceSiteWindowless/GetDC,
		IOleInPlaceSiteWindowless/ReleaseDC,
		IOleInPlaceSiteWindowless/InvalidateRect,
		IOleInPlaceSiteWindowless/InvalidateRgn,
		IOleInPlaceSiteWindowless/ScrollRect,
		IOleInPlaceSiteWindowless/AdjustRect,
		IOleInPlaceSiteWindowless/OnDefWindowMessage;
  export <IViewObjectEx>, $IID-IViewObjectEx,
		IViewObjectEx/GetRect, IViewObjectEx/GetViewStatus,
		IViewObjectEx/QueryHitPoint, IViewObjectEx/QueryHitRect,
		IViewObjectEx/GetNaturalExtent;
  export <IOleUndoUnit>, $IID-IOleUndoUnit, IOleUndoUnit/Do,
		IOleUndoUnit/GetDescription, IOleUndoUnit/GetUnitType,
		IOleUndoUnit/OnNextAdd;
  export <IOleParentUndoUnit>, $IID-IOleParentUndoUnit,
		IOleParentUndoUnit/Open, IOleParentUndoUnit/Close,
		IOleParentUndoUnit/Add, IOleParentUndoUnit/FindUnit,
		IOleParentUndoUnit/GetParentState;
  export <IEnumOleUndoUnits>, $IID-IEnumOleUndoUnits,
		IEnumOleUndoUnits/Next, IEnumOleUndoUnits/Skip,
		IEnumOleUndoUnits/Reset, IEnumOleUndoUnits/Clone;
  export <IOleUndoManager>, $IID-IOleUndoManager,
		IOleUndoManager/Open, IOleUndoManager/Close,
		IOleUndoManager/Add, IOleUndoManager/GetOpenParentState,
		IOleUndoManager/DiscardFrom, IOleUndoManager/UndoTo,
		IOleUndoManager/RedoTo, IOleUndoManager/EnumUndoable,
		IOleUndoManager/EnumRedoable,
		IOleUndoManager/GetLastUndoDescription,
		IOleUndoManager/GetLastRedoDescription, IOleUndoManager/Enable;
  export <IPointerInactive>, $IID-IPointerInactive,
		IPointerInactive/GetActivationPolicy,
		IPointerInactive/OnInactiveMouseMove,
		IPointerInactive/OnInactiveSetCursor;
  export <IObjectWithSite>, $IID-IObjectWithSite,
		IObjectWithSite/SetSite, IObjectWithSite/GetSite;
  export <IErrorLog>, $IID-IErrorLog, IErrorLog/AddError;
  export <IPropertyBag>, $IID-IPropertyBag, IPropertyBag/Read,
		IPropertyBag/Write;
  export <IPerPropertyBrowsing>, $IID-IPerPropertyBrowsing,
		IPerPropertyBrowsing/GetDisplayString,
		IPerPropertyBrowsing/MapPropertyToPage,
		IPerPropertyBrowsing/GetPredefinedStrings,
		IPerPropertyBrowsing/GetPredefinedValue;
  export <IPropertyBag2>, $IID-IPropertyBag2, IPropertyBag2/Read,
		IPropertyBag2/Write, IPropertyBag2/CountProperties,
		IPropertyBag2/GetPropertyInfo, IPropertyBag2/LoadObject;
  export <IPersistPropertyBag2>, $IID-IPersistPropertyBag2,
		IPersistPropertyBag2/InitNew, IPersistPropertyBag2/Load,
		IPersistPropertyBag2/Save, IPersistPropertyBag2/IsDirty;
  export <IAdviseSinkEx>, $IID-IAdviseSinkEx,
		IAdviseSinkEx/OnViewStatusChange;
  export <IQuickActivate>, $IID-IQuickActivate,
		IQuickActivate/QuickActivate, IQuickActivate/SetContentExtent,
		IQuickActivate/GetContentExtent;

  // from "extra.dylan":
  export <LPOLE-HANDLE>, <LPOLE-XSIZE-HIMETRIC>, <LPOLE-YSIZE-HIMETRIC>;

end module OLE-Controls;
