module:    Dylan-user	
Synopsis:  This is a Dylan library to act as an interface to OLE2.  
	   This enables Dylan programs to use OLE
	   in a manner very similar to usage from C++ without MFC.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */



define library OLE
  use functional-dylan;
  use C-FFI;
  use COM;
  use Win32-common;
  use Win32-GDI;
  use Win32-User;
  export OLE;
end;

define module OLE
  use functional-dylan;
  use C-FFI;
  use machine-words, import: { \%+ };
  use COM, export: all;
  use COM-internal;
  use Win32-common;
  use Win32-GDI, import: { <LOGPALETTE>, <LPLOGPALETTE> };
  use Win32-User, import: { <LPMSG>, <LPHWND> };
	
  // This module includes the following interfaces:
  export $IID-IOleAdviseHolder, IOleAdviseHolder/Advise,
		IOleAdviseHolder/Unadvise, IOleAdviseHolder/EnumAdvise,
		IOleAdviseHolder/SendOnRename, IOleAdviseHolder/SendOnSave,
		IOleAdviseHolder/SendOnClose;
  export <IOleCache>, $IID-IOleCache, IOleCache/Cache,
		IOleCache/Uncache, IOleCache/EnumCache, IOleCache/InitCache,
		IOleCache/SetData;
  export <IOleCache2>, $IID-IOleCache2, IOleCache2/UpdateCache,
		IOleCache2/DiscardCache;
  export $IID-IOleCacheControl, IOleCacheControl/OnRun,
		IOleCacheControl/OnStop;
  export <IParseDisplayName>, $IID-IParseDisplayName,
		IParseDisplayName/ParseDisplayName;
  export <IOleContainer>, $IID-IOleContainer,
		IOleContainer/EnumObjects, IOleContainer/LockContainer;
  export <IOleClientSite>, $IID-IOleClientSite,
		IOleClientSite/SaveObject, IOleClientSite/GetMoniker,
		IOleClientSite/GetContainer, IOleClientSite/ShowObject,
		IOleClientSite/OnShowWindow,
		IOleClientSite/RequestNewObjectLayout;
  export <IOleObject>, $IID-IOleObject, IOleObject/SetClientSite,
		IOleObject/GetClientSite, IOleObject/SetHostNames,
		IOleObject/Close, IOleObject/SetMoniker, IOleObject/GetMoniker,
		IOleObject/InitFromData, IOleObject/GetClipboardData,
		IOleObject/DoVerb, IOleObject/EnumVerbs, IOleObject/Update,
		IOleObject/IsUpToDate, IOleObject/GetUserClassID,
		IOleObject/GetUserType, IOleObject/SetExtent,
		IOleObject/GetExtent, IOleObject/Advise, IOleObject/Unadvise,
		IOleObject/EnumAdvise, IOleObject/GetMiscStatus,
		IOleObject/SetColorScheme;
  export <IOleWindow>, $IID-IOleWindow, IOleWindow/GetWindow,
		IOleWindow/ContextSensitiveHelp;
  export $IID-IOleLink, IOleLink/SetUpdateOptions,
		IOleLink/GetUpdateOptions, IOleLink/SetSourceMoniker,
		IOleLink/GetSourceMoniker, IOleLink/SetSourceDisplayName,
		IOleLink/GetSourceDisplayName, IOleLink/BindToSource,
		IOleLink/BindIfRunning, IOleLink/GetBoundSource,
		IOleLink/UnbindSource, IOleLink/Update;
  export <IOleItemContainer>, $IID-IOleItemContainer,
		IOleItemContainer/GetObject,
		IOleItemContainer/GetObjectStorage,
		IOleItemContainer/IsRunning;
  export <IOleInPlaceUIWindow>, $IID-IOleInPlaceUIWindow,
		IOleInPlaceUIWindow/GetBorder,
		IOleInPlaceUIWindow/RequestBorderSpace,
		IOleInPlaceUIWindow/SetBorderSpace,
		IOleInPlaceUIWindow/SetActiveObject;
  export <IOleInPlaceActiveObject>, $IID-IOleInPlaceActiveObject,
		IOleInPlaceActiveObject/TranslateAccelerator,
		IOleInPlaceActiveObject/OnFrameWindowActivate,
		IOleInPlaceActiveObject/OnDocWindowActivate,
		IOleInPlaceActiveObject/ResizeBorder,
		IOleInPlaceActiveObject/EnableModeless;
  export <IOleInPlaceFrame>, $IID-IOleInPlaceFrame,
		IOleInPlaceFrame/InsertMenus, IOleInPlaceFrame/SetMenu,
		IOleInPlaceFrame/RemoveMenus, IOleInPlaceFrame/SetStatusText,
		IOleInPlaceFrame/EnableModeless,
		IOleInPlaceFrame/TranslateAccelerator;
  export <IOleInPlaceObject>, $IID-IOleInPlaceObject,
		IOleInPlaceObject/InPlaceDeactivate,
		IOleInPlaceObject/UIDeactivate,
		IOleInPlaceObject/SetObjectRects,
		IOleInPlaceObject/ReactivateAndUndo;
  export <IOleInPlaceSite>, $IID-IOleInPlaceSite,
		IOleInPlaceSite/CanInPlaceActivate,
		IOleInPlaceSite/OnInPlaceActivate,
		IOleInPlaceSite/OnUIActivate, IOleInPlaceSite/GetWindowContext,
		IOleInPlaceSite/Scroll, IOleInPlaceSite/OnUIDeactivate,
		IOleInPlaceSite/OnInPlaceDeactivate,
		IOleInPlaceSite/DiscardUndoState,
		IOleInPlaceSite/DeactivateAndUndo,
		IOleInPlaceSite/OnPosRectChange;
  export <IContinue>, $IID-IContinue, IContinue/FContinue;
  export <IViewObject>, $IID-IViewObject, IViewObject/Draw,
		IViewObject/GetColorSet, IViewObject/Freeze,
		IViewObject/Unfreeze, IViewObject/SetAdvise,
		IViewObject/GetAdvise;
  export <IViewObject2>, $IID-IViewObject2,
		IViewObject2/GetExtent;
  export <IDropSource>, $IID-IDropSource,
		IDropSource/QueryContinueDrag, IDropSource/GiveFeedback;
  export <IDropTarget>, $IID-IDropTarget, IDropTarget/DragEnter,
		IDropTarget/DragOver, IDropTarget/DragLeave, IDropTarget/Drop;
  export <IEnumOLEVERB>, $IID-IEnumOLEVERB, IEnumOLEVERB/Next,
		IEnumOLEVERB/Skip, IEnumOLEVERB/Reset, IEnumOLEVERB/Clone;

  // misc. stuff in "ole.dylan":
  export <HOLEMENU>;
  export <LPHGLOBAL>;

  // misc. stuff from "ole-misc.dylan":

  // from "ole2.h":
  export $E-DRAW, $DATA-E-FORMATETC;
  export $OLE-E-NOEXTENSION, $OLE-E-VERSEXTENSION, $OLE-E-IPBUSY,
	$OLE-E-NOT-FRONT-PROCESS, $OLE-E-WRONG-MENU, $OLE-E-MENU-NOT-PATCHED,
	$OLE-E-MENUID-NOT-HASHED, $OLE-E-foo;
  export $OLEIVERB-PRIMARY, $OLEIVERB-SHOW, $OLEIVERB-OPEN,
	$OLEIVERB-HIDE, $OLEIVERB-UIACTIVATE, $OLEIVERB-INPLACEACTIVATE,
	$OLEIVERB-DISCARDUNDOSTATE, $EMBDHLP-INPROC-HANDLER,
	$EMBDHLP-INPROC-SERVER, $EMBDHLP-CREATENOW, $EMBDHLP-DELAYCREATE;

  // from "oleidl.h":
  export <LPOLEADVISEHOLDER>;
  export <LPOLECACHE>;
  export <LPOLECACHE2>, $DISCARDCACHE-SAVEIFDIRTY,
	$DISCARDCACHE-NOSAVE;
  export <LPOLECACHECONTROL>;
  export <LPPARSEDISPLAYNAME>;
  export <LPOLECONTAINER>;
  export <LPOLECLIENTSITE>;
  export <LPOLEOBJECT>;
  export $OLEGETMONIKER-ONLYIFTHERE, $OLEGETMONIKER-FORCEASSIGN,
	$OLEGETMONIKER-UNASSIGN, $OLEGETMONIKER-TEMPFORUSER;
  export $OLEWHICHMK-CONTAINER, $OLEWHICHMK-OBJREL,
	$OLEWHICHMK-OBJFULL;
  export $USERCLASSTYPE-FULL, $USERCLASSTYPE-SHORT,
	$USERCLASSTYPE-APPNAME;
  export $OLEMISC-RECOMPOSEONRESIZE, $OLEMISC-ONLYICONIC,
	$OLEMISC-INSERTNOTREPLACE, $OLEMISC-STATIC, $OLEMISC-CANTLINKINSIDE,
	$OLEMISC-CANLINKBYOLE1, $OLEMISC-ISLINKOBJECT, $OLEMISC-INSIDEOUT,
	$OLEMISC-ACTIVATEWHENVISIBLE, $OLEMISC-RENDERINGISDEVICEINDEPENDENT,
	$OLEMISC-INVISIBLEATRUNTIME, $OLEMISC-ALWAYSRUN,
	$OLEMISC-ACTSLIKEBUTTON, $OLEMISC-ACTSLIKELABEL,
	$OLEMISC-NOUIACTIVATE, $OLEMISC-ALIGNABLE, $OLEMISC-SIMPLEFRAME,
	$OLEMISC-SETCLIENTSITEFIRST, $OLEMISC-IMEMODE,
	$OLEMISC-IGNOREACTIVATEWHENVISIBLE, $OLEMISC-WANTSTOMENUMERGE,
	$OLEMISC-SUPPORTSMULTILEVELUNDO;
  export $OLECLOSE-SAVEIFDIRTY, $OLECLOSE-NOSAVE,
	$OLECLOSE-PROMPTSAVE;
  export $OLERENDER-NONE, $OLERENDER-DRAW, $OLERENDER-FORMAT,
	$OLERENDER-ASIS;
  export <LPOLEWINDOW>;
  export <LPOLELINK>;
  export $OLEUPDATE-ALWAYS, $OLEUPDATE-ONCALL;
  export $OLELINKBIND-EVENIFCLASSDIFF;
  export <LPOLEITEMCONTAINER>;
  export $BINDSPEED-INDEFINITE, $BINDSPEED-MODERATE,
	$BINDSPEED-IMMEDIATE;
  export $OLECONTF-EMBEDDINGS, $OLECONTF-LINKS, $OLECONTF-OTHERS,
	$OLECONTF-ONLYUSER, $OLECONTF-ONLYIFRUNNING;
  export <LPOLEINPLACEUIWINDOW>;
  export <LPCBORDERWIDTHS>;
  export <LPOLEINPLACEACTIVEOBJECT>;
  export <LPOLEINPLACEFRAME>;
  export fMDIApp-value, fMDIApp-value-setter, hwndFrame-value,
	hwndFrame-value-setter, cAccelEntries-value,
	cAccelEntries-value-setter, <OLEINPLACEFRAMEINFO>,
	<LPOLEINPLACEFRAMEINFO>;
  export width-array, width-array-setter, width-value,
	<OLEMENUGROUPWIDTHS>, <LPOLEMENUGROUPWIDTHS>;
  export <LPOLEINPLACEOBJECT>;
  export <LPOLEINPLACESITE>;
  export <LPVIEWOBJECT>;
  export <LPVIEWOBJECT2>;
  export <LPDROPSOURCE>;
  export <LPDROPTARGET>;
  export <LPENUMOLEVERB>;
  export lVerb-value, lVerb-value-setter, lpszVerbName-value,
	lpszVerbName-value-setter, fuFlags-value, fuFlags-value-setter,
	grfAttribs-value, grfAttribs-value-setter, <OLEVERB>, <LPOLEVERB>;
  export $OLEVERBATTRIB-NEVERDIRTIES, $OLEVERBATTRIB-ONCONTAINERMENU;
  export ReadClassStg, WriteClassStg, ReadClassStm, WriteClassStm,
	WriteFmtUserTypeStg, ReadFmtUserTypeStg;
  export OleQueryLinkFromData, OleQueryCreateFromData;
  export OleCreate, OleCreateFromData, OleCreateLinkFromData,
	OleCreateStaticFromData;
  export OleCreateLink, OleCreateLinkToFile, OleCreateFromFile,
	OleLoad, OleSave, OleLoadFromStream, OleSaveToStream;
  export OleSetContainedObject, OleNoteObjectVisible;
  export RegisterDragDrop, RevokeDragDrop, DoDragDrop,
	OleSetClipboard, OleGetClipboard, OleFlushClipboard,
	OleIsCurrentClipboard;
  export OleCreateMenuDescriptor, OleSetMenuDescriptor,
	OleDestroyMenuDescriptor, OleTranslateAccelerator;
  export OleDuplicateData, OleDraw, OleRun, OleIsRunning,
	OleLockRunning, ReleaseStgMedium, CreateOleAdviseHolder,
	OleCreateDefaultHandler, OleCreateEmbeddingHelper, IsAccelerator,
	OleGetIconOfFile, OleGetIconOfClass, OleMetafilePictFromIconAndLabel;
  export OleRegGetUserType, OleRegGetMiscStatus, OleRegEnumFormatEtc,
	OleRegEnumVerbs;
  export GetHGlobalFromILockBytes, CreateILockBytesOnHGlobal,
	GetHGlobalFromStream, CreateStreamOnHGlobal;
  export OleDoAutoConvert, OleGetAutoConvert, OleSetAutoConvert,
	GetConvertStg, SetConvertStg;

  // status and error codes from "winerror.h":
  export $OLE-E-OLEVERB, $OLE-E-ADVF, $OLE-E-ENUM-NOMORE,
	$OLE-E-ADVISENOTSUPPORTED, $OLE-E-NOCONNECTION, $OLE-E-NOTRUNNING,
	$OLE-E-NOCACHE, $OLE-E-BLANK, $OLE-E-CLASSDIFF,
	$OLE-E-CANT-GETMONIKER, $OLE-E-CANT-BINDTOSOURCE, $OLE-E-STATIC,
	$OLE-E-PROMPTSAVECANCELLED, $OLE-E-INVALIDRECT, $OLE-E-WRONGCOMPOBJ,
	$OLE-E-INVALIDHWND, $OLE-E-NOT-INPLACEACTIVE, $OLE-E-CANTCONVERT,
	$OLE-E-NOSTORAGE, $DV-E-FORMATETC, $DV-E-DVTARGETDEVICE,
	$DV-E-STGMEDIUM, $DV-E-STATDATA, $DV-E-LINDEX, $DV-E-TYMED,
	$DV-E-CLIPFORMAT, $DV-E-DVASPECT, $DV-E-DVTARGETDEVICE-SIZE,
	$DV-E-NOIVIEWOBJECT, $DRAGDROP-E-NOTREGISTERED,
	$DRAGDROP-E-ALREADYREGISTERED, $DRAGDROP-E-INVALIDHWND, $VIEW-E-DRAW,
	$CAT-E-CATIDNOEXIST, $CAT-E-NODESCRIPTION, $CS-E-PACKAGE-NOTFOUND,
	$CS-E-NOT-DELETABLE, $CS-E-CLASS-NOTFOUND, $CS-E-INVALID-VERSION,
	$CS-E-NO-CLASSSTORE, $CACHE-E-NOCACHE-UPDATED, $OLEOBJ-E-NOVERBS,
	$OLEOBJ-E-INVALIDVERB, $INPLACE-E-NOTUNDOABLE,
	$INPLACE-E-NOTOOLSPACE, $CONVERT10-E-OLESTREAM-GET,
	$CONVERT10-E-OLESTREAM-PUT, $CONVERT10-E-OLESTREAM-FMT,
	$CONVERT10-E-OLESTREAM-BITMAP-TO-DIB, $CONVERT10-E-STG-FMT,
	$CONVERT10-E-STG-NO-STD-STREAM, $CONVERT10-E-STG-DIB-TO-BITMAP,
	$CLIPBRD-E-CANT-OPEN, $CLIPBRD-E-CANT-EMPTY, $CLIPBRD-E-CANT-SET,
	$CLIPBRD-E-BAD-DATA, $CLIPBRD-E-CANT-CLOSE, $OLE-S-USEREG,
	$OLE-S-STATIC, $OLE-S-MAC-CLIPFORMAT, $DRAGDROP-S-DROP,
	$DRAGDROP-S-CANCEL, $DRAGDROP-S-USEDEFAULTCURSORS,
	$DATA-S-SAMEFORMATETC, $VIEW-S-ALREADY-FROZEN,
	$CACHE-S-FORMATETC-NOTSUPPORTED, $CACHE-S-SAMECACHE,
	$CACHE-S-SOMECACHES-NOTUPDATED, $OLEOBJ-S-INVALIDVERB,
	$OLEOBJ-S-CANNOT-DOVERB-NOW, $OLEOBJ-S-INVALIDHWND,
	$INPLACE-S-TRUNCATED, $CONVERT10-S-NO-PRESENTATION,
	$MEM-E-INVALID-ROOT, $MEM-E-INVALID-LINK, $MEM-E-INVALID-SIZE;
end;
