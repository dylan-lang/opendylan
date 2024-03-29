Module:    OLE
Synopsis:  FFI declarations for some some miscellaneous pieces of the
	   OLE interface that do not need special treatment.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $Id: omisc.dylan,v 1.1 2004/03/12 00:09:39 cgay Exp $

// This file is automatically generated from "ole2.h"; do not edit.

// enum inplace_errors:
define inline-only constant $OLE-E-NOEXTENSION =
	\%+($OLE-E-NOSTORAGE, 1);
define inline-only constant $OLE-E-VERSEXTENSION =
	\%+($OLE-E-NOEXTENSION, 1);
define inline-only constant $OLE-E-IPBUSY =
	\%+($OLE-E-VERSEXTENSION, 1);
define inline-only constant $OLE-E-NOT-FRONT-PROCESS =
	\%+($OLE-E-IPBUSY, 1);
define inline-only constant $OLE-E-WRONG-MENU =
	\%+($OLE-E-NOT-FRONT-PROCESS, 1);
define inline-only constant $OLE-E-MENU-NOT-PATCHED =
	\%+($OLE-E-WRONG-MENU, 1);
define inline-only constant $OLE-E-MENUID-NOT-HASHED =
	\%+($OLE-E-MENU-NOT-PATCHED, 1);
define inline-only constant $OLE-E-foo =
	\%+($OLE-E-MENUID-NOT-HASHED, 1);

define inline-only constant $OLEIVERB-PRIMARY           =    0;
define inline-only constant $OLEIVERB-SHOW              =   -1;
define inline-only constant $OLEIVERB-OPEN              =   -2;
define inline-only constant $OLEIVERB-HIDE              =   -3;
define inline-only constant $OLEIVERB-UIACTIVATE        =   -4;
define inline-only constant $OLEIVERB-INPLACEACTIVATE   =   -5;
define inline-only constant $OLEIVERB-DISCARDUNDOSTATE  =   -6;
define inline-only constant $EMBDHLP-INPROC-HANDLER     = #x0000;
define inline-only constant $EMBDHLP-INPROC-SERVER      = #x0001;
define inline-only constant $EMBDHLP-CREATENOW          = #x00000000;
define inline-only constant $EMBDHLP-DELAYCREATE        = #x00010000;

// This file is automatically generated from "oleidl.h"; do not edit.

define constant <LPOLEADVISEHOLDER> = <Interface>;
define constant <LPOLECACHE> = <Interface>;
define constant <LPOLECACHE2> = <Interface>;
// enum DISCARDCACHE:
define inline-only constant $DISCARDCACHE-SAVEIFDIRTY = 0;
define inline-only constant $DISCARDCACHE-NOSAVE = 1;

define constant <LPOLECACHECONTROL> = <Interface>;
define constant <LPPARSEDISPLAYNAME> = <Interface>;
define constant <LPOLECONTAINER> = <Interface>;
define constant <LPOLECLIENTSITE> = <Interface>;
define constant <LPOLEOBJECT> = <Interface>;
// enum OLEGETMONIKER:
define inline-only constant $OLEGETMONIKER-ONLYIFTHERE = 1;
define inline-only constant $OLEGETMONIKER-FORCEASSIGN = 2;
define inline-only constant $OLEGETMONIKER-UNASSIGN = 3;
define inline-only constant $OLEGETMONIKER-TEMPFORUSER = 4;

// enum OLEWHICHMK:
define inline-only constant $OLEWHICHMK-CONTAINER = 1;
define inline-only constant $OLEWHICHMK-OBJREL = 2;
define inline-only constant $OLEWHICHMK-OBJFULL = 3;

// enum USERCLASSTYPE:
define inline-only constant $USERCLASSTYPE-FULL = 1;
define inline-only constant $USERCLASSTYPE-SHORT = 2;
define inline-only constant $USERCLASSTYPE-APPNAME = 3;

// enum OLEMISC:
define inline-only constant $OLEMISC-RECOMPOSEONRESIZE = #x1;
define inline-only constant $OLEMISC-ONLYICONIC = #x2;
define inline-only constant $OLEMISC-INSERTNOTREPLACE = #x4;
define inline-only constant $OLEMISC-STATIC = #x8;
define inline-only constant $OLEMISC-CANTLINKINSIDE = #x10;
define inline-only constant $OLEMISC-CANLINKBYOLE1 = #x20;
define inline-only constant $OLEMISC-ISLINKOBJECT = #x40;
define inline-only constant $OLEMISC-INSIDEOUT = #x80;
define inline-only constant $OLEMISC-ACTIVATEWHENVISIBLE = #x100;
define inline-only constant $OLEMISC-RENDERINGISDEVICEINDEPENDENT = #x200;
define inline-only constant $OLEMISC-INVISIBLEATRUNTIME = #x400;
define inline-only constant $OLEMISC-ALWAYSRUN = #x800;
define inline-only constant $OLEMISC-ACTSLIKEBUTTON = #x1000;
define inline-only constant $OLEMISC-ACTSLIKELABEL = #x2000;
define inline-only constant $OLEMISC-NOUIACTIVATE = #x4000;
define inline-only constant $OLEMISC-ALIGNABLE = #x8000;
define inline-only constant $OLEMISC-SIMPLEFRAME = #x10000;
define inline-only constant $OLEMISC-SETCLIENTSITEFIRST = #x20000;
define inline-only constant $OLEMISC-IMEMODE = #x40000;
define inline-only constant $OLEMISC-IGNOREACTIVATEWHENVISIBLE = #x80000;
define inline-only constant $OLEMISC-WANTSTOMENUMERGE = #x100000;
define inline-only constant $OLEMISC-SUPPORTSMULTILEVELUNDO = #x200000;

// enum OLECLOSE:
define inline-only constant $OLECLOSE-SAVEIFDIRTY = 0;
define inline-only constant $OLECLOSE-NOSAVE = 1;
define inline-only constant $OLECLOSE-PROMPTSAVE = 2;

// enum OLERENDER:
define inline-only constant $OLERENDER-NONE = 0;
define inline-only constant $OLERENDER-DRAW = 1;
define inline-only constant $OLERENDER-FORMAT = 2;
define inline-only constant $OLERENDER-ASIS = 3;

define constant <LPOLEWINDOW> = <Interface>;
define constant <LPOLELINK> = <Interface>;
// enum OLEUPDATE:
define inline-only constant $OLEUPDATE-ALWAYS = 1;
define inline-only constant $OLEUPDATE-ONCALL = 3;

// enum OLELINKBIND:
define inline-only constant $OLELINKBIND-EVENIFCLASSDIFF = 1;

define constant <LPOLEITEMCONTAINER> = <Interface>;
// enum BINDSPEED:
define inline-only constant $BINDSPEED-INDEFINITE = 1;
define inline-only constant $BINDSPEED-MODERATE = 2;
define inline-only constant $BINDSPEED-IMMEDIATE = 3;

// enum OLECONTF:
define inline-only constant $OLECONTF-EMBEDDINGS = 1;
define inline-only constant $OLECONTF-LINKS = 2;
define inline-only constant $OLECONTF-OTHERS = 4;
define inline-only constant $OLECONTF-ONLYUSER = 8;
define inline-only constant $OLECONTF-ONLYIFRUNNING = 16;

define constant <LPOLEINPLACEUIWINDOW> = <Interface>;
define inline constant <LPCBORDERWIDTHS> = <LPCRECT>;
define constant <LPOLEINPLACEACTIVEOBJECT> = <Interface>;
define constant <LPOLEINPLACEFRAME> = <Interface>;

define C-struct <OLEINPLACEFRAMEINFO>
  sealed inline-only slot cb-value       :: <UINT>;
  sealed inline-only slot fMDIApp-value  :: <BOOL>;
  sealed inline-only slot hwndFrame-value :: <HWND>;
  sealed inline-only slot haccel-value   :: <HACCEL>;
  sealed inline-only slot cAccelEntries-value :: <UINT>;
  pack: 8;
  pointer-type-name: <LPOLEINPLACEFRAMEINFO>;
  c-name: "struct tagOIFI";
end C-struct <OLEINPLACEFRAMEINFO>;

define C-struct <OLEMENUGROUPWIDTHS>
  sealed array slot width-array          :: <LONG>, length: 6,
	address-getter: width-value;
  pack: 8;
  pointer-type-name: <LPOLEMENUGROUPWIDTHS>;
  c-name: "struct tagOleMenuGroupWidths";
end C-struct <OLEMENUGROUPWIDTHS>;
define constant <LPOLEINPLACEOBJECT> = <Interface>;
define constant <LPOLEINPLACESITE> = <Interface>;
define constant <LPVIEWOBJECT> = <Interface>;
define constant <LPVIEWOBJECT2> = <Interface>;
define constant <LPDROPSOURCE> = <Interface>;
define constant <LPDROPTARGET> = <Interface>;
define constant <LPENUMOLEVERB> = <Interface>;

define C-struct <OLEVERB>
  sealed inline-only slot lVerb-value    :: <LONG>;
  sealed inline-only slot lpszVerbName-value :: <LPOLESTR>;
  sealed inline-only slot fuFlags-value  :: <DWORD>;
  sealed inline-only slot grfAttribs-value :: <DWORD>;
  pack: 8;
  pointer-type-name: <LPOLEVERB>;
  c-name: "struct tagOLEVERB";
end C-struct <OLEVERB>;
// enum OLEVERBATTRIB:
define inline-only constant $OLEVERBATTRIB-NEVERDIRTIES = 1;
define inline-only constant $OLEVERBATTRIB-ONCONTAINERMENU = 2;


define inline-only constant $E-DRAW      = $VIEW-E-DRAW;
define inline-only constant $DATA-E-FORMATETC = $DV-E-FORMATETC;

// --- end of data from "oleidl.h" ---


define C-function ReadClassStg
  parameter pStg       :: <LPSTORAGE>;
  parameter pclsid     :: <LPCLSID>;
  result status :: <C-HRESULT>;
  c-name: "ReadClassStg", c-modifiers: "__stdcall";
end;

define C-function WriteClassStg
  parameter pStg       :: <LPSTORAGE>;
  parameter rclsid     :: <REFCLSID>;
  result status :: <C-HRESULT>;
  c-name: "WriteClassStg", c-modifiers: "__stdcall";
end;

define C-function ReadClassStm
  parameter pStm       :: <LPSTREAM>;
  parameter pclsid     :: <LPCLSID>;
  result status :: <C-HRESULT>;
  c-name: "ReadClassStm", c-modifiers: "__stdcall";
end;

define C-function WriteClassStm
  parameter pStm       :: <LPSTREAM>;
  parameter rclsid     :: <REFCLSID>;
  result status :: <C-HRESULT>;
  c-name: "WriteClassStm", c-modifiers: "__stdcall";
end;

define C-function WriteFmtUserTypeStg
  parameter pstg       :: <LPSTORAGE>;
  parameter cf         :: <CLIPFORMAT>;
  parameter lpszUserType :: <LPOLESTR>;
  result status :: <C-HRESULT>;
  c-name: "WriteFmtUserTypeStg", c-modifiers: "__stdcall";
end;

define C-function ReadFmtUserTypeStg
  parameter pstg       :: <LPSTORAGE>;
  parameter pcf        :: <LPCLIPFORMAT>;
  output parameter lplpszUserType :: <LPLPOLESTR>;
  result status :: <C-HRESULT>;
  c-name: "ReadFmtUserTypeStg", c-modifiers: "__stdcall";
end;

define C-function OleQueryLinkFromData
  parameter pSrcDataObject :: <LPDATAOBJECT>;
  result status :: <C-HRESULT>;
  c-name: "OleQueryLinkFromData", c-modifiers: "__stdcall";
end;

define C-function OleQueryCreateFromData
  parameter pSrcDataObject :: <LPDATAOBJECT>;
  result status :: <C-HRESULT>;
  c-name: "OleQueryCreateFromData", c-modifiers: "__stdcall";
end;

define C-function OleCreate
  parameter rclsid     :: <REFCLSID>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter pFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreate", c-modifiers: "__stdcall";
end;

define C-function OleCreateFromData
  parameter pSrcDataObj :: <LPDATAOBJECT>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter pFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateFromData", c-modifiers: "__stdcall";
end;

define C-function OleCreateLinkFromData
  parameter pSrcDataObj :: <LPDATAOBJECT>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter pFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateLinkFromData", c-modifiers: "__stdcall";
end;

define C-function OleCreateStaticFromData
  parameter pSrcDataObj :: <LPDATAOBJECT>;
  parameter iid        :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter pFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateStaticFromData", c-modifiers: "__stdcall";
end;

define C-function OleCreateLink
  parameter pmkLinkSrc :: <LPMONIKER>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter lpFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateLink", c-modifiers: "__stdcall";
end;

define C-function OleCreateLinkToFile
  parameter lpszFileName :: <LPCOLESTR>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter lpFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateLinkToFile", c-modifiers: "__stdcall";
end;

define C-function OleCreateFromFile
  parameter rclsid     :: <REFCLSID>;
  parameter lpszFileName :: <LPCOLESTR>;
  parameter riid       :: <REFIID>;
  parameter renderopt  :: <DWORD>;
  parameter lpFormatEtc :: <LPFORMATETC>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  parameter pStg       :: <LPSTORAGE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateFromFile", c-modifiers: "__stdcall";
end;

define C-function OleLoad
  parameter pStg       :: <LPSTORAGE>;
  parameter riid       :: <REFIID>;
  parameter pClientSite :: <LPOLECLIENTSITE>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleLoad", c-modifiers: "__stdcall";
end;

define C-function OleSave
  parameter pPS        :: <LPPERSISTSTORAGE>;
  parameter pStg       :: <LPSTORAGE>;
  parameter fSameAsLoad :: <BOOL>;
  result status :: <C-HRESULT>;
  c-name: "OleSave", c-modifiers: "__stdcall";
end;

define C-function OleLoadFromStream
  parameter pStm       :: <LPSTREAM>;
  parameter iidInterface :: <REFIID>;
  output parameter ppvObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleLoadFromStream", c-modifiers: "__stdcall";
end;

define C-function OleSaveToStream
  parameter pPStm      :: <LPPERSISTSTREAM>;
  parameter pStm       :: <LPSTREAM>;
  result status :: <C-HRESULT>;
  c-name: "OleSaveToStream", c-modifiers: "__stdcall";
end;

define C-function OleSetContainedObject
  parameter pUnknown   :: <LPUNKNOWN>;
  parameter fContained :: <BOOL>;
  result status :: <C-HRESULT>;
  c-name: "OleSetContainedObject", c-modifiers: "__stdcall";
end;

define C-function OleNoteObjectVisible
  parameter pUnknown   :: <LPUNKNOWN>;
  parameter fVisible   :: <BOOL>;
  result status :: <C-HRESULT>;
  c-name: "OleNoteObjectVisible", c-modifiers: "__stdcall";
end;

define C-function RegisterDragDrop
  parameter hwnd       :: <HWND>;
  parameter pDropTarget :: <LPDROPTARGET>;
  result status :: <C-HRESULT>;
  c-name: "RegisterDragDrop", c-modifiers: "__stdcall";
end;

define C-function RevokeDragDrop
  parameter hwnd       :: <HWND>;
  result status :: <C-HRESULT>;
  c-name: "RevokeDragDrop", c-modifiers: "__stdcall";
end;

define C-function DoDragDrop
  parameter pDataObj   :: <LPDATAOBJECT>;
  parameter pDropSource :: <LPDROPSOURCE>;
  parameter dwOKEffects :: <DWORD>;
  output parameter pdwEffect :: <LPDWORD>;
  result status :: <C-HRESULT>;
  c-name: "DoDragDrop", c-modifiers: "__stdcall";
end;

define C-function OleSetClipboard
  parameter pDataObj   :: <LPDATAOBJECT>;
  result status :: <C-HRESULT>;
  c-name: "OleSetClipboard", c-modifiers: "__stdcall";
end;

define C-function OleGetClipboard
  output parameter ppDataObj :: <Interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleGetClipboard", c-modifiers: "__stdcall";
end;

define C-function OleFlushClipboard
  result status :: <C-HRESULT>;
  c-name: "OleFlushClipboard", c-modifiers: "__stdcall";
end;

define C-function OleIsCurrentClipboard
  parameter pDataObj   :: <LPDATAOBJECT>;
  result status :: <C-HRESULT>;
  c-name: "OleIsCurrentClipboard", c-modifiers: "__stdcall";
end;

define C-function OleCreateMenuDescriptor
  parameter hmenuCombined :: <HMENU>;
  parameter lpMenuWidths :: <LPOLEMENUGROUPWIDTHS>;
  result value :: <HOLEMENU>;
  c-name: "OleCreateMenuDescriptor", c-modifiers: "__stdcall";
end;

define C-function OleSetMenuDescriptor
  parameter holemenu   :: <HOLEMENU>;
  parameter hwndFrame  :: <HWND>;
  parameter hwndActiveObject :: <HWND>;
  parameter lpFrame    :: <LPOLEINPLACEFRAME>;
  parameter lpActiveObj :: <LPOLEINPLACEACTIVEOBJECT>;
  result status :: <C-HRESULT>;
  c-name: "OleSetMenuDescriptor", c-modifiers: "__stdcall";
end;

define C-function OleDestroyMenuDescriptor
  parameter holemenu   :: <HOLEMENU>;
  result status :: <C-HRESULT>;
  c-name: "OleDestroyMenuDescriptor", c-modifiers: "__stdcall";
end;

define C-function OleTranslateAccelerator
  parameter lpFrame    :: <LPOLEINPLACEFRAME>;
  parameter lpFrameInfo :: <LPOLEINPLACEFRAMEINFO>;
  parameter lpmsg      :: <LPMSG>;
  result status :: <C-HRESULT>;
  c-name: "OleTranslateAccelerator", c-modifiers: "__stdcall";
end;

define C-function OleDuplicateData
  parameter hSrc       :: <HANDLE>;
  parameter cfFormat   :: <CLIPFORMAT>;
  parameter uiFlags    :: <UINT>;
  result value :: <HANDLE>;
  c-name: "OleDuplicateData", c-modifiers: "__stdcall";
end;

define C-function OleDraw
  parameter pUnknown   :: <LPUNKNOWN>;
  parameter dwAspect   :: <DWORD>;
  parameter hdcDraw    :: <HDC>;
  parameter lprcBounds :: <LPCRECT>;
  result status :: <C-HRESULT>;
  c-name: "OleDraw", c-modifiers: "__stdcall";
end;

define C-function OleRun
  parameter pUnknown   :: <LPUNKNOWN>;
  result status :: <C-HRESULT>;
  c-name: "OleRun", c-modifiers: "__stdcall";
end;

define C-function OleIsRunning
  parameter pObject    :: <LPOLEOBJECT>;
  result value :: <BOOL>;
  c-name: "OleIsRunning", c-modifiers: "__stdcall";
end;

define C-function OleLockRunning
  parameter pUnknown   :: <LPUNKNOWN>;
  parameter fLock      :: <BOOL>;
  parameter fLastUnlockCloses :: <BOOL>;
  result status :: <C-HRESULT>;
  c-name: "OleLockRunning", c-modifiers: "__stdcall";
end;

define C-function ReleaseStgMedium
  parameter lpstgmedium1 :: <LPSTGMEDIUM>;
  c-name: "ReleaseStgMedium", c-modifiers: "__stdcall";
end;

define C-function CreateOleAdviseHolder
  output parameter ppOAHolder :: <C-interface*>; // <LPOLEADVISEHOLDER>
  result status :: <C-HRESULT>;
  c-name: "CreateOleAdviseHolder", c-modifiers: "__stdcall";
end;

define C-function OleCreateDefaultHandler
  parameter clsid      :: <REFCLSID>;
  parameter pUnkOuter  :: <LPUNKNOWN>;
  parameter riid       :: <REFIID>;
  output parameter lplpObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateDefaultHandler", c-modifiers: "__stdcall";
end;

define C-function OleCreateEmbeddingHelper
  parameter clsid      :: <REFCLSID>;
  parameter pUnkOuter  :: <LPUNKNOWN>;
  parameter flags      :: <DWORD>;
  parameter pCF        :: <LPCLASSFACTORY>;
  parameter riid       :: <REFIID>;
  output parameter lplpObj :: <C-interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleCreateEmbeddingHelper", c-modifiers: "__stdcall";
end;

define C-function IsAccelerator
  parameter hAccel     :: <HACCEL>;
  parameter cAccelEntries :: <C-int>;
  parameter lpMsg      :: <LPMSG>;
  output parameter lpwCmd :: <LPWORD>;
  result value :: <BOOL>;
  c-name: "IsAccelerator", c-modifiers: "__stdcall";
end;

define C-function OleGetIconOfFile
  parameter lpszPath   :: <LPOLESTR>;
  parameter fUseFileAsLabel :: <BOOL>;
  result value :: <HGLOBAL>;
  c-name: "OleGetIconOfFile", c-modifiers: "__stdcall";
end;

define C-function OleGetIconOfClass
  parameter rclsid     :: <REFCLSID>;
  parameter lpszLabel  :: <LPOLESTR>;
  parameter fUseTypeAsLabel :: <BOOL>;
  result value :: <HGLOBAL>;
  c-name: "OleGetIconOfClass", c-modifiers: "__stdcall";
end;

define C-function OleMetafilePictFromIconAndLabel
  parameter hIcon      :: <HICON>;
  parameter lpszLabel  :: <LPOLESTR>;
  parameter lpszSourceFile :: <LPOLESTR>;
  parameter iIconIndex :: <UINT>;
  result value :: <HGLOBAL>;
  c-name: "OleMetafilePictFromIconAndLabel",
	c-modifiers: "__stdcall";
end;

define C-function OleRegGetUserType
  parameter clsid      :: <REFCLSID>;
  parameter dwFormOfType :: <DWORD>;
  output parameter pszUserType :: <LPLPOLESTR>;
  result status :: <C-HRESULT>;
  c-name: "OleRegGetUserType", c-modifiers: "__stdcall";
end;

define C-function OleRegGetMiscStatus
  parameter clsid      :: <REFCLSID>;
  parameter dwAspect   :: <DWORD>;
  output parameter pdwStatus :: <LPDWORD>;
  result status :: <C-HRESULT>;
  c-name: "OleRegGetMiscStatus", c-modifiers: "__stdcall";
end;

define C-function OleRegEnumFormatEtc
  parameter clsid      :: <REFCLSID>;
  parameter dwDirection :: <DWORD>;
  output parameter ppenum :: <Interface*>;
  result status :: <C-HRESULT>;
  c-name: "OleRegEnumFormatEtc", c-modifiers: "__stdcall";
end;

define C-function OleRegEnumVerbs
  parameter clsid      :: <REFCLSID>;
  output parameter ppenum :: <C-interface*>; // <LPENUMOLEVERB>
  result status :: <C-HRESULT>;
  c-name: "OleRegEnumVerbs", c-modifiers: "__stdcall";
end;

define C-function GetHGlobalFromILockBytes
  parameter plkbyt     :: <LPLOCKBYTES>;
  output parameter phglobal :: <LPHGLOBAL>;
  result status :: <C-HRESULT>;
  c-name: "GetHGlobalFromILockBytes", c-modifiers: "__stdcall";
end;

define C-function CreateILockBytesOnHGlobal
  parameter hGlobal    :: <HGLOBAL>;
  parameter fDeleteOnRelease :: <BOOL>;
  output parameter pplkbyt :: <Interface*>;
  result status :: <C-HRESULT>;
  c-name: "CreateILockBytesOnHGlobal", c-modifiers: "__stdcall";
end;

define C-function GetHGlobalFromStream
  parameter pstm       :: <LPSTREAM>;
  output parameter phglobal :: <LPHGLOBAL>;
  result status :: <C-HRESULT>;
  c-name: "GetHGlobalFromStream", c-modifiers: "__stdcall";
end;

define C-function CreateStreamOnHGlobal
  parameter hGlobal    :: <HGLOBAL>;
  parameter fDeleteOnRelease :: <BOOL>;
  output parameter ppstm :: <C-interface*>; // <LPSTREAM>
  result status :: <C-HRESULT>;
  c-name: "CreateStreamOnHGlobal", c-modifiers: "__stdcall";
end;

define C-function OleDoAutoConvert
  parameter pStg       :: <LPSTORAGE>;
  parameter pClsidNew  :: <LPCLSID>;
  result status :: <C-HRESULT>;
  c-name: "OleDoAutoConvert", c-modifiers: "__stdcall";
end;

define C-function OleGetAutoConvert
  parameter clsidOld   :: <REFCLSID>;
  parameter pClsidNew  :: <LPCLSID>;
  result status :: <C-HRESULT>;
  c-name: "OleGetAutoConvert", c-modifiers: "__stdcall";
end;

define C-function OleSetAutoConvert
  parameter clsidOld   :: <REFCLSID>;
  parameter clsidNew   :: <REFCLSID>;
  result status :: <C-HRESULT>;
  c-name: "OleSetAutoConvert", c-modifiers: "__stdcall";
end;

define C-function GetConvertStg
  parameter pStg       :: <LPSTORAGE>;
  result status :: <C-HRESULT>;
  c-name: "GetConvertStg", c-modifiers: "__stdcall";
end;

define C-function SetConvertStg
  parameter pStg       :: <LPSTORAGE>;
  parameter fConvert   :: <BOOL>;
  result status :: <C-HRESULT>;
  c-name: "SetConvertStg", c-modifiers: "__stdcall";
end;



