module:    Dylan-user	
Synopsis:  This is a Dylan library to act as an interface to the
	   "OLE Standard Utility Library" OLESTD.LIB which is used in the
	   sample OLE programs in the Windows SDK.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */



define library OLE-std-util
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-dialog;
  use COM;
  export OLE-std-util;
end;

define module OLE-std-util
  use Dylan;
  use C-FFI;
  use Win32-common;
  use COM; // need: <REFGUID>, <LPCLSID>, <REFCLSID>, <Interface>,
	   //	<LPPERSISTFILE>, <LPMONIKER>, <FORMATETC>, <LPUNKNOWN>,
	   //	<LPOLESTR>, <LPSTORAGE>, <LPDATAOBJECT>, <LPFORMATETC>,
	   //	<LPPERSISTSTORAGE>, <CLSID>, <SCODE>, <LPSTATDATA>
  use COM-internal; // need:  <C-HRESULT>
  use Win32-dialog, import: { <LPPRINTDLG> };


  // from "olestd.h":
  export $IDS-OLESTDNOCREATEFILE, $IDS-OLESTDNOOPENFILE,
	$IDS-OLESTDDISKFULL;
  export lpVtbl-value, lpVtbl-value-setter, lpBack-value,
	lpBack-value-setter, cRef-value, cRef-value-setter, <INTERFACEIMPL>,
	<LPINTERFACEIMPL>;
  export $STGM-DFRALL, $STGM-DFALL, $STGM-SALL;
  export $OLESTDRETRYDELAY, $OLESTDCANCELRETRY;
  export $DD-DEFSCROLLINTERVAL, $DD-DEFDRAGDELAY, $DD-DEFDRAGMINDIST;
  export $OLESTDDROP-NONE, $OLESTDDROP-DEFAULT,
	$OLESTDDROP-NONDEFAULT;
  export $HIMETRIC-PER-INCH, $PTS-PER-INCH;
  export SetDCToAnisotropic, SetDCToDrawInHimetricRect, ResetOrigDC,
	XformRectInPixelsToHimetric, XformRectInHimetricToPixels,
	XformSizeInPixelsToHimetric, XformSizeInHimetricToPixels,
	XformWidthInHimetricToPixels, XformWidthInPixelsToHimetric,
	XformHeightInHimetricToPixels, XformHeightInPixelsToHimetric,
	ParseCmdLine, OleStdIsOleLink, OleStdQueryInterface,
	OleStdCreateRootStorage, OleStdOpenRootStorage,
	OleStdOpenOrCreateRootStorage, OleStdCreateChildStorage,
	OleStdOpenChildStorage, OleStdCommitStorage,
	OleStdDestroyAllElements, OleStdCreateStorageOnHGlobal,
	OleStdCreateTempStorage, OleStdDoConvert,
	OleStdGetTreatAsFmtUserType, OleStdDoTreatAsClass,
	OleStdSetupAdvises, OleStdSwitchDisplayAspect, OleStdSetIconInCache,
	OleStdGetData, OleStdMarkPasteEntryList,
	OleStdGetPriorityClipboardFormat, OleStdIsDuplicateFormat,
	OleStdRegisterAsRunning, OleStdRevokeAsRunning,
	OleStdNoteFileChangeTime, OleStdNoteObjectChangeTime,
	OleStdGetOleObjectData, OleStdGetLinkSourceData,
	OleStdGetObjectDescriptorData,
	OleStdGetObjectDescriptorDataFromOleObject,
	OleStdFillObjectDescriptorFromData,
	OleStdGetMetafilePictFromOleObject, OleStdCreateTempFileMoniker,
	OleStdGetFirstMoniker, OleStdGetLenFilePrefixOfMoniker,
	OleStdMkParseDisplayName, OleStdMalloc, OleStdRealloc, OleStdFree,
	OleStdGetSize, OleStdFreeString, OleStdCopyString,
	OleStdGetItemToken, OleStdIconLabelTextOut, OleStdGetAuxUserType,
	OleStdGetUserTypeOfClass, OleStdGetMiscStatusOfClass,
	OleStdGetDefaultFileFormatOfClass, OleStdInitVtbl, OleStdCheckVtbl,
	OleStdVerifyRelease, OleStdRelease, OleStdCreateDC, OleStdCreateIC,
	OleStdCreateTargetDevice, OleStdDeleteTargetDevice,
	OleStdCopyTargetDevice, OleStdCopyFormatEtc, OleDbgPrint,
	OleDbgPrintAlways, OleDbgSetDbgLevel, OleDbgGetDbgLevel,
	OleDbgIndent, OleDbgPrintRefCnt, OleDbgPrintRefCntAlways,
	OleDbgPrintRect, OleDbgPrintRectAlways, OleDbgPrintScodeAlways,
	OleStdEnumFmtEtc-Create, OleStdEnumStatData-Create,
	OleStdCopyStatData, OleStdCreateStandardPalette,
	$UPDATELINKS-STARTDELAY, OleStdInitialize, OleStdUninitialize,
	OleUICanUnloadNow, $IDD-FILEOPEN, $IDS-OLE2UIUNKNOWN,
	$IDS-OLE2UILINK, $IDS-OLE2UIOBJECT, $IDS-OLE2UIEDIT,
	$IDS-OLE2UICONVERT, $IDS-OLE2UIEDITLINKCMD-1VERB,
	$IDS-OLE2UIEDITOBJECTCMD-1VERB, $IDS-OLE2UIEDITLINKCMD-NVERB,
	$IDS-OLE2UIEDITOBJECTCMD-NVERB, $IDS-OLE2UIEDITNOOBJCMD,
	$IDS-DEFICONLABEL, $IDS-OLE2UIPASTELINKEDTYPE;
  export $IDS-FILTERS, $IDS-ICONFILTERS, $IDS-BROWSE, $IDB-RESULTSEGA,
	$IDB-RESULTSVGA, $IDB-RESULTSHIRESVGA;
  export <LPFNOLEUIHOOK>;
  export $ID-OLEUIHELP, $ID-STATIC, $OLEUI-CCHKEYMAX,
	$OLEUI-CCHVERBMAX, $OLEUI-OBJECTMENUMAX, $OLEUI-CCHPATHMAX,
	$OLEUI-CCHFILEMAX, $OLEUI-CCHLABELMAX, $OLEUI-CCHCLSIDSTRING;
  export OleUIMetafilePictIconFree, OleUIMetafilePictIconDraw,
	OleUIMetafilePictExtractLabel, OleUIMetafilePictExtractIcon,
	OleUIMetafilePictExtractIconSource, $ID-IO-CREATENEW,
	$ID-IO-CREATEFROMFILE, $ID-IO-LINKFILE, $ID-IO-OBJECTTYPELIST,
	$ID-IO-DISPLAYASICON, $ID-IO-CHANGEICON, $ID-IO-FILE,
	$ID-IO-FILEDISPLAY, $ID-IO-RESULTIMAGE, $ID-IO-RESULTTEXT,
	$ID-IO-ICONDISPLAY, $ID-IO-OBJECTTYPETEXT, $ID-IO-FILETEXT,
	$ID-IO-FILETYPE, $IDS-IORESULTNEW, $IDS-IORESULTNEWICON,
	$IDS-IORESULTFROMFILE1, $IDS-IORESULTFROMFILE2,
	$IDS-IORESULTFROMFILEICON2, $IDS-IORESULTLINKFILE1,
	$IDS-IORESULTLINKFILE2, $IDS-IORESULTLINKFILEICON1,
	$IDS-IORESULTLINKFILEICON2, $ID-PS-PASTE, $ID-PS-PASTELINK,
	$ID-PS-SOURCETEXT, $ID-PS-PASTELIST, $ID-PS-PASTELINKLIST,
	$ID-PS-DISPLAYLIST, $ID-PS-DISPLAYASICON, $ID-PS-ICONDISPLAY,
	$ID-PS-CHANGEICON, $ID-PS-RESULTIMAGE, $ID-PS-RESULTTEXT,
	$ID-PS-RESULTGROUP, $ID-PS-STXSOURCE, $ID-PS-STXAS, $IDS-PSPASTEDATA,
	$IDS-PSPASTEOBJECT, $IDS-PSPASTEOBJECTASICON, $IDS-PSPASTELINKDATA,
	$IDS-PSPASTELINKOBJECT, $IDS-PSPASTELINKOBJECTASICON, $IDS-PSNONOLE,
	$IDS-PSUNKNOWNTYPE, $IDS-PSUNKNOWNSRC, $IDS-PSUNKNOWNAPP;
  export $ID-EL-CHANGESOURCE, $ID-EL-AUTOMATIC, $ID-EL-CLOSE,
	$ID-EL-CANCELLINK, $ID-EL-UPDATENOW, $ID-EL-OPENSOURCE,
	$ID-EL-MANUAL, $ID-EL-LINKSOURCE, $ID-EL-LINKTYPE, $ID-EL-UPDATE,
	$ID-EL-NULL, $ID-EL-LINKSLISTBOX, $ID-EL-COL1, $ID-EL-COL2,
	$ID-EL-COL3;
  export $ID-GROUP, $ID-CURRENT, $ID-CURRENTICON, $ID-DEFAULT,
	$ID-DEFAULTICON, $ID-FROMFILE, $ID-FROMFILEEDIT, $ID-ICONLIST,
	$ID-LABEL, $ID-LABELEDIT, $ID-BROWSE, $ID-RESULTICON,
	$ID-RESULTLABEL, $IDS-CINOICONSINFILE, $IDS-CIINVALIDFILE,
	$IDS-CIFILEACCESS, $IDS-CIFILESHARE, $IDS-CIFILEOPENFAIL;
  export $IDCV-OBJECTTYPE, $IDCV-DISPLAYASICON, $IDCV-CHANGEICON,
	$IDCV-ACTIVATELIST, $IDCV-CONVERTTO, $IDCV-ACTIVATEAS,
	$IDCV-RESULTTEXT, $IDCV-CONVERTLIST, $IDCV-ICON, $IDCV-ICONLABEL1,
	$IDCV-ICONLABEL2, $IDCV-STXCURTYPE, $IDCV-GRPRESULT,
	$IDCV-STXCONVERTTO, $IDS-CVRESULTCONVERTLINK, $IDS-CVRESULTCONVERTTO,
	$IDS-CVRESULTNOCHANGE, $IDS-CVRESULTDISPLAYASICON,
	$IDS-CVRESULTACTIVATEAS, $IDS-CVRESULTACTIVATEDIFF;
  export $IDBZ-RETRY, $IDBZ-ICON, $IDBZ-MESSAGE1, $IDBZ-SWITCHTO,
	$IDS-BZRESULTTEXTBUSY, $IDS-BZRESULTTEXTNOTRESPONDING,
	$IDS-LINK-AUTO, $IDS-LINK-MANUAL, $IDS-LINK-UNKNOWN, $IDS-LINKS,
	$IDS-FAILED, $IDS-CHANGESOURCE, $IDS-INVALIDSOURCE,
	$IDS-ERR-GETLINKSOURCE, $IDS-ERR-GETLINKUPDATEOPTIONS,
	$IDS-ERR-ADDSTRING, $IDS-CHANGEADDITIONALLINKS, $IDS-CLOSE;
  export $ID-PU-LINKS, $ID-PU-TEXT, $ID-PU-CONVERT, $ID-PU-BROWSE,
	$ID-PU-METER, $ID-PU-PERCENT, $ID-PU-STOP, $ID-DUMMY,
	$OLEUI-HANDLES-USEINVERSE, $OLEUI-HANDLES-NOBORDER,
	$OLEUI-HANDLES-INSIDE, $OLEUI-HANDLES-OUTSIDE;
  export $OLEUI-SHADE-FULLRECT, $OLEUI-SHADE-BORDERIN,
	$OLEUI-SHADE-BORDEROUT, OleUIDrawHandles, OleUIDrawShading,
	OleUIShowObject, $DEFAULT-HATCHBORDER-WIDTH,
	RegisterHatchWindowClass, CreateHatchWindow, GetHatchWidth,
	GetHatchRect, SetHatchRect, SetHatchWindowSize, $OLEUI-VERSION-MAGIC;

  // in "misc.dylan":
  export OLEDBGDATA-MAIN;
end;
