Module:    OLE
Synopsis:  FFI declarations for some some miscellaneous pieces of the
	   OLE interface that do not need special treatment. 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $Id: ole-misc.dylan,v 1.1 2004/03/12 00:09:39 cgay Exp $

define interface OLE-misc
 #include: { "ole2.h", "oleidl.h" };
 import: { 
	// structures used in member function arguments:
	LPCBORDERWIDTHS, DVTARGETDEVICE, LOGPALETTE,
	OleMenuGroupWidths, OLEMENUGROUPWIDTHS, OLEVERB, 

	// structures whose pointers are used in function arguments:
	OIFI, OLEINPLACEFRAMEINFO, OLEINPLACEFRAMEINFO,

	// structure pointers used in member function arguments:
	LPOLEINPLACEFRAMEINFO, LPOLEMENUGROUPWIDTHS, LPOLEVERB, LPFORMATETC,
	LPSTGMEDIUM, LPMSG,

	// assorted non-member functions (probably not all of these are
	//   really needed; at some time this list should be reviewed 
	//   for relevance.)
	ReadClassStg,
	WriteClassStg, ReadClassStm, WriteClassStm, WriteFmtUserTypeStg,
	ReadFmtUserTypeStg, 
	OleQueryLinkFromData, OleQueryCreateFromData, OleCreate,
	OleCreateFromData, OleCreateLinkFromData, OleCreateStaticFromData,
	OleCreateLink, OleCreateLinkToFile, OleCreateFromFile, OleLoad,
	OleSave, OleLoadFromStream, OleSaveToStream, OleSetContainedObject,
	OleNoteObjectVisible, RegisterDragDrop, RevokeDragDrop, DoDragDrop,
	OleSetClipboard, OleGetClipboard, OleFlushClipboard,
	OleIsCurrentClipboard, OleCreateMenuDescriptor,
	OleSetMenuDescriptor, OleDestroyMenuDescriptor,
	OleTranslateAccelerator, OleDuplicateData, OleDraw, OleRun,
	OleIsRunning, OleLockRunning, ReleaseStgMedium,
	CreateOleAdviseHolder, OleCreateDefaultHandler,
	OleCreateEmbeddingHelper, IsAccelerator, OleGetIconOfFile,
	OleGetIconOfClass, OleMetafilePictFromIconAndLabel,
	OleRegGetUserType, OleRegGetMiscStatus, OleRegEnumFormatEtc,
	OleRegEnumVerbs, GetHGlobalFromILockBytes,
	CreateILockBytesOnHGlobal, GetHGlobalFromStream,
	CreateStreamOnHGlobal, OleDoAutoConvert, OleGetAutoConvert,
	OleSetAutoConvert, GetConvertStg, SetConvertStg,

        // additional integer constants in "ole2.h" for user:
	OLEIVERB_*, EMBDHLP_*, E_DRAW, DATA_E_FORMATETC

	// enumeration types:
	OLEGETMONIKER, OLEWHICHMK, USERCLASSTYPE, OLEMISC, OLECLOSE,
	OLERENDER, OLEUPDATE, OLELINKBIND, BINDSPEED,

	DISCARDCACHE, OLECONTF, OLEVERBATTRIB,

	// Note: the following names are deliberately excluded because
	//   they are only used for OLE1 compatibility:
	//	OLESTREAMVTBL
	//	LPOLESTREAMVTBL
	//	OLESTREAM
	//	LPOLESTREAM
	//	OleConvertIStorageToOLESTREAM
	//	OleConvertIStorageToOLESTREAMEx
	//	OleConvertOLESTREAMToIStorage
	//	OleConvertOLESTREAMToIStorageEx

	// Other functions documented as obsolete:
	//	OleBuildVersion

	// These two are defined in "ole.dylan" in order to 
	//   correct the argument type:
	//	OleInitialize, OleUninitialize

	// This is declared in "ole2.h", but it is a duplicate
	//   of a declaration in "objbase.h", so don't include it here:
	//	CreateDataAdviseHolder
    };
end interface;
 
