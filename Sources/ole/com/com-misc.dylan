Module:    COM
Synopsis:  FFI declarations for some miscellaneous pieces of the
	   COM interface that do not need special treatment. 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface COM-misc
 #include: { "objbase.h", "wtypes.h", "unknwn.h", "objidl.h" };
 import: { 
	// structures used in member function arguments:
	RPCOLEMESSAGE, BIND_OPTS, STATSTG, STATDATA,
	FORMATETC, STGMEDIUM, INTERFACEINFO, CLASSDETAIL, StorageLayout,
	MULTI_QI,
	// structure pointers used in member function arguments:
	LPINTERFACEINFO, LPCLSID, LPIID, LPCLASSDETAIL, LPStorageLayout,
	LPMULTI_QI,
	// structure needed for initialization:
	GUID,

	// types needed:
	REFGUID, REFIID, REFCLSID, REFFMTID,

	// types needed internally:
	CLIPFORMAT, RPCOLEDATAREP, IID, CLSID, DVTARGETDEVICE, HMETAFILEPICT,
	FMTID, LPFMTID, uSTGMEDIUM,

	// structure pointer types not used internally but otherwise unnamed:
	LPBIND_OPTS, 

	// types needed by OLE although not used in COM itself:
	LPFORMATETC, LPSTGMEDIUM,

	// types needed by the OLE2UI library:
	LPSTATDATA, 

	// assorted non-member functions (probably not all of these are
	//   really needed; at some time this list should be reviewed 
	//   for relevance.)
	CoInitialize, CoUninitialize, CoGetMalloc,
	CoGetCurrentProcess, CoGetClassObject, CoRegisterClassObject,
	CoRevokeClassObject, CoGetMarshalSizeMax, CoMarshalInterface,
	CoUnmarshalInterface, CoMarshalHresult, CoUnmarshalHresult,
	CoReleaseMarshalData, CoDisconnectObject, CoLockObjectExternal,
	CoGetStandardMarshal, CoIsHandlerConnected,
	CoLoadLibrary, CoFreeLibrary,
	CoFreeAllLibraries, CoFreeUnusedLibraries, CoCreateInstance,
	StringFromCLSID, CLSIDFromString, StringFromIID, IIDFromString,
	CoIsOle1Class, ProgIDFromCLSID, CLSIDFromProgID, StringFromGUID2,
	CoCreateGuid, CoFileTimeToDosDateTime, CoDosDateTimeToFileTime,
	CoFileTimeNow, CoRegisterMessageFilter, CoGetTreatAsClass,
	CoTreatAsClass, CoTaskMemAlloc,
	CoTaskMemRealloc, CoTaskMemFree, CreateDataAdviseHolder,
	CreateDataCache, StgCreateDocfile, StgCreateDocfileOnILockBytes,
	StgOpenStorage, StgOpenStorageOnILockBytes, StgIsStorageFile,
	StgIsStorageILockBytes, StgSetTimes, BindMoniker,
	MkParseDisplayName, MonikerRelativePathTo, MonikerCommonPrefixWith,
	CreateBindCtx, CreateGenericComposite, GetClassFile,
	CreateFileMoniker, CreateItemMoniker, CreateAntiMoniker,
	CreatePointerMoniker, GetRunningObjectTable,
	CoMarshalInterThreadInterfaceInStream,
	CoGetInterfaceAndReleaseStream, CoCreateFreeThreadedMarshaler,

     //  obsolete functions deliberately excluded:
     //		CoBuildVersion
     //		CoCreateStandardMalloc, CoHasStrongExternalConnections

	// internal functions, not user-callable:
	//	DllGetClassObject, DllCanUnloadNow

	// These are used in OLE, but they shouldn't be defined
	// here because they duplicate what is in the `win32-user' library:
	//	MSG, LPMSG,

	// enumerations whose elements are used as function arguments:
	REGCLS, MEMCTX, CLSCTX, MSHLFLAGS, MSHCTX, DVASPECT, STGC, STGMOVE,
	STATFLAG, EXTCONN, BIND_FLAGS, MKSYS, MKRREDUCE, STGTY,
	STREAM_SEEK, LOCKTYPE, ADVF, TYMED, DATADIR, CALLTYPE, SERVERCALL,
	PENDINGTYPE, PENDINGMSG,

        // more types new in 1997 header files:
	LPPROPID, LPPROPSPEC, LPPUBLISHEDINFOLIST,
	LPSTATPROPSETSTG, LPSTATPROPSTG,
	PROPID, PROPSPEC, PUBLISHEDINFOLIST,
	STATPROPSETSTG, STATPROPSTG, QUERYCONTEXT,
	LPPACKAGEDETAIL LPPACKAGEINFO PACKAGEDETAIL PACKAGEINFO,
	CSPLATFORM, LPAPPDETAIL, LPPUBLISHEDAPPINFO,
	APPDETAIL, PUBLISHEDAPPINFO,
	uCLSSPEC, PACKAGEINFOLIST, CLASSPATHTYPE,

	// new functions in 1997 header files:
	CoGetPSClsid, CoRegisterPSCLsid,

	// only used for IPropertyStorage, not yet fully supported:
	// PROPVARIANT, LPPROPVARIANT, BLOB, LPBLOB, CLIPDATA, LPCLIPDATA,
 
    // additional integer constants for user:
    STGM_*, CLSCTX_*
    };
  exclude: { BIND_OPTS2,
	    FLAG_STGMEDIUM // used only in stubs
	      };
end interface;
 
/*
Note: the following functions are not included because they are not
      supported in Windows 95:

 CoAddRefServerProcess
 CoCopyProxy
 CoCreateInstanceEx
 CoGetCallContext
 CoGetInstanceFromFile
 CoGetInstanceFromIStorage
 CoGetObject
 CoImpersonateClient
 CoInitializeEx
 CoInitializeSecurity
 CoQueryAuthenticationServices
 CoQueryClientBlanket
 CoQueryProxyBlanket
 CoRegisterChannelHook
 CoRegisterPSClsid
 CoRegisterSurrogate
 CoReleaseServerProcess
 CoResumeClassObjects
 CoRevertToSelf
 CoSetProxyBlanket
 CoSuspendClassObjects
 CoSwitchCallContext
 CreateClassMoniker
 StgGetIFillLockBytesOnFile
 StgGetIFillLockBytesOnILockBytes
 StgOpenAsyncDocfileOnIFillLockBytes
*/
