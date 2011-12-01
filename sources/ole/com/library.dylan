module:    Dylan-user	
Synopsis:  This is a Dylan library to act as an interface to the Microsoft
	   ``Component Object Model'' (32-bit API) which is the
	   foundation for OLE.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library COM
  use dylan;
  use common-dylan;
  use C-FFI;
  use Win32-common;
  use Win32-kernel;
  export COM, COM-internal;
end;

define module COM-internal
  // The following exports would probably not be used by application
  // programmers, but are needed in the OLE or OLE-Automation library:
  create <C-COM-vtbl>;
  create vtbl, vtbl-setter;
  create c-type-macro, c-pointer-type-macro, dylan-type-macro;
end;

define module COM
  use common-dylan;
  use streams-protocol;
  use Dylan-extensions, import: {<byte>,
				 <byte-character>,
				 element-type => collection-element-type
				};
  use C-FFI, export: { null-pointer, null-pointer?, pointer-address,
			pointer-value, pointer-value-setter,
			<C-pointer>, <C-string>, <C-unicode-string>,
		        <C-void*>, pointer-cast, c-type-cast,
			destroy, with-stack-structure, with-c-string,
		        size-of, <machine-word>, <ffi-integer>
			 };
  use dylan-direct-c-ffi, import: { primitive-runtime-module-handle };
  use machine-words;
  use Win32-common,
    export: {  <LPVOID>, <SIZEL>, <HWND>, <HTASK>,
	       <LARGE-INTEGER>, <ULARGE-INTEGER>,
	       <PLARGE-INTEGER>, <PULARGE-INTEGER>,
	       $NULL-string, <C-void**>, <LPLPVOID>, <LPULONG>,
	       cbSize-value, cbSize-value-setter,
	       Buffer-value, Buffer-value-setter,
	       cb-value, cb-value-setter, u-value, u-value-setter };
  use Win32-kernel,
    export: { <FILETIME>, <LPFILETIME>,
	      dwLowDateTime-value, dwHighDateTime-value,
	      dwLowDateTime-value-setter, dwHighDateTime-value-setter,
	      cBytes-value, cBytes-value-setter,
	      dwPlatformId-value, dwPlatformId-value-setter };
  use COM-internal;

  // This module includes the following interfaces:
  export <IUnknown>, $IID-IUnknown, QueryInterface, AddRef, Release,
	IUnknown/QueryInterface, IUnknown/AddRef, IUnknown/Release;
  export <IClassFactory>, $IID-IClassFactory,
		IClassFactory/CreateInstance, IClassFactory/LockServer;
  export <IMarshal>, $IID-IMarshal, IMarshal/GetUnmarshalClass,
		IMarshal/GetMarshalSizeMax, IMarshal/MarshalInterface,
		IMarshal/UnmarshalInterface, IMarshal/ReleaseMarshalData,
		IMarshal/DisconnectObject;
  export <IMalloc>, $IID-IMalloc, IMalloc/Alloc, IMalloc/Realloc,
		IMalloc/Free, IMalloc/GetSize, IMalloc/DidAlloc,
		IMalloc/HeapMinimize;
  export <IMallocSpy>, $IID-IMallocSpy, IMallocSpy/PreAlloc,
		IMallocSpy/PostAlloc, IMallocSpy/PreFree, IMallocSpy/PostFree,
		IMallocSpy/PreRealloc, IMallocSpy/PostRealloc,
		IMallocSpy/PreGetSize, IMallocSpy/PostGetSize,
		IMallocSpy/PreDidAlloc, IMallocSpy/PostDidAlloc,
		IMallocSpy/PreHeapMinimize, IMallocSpy/PostHeapMinimize;
  export <IStdMarshalInfo>, $IID-IStdMarshalInfo,
		IStdMarshalInfo/GetClassForHandler;
  export <IExternalConnection>, $IID-IExternalConnection,
		IExternalConnection/AddConnection,
		IExternalConnection/ReleaseConnection;
  export $IID-IMultiQI, IMultiQI/QueryMultipleInterfaces;
  export <IEnumUnknown>, $IID-IEnumUnknown, IEnumUnknown/Next,
		IEnumUnknown/Skip, IEnumUnknown/Reset, IEnumUnknown/Clone;
  export <IBindCtx>, $IID-IBindCtx, IBindCtx/RegisterObjectBound,
		IBindCtx/RevokeObjectBound, IBindCtx/ReleaseBoundObjects,
		IBindCtx/SetBindOptions, IBindCtx/GetBindOptions,
		IBindCtx/GetRunningObjectTable, IBindCtx/RegisterObjectParam,
		IBindCtx/GetObjectParam, IBindCtx/EnumObjectParam,
		IBindCtx/RevokeObjectParam;
  export <IEnumMoniker>, $IID-IEnumMoniker, IEnumMoniker/Next,
		IEnumMoniker/Skip, IEnumMoniker/Reset, IEnumMoniker/Clone;
  export <IRunnableObject>, $IID-IRunnableObject,
		IRunnableObject/GetRunningClass, IRunnableObject/Run,
		IRunnableObject/IsRunning, IRunnableObject/LockRunning,
		IRunnableObject/SetContainedObject;
  export $IID-IRunningObjectTable, IRunningObjectTable/Register,
		IRunningObjectTable/Revoke, IRunningObjectTable/IsRunning,
		IRunningObjectTable/GetObject,
		IRunningObjectTable/NoteChangeTime,
		IRunningObjectTable/GetTimeOfLastChange,
		IRunningObjectTable/EnumRunning;
  export <IPersist>, $IID-IPersist, IPersist/GetClassID;
  export <IPersistStream>, $IID-IPersistStream,
		IPersistStream/IsDirty, IPersistStream/Load,
		IPersistStream/Save, IPersistStream/GetSizeMax;
  export <IMoniker>, $IID-IMoniker, IMoniker/BindToObject,
		IMoniker/BindToStorage, IMoniker/Reduce, IMoniker/ComposeWith,
		IMoniker/Enum, IMoniker/IsEqual, IMoniker/Hash,
		IMoniker/IsRunning, IMoniker/GetTimeOfLastChange,
		IMoniker/Inverse, IMoniker/CommonPrefixWith,
		IMoniker/RelativePathTo, IMoniker/GetDisplayName,
		IMoniker/ParseDisplayName, IMoniker/IsSystemMoniker;
  export <IROTData>, $IID-IROTData, IROTData/GetComparisonData;
  export <IEnumString>, $IID-IEnumString, IEnumString/Next,
		IEnumString/Skip, IEnumString/Reset, IEnumString/Clone;
  export <ISequentialStream>, $IID-ISequentialStream,
		ISequentialStream/Read, ISequentialStream/Write;
  export <IStream>, $IID-IStream, IStream/Seek, IStream/SetSize,
		IStream/CopyTo, IStream/Commit, IStream/Revert,
		IStream/LockRegion, IStream/UnlockRegion, IStream/Stat,
		IStream/Clone;
  export <IEnumSTATSTG>, $IID-IEnumSTATSTG, IEnumSTATSTG/Next,
		IEnumSTATSTG/Skip, IEnumSTATSTG/Reset, IEnumSTATSTG/Clone;
  export <IStorage>, $IID-IStorage, IStorage/CreateStream,
		IStorage/OpenStream, IStorage/CreateStorage,
		IStorage/OpenStorage, IStorage/CopyTo, IStorage/MoveElementTo,
		IStorage/Commit, IStorage/Revert, IStorage/EnumElements,
		IStorage/DestroyElement, IStorage/RenameElement,
		IStorage/SetElementTimes, IStorage/SetClass,
		IStorage/SetStateBits, IStorage/Stat;
  export <IPersistFile>, $IID-IPersistFile, IPersistFile/IsDirty,
		IPersistFile/Load, IPersistFile/Save,
		IPersistFile/SaveCompleted, IPersistFile/GetCurFile;
  export <IPersistStorage>, $IID-IPersistStorage,
		IPersistStorage/IsDirty, IPersistStorage/InitNew,
		IPersistStorage/Load, IPersistStorage/Save,
		IPersistStorage/SaveCompleted, IPersistStorage/HandsOffStorage;
  export <ILockBytes>, $IID-ILockBytes, ILockBytes/ReadAt,
		ILockBytes/WriteAt, ILockBytes/Flush, ILockBytes/SetSize,
		ILockBytes/LockRegion, ILockBytes/UnlockRegion,
		ILockBytes/Stat;
  export <IEnumFORMATETC>, $IID-IEnumFORMATETC,
		IEnumFORMATETC/Next, IEnumFORMATETC/Skip, IEnumFORMATETC/Reset,
		IEnumFORMATETC/Clone;
  export $IID-IEnumSTATDATA, IEnumSTATDATA/Next,
		IEnumSTATDATA/Skip, IEnumSTATDATA/Reset, IEnumSTATDATA/Clone;
  export <IRootStorage>, $IID-IRootStorage,
		IRootStorage/SwitchToFile;
  export <IAdviseSink>, $IID-IAdviseSink,
		IAdviseSink/OnDataChange, IAdviseSink/OnViewChange,
		IAdviseSink/OnRename, IAdviseSink/OnSave, IAdviseSink/OnClose;
  export <IAdviseSink2>, $IID-IAdviseSink2,
		IAdviseSink2/OnLinkSrcChange;
  export <IDataObject>, $IID-IDataObject, IDataObject/GetData,
		IDataObject/GetDataHere, IDataObject/QueryGetData,
		IDataObject/GetCanonicalFormatEtc, IDataObject/SetData,
		IDataObject/EnumFormatEtc, IDataObject/DAdvise,
		IDataObject/DUnadvise, IDataObject/EnumDAdvise;
  export <IDataAdviseHolder>, $IID-IDataAdviseHolder,
		IDataAdviseHolder/Advise, IDataAdviseHolder/Unadvise,
		IDataAdviseHolder/EnumAdvise,
		IDataAdviseHolder/SendOnDataChange;
  export <IMessageFilter>, $IID-IMessageFilter,
		IMessageFilter/HandleInComingCall,
		IMessageFilter/RetryRejectedCall,
		IMessageFilter/MessagePending;
  export $IID-IRpcChannelBuffer, IRpcChannelBuffer/GetBuffer,
		IRpcChannelBuffer/SendReceive, IRpcChannelBuffer/FreeBuffer,
		IRpcChannelBuffer/GetDestCtx, IRpcChannelBuffer/IsConnected;
  export <IRpcProxyBuffer>, $IID-IRpcProxyBuffer,
		IRpcProxyBuffer/Connect, IRpcProxyBuffer/Disconnect;
  export <IRpcStubBuffer>, $IID-IRpcStubBuffer,
		IRpcStubBuffer/Connect, IRpcStubBuffer/Disconnect,
		IRpcStubBuffer/Invoke, IRpcStubBuffer/IsIIDSupported,
		IRpcStubBuffer/CountRefs,
		IRpcStubBuffer/DebugServerQueryInterface,
		IRpcStubBuffer/DebugServerRelease;
  export <IPSFactoryBuffer>, $IID-IPSFactoryBuffer,
		IPSFactoryBuffer/CreateProxy, IPSFactoryBuffer/CreateStub;
  export <IPropertyStorage>, $IID-IPropertyStorage,
		IPropertyStorage/ReadMultiple, IPropertyStorage/WriteMultiple,
		IPropertyStorage/DeleteMultiple,
		IPropertyStorage/ReadPropertyNames,
		IPropertyStorage/WritePropertyNames,
		IPropertyStorage/DeletePropertyNames, IPropertyStorage/Commit,
		IPropertyStorage/Revert, IPropertyStorage/Enum,
		IPropertyStorage/SetTimes, IPropertyStorage/SetClass,
		IPropertyStorage/Stat;
  export <IPropertySetStorage>, $IID-IPropertySetStorage,
		IPropertySetStorage/Create, IPropertySetStorage/Open,
		IPropertySetStorage/Delete, IPropertySetStorage/Enum;
  export <IEnumSTATPROPSTG>, $IID-IEnumSTATPROPSTG,
		IEnumSTATPROPSTG/Next, IEnumSTATPROPSTG/Skip,
		IEnumSTATPROPSTG/Reset, IEnumSTATPROPSTG/Clone;
  export <IEnumSTATPROPSETSTG>, $IID-IEnumSTATPROPSETSTG,
		IEnumSTATPROPSETSTG/Next, IEnumSTATPROPSETSTG/Skip,
		IEnumSTATPROPSETSTG/Reset, IEnumSTATPROPSETSTG/Clone;
  export $IID-ILayoutStorage, ILayoutStorage/LayoutScript,
		ILayoutStorage/BeginMonitor, ILayoutStorage/EndMonitor,
		ILayoutStorage/ReLayoutDocfile,
		ILayoutStorage/ReLayoutDocfileOnILockBytes;
  export $IID-IDirectWriterLock,
		IDirectWriterLock/WaitForWriteAccess,
		IDirectWriterLock/ReleaseWriteAccess,
		IDirectWriterLock/HaveWriteAccess;
  export $IID-ICancelMethodCalls, ICancelMethodCalls/Cancel,
		ICancelMethodCalls/TestCancel;

  // Defined in "com.dylan" or "after.dylan":
  export <SCODE>, <HRESULT>, SCODE-CODE, SCODE-FACILITY, SCODE-SEVERITY,
	MAKE-SCODE, SUCCEEDED?, FAILED?, $NOERROR,
	HRESULT-FROM-WIN32, HRESULT-FROM-NT;
  export <C-interface>, <C-interface*>, <Dylan-interface>;
  export <C-HRESULT>, <C-HRESULT*>;
  export <REFGUID>, make-GUID, <REFIID>, IsEqualGUID?, IsEqualIID?, $IID-NULL;
  export <CLSID>, <REFCLSID>, IsEqualCLSID?, $CLSID-NULL;
  export COM-interface-definer;
  export <Interface>, <Interface*>, $NULL-interface, null?, add-interface,
	controlling-unknown, terminate, dylan-interface,
        <mapped-interface>;
  export SubRef;
  export $NULL-OLESTR, <SNB>, <LPOLESTR>, <LPCOLESTR>, OLESTR;
  export <LPCLIPFORMAT>, <LPLPOLESTR>, <LPPROPID>;
  export %memcpy; // temporary for compatibility
  export IStream/Read, IStream/Write;
  export <PROPVARIANT>, <LPPROPVARIANT>;

  export $null-vtable, <IUnknown>-vstruct;

  // misc. stuff from "com-misc.dylan":

  // from "objbase.h":
  export $CLSCTX-INPROC, $CLSCTX-ALL, $CLSCTX-SERVER;
  export $REGCLS-SINGLEUSE, $REGCLS-MULTIPLEUSE,
	$REGCLS-MULTI-SEPARATE, $REGCLS-SUSPENDED, $REGCLS-SURROGATE,
	$STGM-DIRECT, $STGM-TRANSACTED, $STGM-SIMPLE, $STGM-READ,
	$STGM-WRITE, $STGM-READWRITE, $STGM-SHARE-DENY-NONE,
	$STGM-SHARE-DENY-READ, $STGM-SHARE-DENY-WRITE, $STGM-SHARE-EXCLUSIVE,
	$STGM-PRIORITY, $STGM-DELETEONRELEASE, $STGM-NOSCRATCH, $STGM-CREATE,
	$STGM-CONVERT, $STGM-FAILIFTHERE, $STGM-NOSNAPSHOT;

  // from "wtypes.h":
  export Data1-value, Data1-value-setter, Data2-value,
	Data2-value-setter, Data3-value, Data3-value-setter, Data4-array,
	Data4-array-setter, Data4-value, <GUID>, <LPGUID>, <IID>;
  export <LPIID>, <CLSID>;
  export <LPCLSID>, <FMTID>;
  export <LPFMTID>, <REFGUID>, <REFIID>, <REFCLSID>, <REFFMTID>,
	$MEMCTX-TASK, $MEMCTX-SHARED, $MEMCTX-MACSYSTEM, $MEMCTX-UNKNOWN,
	$MEMCTX-SAME, $CLSCTX-INPROC-SERVER, $CLSCTX-INPROC-HANDLER,
	$CLSCTX-LOCAL-SERVER, $CLSCTX-INPROC-SERVER16, $CLSCTX-REMOTE-SERVER,
	$CLSCTX-INPROC-HANDLER16, $CLSCTX-INPROC-SERVERX86,
	$CLSCTX-INPROC-HANDLERX86, $CLSCTX-ESERVER-HANDLER;
  export $MSHLFLAGS-NORMAL, $MSHLFLAGS-TABLESTRONG,
	$MSHLFLAGS-TABLEWEAK, $MSHLFLAGS-NOPING;
  export $MSHCTX-LOCAL, $MSHCTX-NOSHAREDMEM, $MSHCTX-DIFFERENTMACHINE,
	$MSHCTX-INPROC;
  export $DVASPECT-CONTENT, $DVASPECT-THUMBNAIL, $DVASPECT-ICON,
	$DVASPECT-DOCPRINT;
  export $STGC-DEFAULT, $STGC-OVERWRITE, $STGC-ONLYIFCURRENT,
	$STGC-DANGEROUSLYCOMMITMERELYTODISKCACHE, $STGC-CONSOLIDATE;
  export $STGMOVE-MOVE, $STGMOVE-COPY, $STGMOVE-SHALLOWCOPY;
  export $STATFLAG-DEFAULT, $STATFLAG-NONAME, $STATFLAG-NOOPEN;
  export <CLIPFORMAT>;
  export <HMETAFILEPICT>;
  export <PROPID>;
  export dwVersionHi-value, dwVersionHi-value-setter,
	dwVersionLo-value, dwVersionLo-value-setter, dwProcessorArch-value,
	dwProcessorArch-value-setter, <CSPLATFORM>, <LPCSPLATFORM>;
  export dwContext-value, dwContext-value-setter, Platform-value,
	Platform-value-setter, Locale-value, Locale-value-setter,
	dwVersionHi-value, dwVersionHi-value-setter, dwVersionLo-value,
	dwVersionLo-value-setter, <QUERYCONTEXT>, <LPQUERYCONTEXT>;
  export tyspec-value, tyspec-value-setter, clsid-value,
	clsid-value-setter, iid-value, iid-value-setter, typelibID-value,
	typelibID-value-setter, pFileExt-value, pFileExt-value-setter,
	pMimeType-value, pMimeType-value-setter, pProgId-value,
	pProgId-value-setter, pFileName-value, pFileName-value-setter,
	pJavaClassName-value, pJavaClassName-value-setter,
	pPackageName-value, pPackageName-value-setter, tagged-union-value,
	tagged-union-value-setter, <uCLSSPEC>, <LPuCLSSPEC>;
  export pwszFileExtension-value, pwszFileExtension-value-setter,
	pwszDisplayName-value, pwszDisplayName-value-setter,
	pwszPackagePath-value, pwszPackagePath-value-setter,
	<PUBLISHEDAPPINFO>, <LPPUBLISHEDAPPINFO>;
  export $ExeNamePath, $DllNamePath, $TlbNamePath, $CabFilePath,
	$InfFilePath, $DrwFilePath, $SetupNamePath;
  export AppID-value, AppID-value-setter, cClasses-value,
	cClasses-value-setter, prgClsIdList-value, prgClsIdList-value-setter,
	cTypeLibIds-value, cTypeLibIds-value-setter, prgTypeLibIdList-value,
	prgTypeLibIdList-value-setter, cServers-value, cServers-value-setter,
	prgServerNames-value, prgServerNames-value-setter, <APPDETAIL>,
	<LPAPPDETAIL>;
  export PathType-value, PathType-value-setter, pszPath-value,
	pszPath-value-setter, pszIconPath-value, pszIconPath-value-setter,
	pszSetupCommand-value, pszSetupCommand-value-setter,
	dwActFlags-value, dwActFlags-value-setter, pszVendor-value,
	pszVendor-value-setter, pszPackageName-value,
	pszPackageName-value-setter, pszProductName-value,
	pszProductName-value-setter, dwContext-value, dwContext-value-setter,
	Platform-value, Platform-value-setter, Locale-value,
	Locale-value-setter, dwVersionHi-value, dwVersionHi-value-setter,
	dwVersionLo-value, dwVersionLo-value-setter, Usn-value,
	Usn-value-setter, cApps-value, cApps-value-setter, pAppDetail-value,
	pAppDetail-value-setter, <PACKAGEDETAIL>, <LPPACKAGEDETAIL>;
  export pszClassIconPath-value, pszClassIconPath-value-setter,
	pTreatAsClsid-value, pTreatAsClsid-value-setter, cPackages-value,
	cPackages-value-setter, pPackageDetail-value,
	pPackageDetail-value-setter, <PACKAGEINFO>, <LPPACKAGEINFO>;

  // from "unknwn.h":
  export <LPUNKNOWN>;
  export <LPCLASSFACTORY>;

  // from "objidl.h":
  export <LPMARSHAL>;
  export <LPMALLOC>;
  export <LPMALLOCSPY>;
  export <LPSTDMARSHALINFO>;
  export <LPEXTERNALCONNECTION>;
  export $EXTCONN-STRONG, $EXTCONN-WEAK, $EXTCONN-CALLABLE;
  export <LPMULTIQI>;
  export pIID-value, pIID-value-setter, pItf-value, pItf-value-setter,
	hr-value, hr-value-setter, <MULTI-QI>, <LPMULTI-QI>;
  export <LPENUMUNKNOWN>;
  export <LPBC>;
  export <LPBINDCTX>;
  export cbStruct-value, cbStruct-value-setter, grfFlags-value,
	grfFlags-value-setter, grfMode-value, grfMode-value-setter,
	dwTickCountDeadline-value, dwTickCountDeadline-value-setter,
	<BIND-OPTS>, <LPBIND-OPTS>;
  export $BIND-MAYBOTHERUSER, $BIND-JUSTTESTEXISTENCE;
  export <LPENUMMONIKER>;
  export <LPRUNNABLEOBJECT>;
  export <LPRUNNINGOBJECTTABLE>;
  export <LPPERSIST>;
  export <LPPERSISTSTREAM>;
  export <LPMONIKER>;
  export $MKSYS-NONE, $MKSYS-GENERICCOMPOSITE, $MKSYS-FILEMONIKER,
	$MKSYS-ANTIMONIKER, $MKSYS-ITEMMONIKER, $MKSYS-POINTERMONIKER,
	$MKSYS-CLASSMONIKER;
  export $MKRREDUCE-ONE, $MKRREDUCE-TOUSER, $MKRREDUCE-THROUGHUSER,
	$MKRREDUCE-ALL;
  export <LPENUMSTRING>;
  export pwcsName-value, pwcsName-value-setter, type-value,
	type-value-setter, mtime-value, mtime-value-setter, ctime-value,
	ctime-value-setter, atime-value, atime-value-setter, grfMode-value,
	grfMode-value-setter, grfLocksSupported-value,
	grfLocksSupported-value-setter, clsid-value, clsid-value-setter,
	grfStateBits-value, grfStateBits-value-setter, reserved-value,
	reserved-value-setter, <STATSTG>, <LPSTATSTG>, $STGTY-STORAGE,
	$STGTY-STREAM, $STGTY-LOCKBYTES, $STGTY-PROPERTY;
  export $STREAM-SEEK-SET, $STREAM-SEEK-CUR, $STREAM-SEEK-END;
  export $LOCK-WRITE, $LOCK-EXCLUSIVE, $LOCK-ONLYONCE;
  export <LPENUMSTATSTG>;
  export <LPSTORAGE>;
  export <LPPERSISTFILE>;
  export <LPPERSISTSTORAGE>;
  export <LPLOCKBYTES>;
  export <LPENUMFORMATETC>;
  export tdSize-value, tdSize-value-setter, tdDriverNameOffset-value,
	tdDriverNameOffset-value-setter, tdDeviceNameOffset-value,
	tdDeviceNameOffset-value-setter, tdPortNameOffset-value,
	tdPortNameOffset-value-setter, tdExtDevmodeOffset-value,
	tdExtDevmodeOffset-value-setter, tdData-array, tdData-array-setter,
	tdData-value, <DVTARGETDEVICE>, <LPDVTARGETDEVICE>;
  export cfFormat-value, cfFormat-value-setter, ptd-value,
	ptd-value-setter, dwAspect-value, dwAspect-value-setter,
	lindex-value, lindex-value-setter, tymed-value, tymed-value-setter,
	<FORMATETC>, <LPFORMATETC>;
  export <LPENUMSTATDATA>;
  export $ADVF-NODATA, $ADVF-PRIMEFIRST, $ADVF-ONLYONCE,
	$ADVF-DATAONSTOP, $ADVFCACHE-NOHANDLER, $ADVFCACHE-FORCEBUILTIN,
	$ADVFCACHE-ONSAVE;
  export formatetc-value, formatetc-value-setter, advf-value,
	advf-value-setter, pAdvSink-value, pAdvSink-value-setter,
	dwConnection-value, dwConnection-value-setter, <STATDATA>,
	<LPSTATDATA>;
  export <LPROOTSTORAGE>;
  export <LPADVISESINK>;
  export $TYMED-HGLOBAL, $TYMED-FILE, $TYMED-ISTREAM, $TYMED-ISTORAGE,
	$TYMED-GDI, $TYMED-MFPICT, $TYMED-ENHMF, $TYMED-NULL, tymed-value,
	tymed-value-setter, hBitmap-value, hBitmap-value-setter,
	hMetaFilePict-value, hMetaFilePict-value-setter, hEnhMetaFile-value,
	hEnhMetaFile-value-setter, hGlobal-value, hGlobal-value-setter,
	lpszFileName-value, lpszFileName-value-setter, pstm-value,
	pstm-value-setter, pstg-value, pstg-value-setter, hBitmap-value,
	hBitmap-value-setter, hMetaFilePict-value,
	hMetaFilePict-value-setter, hEnhMetaFile-value,
	hEnhMetaFile-value-setter, hGlobal-value, hGlobal-value-setter,
	lpszFileName-value, lpszFileName-value-setter, pstm-value,
	pstm-value-setter, pstg-value, pstg-value-setter,
	pUnkForRelease-value, pUnkForRelease-value-setter, <uSTGMEDIUM>,
	<LPuSTGMEDIUM>;
  export <STGMEDIUM>;
  export <LPSTGMEDIUM>;
  export <LPADVISESINK2>;
  export <LPDATAOBJECT>;
  export $DATADIR-GET, $DATADIR-SET;
  export <LPDATAADVISEHOLDER>;
  export <LPMESSAGEFILTER>;
  export $CALLTYPE-TOPLEVEL, $CALLTYPE-NESTED, $CALLTYPE-ASYNC,
	$CALLTYPE-TOPLEVEL-CALLPENDING, $CALLTYPE-ASYNC-CALLPENDING;
  export $SERVERCALL-ISHANDLED, $SERVERCALL-REJECTED,
	$SERVERCALL-RETRYLATER;
  export $PENDINGTYPE-TOPLEVEL, $PENDINGTYPE-NESTED;
  export $PENDINGMSG-CANCELCALL, $PENDINGMSG-WAITNOPROCESS,
	$PENDINGMSG-WAITDEFPROCESS;
  export pUnk-value, pUnk-value-setter, iid-value, iid-value-setter,
	wMethod-value, wMethod-value-setter, <INTERFACEINFO>,
	<LPINTERFACEINFO>;
  export <RPCOLEDATAREP>;
  export reserved1-value, reserved1-value-setter,
	dataRepresentation-value, dataRepresentation-value-setter,
	cbBuffer-value, cbBuffer-value-setter, iMethod-value,
	iMethod-value-setter, reserved2-array, reserved2-array-setter,
	reserved2-value, rpcFlags-value, rpcFlags-value-setter,
	<RPCOLEMESSAGE>, <LPRPCOLEMESSAGE>;
  export <LPPROPERTYSTORAGE>;
  export ulKind-value, ulKind-value-setter, propid-value,
	propid-value-setter, lpwstr-value, lpwstr-value-setter, <PROPSPEC>,
	<LPPROPSPEC>;
  export lpwstrName-value, lpwstrName-value-setter, propid-value,
	propid-value-setter, vt-value, vt-value-setter, <STATPROPSTG>,
	<LPSTATPROPSTG>, fmtid-value, fmtid-value-setter, clsid-value,
	clsid-value-setter, grfFlags-value, grfFlags-value-setter,
	mtime-value, mtime-value-setter, ctime-value, ctime-value-setter,
	atime-value, atime-value-setter, dwOSVersion-value,
	dwOSVersion-value-setter, <STATPROPSETSTG>, <LPSTATPROPSETSTG>;
  export <LPPROPERTYSETSTORAGE>;
  export <LPENUMSTATPROPSTG>;
  export <LPENUMSTATPROPSETSTG>;
  export LayoutType-value, LayoutType-value-setter,
	pwcsElementName-value, pwcsElementName-value-setter, cOffset-value,
	cOffset-value-setter, <StorageLayout>, <LPStorageLayout>;
  export <LPSURROGATE>;
  export <LPGLOBALINTERFACETABLE>;
  export <LPCANCELMETHODCALLS>;
  export cPublApps-value, cPublApps-value-setter, pPublAppInfo-value,
	pPublAppInfo-value-setter, <PUBLISHEDINFOLIST>,
	<LPPUBLISHEDINFOLIST>;
  export cPackInfo-value, cPackInfo-value-setter, pPackageInfo-value,
	pPackageInfo-value-setter, <PACKAGEINFOLIST>, <LPPACKAGEINFOLIST>;
  export Clsid-value, Clsid-value-setter, pszDesc-value,
	pszDesc-value-setter, pszIconPath-value, pszIconPath-value-setter,
	TreatAsClsid-value, TreatAsClsid-value-setter,
	AutoConvertClsid-value, AutoConvertClsid-value-setter,
	cFileExt-value, cFileExt-value-setter, prgFileExt-value,
	prgFileExt-value-setter, pMimeType-value, pMimeType-value-setter,
	pDefaultProgId-value, pDefaultProgId-value-setter,
	cOtherProgId-value, cOtherProgId-value-setter, prgOtherProgId-value,
	prgOtherProgId-value-setter, <CLASSDETAIL>, <LPCLASSDETAIL>;
  export CoInitialize, CoUninitialize, CoGetMalloc,
	CoGetCurrentProcess, CoGetClassObject, CoRegisterClassObject,
	CoRevokeClassObject, CoGetPSClsid, CoGetMarshalSizeMax,
	CoMarshalInterface, CoUnmarshalInterface, CoMarshalHresult,
	CoUnmarshalHresult, CoReleaseMarshalData, CoDisconnectObject,
	CoLockObjectExternal, CoGetStandardMarshal;
  export CoIsHandlerConnected, CoMarshalInterThreadInterfaceInStream,
	CoGetInterfaceAndReleaseStream, CoCreateFreeThreadedMarshaler,
	CoLoadLibrary, CoFreeLibrary, CoFreeAllLibraries,
	CoFreeUnusedLibraries;
  export CoCreateInstance;
  export StringFromCLSID, CLSIDFromString, StringFromIID,
	IIDFromString, CoIsOle1Class, ProgIDFromCLSID, CLSIDFromProgID,
	StringFromGUID2, CoCreateGuid, CoFileTimeToDosDateTime,
	CoDosDateTimeToFileTime, CoFileTimeNow;
  export CoRegisterMessageFilter, CoGetTreatAsClass, CoTreatAsClass;
  export CoTaskMemAlloc, CoTaskMemRealloc, CoTaskMemFree;
  export CreateDataAdviseHolder, CreateDataCache;
  export StgCreateDocfile, StgCreateDocfileOnILockBytes,
	StgOpenStorage, StgOpenStorageOnILockBytes, StgIsStorageFile,
	StgIsStorageILockBytes, StgSetTimes;
  export BindMoniker, MkParseDisplayName, MonikerRelativePathTo,
	MonikerCommonPrefixWith, CreateBindCtx, CreateGenericComposite,
	GetClassFile, CreateFileMoniker, CreateItemMoniker,
	CreateAntiMoniker, CreatePointerMoniker, GetRunningObjectTable;

  // status and error codes from "winerror.h":
  export $FACILITY-WINDOWS, $FACILITY-STORAGE, $FACILITY-SSPI,
	$FACILITY-SETUPAPI, $FACILITY-RPC, $FACILITY-WIN32,
	$FACILITY-CONTROL, $FACILITY-NULL, $FACILITY-MSMQ,
	$FACILITY-MEDIASERVER, $FACILITY-INTERNET, $FACILITY-ITF,
	$FACILITY-DISPATCH, $FACILITY-CERT, $E-UNEXPECTED, $E-NOTIMPL,
	$E-OUTOFMEMORY, $E-INVALIDARG, $E-NOINTERFACE, $E-POINTER, $E-HANDLE,
	$E-ABORT, $E-FAIL, $E-ACCESSDENIED, $E-PENDING, $CO-E-INIT-TLS,
	$CO-E-INIT-SHARED-ALLOCATOR, $CO-E-INIT-MEMORY-ALLOCATOR,
	$CO-E-INIT-CLASS-CACHE, $CO-E-INIT-RPC-CHANNEL,
	$CO-E-INIT-TLS-SET-CHANNEL-CONTROL, $CO-E-INIT-TLS-CHANNEL-CONTROL,
	$CO-E-INIT-UNACCEPTED-USER-ALLOCATOR, $CO-E-INIT-SCM-MUTEX-EXISTS,
	$CO-E-INIT-SCM-FILE-MAPPING-EXISTS, $CO-E-INIT-SCM-MAP-VIEW-OF-FILE,
	$CO-E-INIT-SCM-EXEC-FAILURE, $CO-E-INIT-ONLY-SINGLE-THREADED,
	$CO-E-CANT-REMOTE, $CO-E-BAD-SERVER-NAME,
	$CO-E-WRONG-SERVER-IDENTITY, $CO-E-OLE1DDE-DISABLED,
	$CO-E-RUNAS-SYNTAX, $CO-E-CREATEPROCESS-FAILURE,
	$CO-E-RUNAS-CREATEPROCESS-FAILURE, $CO-E-RUNAS-LOGON-FAILURE,
	$CO-E-LAUNCH-PERMSSION-DENIED, $CO-E-START-SERVICE-FAILURE,
	$CO-E-REMOTE-COMMUNICATION-FAILURE, $CO-E-SERVER-START-TIMEOUT,
	$CO-E-CLSREG-INCONSISTENT, $CO-E-IIDREG-INCONSISTENT,
	$CO-E-NOT-SUPPORTED, $CO-E-RELOAD-DLL, $CO-E-MSI-ERROR, $S-OK,
	$S-FALSE, $CLASS-E-NOAGGREGATION, $CLASS-E-CLASSNOTAVAILABLE,
	$CLASS-E-NOTLICENSED, $REGDB-E-READREGDB, $REGDB-E-WRITEREGDB,
	$REGDB-E-KEYMISSING, $REGDB-E-INVALIDVALUE, $REGDB-E-CLASSNOTREG,
	$REGDB-E-IIDNOTREG, $MK-E-CONNECTMANUALLY, $MK-E-EXCEEDEDDEADLINE,
	$MK-E-NEEDGENERIC, $MK-E-UNAVAILABLE, $MK-E-SYNTAX, $MK-E-NOOBJECT,
	$MK-E-INVALIDEXTENSION, $MK-E-INTERMEDIATEINTERFACENOTSUPPORTED,
	$MK-E-NOTBINDABLE, $MK-E-NOTBOUND, $MK-E-CANTOPENFILE,
	$MK-E-MUSTBOTHERUSER, $MK-E-NOINVERSE, $MK-E-NOSTORAGE,
	$MK-E-NOPREFIX, $MK-E-ENUMERATION-FAILED, $CO-E-NOTINITIALIZED,
	$CO-E-ALREADYINITIALIZED, $CO-E-CANTDETERMINECLASS,
	$CO-E-CLASSSTRING, $CO-E-IIDSTRING, $CO-E-APPNOTFOUND,
	$CO-E-APPSINGLEUSE, $CO-E-ERRORINAPP, $CO-E-DLLNOTFOUND,
	$CO-E-ERRORINDLL, $CO-E-WRONGOSFORAPP, $CO-E-OBJNOTREG,
	$CO-E-OBJISREG, $CO-E-OBJNOTCONNECTED, $CO-E-APPDIDNTREG,
	$CO-E-RELEASED, $CO-E-FAILEDTOIMPERSONATE, $CO-E-FAILEDTOGETSECCTX,
	$CO-E-FAILEDTOOPENTHREADTOKEN, $CO-E-FAILEDTOGETTOKENINFO,
	$CO-E-TRUSTEEDOESNTMATCHCLIENT, $CO-E-FAILEDTOQUERYCLIENTBLANKET,
	$CO-E-FAILEDTOSETDACL, $CO-E-ACCESSCHECKFAILED,
	$CO-E-NETACCESSAPIFAILED, $CO-E-WRONGTRUSTEENAMESYNTAX,
	$CO-E-INVALIDSID, $CO-E-CONVERSIONFAILED, $CO-E-NOMATCHINGSIDFOUND,
	$CO-E-LOOKUPACCSIDFAILED, $CO-E-NOMATCHINGNAMEFOUND,
	$CO-E-LOOKUPACCNAMEFAILED, $CO-E-SETSERLHNDLFAILED,
	$CO-E-FAILEDTOGETWINDIR, $CO-E-PATHTOOLONG, $CO-E-FAILEDTOGENUUID,
	$CO-E-FAILEDTOCREATEFILE, $CO-E-FAILEDTOCLOSEHANDLE,
	$CO-E-EXCEEDSYSACLLIMIT, $CO-E-ACESINWRONGORDER,
	$CO-E-INCOMPATIBLESTREAMVERSION, $CO-E-FAILEDTOOPENPROCESSTOKEN,
	$CO-E-DECODEFAILED, $CO-E-ACNOTINITIALIZED, $MK-S-REDUCED-TO-SELF,
	$MK-S-ME, $MK-S-HIM, $MK-S-US, $MK-S-MONIKERALREADYREGISTERED,
	$CO-E-CLASS-CREATE-FAILED, $CO-E-SCM-ERROR, $CO-E-SCM-RPC-FAILURE,
	$CO-E-BAD-PATH, $CO-E-SERVER-EXEC-FAILURE, $CO-E-OBJSRV-RPC-FAILURE,
	$MK-E-NO-NORMALIZED, $CO-E-SERVER-STOPPING, $STG-E-INVALIDFUNCTION,
	$STG-E-FILENOTFOUND, $STG-E-PATHNOTFOUND, $STG-E-TOOMANYOPENFILES,
	$STG-E-ACCESSDENIED, $STG-E-INVALIDHANDLE, $STG-E-INSUFFICIENTMEMORY,
	$STG-E-INVALIDPOINTER, $STG-E-NOMOREFILES,
	$STG-E-DISKISWRITEPROTECTED, $STG-E-SEEKERROR, $STG-E-WRITEFAULT,
	$STG-E-READFAULT, $STG-E-SHAREVIOLATION, $STG-E-LOCKVIOLATION,
	$STG-E-FILEALREADYEXISTS, $STG-E-INVALIDPARAMETER, $STG-E-MEDIUMFULL,
	$STG-E-PROPSETMISMATCHED, $STG-E-ABNORMALAPIEXIT,
	$STG-E-INVALIDHEADER, $STG-E-INVALIDNAME, $STG-E-UNKNOWN,
	$STG-E-UNIMPLEMENTEDFUNCTION, $STG-E-INVALIDFLAG, $STG-E-INUSE,
	$STG-E-NOTCURRENT, $STG-E-REVERTED, $STG-E-CANTSAVE,
	$STG-E-OLDFORMAT, $STG-E-OLDDLL, $STG-E-SHAREREQUIRED,
	$STG-E-NOTFILEBASEDSTORAGE, $STG-E-EXTANTMARSHALLINGS,
	$STG-E-DOCFILECORRUPT, $STG-E-BADBASEADDRESS, $STG-E-INCOMPLETE,
	$STG-E-TERMINATED, $STG-S-CONVERTED, $STG-S-BLOCK, $STG-S-RETRYNOW,
	$STG-S-MONITORING, $STG-S-MULTIPLEOPENS, $STG-S-CONSOLIDATIONFAILED,
	$STG-S-CANNOTCONSOLIDATE, $RPC-E-CALL-REJECTED, $RPC-E-CALL-CANCELED,
	$RPC-E-CANTPOST-INSENDCALL, $RPC-E-CANTCALLOUT-INASYNCCALL,
	$RPC-E-CANTCALLOUT-INEXTERNALCALL, $RPC-E-CONNECTION-TERMINATED,
	$RPC-E-SERVER-DIED, $RPC-E-CLIENT-DIED, $RPC-E-INVALID-DATAPACKET,
	$RPC-E-CANTTRANSMIT-CALL, $RPC-E-CLIENT-CANTMARSHAL-DATA,
	$RPC-E-CLIENT-CANTUNMARSHAL-DATA, $RPC-E-SERVER-CANTMARSHAL-DATA,
	$RPC-E-SERVER-CANTUNMARSHAL-DATA, $RPC-E-INVALID-DATA,
	$RPC-E-INVALID-PARAMETER, $RPC-E-CANTCALLOUT-AGAIN,
	$RPC-E-SERVER-DIED-DNE, $RPC-E-SYS-CALL-FAILED,
	$RPC-E-OUT-OF-RESOURCES, $RPC-E-ATTEMPTED-MULTITHREAD,
	$RPC-E-NOT-REGISTERED, $RPC-E-FAULT, $RPC-E-SERVERFAULT,
	$RPC-E-CHANGED-MODE, $RPC-E-INVALIDMETHOD, $RPC-E-DISCONNECTED,
	$RPC-E-RETRY, $RPC-E-SERVERCALL-RETRYLATER,
	$RPC-E-SERVERCALL-REJECTED, $RPC-E-INVALID-CALLDATA,
	$RPC-E-CANTCALLOUT-ININPUTSYNCCALL, $RPC-E-WRONG-THREAD,
	$RPC-E-THREAD-NOT-INIT, $RPC-E-VERSION-MISMATCH,
	$RPC-E-INVALID-HEADER, $RPC-E-INVALID-EXTENSION, $RPC-E-INVALID-IPID,
	$RPC-E-INVALID-OBJECT, $RPC-S-CALLPENDING, $RPC-S-WAITONTIMER,
	$RPC-E-CALL-COMPLETE, $RPC-E-UNSECURE-CALL, $RPC-E-TOO-LATE,
	$RPC-E-NO-GOOD-SECURITY-PACKAGES, $RPC-E-ACCESS-DENIED,
	$RPC-E-REMOTE-DISABLED, $RPC-E-INVALID-OBJREF, $RPC-E-NO-CONTEXT,
	$RPC-E-TIMEOUT, $RPC-E-NO-SYNC, $RPC-E-UNEXPECTED;

  // helper functions:
  export IStream/Write-integer, IStream/Read-integer;

  // from "factory.dylan":
  export <class-factory>, revoke-registration, 
    <factory-args-mixin>, \with-class-factory;
  export create-COM-instance, \with-COM-interface;

  // from "dll-init.dylan":
  export <dll-lock-mixin>, <object-with-dll-lock>,
	<class-factory-with-lock>,
	dll-active-interface-counter, dll-active-interface-counter-setter,
	\initialize-in-process-server;

  // from "custom.dylan":
  export custom-interface-definer, dummy-value-for-type;

  // from "com-util.dylan":
  export ole-error, ole-cerror, <ole-error>, make-ole-error,
	ole-error-status, ole-error-instance,
	ole-error-context, ole-error-args, check-ole-status;
  export <LPSTREAM>, <storage-istream>, $null-istream;
  export OLE-util-in-process-startup?,
         OLE-util-register-only?, OLE-util-unregister?,
	 OLE-util-started-by-OLE?, OLE-util-automation?, OLE-util-file-arg;
  export OleInitialize, OleUninitialize;
  export OLE-Initialize, OLE-UnInitialize, \with-ole, <lib-init-mixin>;
  export Output-Debug-String;

  export hAccel-value, hAccel-value-setter;

end;
