module:    Dylan-user	
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */

define library win32-automation
  use functional-dylan;
  use c-ffi;
  use com;
  use win32-common;
  use win32-kernel;
  use win32-registry;
  export win32-automation;
end;

define module win32-automation
  use functional-dylan;
  use Dylan-extensions,
    import: {<abstract-integer>, <big-integer>, <double-integer>,
	     <simple-byte-vector>, <simple-double-byte-vector>,
	     <simple-integer-vector>, <simple-machine-word-vector>,
	     <simple-single-float-vector>, <simple-double-float-vector>,
	     <simple-byte-array>, <simple-double-byte-array>,
	     <simple-integer-array>, <simple-machine-word-array>,
	     <simple-single-float-array> };
  use simple-format;
  use finalization;
  use C-FFI;
  use COM;
  use COM-internal;
  use Win32-common; // this defines types such as <DWORD>, <ULONG>, etc.
  use Win32-kernel, import: { GetLastError, GetModuleFileName,
			      OutputDebugString, GetFileAttributes,
			      win32-error-message,
			      application-instance-handle,
			      application-command-line };
  use Win32-registry;

  // This module includes the following interfaces:
  export <ICreateTypeInfo>, $IID-ICreateTypeInfo,
		ICreateTypeInfo/SetGuid, ICreateTypeInfo/SetTypeFlags,
		ICreateTypeInfo/SetDocString, ICreateTypeInfo/SetHelpContext,
		ICreateTypeInfo/SetVersion, ICreateTypeInfo/AddRefTypeInfo,
		ICreateTypeInfo/AddFuncDesc, ICreateTypeInfo/AddImplType,
		ICreateTypeInfo/SetImplTypeFlags, ICreateTypeInfo/SetAlignment,
		ICreateTypeInfo/SetSchema, ICreateTypeInfo/AddVarDesc,
		ICreateTypeInfo/SetFuncAndParamNames,
		ICreateTypeInfo/SetVarName, ICreateTypeInfo/SetTypeDescAlias,
		ICreateTypeInfo/DefineFuncAsDllEntry,
		ICreateTypeInfo/SetFuncDocString,
		ICreateTypeInfo/SetVarDocString,
		ICreateTypeInfo/SetFuncHelpContext,
		ICreateTypeInfo/SetVarHelpContext, ICreateTypeInfo/SetMops,
		ICreateTypeInfo/SetTypeIdldesc, ICreateTypeInfo/LayOut;
  export <ICreateTypeInfo2>, $IID-ICreateTypeInfo2,
		ICreateTypeInfo2/DeleteFuncDesc,
		ICreateTypeInfo2/DeleteFuncDescByMemId,
		ICreateTypeInfo2/DeleteVarDesc,
		ICreateTypeInfo2/DeleteVarDescByMemId,
		ICreateTypeInfo2/DeleteImplType, ICreateTypeInfo2/SetCustData,
		ICreateTypeInfo2/SetFuncCustData,
		ICreateTypeInfo2/SetParamCustData,
		ICreateTypeInfo2/SetVarCustData,
		ICreateTypeInfo2/SetImplTypeCustData,
		ICreateTypeInfo2/SetHelpStringContext,
		ICreateTypeInfo2/SetFuncHelpStringContext,
		ICreateTypeInfo2/SetVarHelpStringContext,
		ICreateTypeInfo2/Invalidate, ICreateTypeInfo2/SetName;
  export <ICreateTypeLib>, $IID-ICreateTypeLib,
		ICreateTypeLib/CreateTypeInfo, ICreateTypeLib/SetName,
		ICreateTypeLib/SetVersion, ICreateTypeLib/SetGuid,
		ICreateTypeLib/SetDocString, ICreateTypeLib/SetHelpFileName,
		ICreateTypeLib/SetHelpContext, ICreateTypeLib/SetLcid,
		ICreateTypeLib/SetLibFlags, ICreateTypeLib/SaveAllChanges;
  export <ICreateTypeLib2>, $IID-ICreateTypeLib2,
		ICreateTypeLib2/DeleteTypeInfo, ICreateTypeLib2/SetCustData,
		ICreateTypeLib2/SetHelpStringContext,
		ICreateTypeLib2/SetHelpStringDll;
  export <IDispatch>, $IID-IDispatch, IDispatch/GetTypeInfoCount,
		IDispatch/GetTypeInfo, IDispatch/GetIDsOfNames,
		IDispatch/Invoke;
  export <IEnumVARIANT>, $IID-IEnumVARIANT, IEnumVARIANT/Next,
		IEnumVARIANT/Skip, IEnumVARIANT/Reset, IEnumVARIANT/Clone;
  export <ITypeComp>, $IID-ITypeComp, ITypeComp/Bind,
		ITypeComp/BindType;
  export <ITypeInfo>, $IID-ITypeInfo, ITypeInfo/GetTypeAttr,
		ITypeInfo/GetTypeComp, ITypeInfo/GetFuncDesc,
		ITypeInfo/GetVarDesc, ITypeInfo/GetNames,
		ITypeInfo/GetRefTypeOfImplType, ITypeInfo/GetImplTypeFlags,
		ITypeInfo/GetIDsOfNames, ITypeInfo/Invoke,
		ITypeInfo/GetDocumentation, ITypeInfo/GetDllEntry,
		ITypeInfo/GetRefTypeInfo, ITypeInfo/AddressOfMember,
		ITypeInfo/CreateInstance, ITypeInfo/GetMops,
		ITypeInfo/GetContainingTypeLib, ITypeInfo/ReleaseTypeAttr,
		ITypeInfo/ReleaseFuncDesc, ITypeInfo/ReleaseVarDesc;
  export <ITypeInfo2>, $IID-ITypeInfo2, ITypeInfo2/GetTypeKind,
		ITypeInfo2/GetTypeFlags, ITypeInfo2/GetFuncIndexOfMemId,
		ITypeInfo2/GetVarIndexOfMemId, ITypeInfo2/GetCustData,
		ITypeInfo2/GetFuncCustData, ITypeInfo2/GetParamCustData,
		ITypeInfo2/GetVarCustData, ITypeInfo2/GetImplTypeCustData,
		ITypeInfo2/GetDocumentation2, ITypeInfo2/GetAllCustData,
		ITypeInfo2/GetAllFuncCustData, ITypeInfo2/GetAllParamCustData,
		ITypeInfo2/GetAllVarCustData,
		ITypeInfo2/GetAllImplTypeCustData;
  export <ITypeLib>, $IID-ITypeLib, ITypeLib/GetTypeInfoCount,
		ITypeLib/GetTypeInfo, ITypeLib/GetTypeInfoType,
		ITypeLib/GetTypeInfoOfGuid, ITypeLib/GetLibAttr,
		ITypeLib/GetTypeComp, ITypeLib/GetDocumentation,
		ITypeLib/IsName, ITypeLib/FindName, ITypeLib/ReleaseTLibAttr;
  export <ITypeLib2>, $IID-ITypeLib2, ITypeLib2/GetCustData,
		ITypeLib2/GetLibStatistics, ITypeLib2/GetDocumentation2,
		ITypeLib2/GetAllCustData;
  export <IErrorInfo>, $IID-IErrorInfo, IErrorInfo/GetGUID,
		IErrorInfo/GetSource, IErrorInfo/GetDescription,
		IErrorInfo/GetHelpFile, IErrorInfo/GetHelpContext;
  export <ICreateErrorInfo>, $IID-ICreateErrorInfo,
		ICreateErrorInfo/SetGUID, ICreateErrorInfo/SetSource,
		ICreateErrorInfo/SetDescription, ICreateErrorInfo/SetHelpFile,
		ICreateErrorInfo/SetHelpContext;
  export <ISupportErrorInfo>, $IID-ISupportErrorInfo,
		ISupportErrorInfo/InterfaceSupportsErrorInfo;
  export <ITypeFactory>, $IID-ITypeFactory,
		ITypeFactory/CreateFromTypeInfo;
  export <ITypeMarshal>, $IID-ITypeMarshal, ITypeMarshal/Size,
		ITypeMarshal/Marshal, ITypeMarshal/Unmarshal,
		ITypeMarshal/Free;
  export <IRecordInfo>, $IID-IRecordInfo, IRecordInfo/RecordInit,
		IRecordInfo/RecordClear, IRecordInfo/RecordCopy,
		IRecordInfo/GetGuid, IRecordInfo/GetName, IRecordInfo/GetSize,
		IRecordInfo/GetTypeInfo, IRecordInfo/GetField,
		IRecordInfo/GetFieldNoCopy, IRecordInfo/PutField,
		IRecordInfo/PutFieldNoCopy, IRecordInfo/GetFieldNames,
		IRecordInfo/IsMatchingType, IRecordInfo/RecordCreate,
		IRecordInfo/RecordCreateCopy, IRecordInfo/RecordDestroy;

  // stuff from "auto-misc":

  // from "wtypes.h":
  export <BSTR>;
  export <LPBSTR>;
  export $VARIANT-TRUE, $VARIANT-FALSE;
  export <VARTYPE>, $VT-EMPTY, $VT-NULL, $VT-I2, $VT-I4, $VT-R4,
	$VT-R8, $VT-CY, $VT-DATE, $VT-BSTR, $VT-DISPATCH, $VT-ERROR,
	$VT-BOOL, $VT-VARIANT, $VT-UNKNOWN, $VT-DECIMAL, $VT-I1, $VT-UI1,
	$VT-UI2, $VT-UI4, $VT-I8, $VT-UI8, $VT-INT, $VT-UINT, $VT-VOID,
	$VT-HRESULT, $VT-PTR, $VT-SAFEARRAY, $VT-CARRAY, $VT-USERDEFINED,
	$VT-LPSTR, $VT-LPWSTR, $VT-RECORD, $VT-FILETIME, $VT-BLOB,
	$VT-STREAM, $VT-STORAGE, $VT-STREAMED-OBJECT, $VT-STORED-OBJECT,
	$VT-BLOB-OBJECT, $VT-CF, $VT-CLSID, $VT-BSTR-BLOB, $VT-VECTOR,
	$VT-ARRAY, $VT-BYREF, $VT-RESERVED, $VT-ILLEGAL, $VT-ILLEGALMASKED,
	$VT-TYPEMASK;

  // from "oleauto.h":

  // from "oaidl.h":
  export cElements-value, cElements-value-setter, lLbound-value,
	lLbound-value-setter, <SAFEARRAYBOUND>, <LPSAFEARRAYBOUND>;
  export cDims-value, cDims-value-setter, fFeatures-value,
	fFeatures-value-setter, cbElements-value, cbElements-value-setter,
	cLocks-value, cLocks-value-setter, pvData-value, pvData-value-setter,
	rgsabound-array, rgsabound-array-setter, rgsabound-value,
	<SAFEARRAY>, <LPSAFEARRAY>;
  export $FADF-AUTO, $FADF-STATIC, $FADF-EMBEDDED, $FADF-FIXEDSIZE,
	$FADF-RECORD, $FADF-HAVEIID, $FADF-HAVEVARTYPE, $FADF-BSTR,
	$FADF-UNKNOWN, $FADF-DISPATCH, $FADF-VARIANT, $FADF-RESERVED;
  export <DISPID>;
  export <MEMBERID>;
  export <HREFTYPE>;
  export $TKIND-ENUM, $TKIND-RECORD, $TKIND-MODULE, $TKIND-INTERFACE,
	$TKIND-DISPATCH, $TKIND-COCLASS, $TKIND-ALIAS, $TKIND-UNION,
	$TKIND-MAX;
  export lptdesc-value, lptdesc-value-setter, lpadesc-value,
	lpadesc-value-setter, hreftype-value, hreftype-value-setter,
	<TYPEDESC>, <LPTYPEDESC>;
  export tdescElem-value, tdescElem-value-setter, cDims-value,
	cDims-value-setter, rgbounds-array, rgbounds-array-setter,
	rgbounds-value, <ARRAYDESC>, <LPARRAYDESC>;
  export varDefaultValue-value, varDefaultValue-value-setter,
	<PARAMDESCEX>, <LPPARAMDESCEX>;
  export pparamdescex-value, pparamdescex-value-setter,
	wParamFlags-value, wParamFlags-value-setter, <PARAMDESC>,
	<LPPARAMDESC>;
  export $PARAMFLAG-NONE, $PARAMFLAG-FIN, $PARAMFLAG-FOUT,
	$PARAMFLAG-FLCID, $PARAMFLAG-FRETVAL, $PARAMFLAG-FOPT,
	$PARAMFLAG-FHASDEFAULT, $PARAMFLAG-FHASCUSTDATA, wIDLFlags-value,
	wIDLFlags-value-setter, <IDLDESC>, <LPIDLDESC>;
  export $IDLFLAG-NONE, $IDLFLAG-FIN, $IDLFLAG-FOUT, $IDLFLAG-FLCID,
	$IDLFLAG-FRETVAL, tdesc-value, tdesc-value-setter, idldesc-value,
	idldesc-value-setter, paramdesc-value, paramdesc-value-setter,
	<ELEMDESC>, <LPELEMDESC>, guid-value, guid-value-setter, lcid-value,
	lcid-value-setter, memidConstructor-value,
	memidConstructor-value-setter, memidDestructor-value,
	memidDestructor-value-setter, lpstrSchema-value,
	lpstrSchema-value-setter, cbSizeInstance-value,
	cbSizeInstance-value-setter, typekind-value, typekind-value-setter,
	cFuncs-value, cFuncs-value-setter, cVars-value, cVars-value-setter,
	cImplTypes-value, cImplTypes-value-setter, cbSizeVft-value,
	cbSizeVft-value-setter, cbAlignment-value, cbAlignment-value-setter,
	wTypeFlags-value, wTypeFlags-value-setter, wMajorVerNum-value,
	wMajorVerNum-value-setter, wMinorVerNum-value,
	wMinorVerNum-value-setter, tdescAlias-value, tdescAlias-value-setter,
	idldescType-value, idldescType-value-setter, <TYPEATTR>,
	<LPTYPEATTR>;
  export rgvarg-value, rgvarg-value-setter, rgdispidNamedArgs-value,
	rgdispidNamedArgs-value-setter, cArgs-value, cArgs-value-setter,
	cNamedArgs-value, cNamedArgs-value-setter, <DISPPARAMS>,
	<LPDISPPARAMS>;
  export wCode-value, wCode-value-setter, bstrSource-value,
	bstrSource-value-setter, bstrDescription-value,
	bstrDescription-value-setter, bstrHelpFile-value,
	bstrHelpFile-value-setter, dwHelpContext-value,
	dwHelpContext-value-setter, pfnDeferredFillIn-value,
	pfnDeferredFillIn-value-setter, scode-value, scode-value-setter,
	<EXCEPINFO>, <LPEXCEPINFO>;
  export $CC-FASTCALL, $CC-CDECL, $CC-MSCPASCAL, $CC-PASCAL,
	$CC-MACPASCAL, $CC-STDCALL, $CC-FPFASTCALL, $CC-SYSCALL,
	$CC-MPWCDECL, $CC-MPWPASCAL, $CC-MAX;
  export $FUNC-VIRTUAL, $FUNC-PUREVIRTUAL, $FUNC-NONVIRTUAL,
	$FUNC-STATIC, $FUNC-DISPATCH;
  export $INVOKE-FUNC, $INVOKE-PROPERTYGET, $INVOKE-PROPERTYPUT,
	$INVOKE-PROPERTYPUTREF;
  export memid-value, memid-value-setter, lprgscode-value,
	lprgscode-value-setter, lprgelemdescParam-value,
	lprgelemdescParam-value-setter, funckind-value,
	funckind-value-setter, invkind-value, invkind-value-setter,
	callconv-value, callconv-value-setter, cParams-value,
	cParams-value-setter, cParamsOpt-value, cParamsOpt-value-setter,
	oVft-value, oVft-value-setter, cScodes-value, cScodes-value-setter,
	elemdescFunc-value, elemdescFunc-value-setter, wFuncFlags-value,
	wFuncFlags-value-setter, <FUNCDESC>, <LPFUNCDESC>;
  export $VAR-PERINSTANCE, $VAR-STATIC, $VAR-CONST, $VAR-DISPATCH,
	$IMPLTYPEFLAG-FDEFAULT, $IMPLTYPEFLAG-FSOURCE,
	$IMPLTYPEFLAG-FRESTRICTED, $IMPLTYPEFLAG-FDEFAULTVTABLE, memid-value,
	memid-value-setter, lpstrSchema-value, lpstrSchema-value-setter,
	oInst-value, oInst-value-setter, lpvarValue-value,
	lpvarValue-value-setter, elemdescVar-value, elemdescVar-value-setter,
	wVarFlags-value, wVarFlags-value-setter, varkind-value,
	varkind-value-setter, <VARDESC>, <LPVARDESC>;
  export $TYPEFLAG-FAPPOBJECT, $TYPEFLAG-FCANCREATE,
	$TYPEFLAG-FLICENSED, $TYPEFLAG-FPREDECLID, $TYPEFLAG-FHIDDEN,
	$TYPEFLAG-FCONTROL, $TYPEFLAG-FDUAL, $TYPEFLAG-FNONEXTENSIBLE,
	$TYPEFLAG-FOLEAUTOMATION, $TYPEFLAG-FRESTRICTED,
	$TYPEFLAG-FAGGREGATABLE, $TYPEFLAG-FREPLACEABLE,
	$TYPEFLAG-FDISPATCHABLE, $TYPEFLAG-FREVERSEBIND;
  export $FUNCFLAG-FRESTRICTED, $FUNCFLAG-FSOURCE,
	$FUNCFLAG-FBINDABLE, $FUNCFLAG-FREQUESTEDIT, $FUNCFLAG-FDISPLAYBIND,
	$FUNCFLAG-FDEFAULTBIND, $FUNCFLAG-FHIDDEN,
	$FUNCFLAG-FUSESGETLASTERROR, $FUNCFLAG-FDEFAULTCOLLELEM,
	$FUNCFLAG-FUIDEFAULT, $FUNCFLAG-FNONBROWSABLE,
	$FUNCFLAG-FREPLACEABLE, $FUNCFLAG-FIMMEDIATEBIND;
  export $VARFLAG-FREADONLY, $VARFLAG-FSOURCE, $VARFLAG-FBINDABLE,
	$VARFLAG-FREQUESTEDIT, $VARFLAG-FDISPLAYBIND, $VARFLAG-FDEFAULTBIND,
	$VARFLAG-FHIDDEN, $VARFLAG-FRESTRICTED, $VARFLAG-FDEFAULTCOLLELEM,
	$VARFLAG-FUIDEFAULT, $VARFLAG-FNONBROWSABLE, $VARFLAG-FREPLACEABLE,
	$VARFLAG-FIMMEDIATEBIND;
  export guid-value, guid-value-setter, varValue-value,
	varValue-value-setter, <CUSTDATAITEM>, <LPCUSTDATAITEM>;
  export cCustData-value, cCustData-value-setter, prgCustData-value,
	prgCustData-value-setter, <CUSTDATA>, <LPCUSTDATA>;
  export <LPCREATETYPEINFO>;
  export <LPCREATETYPEINFO2>;
  export <LPCREATETYPELIB>;
  export <LPCREATETYPELIB2>;
  export <LPDISPATCH>, $DISPID-UNKNOWN, $DISPID-VALUE,
	$DISPID-PROPERTYPUT, $DISPID-NEWENUM, $DISPID-EVALUATE,
	$DISPID-CONSTRUCTOR, $DISPID-DESTRUCTOR, $DISPID-COLLECT;
  export <LPENUMVARIANT>;
  export <LPTYPECOMP>;
  export $DESCKIND-NONE, $DESCKIND-FUNCDESC, $DESCKIND-VARDESC,
	$DESCKIND-TYPECOMP, $DESCKIND-IMPLICITAPPOBJ, $DESCKIND-MAX;
  export lpfuncdesc-value, lpfuncdesc-value-setter, lpvardesc-value,
	lpvardesc-value-setter, lptcomp-value, lptcomp-value-setter,
	<BINDPTR>;
  export <LPBINDPTR>;
  export <LPTYPEINFO>;
  export <LPTYPEINFO2>;
  export $SYS-WIN16, $SYS-WIN32, $SYS-MAC;
  export $LIBFLAG-FRESTRICTED, $LIBFLAG-FCONTROL, $LIBFLAG-FHIDDEN,
	$LIBFLAG-FHASDISKIMAGE;
  export <LPTYPELIB>;
  export guid-value, guid-value-setter, lcid-value, lcid-value-setter,
	syskind-value, syskind-value-setter, wMajorVerNum-value,
	wMajorVerNum-value-setter, wMinorVerNum-value,
	wMinorVerNum-value-setter, wLibFlags-value, wLibFlags-value-setter,
	<TLIBATTR>, <LPTLIBATTR>;
  export <LPTYPELIB2>;
  export <LPTYPECHANGEEVENTS>;
  export <LPERRORINFO>;
  export <LPCREATEERRORINFO>;
  export <LPSUPPORTERRORINFO>;
  export <LPRECORDINFO>;
  export SysAllocString, SysReAllocString, SysAllocStringLen,
	SysReAllocStringLen, SysFreeString, SysStringLen, SysStringByteLen,
	SysAllocStringByteLen, DosDateTimeToVariantTime,
	VariantTimeToDosDateTime;
  export SafeArrayAllocDescriptor, SafeArrayAllocData,
	SafeArrayCreate, SafeArrayDestroyDescriptor, SafeArrayDestroyData,
	SafeArrayDestroy, SafeArrayRedim, SafeArrayGetDim,
	SafeArrayGetElemsize, SafeArrayGetUBound, SafeArrayGetLBound,
	SafeArrayLock, SafeArrayUnlock, SafeArrayAccessData,
	SafeArrayUnaccessData, SafeArrayGetElement, SafeArrayPutElement,
	SafeArrayCopy, SafeArrayPtrOfIndex, VariantInit, VariantClear,
	VariantCopy, VariantCopyInd, VariantChangeType, VariantChangeTypeEx,
	$VARIANT-NOVALUEPROP;
  export $VT-HARDTYPE;
  export $MEMBERID-NIL, $DISPATCH-METHOD, $DISPATCH-PROPERTYGET,
	$DISPATCH-PROPERTYPUT, $DISPATCH-PROPERTYPUTREF;
  export LHashValOfNameSysA, LHashValOfNameSys, LoadTypeLib,
	LoadRegTypeLib, QueryPathOfRegTypeLib, RegisterTypeLib,
	CreateTypeLib;
  export DispGetParam, DispGetIDsOfNames, DispInvoke,
	CreateStdDispatch;
  export RegisterActiveObject, RevokeActiveObject, GetActiveObject,
	SetErrorInfo, GetErrorInfo, CreateErrorInfo;

  // error codes from "winerror.h":
  export $DISP-E-UNKNOWNINTERFACE, $DISP-E-MEMBERNOTFOUND,
	$DISP-E-PARAMNOTFOUND, $DISP-E-TYPEMISMATCH, $DISP-E-UNKNOWNNAME,
	$DISP-E-NONAMEDARGS, $DISP-E-BADVARTYPE, $DISP-E-EXCEPTION,
	$DISP-E-OVERFLOW, $DISP-E-BADINDEX, $DISP-E-UNKNOWNLCID,
	$DISP-E-ARRAYISLOCKED, $DISP-E-BADPARAMCOUNT,
	$DISP-E-PARAMNOTOPTIONAL, $DISP-E-BADCALLEE, $DISP-E-NOTACOLLECTION,
	$DISP-E-DIVBYZERO, $TYPE-E-BUFFERTOOSMALL, $TYPE-E-FIELDNOTFOUND,
	$TYPE-E-INVDATAREAD, $TYPE-E-UNSUPFORMAT, $TYPE-E-REGISTRYACCESS,
	$TYPE-E-LIBNOTREGISTERED, $TYPE-E-UNDEFINEDTYPE,
	$TYPE-E-QUALIFIEDNAMEDISALLOWED, $TYPE-E-INVALIDSTATE,
	$TYPE-E-WRONGTYPEKIND, $TYPE-E-ELEMENTNOTFOUND,
	$TYPE-E-AMBIGUOUSNAME, $TYPE-E-NAMECONFLICT, $TYPE-E-UNKNOWNLCID,
	$TYPE-E-DLLFUNCTIONNOTFOUND, $TYPE-E-BADMODULEKIND,
	$TYPE-E-SIZETOOBIG, $TYPE-E-DUPLICATEID, $TYPE-E-INVALIDID,
	$TYPE-E-TYPEMISMATCH, $TYPE-E-OUTOFBOUNDS, $TYPE-E-IOERROR,
	$TYPE-E-CANTCREATETMPFILE, $TYPE-E-CANTLOADLIBRARY,
	$TYPE-E-INCONSISTENTPROPFUNCS, $TYPE-E-CIRCULARTYPE;

  // from "variant.dylan":
  export <VARIANT>, <LPVARIANT>, <VARIANTARG>, <LPVARIANTARG>;
  export lVal-value, bVal-value, iVal-value, fltVal-value,
    dblVal-value, bool-value, scode-value, cyVal-value, date-value,
    ptr-value, parray-value;
  export lVal-value-setter, bVal-value-setter,
    iVal-value-setter, fltVal-value-setter, dblVal-value-setter,
    bool-value-setter, scode-value-setter, date-value-setter,
    ptr-value-setter, parray-value-setter;
  export <CY>, <VARIANT-BOOL>;
  export $SQL-NULL;
  export $NULL-VARIANT;
  export <CY*>, <DATE*>, <PLARGE-INTEGER*>, <PULARGE-INTEGER*>, <BSTR*>;
  export <ole-arg-spec>, <ole-value-arg-spec>, <ole-by-ref-arg-spec>,
     out-ref, inout-ref, pass-as, arg-spec-vt, arg-spec-value, 
     arg-spec-value-setter, arg-spec-direction, arg-spec-ptr;

  // from "strings.dylan":
  export $NULL-BSTR, copy-as-BSTR;

  // from "arrays.dylan":
  export <ole-array>, <ole-vector>, ole-vector, <c-safe-array>;

  // In IDLDESC, this field is supposed to be set to NULL:
  export dwReserved, dwReserved-setter;

  // Extra stuff needed when split off win32-automation
  //  - used to be in typeinfo
  export <array-type-description>, ole-array-type,
         as-typedesc, typedesc-dylan-class, typedesc-c-class;
  // - didn't used to be exported at all
  export <ole-type>;
  export <disp-id>;
  export $VT-VARIANT-MIN;
  export $VT-C-CLASSES, $VT-C-POINTER-CLASSES, $VT-DYLAN-CLASSES;
  export <LPDISPID>, <LPMEMBERID>, <LPLPDISPATCH>;
  export  set-typedesc,;
  export indexed-variant, vt-from-type;

end;
