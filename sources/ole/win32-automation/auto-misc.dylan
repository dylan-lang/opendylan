Module:    OLE-Automation
Synopsis:  FFI for assorted types, constants, and functions.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Use an imitation of Collage to pick up some miscellaneous pieces of the
// interface that don't need special treatment. 

define interface auto-misc
 #include: { "wtypes.h", "oleauto.h", "oaidl.h" };
 import: { 
	// structures used in member function arguments:
	DISPPARAMS, EXCEPINFO, BINDPTR, TYPEATTR, TLIBATTR,
	TYPEDESC, ELEMDESC, ARRAYDESC, CUSTDATA, CUSTDATAITEM,

	// structure pointers used in member function arguments:
	LPCUSTDATA, LPCUSTDATAITEM,

	// structures used in misc. functions:
	SAFEARRAY, SAFEARRAYBOUND, 

	// types needed:
	BSTR, SYSKIND, HREFTYPE, TYPEKIND, FUNCDESC, VARDESC,
	IDLDESC, DISPID, MEMBERID, INVOKEKIND, FUNCKIND, LPBSTR,
	DESCKIND, CALLCONV, VARKIND, VARENUM, VARTYPE,

	// pointer types needed internally:
	LPBINDPTR,

	// enumeration elements:
	VT_*,

	// enumeration types whose elements are needed:
	VARENUM, TYPEKIND, CALLCONV, FUNCKIND, INVOKEKIND, VARKIND,
	TYPEFLAGS, FUNCFLAGS, VARFLAGS, DESCKIND, SYSKIND, LIBFLAGS,

	// other constants documented for users:
	DISPATCH_METHOD, DISPATCH_PROPERTY*, IDLFLAG_*, DISPID_*,
	VARIANT_TRUE, VARIANT_FALSE, VARIANT_NOVALUEPROP,
        FADF_*, MEMBERID_NIL, IMPLTYPEFLAG_*,
 
	// assorted non-member functions
	SysAllocString, SysReAllocString, SysAllocStringLen,
	SysReAllocStringLen, SysFreeString, SysStringLen, SysStringByteLen,
	SysAllocStringByteLen, SafeArrayAllocDescriptor,
	SafeArrayAllocData, SafeArrayCreate, SafeArrayDestroyDescriptor,
	SafeArrayDestroyData, SafeArrayDestroy, SafeArrayRedim,
	SafeArrayGetDim, SafeArrayGetElemsize, SafeArrayGetUBound,
	SafeArrayGetLBound, SafeArrayLock, SafeArrayUnlock,
	SafeArrayAccessData, SafeArrayUnaccessData, SafeArrayGetElement,
	SafeArrayPutElement, SafeArrayCopy, SafeArrayPtrOfIndex,
	VariantInit, VariantClear, VariantCopy, VariantCopyInd,
	VariantChangeType, VariantChangeTypeEx,
	DosDateTimeToVariantTime, VariantTimeToDosDateTime, 

   /*	// low-level functions that probably aren't needed directly.
	VarUI1FromI2, VarUI1FromI4,
	VarUI1FromR4, VarUI1FromR8, VarUI1FromCy, VarUI1FromDate,
	VarUI1FromStr, VarUI1FromDisp, VarUI1FromBool, VarI2FromUI1,
	VarI2FromI4, VarI2FromR4, VarI2FromR8, VarI2FromCy, VarI2FromDate,
	VarI2FromStr, VarI2FromDisp, VarI2FromBool, VarI4FromUI1,
	VarI4FromI2, VarI4FromR4, VarI4FromR8, VarI4FromCy, VarI4FromDate,
	VarI4FromStr, VarI4FromDisp, VarI4FromBool, VarR4FromUI1,
	VarR4FromI2, VarR4FromI4, VarR4FromR8, VarR4FromCy, VarR4FromDate,
	VarR4FromStr, VarR4FromDisp, VarR4FromBool, VarR8FromUI1,
	VarR8FromI2, VarR8FromI4, VarR8FromR4, VarR8FromCy, VarR8FromDate,
	VarR8FromStr, VarR8FromDisp, VarR8FromBool, VarDateFromUI1,
	VarDateFromI2, VarDateFromI4, VarDateFromR4, VarDateFromR8,
	VarDateFromCy, VarDateFromStr, VarDateFromDisp, VarDateFromBool,
	VarCyFromUI1, VarCyFromI2, VarCyFromI4, VarCyFromR4, VarCyFromR8,
	VarCyFromDate, VarCyFromStr, VarCyFromDisp, VarCyFromBool,
	VarBstrFromUI1, VarBstrFromI2, VarBstrFromI4, VarBstrFromR4,
	VarBstrFromR8, VarBstrFromCy, VarBstrFromDate, VarBstrFromDisp,
	VarBstrFromBool, VarBoolFromUI1, VarBoolFromI2, VarBoolFromI4,
	VarBoolFromR4, VarBoolFromR8, VarBoolFromDate, VarBoolFromCy,
	VarBoolFromStr, VarBoolFromDisp,
   */

	LHashValOfNameSysA,
	LHashValOfNameSys, LoadTypeLib, LoadRegTypeLib,
	QueryPathOfRegTypeLib, RegisterTypeLib, 
	CreateTypeLib, DispGetParam, DispGetIDsOfNames, DispInvoke,
	CreateStdDispatch, RegisterActiveObject,
	RevokeActiveObject, GetActiveObject, SetErrorInfo, GetErrorInfo,
	CreateErrorInfo

	// more data types and constants new in the 1997 headers:
        PARAMDESCEX, LPPARAMDESCEX, PARAMDESC, LPPARAMDESC,
	PARAMFLAG_*,

	// undocumented:
	//	OaBuildVersion

	// obsolete:
	// INTERFACEDATA, METHODDATA, PARAMDATA, CreateDispTypeInfo

	// declared in "oleauto.h" but undocumented and unimplemented:
	//	DeregisterTypeLib

    };
  exclude: {
	// The following functions are not defined in Windows 95:
	BSTR_UserFree, BSTR_UserMarshal, BSTR_UserSize, BSTR_UserUnmarshal,
	BstrFromVector,
	ClearCustData, CreateTypeLib2,
	DispCallFunc, DllRegisterServer, DllUnregisterServer,
	GetAltMonthNames,
	LPSAFEARRAY_Marshal, LPSAFEARRAY_Size, LPSAFEARRAY_Unmarshal,
	LPSAFEARRAY_UserFree, LPSAFEARRAY_UserMarshal,
	LPSAFEARRAY_UserSize, LPSAFEARRAY_UserUnmarshal, LoadTypeLibEx,
	OACreateTypeLib2, OleCreateFontIndirect, OleCreatePictureIndirect,
	OleCreatePropertyFrame, OleCreatePropertyFrameIndirect,
	OleIconToCursor, OleLoadPicture, OleLoadPictureFile,
	OleLoadPicturePath, OleSavePictureFile, OleTranslateColor,
	SafeArrayCopyData, SafeArrayCreateVector, SystemTimeToVariantTime,
	UnRegisterTypeLib, UserBSTR_free_inst, UserBSTR_free_local,
	UserBSTR_from_local, UserBSTR_to_local, UserEXCEPINFO_free_inst,
	UserEXCEPINFO_free_local, UserEXCEPINFO_from_local,
	UserEXCEPINFO_to_local, UserHWND_free_inst, UserHWND_free_local,
	UserHWND_from_local, UserHWND_to_local, UserMSG_free_inst,
	UserMSG_free_local, UserMSG_from_local, UserMSG_to_local,
	UserVARIANT_free_inst, UserVARIANT_free_local,
	UserVARIANT_from_local, UserVARIANT_to_local,
	VARIANT_UserFree, VARIANT_UserMarshal, VARIANT_UserSize,
	VARIANT_UserUnmarshal, VarBoolFromDec, VarBoolFromI1,
	VarBoolFromUI2, VarBoolFromUI4, VarBstrFromDec, VarBstrFromI1,
	VarBstrFromUI2, VarBstrFromUI4, VarCyFromDec, VarCyFromI1,
	VarCyFromUI2, VarCyFromUI4, VarDateFromDec, VarDateFromI1,
	VarDateFromUI2, VarDateFromUI4, VarDateFromUdate, VarDecFromBool,
	VarDecFromCy, VarDecFromDate, VarDecFromDisp, VarDecFromI1,
	VarDecFromI2, VarDecFromI4, VarDecFromR4, VarDecFromR8,
	VarDecFromStr, VarDecFromUI1, VarDecFromUI2, VarDecFromUI4,
	VarI1FromBool, VarI1FromCy, VarI1FromDate, VarI1FromDec,
	VarI1FromDisp, VarI1FromI2, VarI1FromI4, VarI1FromR4, VarI1FromR8,
	VarI1FromStr, VarI1FromUI1, VarI1FromUI2, VarI1FromUI4,
	VarI2FromDec, VarI2FromI1, VarI2FromUI2, VarI2FromUI4,
	VarI4FromDec, VarI4FromI1, VarI4FromUI2, VarI4FromUI4,
	VarNumFromParseNum, VarParseNumFromStr, VarR4FromDec, VarR4FromI1,
	VarR4FromUI2, VarR4FromUI4, VarR8FromDec, VarR8FromI1,
	VarR8FromUI2, VarR8FromUI4, VarUI1FromDec, VarUI1FromI1,
	VarUI1FromUI2, VarUI1FromUI4, VarUI2FromBool, VarUI2FromCy,
	VarUI2FromDate, VarUI2FromDec, VarUI2FromDisp, VarUI2FromI1,
	VarUI2FromI2, VarUI2FromI4, VarUI2FromR4, VarUI2FromR8,
	VarUI2FromStr, VarUI2FromUI1, VarUI2FromUI4, VarUI4FromBool,
	VarUI4FromCy, VarUI4FromDate, VarUI4FromDec, VarUI4FromDisp,
	VarUI4FromI1, VarUI4FromI2, VarUI4FromI4, VarUI4FromR4,
	VarUI4FromR8, VarUI4FromStr, VarUI4FromUI1, VarUI4FromUI2,
	VarUdateFromDate, VariantTimeToSystemTime, VectorFromBstr,
	// The following types are only used in the functions above.
	UDATE, LPUDATE, NUMPARSE, LPNUMPARSE
    }; end interface;
 
