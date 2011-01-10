module:    Dylan-user	
Synopsis:  Win32 API for "DDE"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-DDE
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-User;
  export Win32-DDE;
end;

define module Win32-DDE
  use Dylan;
  use C-FFI;
  use Win32-common,
    export: {// Accessors for which we need to add methods:
	     cb-value, cb-value-setter,
	     wFlags-value, wFlags-value-setter };
  use Win32-User,
    export: {// Accessors for which we need to add methods:
	     hwnd-value, hwnd-value-setter,
	     lParam-value, lParam-value-setter,
	     wParam-value, wParam-value-setter,
	     length-value, length-value-setter,
	     wFmt-value, wFmt-value-setter,
	     // Constants used in data structures here:
	     $CF-BITMAP, $CF-DIB, $CF-DIF, $CF-ENHMETAFILE, $CF-METAFILEPICT,
	     $CF-OEMTEXT, $CF-PALETTE, $CF-PENDATA, $CF-RIFF, $CF-SYLK,
	     $CF-TEXT, $CF-TIFF, $CF-WAVE, $CF-UNICODETEXT };
 

  // from "dde.h":
  export $WM-DDE-FIRST, $WM-DDE-INITIATE, $WM-DDE-TERMINATE,
	$WM-DDE-ADVISE, $WM-DDE-UNADVISE, $WM-DDE-ACK, $WM-DDE-DATA,
	$WM-DDE-REQUEST, $WM-DDE-POKE, $WM-DDE-EXECUTE, $WM-DDE-LAST;
  export bAppReturnCode-value, bAppReturnCode-value-setter,
	fBusy-value, fBusy-value-setter, fAck-value, fAck-value-setter,
	<DDEACK>, <LPDDEACK>;
  export fDeferUpd-value, fDeferUpd-value-setter, fAckReq-value,
	fAckReq-value-setter, cfFormat-value, cfFormat-value-setter,
	<DDEADVISE>, <LPDDEADVISE>;
  export fResponse-value, fResponse-value-setter, fRelease-value,
	fRelease-value-setter, fAckReq-value, fAckReq-value-setter,
	cfFormat-value, cfFormat-value-setter, Value-array,
	Value-array-setter, Value-value, <DDEDATA>, <LPDDEDATA>;
  export fRelease-value, fRelease-value-setter, cfFormat-value,
	cfFormat-value-setter, Value-array, Value-array-setter, Value-value,
	<DDEPOKE>, <LPDDEPOKE>;
  export fRelease-value, fRelease-value-setter, fDeferUpd-value,
	fDeferUpd-value-setter, fAckReq-value, fAckReq-value-setter,
	cfFormat-value, cfFormat-value-setter, <DDELN>, <LPDDELN>;
  export fAck-value, fAck-value-setter, fRelease-value,
	fRelease-value-setter, fAckReq-value, fAckReq-value-setter,
	cfFormat-value, cfFormat-value-setter, rgb-array, rgb-array-setter,
	rgb-value, <DDEUP>, <LPDDEUP>;
  export ImpersonateDdeClientWindow;
  export PackDDElParam, UnpackDDElParam, FreeDDElParam,
	ReuseDDElParam;

  // from "ddeml.h":
  export <HCONVLIST>, <HCONV>, <HSZ>, <HDDEDATA>;
  export hszSvc-value, hszSvc-value-setter, hszTopic-value,
	hszTopic-value-setter, <HSZPAIR>, <LPHSZPAIR>, <PHSZPAIR>;
  export wCountryID-value, wCountryID-value-setter, iCodePage-value,
	iCodePage-value-setter, dwLangID-value, dwLangID-value-setter,
	dwSecurity-value, dwSecurity-value-setter, qos-value,
	qos-value-setter, <CONVCONTEXT>, <LPCONVCONTEXT>, <PCONVCONTEXT>;
  export hUser-value, hUser-value-setter, hConvPartner-value,
	hConvPartner-value-setter, hszSvcPartner-value,
	hszSvcPartner-value-setter, hszServiceReq-value,
	hszServiceReq-value-setter, hszTopic-value, hszTopic-value-setter,
	hszItem-value, hszItem-value-setter, wType-value, wType-value-setter,
	wStatus-value, wStatus-value-setter, wConvst-value,
	wConvst-value-setter, wLastError-value, wLastError-value-setter,
	hConvList-value, hConvList-value-setter, ConvCtxt-value,
	ConvCtxt-value-setter, hwndPartner-value, hwndPartner-value-setter,
	<CONVINFO>, <LPCONVINFO>, <PCONVINFO>, $XST-NULL, $XST-INCOMPLETE,
	$XST-CONNECTED, $XST-INIT1, $XST-INIT2, $XST-REQSENT, $XST-DATARCVD,
	$XST-POKESENT, $XST-POKEACKRCVD, $XST-EXECSENT, $XST-EXECACKRCVD,
	$XST-ADVSENT, $XST-UNADVSENT, $XST-ADVACKRCVD, $XST-UNADVACKRCVD,
	$XST-ADVDATASENT, $XST-ADVDATAACKRCVD, $CADV-LATEACK, $ST-CONNECTED,
	$ST-ADVISE, $ST-ISLOCAL, $ST-BLOCKED, $ST-CLIENT, $ST-TERMINATED,
	$ST-INLIST, $ST-BLOCKNEXT, $ST-ISSELF, $DDE-FACK, $DDE-FBUSY,
	$DDE-FDEFERUPD, $DDE-FACKREQ, $DDE-FRELEASE, $DDE-FREQUESTED,
	$DDE-FAPPSTATUS, $DDE-FNOTPROCESSED, $DDE-FACKRESERVED,
	$DDE-FADVRESERVED, $DDE-FDATRESERVED, $DDE-FPOKRESERVED,
	$MSGF-DDEMGR, $CP-WINANSI, $CP-WINUNICODE, $CP-WINNEUTRAL,
	$XTYPF-NOBLOCK, $XTYPF-NODATA, $XTYPF-ACKREQ, $XCLASS-MASK,
	$XCLASS-BOOL, $XCLASS-DATA, $XCLASS-FLAGS, $XCLASS-NOTIFICATION,
	$XTYP-ERROR, $XTYP-ADVDATA, $XTYP-ADVREQ, $XTYP-ADVSTART,
	$XTYP-ADVSTOP, $XTYP-EXECUTE, $XTYP-CONNECT, $XTYP-CONNECT-CONFIRM,
	$XTYP-XACT-COMPLETE, $XTYP-POKE, $XTYP-REGISTER, $XTYP-REQUEST,
	$XTYP-DISCONNECT, $XTYP-UNREGISTER, $XTYP-WILDCONNECT, $XTYP-MASK,
	$XTYP-SHIFT, $TIMEOUT-ASYNC, $QID-SYNC, $SZDDESYS-TOPIC,
	$SZDDESYS-ITEM-TOPICS, $SZDDESYS-ITEM-SYSITEMS,
	$SZDDESYS-ITEM-RTNMSG, $SZDDESYS-ITEM-STATUS, $SZDDESYS-ITEM-FORMATS,
	$SZDDESYS-ITEM-HELP, $SZDDE-ITEM-ITEMLIST;
  export <PFNCALLBACK>, <PFNCALLBACK>-callback-wrapper, $CBR-BLOCK,
	DdeInitialize;
  export $CBF-FAIL-SELFCONNECTIONS, $CBF-FAIL-CONNECTIONS,
	$CBF-FAIL-ADVISES, $CBF-FAIL-EXECUTES, $CBF-FAIL-POKES,
	$CBF-FAIL-REQUESTS, $CBF-FAIL-ALLSVRXACTIONS,
	$CBF-SKIP-CONNECT-CONFIRMS, $CBF-SKIP-REGISTRATIONS,
	$CBF-SKIP-UNREGISTRATIONS, $CBF-SKIP-DISCONNECTS,
	$CBF-SKIP-ALLNOTIFICATIONS;
  export $APPCMD-CLIENTONLY, $APPCMD-FILTERINITS, $APPCMD-MASK;
  export $APPCLASS-STANDARD, $APPCLASS-MASK, DdeUninitialize;
  export DdeConnectList, DdeQueryNextServer, DdeDisconnectList;
  export DdeConnect, DdeDisconnect, DdeReconnect, DdeQueryConvInfo,
	DdeSetUserHandle, DdeAbandonTransaction;
  export DdePostAdvise, DdeEnableCallback, DdeImpersonateClient,
	$EC-ENABLEALL, $EC-ENABLEONE, $EC-DISABLE, $EC-QUERYWAITING,
	DdeNameService, $DNS-REGISTER, $DNS-UNREGISTER, $DNS-FILTERON,
	$DNS-FILTEROFF;
  export DdeClientTransaction;
  export DdeCreateDataHandle, DdeAddData, DdeGetData, DdeAccessData,
	DdeUnaccessData, DdeFreeDataHandle, $HDATA-APPOWNED, DdeGetLastError,
	$DMLERR-NO-ERROR, $DMLERR-FIRST, $DMLERR-ADVACKTIMEOUT, $DMLERR-BUSY,
	$DMLERR-DATAACKTIMEOUT, $DMLERR-DLL-NOT-INITIALIZED,
	$DMLERR-DLL-USAGE, $DMLERR-EXECACKTIMEOUT, $DMLERR-INVALIDPARAMETER,
	$DMLERR-LOW-MEMORY, $DMLERR-MEMORY-ERROR, $DMLERR-NOTPROCESSED,
	$DMLERR-NO-CONV-ESTABLISHED, $DMLERR-POKEACKTIMEOUT,
	$DMLERR-POSTMSG-FAILED, $DMLERR-REENTRANCY, $DMLERR-SERVER-DIED,
	$DMLERR-SYS-ERROR, $DMLERR-UNADVACKTIMEOUT, $DMLERR-UNFOUND-QUEUE-ID,
	$DMLERR-LAST, DdeCreateStringHandle, DdeQueryString,
	DdeFreeStringHandle, DdeKeepStringHandle, DdeCmpStringHandles;

  // from "winnt.h":
  export $SecurityAnonymous, $SecurityIdentification,
	$SecurityImpersonation, $SecurityDelegation;
  export <SECURITY-CONTEXT-TRACKING-MODE>;
  export ImpersonationLevel-value, ImpersonationLevel-value-setter,
	ContextTrackingMode-value, ContextTrackingMode-value-setter,
	EffectiveOnly-value, EffectiveOnly-value-setter,
	<SECURITY-QUALITY-OF-SERVICE>, <LPSECURITY-QUALITY-OF-SERVICE>,
	<PSECURITY-QUALITY-OF-SERVICE>;
  export $LT-DONT-CARE, $LT-LOWEST-LATENCY;

  // additional constants for convenience:
  export $NULL-HCONVLIST, $NULL-HCONV, $NULL-HSZ, $NULL-HDDEDATA;

end module Win32-DDE;
