module:    dylan-user	
Synopsis:  Win32 API for Windows HTML Help corresponding to
	   "HTMLHELP.H"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */



define library Win32-HTML-Help
  use Dylan;
  use C-FFI;
  use Win32-Common;
  use Win32-User;
  export Win32-HTML-Help;
end;

define module Win32-HTML-Help
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-User,
    exclude: {hdr-value,
	      hdr-value-setter,
	      pt-value,
	      pt-value-setter};
  export fReserved, fReserved-setter; // so it can be set to FALSE

  // from "htmlhelp.h":
  export $HH-DISPLAY-TOPIC, $HH-HELP-FINDER, $HH-DISPLAY-TOC,
	$HH-DISPLAY-INDEX, $HH-DISPLAY-SEARCH, $HH-SET-WIN-TYPE,
	$HH-GET-WIN-TYPE, $HH-GET-WIN-HANDLE, $HH-ENUM-INFO-TYPE,
	$HH-SET-INFO-TYPE, $HH-SYNC, $HH-ADD-NAV-UI, $HH-ADD-BUTTON,
	$HH-GETBROWSER-APP, $HH-KEYWORD-LOOKUP, $HH-DISPLAY-TEXT-POPUP,
	$HH-HELP-CONTEXT, $HH-TP-HELP-CONTEXTMENU, $HH-TP-HELP-WM-HELP,
	$HH-CLOSE-ALL, $HH-ALINK-LOOKUP, $HH-GET-LAST-ERROR,
	$HH-ENUM-CATEGORY, $HH-ENUM-CATEGORY-IT, $HH-RESET-IT-FILTER,
	$HH-SET-INCLUSIVE-FILTER, $HH-SET-EXCLUSIVE-FILTER, $HH-SET-GUID,
	$HH-INTERNAL, $HHWIN-PROP-ONTOP, $HHWIN-PROP-NOTITLEBAR,
	$HHWIN-PROP-NODEF-STYLES, $HHWIN-PROP-NODEF-EXSTYLES,
	$HHWIN-PROP-TRI-PANE, $HHWIN-PROP-NOTB-TEXT, $HHWIN-PROP-POST-QUIT,
	$HHWIN-PROP-AUTO-SYNC, $HHWIN-PROP-TRACKING, $HHWIN-PROP-TAB-SEARCH,
	$HHWIN-PROP-TAB-HISTORY, $HHWIN-PROP-TAB-BOOKMARKS,
	$HHWIN-PROP-CHANGE-TITLE, $HHWIN-PROP-NAV-ONLY-WIN,
	$HHWIN-PROP-NO-TOOLBAR, $HHWIN-PROP-MENU, $HHWIN-PROP-TAB-ADVSEARCH,
	$HHWIN-PROP-USER-POS, $HHWIN-PARAM-PROPERTIES, $HHWIN-PARAM-STYLES,
	$HHWIN-PARAM-EXSTYLES, $HHWIN-PARAM-RECT, $HHWIN-PARAM-NAV-WIDTH,
	$HHWIN-PARAM-SHOWSTATE, $HHWIN-PARAM-INFOTYPES,
	$HHWIN-PARAM-TB-FLAGS, $HHWIN-PARAM-EXPANSION, $HHWIN-PARAM-TABPOS,
	$HHWIN-PARAM-TABORDER, $HHWIN-PARAM-HISTORY-COUNT,
	$HHWIN-PARAM-CUR-TAB, $HHWIN-BUTTON-EXPAND, $HHWIN-BUTTON-BACK,
	$HHWIN-BUTTON-FORWARD, $HHWIN-BUTTON-STOP, $HHWIN-BUTTON-REFRESH,
	$HHWIN-BUTTON-HOME, $HHWIN-BUTTON-BROWSE-FWD,
	$HHWIN-BUTTON-BROWSE-BCK, $HHWIN-BUTTON-NOTES,
	$HHWIN-BUTTON-CONTENTS, $HHWIN-BUTTON-SYNC, $HHWIN-BUTTON-OPTIONS,
	$HHWIN-BUTTON-PRINT, $HHWIN-BUTTON-INDEX, $HHWIN-BUTTON-SEARCH,
	$HHWIN-BUTTON-HISTORY, $HHWIN-BUTTON-BOOKMARKS, $HHWIN-BUTTON-JUMP1,
	$HHWIN-BUTTON-JUMP2, $HHWIN-BUTTON-ZOOM, $HHWIN-BUTTON-TOC-NEXT,
	$HHWIN-BUTTON-TOC-PREV, $IDTB-EXPAND, $IDTB-CONTRACT, $IDTB-STOP,
	$IDTB-REFRESH, $IDTB-BACK, $IDTB-HOME, $IDTB-SYNC, $IDTB-PRINT,
	$IDTB-OPTIONS, $IDTB-FORWARD, $IDTB-NOTES, $IDTB-BROWSE-FWD,
	$IDTB-BROWSE-BACK, $IDTB-CONTENTS, $IDTB-INDEX, $IDTB-SEARCH,
	$IDTB-HISTORY, $IDTB-BOOKMARKS, $IDTB-JUMP1, $IDTB-JUMP2,
	$IDTB-CUSTOMIZE, $IDTB-ZOOM, $IDTB-TOC-NEXT, $IDTB-TOC-PREV,
	hdr-value, hdr-value-setter, pszUrl-value, pszUrl-value-setter,
	<HHN-NOTIFY>, <LPHHN-NOTIFY>, cbStruct-value, cbStruct-value-setter,
	hinst-value, hinst-value-setter, idString-value,
	idString-value-setter, pszText-value, pszText-value-setter, pt-value,
	pt-value-setter, clrForeground-value, clrForeground-value-setter,
	clrBackground-value, clrBackground-value-setter, rcMargins-value,
	rcMargins-value-setter, pszFont-value, pszFont-value-setter,
	<HH-POPUP>, <LPHH-POPUP>, cbStruct-value, cbStruct-value-setter,
	pszKeywords-value, pszKeywords-value-setter, pszUrl-value,
	pszUrl-value-setter, pszMsgText-value, pszMsgText-value-setter,
	pszMsgTitle-value, pszMsgTitle-value-setter, pszWindow-value,
	pszWindow-value-setter, fIndexOnFail-value,
	fIndexOnFail-value-setter, <HH-AKLINK>, <LPHH-AKLINK>,
	cbStruct-value, cbStruct-value-setter, iType-value,
	iType-value-setter, pszCatName-value, pszCatName-value-setter,
	pszITName-value, pszITName-value-setter, pszITDescription-value,
	pszITDescription-value-setter, <HH-ENUM-IT>, <LPHH-ENUM-IT>,
	<PHH-ENUM-IT>, cbStruct-value, cbStruct-value-setter,
	pszCatName-value, pszCatName-value-setter, pszCatDescription-value,
	pszCatDescription-value-setter, <HH-ENUM-CAT>, <LPHH-ENUM-CAT>,
	<PHH-ENUM-CAT>, cbStruct-value, cbStruct-value-setter,
	pszCatName-value, pszCatName-value-setter, pszInfoTypeName-value,
	pszInfoTypeName-value-setter, <HH-SET-INFOTYPE>, <LPHH-SET-INFOTYPE>,
	<PHH-SET-INFOTYPE>, <HH-INFOTYPE>, <PHH-INFOTYPE>, $HH-MAX-TABS,
	$HH-FTS-DEFAULT-PROXIMITY, cbStruct-value, cbStruct-value-setter,
	fUniCodeStrings-value, fUniCodeStrings-value-setter,
	pszSearchQuery-value, pszSearchQuery-value-setter, iProximity-value,
	iProximity-value-setter, fStemmedSearch-value,
	fStemmedSearch-value-setter, fTitleOnly-value,
	fTitleOnly-value-setter, fExecute-value, fExecute-value-setter,
	pszWindow-value, pszWindow-value-setter, <HH-FTS-QUERY>,
	<LPHH-FTS-QUERY>, cbStruct-value, cbStruct-value-setter,
	fUniCodeStrings-value, fUniCodeStrings-value-setter, pszType-value,
	pszType-value-setter, fsValidMembers-value,
	fsValidMembers-value-setter, fsWinProperties-value,
	fsWinProperties-value-setter, pszCaption-value,
	pszCaption-value-setter, dwStyles-value, dwStyles-value-setter,
	dwExStyles-value, dwExStyles-value-setter, rcWindowPos-value,
	rcWindowPos-value-setter, nShowState-value, nShowState-value-setter,
	hwndHelp-value, hwndHelp-value-setter, hwndCaller-value,
	hwndCaller-value-setter, paInfoTypes-value, paInfoTypes-value-setter,
	hwndToolBar-value, hwndToolBar-value-setter, hwndNavigation-value,
	hwndNavigation-value-setter, hwndHTML-value, hwndHTML-value-setter,
	iNavWidth-value, iNavWidth-value-setter, rcHTML-value,
	rcHTML-value-setter, pszToc-value, pszToc-value-setter,
	pszIndex-value, pszIndex-value-setter, pszFile-value,
	pszFile-value-setter, pszHome-value, pszHome-value-setter,
	fsToolBarFlags-value, fsToolBarFlags-value-setter,
	fNotExpanded-value, fNotExpanded-value-setter, curNavType-value,
	curNavType-value-setter, tabpos-value, tabpos-value-setter,
	idNotify-value, idNotify-value-setter, tabOrder-array,
	tabOrder-array-setter, tabOrder-value, cHistory-value,
	cHistory-value-setter, pszJump1-value, pszJump1-value-setter,
	pszJump2-value, pszJump2-value-setter, pszUrlJump1-value,
	pszUrlJump1-value-setter, pszUrlJump2-value,
	pszUrlJump2-value-setter, rcMinSize-value, rcMinSize-value-setter,
	cbInfoTypes-value, cbInfoTypes-value-setter, <HH-WINTYPE>,
	<LPHH-WINTYPE>, <PHH-WINTYPE>, hdr-value, hdr-value-setter,
	pszCurUrl-value, pszCurUrl-value-setter, idAction-value,
	idAction-value-setter, phhWinType-value, phhWinType-value-setter,
	<HHNTRACK>, <LPHHNTRACK>, HtmlHelp;

end module Win32-HTML-Help;
