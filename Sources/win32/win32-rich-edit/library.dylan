module:    Dylan-user	
Synopsis:  Win32 API for "rich edit" controls in "RICHEDIT.H", "RICHED32.DLL"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


// Note: there isn't any "RICHED32.LIB"; the user needs to 
//       do an explicit dynamic load of "RICHED32.DLL".

define library Win32-Rich-Edit
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-User;
  use Win32-GDI;
  export Win32-Rich-Edit;
end;

define module Win32-Rich-Edit
  use Dylan;
  use C-FFI;
  use Win32-common,
    export: {// this library adds methods to these generic functions:
	     cb-value, cb-value-setter, flags-value, flags-value-setter };
  use Win32-User,
    export: {$EM-SCROLLCARET, $WM-CONTEXTMENU, $WM-NOTIFY,
	     <NMHDR>, <LPNMHDR>, hwndFrom-value, idFrom-value, code-value,
	     hwndFrom-value-setter, idFrom-value-setter, code-value-setter,
	     // this library adds methods to these generic functions:
	     cbSize-value, cbSize-value-setter,
	     cch-value, cch-value-setter,
	     hdc-value, hdc-value-setter,
	     lParam-value, lParam-value-setter,
	     wParam-value, wParam-value-setter };

  use Win32-GDI, import: { $LF-FACESIZE };


  // from "richedit.h":
  export $cchTextLimitDefault, $RICHEDIT-CLASSA, $RICHEDIT-CLASS10A,
	$RICHEDIT-CLASSW, $RICHEDIT-CLASS, $EM-CANPASTE, $EM-DISPLAYBAND,
	$EM-EXGETSEL, $EM-EXLIMITTEXT, $EM-EXLINEFROMCHAR, $EM-EXSETSEL,
	$EM-FINDTEXT, $EM-FORMATRANGE, $EM-GETCHARFORMAT, $EM-GETEVENTMASK,
	$EM-GETOLEINTERFACE, $EM-GETPARAFORMAT, $EM-GETSELTEXT,
	$EM-HIDESELECTION, $EM-PASTESPECIAL, $EM-REQUESTRESIZE,
	$EM-SELECTIONTYPE, $EM-SETBKGNDCOLOR, $EM-SETCHARFORMAT,
	$EM-SETEVENTMASK, $EM-SETOLECALLBACK, $EM-SETPARAFORMAT,
	$EM-SETTARGETDEVICE, $EM-STREAMIN, $EM-STREAMOUT, $EM-GETTEXTRANGE,
	$EM-FINDWORDBREAK, $EM-SETOPTIONS, $EM-GETOPTIONS, $EM-FINDTEXTEX,
	$EM-GETWORDBREAKPROCEX, $EM-SETWORDBREAKPROCEX, $EM-SETUNDOLIMIT,
	$EM-REDO, $EM-CANREDO, $EM-GETUNDONAME, $EM-GETREDONAME,
	$EM-STOPGROUPTYPING, $EM-SETTEXTMODE, $EM-GETTEXTMODE, $TM-PLAINTEXT,
	$TM-RICHTEXT, $TM-SINGLELEVELUNDO, $TM-MULTILEVELUNDO,
	$TM-SINGLECODEPAGE, $TM-MULTICODEPAGE, $EM-AUTOURLDETECT,
	$EM-GETAUTOURLDETECT, $EM-SETPALETTE, $EM-GETTEXTEX,
	$EM-GETTEXTLENGTHEX, $EM-SETPUNCTUATION, $EM-GETPUNCTUATION,
	$EM-SETWORDWRAPMODE, $EM-GETWORDWRAPMODE, $EM-SETIMECOLOR,
	$EM-GETIMECOLOR, $EM-SETIMEOPTIONS, $EM-GETIMEOPTIONS,
	$EM-CONVPOSITION, $EM-SETLANGOPTIONS, $EM-GETLANGOPTIONS,
	$EM-GETIMECOMPMODE, $EM-FINDTEXTW, $EM-FINDTEXTEXW,
	$EM-SETBIDIOPTIONS, $EM-GETBIDIOPTIONS, $IMF-AUTOKEYBOARD,
	$IMF-AUTOFONT, $IMF-IMECANCELCOMPLETE, $IMF-IMEALWAYSSENDNOTIFY,
	$ICM-NOTOPEN, $ICM-LEVEL3, $ICM-LEVEL2, $ICM-LEVEL2-5,
	$ICM-LEVEL2-SUI, $EN-MSGFILTER, $EN-REQUESTRESIZE, $EN-SELCHANGE,
	$EN-DROPFILES, $EN-PROTECTED, $EN-CORRECTTEXT, $EN-STOPNOUNDO,
	$EN-IMECHANGE, $EN-SAVECLIPBOARD, $EN-OLEOPFAILED,
	$EN-OBJECTPOSITIONS, $EN-LINK, $EN-DRAGDROPDONE, $EN-ALIGN-LTR,
	$EN-ALIGN-RTL, $ENM-NONE, $ENM-CHANGE, $ENM-UPDATE, $ENM-SCROLL,
	$ENM-KEYEVENTS, $ENM-MOUSEEVENTS, $ENM-REQUESTRESIZE, $ENM-SELCHANGE,
	$ENM-DROPFILES, $ENM-PROTECTED, $ENM-CORRECTTEXT, $ENM-SCROLLEVENTS,
	$ENM-DRAGDROPDONE, $ENM-IMECHANGE, $ENM-LANGCHANGE,
	$ENM-OBJECTPOSITIONS, $ENM-LINK, $ES-SAVESEL, $ES-SUNKEN,
	$ES-DISABLENOSCROLL, $ES-SELECTIONBAR, $ES-NOOLEDRAGDROP,
	$ES-EX-NOCALLOLEINIT, $ES-VERTICAL, $ES-NOIME, $ES-SELFIME,
	$ECO-AUTOWORDSELECTION, $ECO-AUTOVSCROLL, $ECO-AUTOHSCROLL,
	$ECO-NOHIDESEL, $ECO-READONLY, $ECO-WANTRETURN, $ECO-SAVESEL,
	$ECO-SELECTIONBAR, $ECO-VERTICAL, $ECOOP-SET, $ECOOP-OR, $ECOOP-AND,
	$ECOOP-XOR, $WB-CLASSIFY, $WB-MOVEWORDLEFT, $WB-MOVEWORDRIGHT,
	$WB-LEFTBREAK, $WB-RIGHTBREAK, $WB-MOVEWORDPREV, $WB-MOVEWORDNEXT,
	$WB-PREVBREAK, $WB-NEXTBREAK, $PC-FOLLOWING, $PC-LEADING,
	$PC-OVERFLOW, $PC-DELIMITER, $WBF-WORDWRAP, $WBF-WORDBREAK,
	$WBF-OVERFLOW, $WBF-LEVEL1, $WBF-LEVEL2, $WBF-CUSTOM, $IMF-FORCENONE,
	$IMF-FORCEENABLE, $IMF-FORCEDISABLE, $IMF-CLOSESTATUSWINDOW,
	$IMF-VERTICAL, $IMF-FORCEACTIVE, $IMF-FORCEINACTIVE,
	$IMF-FORCEREMEMBER, $IMF-MULTIPLEEDIT, $WBF-CLASS, $WBF-ISWHITE,
	$WBF-BREAKLINE, $WBF-BREAKAFTER, <EDITWORDBREAKPROCEX>,
	<EDITWORDBREAKPROCEX>-callback-wrapper, dwMask-value,
	dwMask-value-setter, dwEffects-value, dwEffects-value-setter,
	yHeight-value, yHeight-value-setter, yOffset-value,
	yOffset-value-setter, crTextColor-value, crTextColor-value-setter,
	bCharSet-value, bCharSet-value-setter, bPitchAndFamily-value,
	bPitchAndFamily-value-setter, szFaceName-array,
	szFaceName-array-setter, szFaceName-value, <CHARFORMATA>,
	<LPCHARFORMATA>;
  export <CHARFORMAT>, <LPCHARFORMAT>, $CFM-BOLD, $CFM-ITALIC,
	$CFM-UNDERLINE, $CFM-STRIKEOUT, $CFM-PROTECTED, $CFM-LINK, $CFM-SIZE,
	$CFM-COLOR, $CFM-FACE, $CFM-OFFSET, $CFM-CHARSET, $CFE-BOLD,
	$CFE-ITALIC, $CFE-UNDERLINE, $CFE-STRIKEOUT, $CFE-PROTECTED,
	$CFE-LINK, $CFE-AUTOCOLOR, $yHeightCharPtsMost, $SCF-SELECTION,
	$SCF-WORD, $SCF-DEFAULT, $SCF-ALL, $SCF-USEUIRULES;
  export cpMin-value, cpMin-value-setter, cpMax-value,
	cpMax-value-setter, <CHARRANGE>, <LPCHARRANGE>;
  export chrg-value, chrg-value-setter, lpstrText-value,
	lpstrText-value-setter, <TEXTRANGEA>, <LPTEXTRANGEA>;
  export <TEXTRANGE>, <LPTEXTRANGE>;
  export <EDITSTREAMCALLBACK>, <EDITSTREAMCALLBACK>-callback-wrapper;
  export dwCookie-value, dwCookie-value-setter, dwError-value,
	dwError-value-setter, pfnCallback-value, pfnCallback-value-setter,
	<EDITSTREAM>, <LPEDITSTREAM>, $SF-TEXT, $SF-RTF, $SF-RTFNOOBJS,
	$SF-TEXTIZED, $SF-UNICODE, $SFF-SELECTION, $SFF-PLAINRTF, chrg-value,
	chrg-value-setter, lpstrText-value, lpstrText-value-setter,
	<FINDTEXTA>, <LPFINDTEXTA>;
  export <FINDTEXT>, <LPFINDTEXT>;
  export chrg-value, chrg-value-setter, lpstrText-value,
	lpstrText-value-setter, chrgText-value, chrgText-value-setter,
	<FINDTEXTEXA>, <LPFINDTEXTEXA>;
  export <FINDTEXTEX>, <LPFINDTEXTEX>;
  export hdcTarget-value, hdcTarget-value-setter, rc-value,
	rc-value-setter, rcPage-value, rcPage-value-setter, chrg-value,
	chrg-value-setter, <FORMATRANGE>, <LPFORMATRANGE>, $MAX-TAB-STOPS,
	$lDefaultTab, dwMask-value, dwMask-value-setter, wNumbering-value,
	wNumbering-value-setter, dxStartIndent-value,
	dxStartIndent-value-setter, dxRightIndent-value,
	dxRightIndent-value-setter, dxOffset-value, dxOffset-value-setter,
	wAlignment-value, wAlignment-value-setter, cTabCount-value,
	cTabCount-value-setter, rgxTabs-array, rgxTabs-array-setter,
	rgxTabs-value, <PARAFORMAT>, <LPPARAFORMAT>, $PFM-STARTINDENT,
	$PFM-RIGHTINDENT, $PFM-OFFSET, $PFM-ALIGNMENT, $PFM-TABSTOPS,
	$PFM-NUMBERING, $PFM-OFFSETINDENT, $PFN-BULLET, $PFA-LEFT,
	$PFA-RIGHT, $PFA-CENTER;
  export dwMask-value, dwMask-value-setter, dwEffects-value,
	dwEffects-value-setter, yHeight-value, yHeight-value-setter,
	yOffset-value, yOffset-value-setter, crTextColor-value,
	crTextColor-value-setter, bCharSet-value, bCharSet-value-setter,
	bPitchAndFamily-value, bPitchAndFamily-value-setter,
	szFaceName-array, szFaceName-array-setter, szFaceName-value,
	wWeight-value, wWeight-value-setter, sSpacing-value,
	sSpacing-value-setter, crBackColor-value, crBackColor-value-setter,
	lcid-value, lcid-value-setter, sStyle-value, sStyle-value-setter,
	wKerning-value, wKerning-value-setter, bUnderlineType-value,
	bUnderlineType-value-setter, bAnimation-value,
	bAnimation-value-setter, bRevAuthor-value, bRevAuthor-value-setter,
	<CHARFORMAT2W>, <LPCHARFORMAT2W>;
  export dwMask-value, dwMask-value-setter, dwEffects-value,
	dwEffects-value-setter, yHeight-value, yHeight-value-setter,
	yOffset-value, yOffset-value-setter, crTextColor-value,
	crTextColor-value-setter, bCharSet-value, bCharSet-value-setter,
	bPitchAndFamily-value, bPitchAndFamily-value-setter,
	szFaceName-array, szFaceName-array-setter, szFaceName-value,
	wWeight-value, wWeight-value-setter, sSpacing-value,
	sSpacing-value-setter, crBackColor-value, crBackColor-value-setter,
	lcid-value, lcid-value-setter, sStyle-value, sStyle-value-setter,
	wKerning-value, wKerning-value-setter, bUnderlineType-value,
	bUnderlineType-value-setter, bAnimation-value,
	bAnimation-value-setter, bRevAuthor-value, bRevAuthor-value-setter,
	<CHARFORMAT2A>, <LPCHARFORMAT2A>, $CFM-EFFECTS, $CFM-ALL, $PFM-ALL,
	$CFM-SMALLCAPS, $CFM-ALLCAPS, $CFM-HIDDEN, $CFM-OUTLINE, $CFM-SHADOW,
	$CFM-EMBOSS, $CFM-IMPRINT, $CFM-DISABLED, $CFM-REVISED,
	$CFM-BACKCOLOR, $CFM-LCID, $CFM-UNDERLINETYPE, $CFM-WEIGHT,
	$CFM-SPACING, $CFM-KERNING, $CFM-STYLE, $CFM-ANIMATION,
	$CFM-REVAUTHOR, $CFE-SUBSCRIPT, $CFE-SUPERSCRIPT, $CFM-SUBSCRIPT,
	$CFM-SUPERSCRIPT, $CFM-EFFECTS2, $CFM-ALL2, $CFE-SMALLCAPS,
	$CFE-ALLCAPS, $CFE-HIDDEN, $CFE-OUTLINE, $CFE-SHADOW, $CFE-EMBOSS,
	$CFE-IMPRINT, $CFE-DISABLED, $CFE-REVISED, $CFE-AUTOBACKCOLOR,
	$CFU-CF1UNDERLINE, $CFU-INVERT, $CFU-UNDERLINEDOTTED,
	$CFU-UNDERLINEDOUBLE, $CFU-UNDERLINEWORD, $CFU-UNDERLINE,
	$CFU-UNDERLINENONE;
  export dwMask-value, dwMask-value-setter, wNumbering-value,
	wNumbering-value-setter, dxStartIndent-value,
	dxStartIndent-value-setter, dxRightIndent-value,
	dxRightIndent-value-setter, dxOffset-value, dxOffset-value-setter,
	wAlignment-value, wAlignment-value-setter, cTabCount-value,
	cTabCount-value-setter, rgxTabs-array, rgxTabs-array-setter,
	rgxTabs-value, dySpaceBefore-value, dySpaceBefore-value-setter,
	dySpaceAfter-value, dySpaceAfter-value-setter, dyLineSpacing-value,
	dyLineSpacing-value-setter, sStyle-value, sStyle-value-setter,
	bLineSpacingRule-value, bLineSpacingRule-value-setter, bCRC-value,
	bCRC-value-setter, wShadingWeight-value, wShadingWeight-value-setter,
	wShadingStyle-value, wShadingStyle-value-setter,
	wNumberingStart-value, wNumberingStart-value-setter,
	wNumberingStyle-value, wNumberingStyle-value-setter,
	wNumberingTab-value, wNumberingTab-value-setter, wBorderSpace-value,
	wBorderSpace-value-setter, wBorderWidth-value,
	wBorderWidth-value-setter, wBorders-value, wBorders-value-setter,
	<PARAFORMAT2>, <LPPARAFORMAT2>, $PFM-SPACEBEFORE, $PFM-SPACEAFTER,
	$PFM-LINESPACING, $PFM-STYLE, $PFM-BORDER, $PFM-SHADING,
	$PFM-NUMBERINGSTYLE, $PFM-NUMBERINGTAB, $PFM-NUMBERINGSTART,
	$PFM-DIR, $PFM-RTLPARA, $PFM-KEEP, $PFM-KEEPNEXT,
	$PFM-PAGEBREAKBEFORE, $PFM-NOLINENUMBER, $PFM-NOWIDOWCONTROL,
	$PFM-DONOTHYPHEN, $PFM-SIDEBYSIDE, $PFM-TABLE, $PFM-EFFECTS,
	$PFM-ALL2, $PFE-TABLEROW, $PFE-TABLECELLEND, $PFE-TABLECELL;
  export nmhdr-value, nmhdr-value-setter, msg-value, msg-value-setter,
	<MSGFILTER>, <LPMSGFILTER>;
  export nmhdr-value, nmhdr-value-setter, rc-value, rc-value-setter,
	<REQRESIZE>, <LPREQRESIZE>;
  export nmhdr-value, nmhdr-value-setter, chrg-value,
	chrg-value-setter, seltyp-value, seltyp-value-setter, <SELCHANGE>,
	<LPSELCHANGE>, $SEL-EMPTY, $SEL-TEXT, $SEL-OBJECT, $SEL-MULTICHAR,
	$SEL-MULTIOBJECT, $GCM-RIGHTMOUSEDROP, nmhdr-value,
	nmhdr-value-setter, hDrop-value, hDrop-value-setter, cp-value,
	cp-value-setter, fProtected-value, fProtected-value-setter,
	<ENDROPFILES>, <LPENDROPFILES>;
  export nmhdr-value, nmhdr-value-setter, msg-value, msg-value-setter,
	chrg-value, chrg-value-setter, <ENPROTECTED>, <LPENPROTECTED>;
  export nmhdr-value, nmhdr-value-setter, cObjectCount-value,
	cObjectCount-value-setter, <ENSAVECLIPBOARD>, <LPENSAVECLIPBOARD>,
	nmhdr-value, nmhdr-value-setter, iob-value, iob-value-setter,
	lOper-value, lOper-value-setter, hr-value, hr-value-setter,
	<ENOLEOPFAILED>, <LPENOLEOPFAILED>, $OLEOP-DOVERB, nmhdr-value,
	nmhdr-value-setter, cObjectCount-value, cObjectCount-value-setter,
	pcpPositions-value, pcpPositions-value-setter, <OBJECTPOSITIONS>,
	<LPOBJECTPOSITIONS>;
  export nmhdr-value, nmhdr-value-setter, msg-value, msg-value-setter,
	chrg-value, chrg-value-setter, <ENLINK>, <LPENLINK>, nmhdr-value,
	nmhdr-value-setter, chrg-value, chrg-value-setter, seltyp-value,
	seltyp-value-setter, <ENCORRECTTEXT>, <LPENCORRECTTEXT>, iSize-value,
	iSize-value-setter, szPunctuation-value, szPunctuation-value-setter,
	<PUNCTUATION>, <LPPUNCTUATION>, crText-value, crText-value-setter,
	crBackground-value, crBackground-value-setter, dwEffects-value,
	dwEffects-value-setter, <COMPCOLOR>, <LPCOMPCOLOR>;
  export $CF-RTF, $CF-RTFNOOBJS, $CF-RETEXTOBJ, dwAspect-value,
	dwAspect-value-setter, dwParam-value, dwParam-value-setter,
	<REPASTESPECIAL>, <LPREPASTESPECIAL>, $UID-UNKNOWN, $UID-TYPING,
	$UID-DELETE, $UID-DRAGDROP, $UID-CUT, $UID-PASTE, $GT-DEFAULT,
	$GT-USECRLF, codepage-value, codepage-value-setter,
	lpDefaultChar-value, lpDefaultChar-value-setter, lpUsedDefChar-value,
	lpUsedDefChar-value-setter, <GETTEXTEX>, <LPGETTEXTEX>, $GTL-DEFAULT,
	$GTL-USECRLF, $GTL-PRECISE, $GTL-CLOSE, $GTL-NUMCHARS, $GTL-NUMBYTES,
	codepage-value, codepage-value-setter, <GETTEXTLENGTHEX>,
	<LPGETTEXTLENGTHEX>, wMask-value, wMask-value-setter, wEffects-value,
	wEffects-value-setter, <BIDIOPTIONS>, <LPBIDIOPTIONS>,
	$BOM-DEFPARADIR, $BOM-PLAINTEXT, $BOM-NEUTRALOVERRIDE,
	$BOM-CONTEXTREADING, $BOM-CONTEXTALIGNMENT, $BOE-RTLDIR,
	$BOE-PLAINTEXT, $BOE-NEUTRALOVERRIDE, $BOE-CONTEXTREADING,
	$BOE-CONTEXTALIGNMENT, $FR-MATCHDIAC, $FR-MATCHKASHIDA,
	$FR-MATCHALEFHAMZA, $WCH-EMBEDDING;

end module Win32-Rich-Edit;
