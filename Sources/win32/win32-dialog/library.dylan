module:    Dylan-user	
Synopsis:  Win32 API for common dialog boxes
	   corresponding to "COMMDLG.H" and "COMDLG32.DLL"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-dialog
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  export Win32-dialog;
end;

define module Win32-dialog
  use Dylan;
  use C-FFI , export: { size-of };
  use Win32-common,
    export: { Flags-value, Flags-value-setter };
  use Win32-user,
    export: { hInstance-value, hInstance-value-setter,
	      hDC-value, hDC-value-setter,
	      hdr-value, hdr-value-setter,
	      <NMHDR>, <LPNMHDR>, hwndFrom-value, idFrom-value, code-value,
	      hwndFrom-value-setter, idFrom-value-setter, code-value-setter,
	      hwndOwner-value, hwndOwner-value-setter,
	      lpszCaption-value, lpszCaption-value-setter,
	      pszTitle-value, pszTitle-value-setter };
  use Win32-GDI, import: { <LPLOGFONTA>, <LPLOGFONT>,
			   lStructSize-value, lStructSize-value-setter },
		 export: { <LPLOGFONTA>, <LPLOGFONT>,
			   lStructSize-value, lStructSize-value-setter };


  // from "dlgs.h":
  export $ctlFirst, $ctlLast;
  export $psh1, $psh2, $psh3, $psh4, $psh5, $psh6, $psh7, $psh8,
	$psh9, $psh10, $psh11, $psh12, $psh13, $psh14, $psh15, $psh16;
  export $chx1, $chx2, $chx3, $chx4, $chx5, $chx6, $chx7, $chx8,
	$chx9, $chx10, $chx11, $chx12, $chx13, $chx14, $chx15, $chx16;
  export $rad1, $rad2, $rad3, $rad4, $rad5, $rad6, $rad7, $rad8,
	$rad9, $rad10, $rad11, $rad12, $rad13, $rad14, $rad15, $rad16;
  export $grp1, $grp2, $grp3, $grp4, $frm1, $frm2, $frm3, $frm4,
	$rct1, $rct2, $rct3, $rct4, $ico1, $ico2, $ico3, $ico4;
  export $stc1, $stc2, $stc3, $stc4, $stc5, $stc6, $stc7, $stc8,
	$stc9, $stc10, $stc11, $stc12, $stc13, $stc14, $stc15, $stc16,
	$stc17, $stc18, $stc19, $stc20, $stc21, $stc22, $stc23, $stc24,
	$stc25, $stc26, $stc27, $stc28, $stc29, $stc30, $stc31, $stc32;
  export $lst1, $lst2, $lst3, $lst4, $lst5, $lst6, $lst7, $lst8,
	$lst9, $lst10, $lst11, $lst12, $lst13, $lst14, $lst15, $lst16;
  export $cmb1, $cmb2, $cmb3, $cmb4, $cmb5, $cmb6, $cmb7, $cmb8,
	$cmb9, $cmb10, $cmb11, $cmb12, $cmb13, $cmb14, $cmb15, $cmb16;
  export $edt1, $edt2, $edt3, $edt4, $edt5, $edt6, $edt7, $edt8,
	$edt9, $edt10, $edt11, $edt12, $edt13, $edt14, $edt15, $edt16;
  export $scr1, $scr2, $scr3, $scr4, $scr5, $scr6, $scr7, $scr8,
	$FILEOPENORD, $MULTIFILEOPENORD, $PRINTDLGORD, $PRNSETUPDLGORD,
	$FINDDLGORD, $REPLACEDLGORD, $FONTDLGORD, $FORMATDLGORD31,
	$FORMATDLGORD30, $RUNDLGORD, $PAGESETUPDLGORD, $NEWFILEOPENORD,
	$NEWOBJECTOPENORD;


  // from "cderr.h":
  export $CDERR-DIALOGFAILURE, $CDERR-GENERALCODES, $CDERR-STRUCTSIZE,
	$CDERR-INITIALIZATION, $CDERR-NOTEMPLATE, $CDERR-NOHINSTANCE,
	$CDERR-LOADSTRFAILURE, $CDERR-FINDRESFAILURE, $CDERR-LOADRESFAILURE,
	$CDERR-LOCKRESFAILURE, $CDERR-MEMALLOCFAILURE, $CDERR-MEMLOCKFAILURE,
	$CDERR-NOHOOK, $CDERR-REGISTERMSGFAIL, $PDERR-PRINTERCODES,
	$PDERR-SETUPFAILURE, $PDERR-PARSEFAILURE, $PDERR-RETDEFFAILURE,
	$PDERR-LOADDRVFAILURE, $PDERR-GETDEVMODEFAIL, $PDERR-INITFAILURE,
	$PDERR-NODEVICES, $PDERR-NODEFAULTPRN, $PDERR-DNDMMISMATCH,
	$PDERR-CREATEICFAILURE, $PDERR-PRINTERNOTFOUND,
	$PDERR-DEFAULTDIFFERENT, $CFERR-CHOOSEFONTCODES, $CFERR-NOFONTS,
	$CFERR-MAXLESSTHANMIN, $FNERR-FILENAMECODES, $FNERR-SUBCLASSFAILURE,
	$FNERR-INVALIDFILENAME, $FNERR-BUFFERTOOSMALL,
	$FRERR-FINDREPLACECODES, $FRERR-BUFFERLENGTHZERO,
	$CCERR-CHOOSECOLORCODES;


  // from "commdlg.h":
  export <LPOFNHOOKPROC>;
  export lpstrFilter-value, lpstrFilter-value-setter,
	lpstrCustomFilter-value, lpstrCustomFilter-value-setter,
	nMaxCustFilter-value, nMaxCustFilter-value-setter,
	nFilterIndex-value, nFilterIndex-value-setter, lpstrFile-value,
	lpstrFile-value-setter, nMaxFile-value, nMaxFile-value-setter,
	lpstrFileTitle-value, lpstrFileTitle-value-setter,
	nMaxFileTitle-value, nMaxFileTitle-value-setter,
	lpstrInitialDir-value, lpstrInitialDir-value-setter,
	lpstrTitle-value, lpstrTitle-value-setter, nFileOffset-value,
	nFileOffset-value-setter, nFileExtension-value,
	nFileExtension-value-setter, lpstrDefExt-value,
	lpstrDefExt-value-setter, lCustData-value, lCustData-value-setter,
	lpfnHook-value, lpfnHook-value-setter, lpTemplateName-value,
	lpTemplateName-value-setter, <OPENFILENAMEA>, <LPOPENFILENAMEA>,
	<OPENFILENAME>, <LPOPENFILENAME>, GetOpenFileName, GetSaveFileName,
	GetFileTitle, $OFN-READONLY, $OFN-OVERWRITEPROMPT, $OFN-HIDEREADONLY,
	$OFN-NOCHANGEDIR, $OFN-SHOWHELP, $OFN-ENABLEHOOK,
	$OFN-ENABLETEMPLATE, $OFN-ENABLETEMPLATEHANDLE, $OFN-NOVALIDATE,
	$OFN-ALLOWMULTISELECT, $OFN-EXTENSIONDIFFERENT, $OFN-PATHMUSTEXIST,
	$OFN-FILEMUSTEXIST, $OFN-CREATEPROMPT, $OFN-SHAREAWARE,
	$OFN-NOREADONLYRETURN, $OFN-NOTESTFILECREATE, $OFN-NONETWORKBUTTON,
	$OFN-NOLONGNAMES, $OFN-EXPLORER, $OFN-NODEREFERENCELINKS,
	$OFN-LONGNAMES, $OFN-ENABLEINCLUDENOTIFY, $OFN-ENABLESIZING,
	$OFN-SHAREFALLTHROUGH, $OFN-SHARENOWARN, $OFN-SHAREWARN,
	<LPCCHOOKPROC>, lpOFN-value, lpOFN-value-setter, pszFile-value,
	pszFile-value-setter, <OFNOTIFYA>, <LPOFNOTIFYA>, <OFNOTIFY>,
	<LPOFNOTIFY>;
  export lpOFN-value, lpOFN-value-setter, psf-value, psf-value-setter,
	pidl-value, pidl-value-setter, <OFNOTIFYEXA>, <LPOFNOTIFYEXA>,
	<OFNOTIFYEX>, <LPOFNOTIFYEX>;
  export $CDM-FIRST, $CDM-LAST, rgbResult-value,
	rgbResult-value-setter, lpCustColors-value,
	lpCustColors-value-setter, lCustData-value, lCustData-value-setter,
	lpfnHook-value, lpfnHook-value-setter, lpTemplateName-value,
	lpTemplateName-value-setter, <CHOOSECOLORA>, <LPCHOOSECOLORA>,
	<CHOOSECOLOR>, <LPCHOOSECOLOR>, ChooseColor, $CC-RGBINIT,
	$CC-FULLOPEN, $CC-PREVENTFULLOPEN, $CC-SHOWHELP, $CC-ENABLEHOOK,
	$CC-ENABLETEMPLATE, $CC-ENABLETEMPLATEHANDLE, $CC-SOLIDCOLOR,
	$CC-ANYCOLOR, <LPFRHOOKPROC>;
  export lpstrFindWhat-value, lpstrFindWhat-value-setter,
	lpstrReplaceWith-value, lpstrReplaceWith-value-setter,
	wFindWhatLen-value, wFindWhatLen-value-setter, wReplaceWithLen-value,
	wReplaceWithLen-value-setter, lCustData-value,
	lCustData-value-setter, lpfnHook-value, lpfnHook-value-setter,
	lpTemplateName-value, lpTemplateName-value-setter, <FINDREPLACEA>,
	<LPFINDREPLACEA>, <FINDREPLACE>, <LPFINDREPLACE>, $FR-DOWN,
	$FR-WHOLEWORD, $FR-MATCHCASE, $FR-FINDNEXT, $FR-REPLACE,
	$FR-REPLACEALL, $FR-DIALOGTERM, $FR-SHOWHELP, $FR-ENABLEHOOK,
	$FR-ENABLETEMPLATE, $FR-NOUPDOWN, $FR-NOMATCHCASE, $FR-NOWHOLEWORD,
	$FR-ENABLETEMPLATEHANDLE, $FR-HIDEUPDOWN, $FR-HIDEMATCHCASE,
	$FR-HIDEWHOLEWORD, FindText, ReplaceText, <LPCFHOOKPROC>;
  export lpLogFont-value, lpLogFont-value-setter, iPointSize-value,
	iPointSize-value-setter, rgbColors-value, rgbColors-value-setter,
	lCustData-value, lCustData-value-setter, lpfnHook-value,
	lpfnHook-value-setter, lpTemplateName-value,
	lpTemplateName-value-setter, lpszStyle-value, lpszStyle-value-setter,
	nFontType-value, nFontType-value-setter, nSizeMin-value,
	nSizeMin-value-setter, nSizeMax-value, nSizeMax-value-setter,
	<CHOOSEFONTA>, <LPCHOOSEFONTA>, <CHOOSEFONT>, <LPCHOOSEFONT>,
	ChooseFont, $CF-SCREENFONTS, $CF-PRINTERFONTS, $CF-BOTH,
	$CF-SHOWHELP, $CF-ENABLEHOOK, $CF-ENABLETEMPLATE,
	$CF-ENABLETEMPLATEHANDLE, $CF-INITTOLOGFONTSTRUCT, $CF-USESTYLE,
	$CF-EFFECTS, $CF-APPLY, $CF-ANSIONLY, $CF-SCRIPTSONLY,
	$CF-NOVECTORFONTS, $CF-NOOEMFONTS, $CF-NOSIMULATIONS, $CF-LIMITSIZE,
	$CF-FIXEDPITCHONLY, $CF-WYSIWYG, $CF-FORCEFONTEXIST,
	$CF-SCALABLEONLY, $CF-TTONLY, $CF-NOFACESEL, $CF-NOSTYLESEL,
	$CF-NOSIZESEL, $CF-SELECTSCRIPT, $CF-NOSCRIPTSEL, $CF-NOVERTFONTS,
	$SIMULATED-FONTTYPE, $PRINTER-FONTTYPE, $SCREEN-FONTTYPE,
	$BOLD-FONTTYPE, $ITALIC-FONTTYPE, $REGULAR-FONTTYPE,
	$OPENTYPE-FONTTYPE, $TYPE1-FONTTYPE, $DSIG-FONTTYPE,
	$WM-CHOOSEFONT-GETLOGFONT, $WM-CHOOSEFONT-SETLOGFONT,
	$WM-CHOOSEFONT-SETFLAGS, $LBSELCHSTRING, $SHAREVISTRING,
	$FILEOKSTRING, $COLOROKSTRING, $SETRGBSTRING, $HELPMSGSTRING,
	$FINDMSGSTRING, $CD-LBSELNOITEMS, $CD-LBSELCHANGE, $CD-LBSELSUB,
	$CD-LBSELADD, <LPPRINTHOOKPROC>, <LPSETUPHOOKPROC>;
  export hDevMode-value, hDevMode-value-setter, hDevNames-value,
	hDevNames-value-setter, nFromPage-value, nFromPage-value-setter,
	nToPage-value, nToPage-value-setter, nMinPage-value,
	nMinPage-value-setter, nMaxPage-value, nMaxPage-value-setter,
	nCopies-value, nCopies-value-setter, lCustData-value,
	lCustData-value-setter, lpfnPrintHook-value,
	lpfnPrintHook-value-setter, lpfnSetupHook-value,
	lpfnSetupHook-value-setter, lpPrintTemplateName-value,
	lpPrintTemplateName-value-setter, lpSetupTemplateName-value,
	lpSetupTemplateName-value-setter, hPrintTemplate-value,
	hPrintTemplate-value-setter, hSetupTemplate-value,
	hSetupTemplate-value-setter, <PRINTDLGA>, <LPPRINTDLGA>, <PRINTDLG>,
	<LPPRINTDLG>, PrintDlg, $PD-ALLPAGES, $PD-SELECTION, $PD-PAGENUMS,
	$PD-NOSELECTION, $PD-NOPAGENUMS, $PD-COLLATE, $PD-PRINTTOFILE,
	$PD-PRINTSETUP, $PD-NOWARNING, $PD-RETURNDC, $PD-RETURNIC,
	$PD-RETURNDEFAULT, $PD-SHOWHELP, $PD-ENABLEPRINTHOOK,
	$PD-ENABLESETUPHOOK, $PD-ENABLEPRINTTEMPLATE,
	$PD-ENABLESETUPTEMPLATE, $PD-ENABLEPRINTTEMPLATEHANDLE,
	$PD-ENABLESETUPTEMPLATEHANDLE, $PD-USEDEVMODECOPIES,
	$PD-USEDEVMODECOPIESANDCOLLATE, $PD-DISABLEPRINTTOFILE,
	$PD-HIDEPRINTTOFILE, $PD-NONETWORKBUTTON, wDriverOffset-value,
	wDriverOffset-value-setter, wDeviceOffset-value,
	wDeviceOffset-value-setter, wOutputOffset-value,
	wOutputOffset-value-setter, wDefault-value, wDefault-value-setter,
	<DEVNAMES>, <LPDEVNAMES>;
  export $DN-DEFAULTPRN, CommDlgExtendedError, $WM-PSD-PAGESETUPDLG,
	$WM-PSD-FULLPAGERECT, $WM-PSD-MINMARGINRECT, $WM-PSD-MARGINRECT,
	$WM-PSD-GREEKTEXTRECT, $WM-PSD-ENVSTAMPRECT, $WM-PSD-YAFULLPAGERECT,
	<LPPAGEPAINTHOOK>, <LPPAGESETUPHOOK>;
  export hDevMode-value, hDevMode-value-setter, hDevNames-value,
	hDevNames-value-setter, ptPaperSize-value, ptPaperSize-value-setter,
	rtMinMargin-value, rtMinMargin-value-setter, rtMargin-value,
	rtMargin-value-setter, lCustData-value, lCustData-value-setter,
	lpfnPageSetupHook-value, lpfnPageSetupHook-value-setter,
	lpfnPagePaintHook-value, lpfnPagePaintHook-value-setter,
	lpPageSetupTemplateName-value, lpPageSetupTemplateName-value-setter,
	hPageSetupTemplate-value, hPageSetupTemplate-value-setter,
	<PAGESETUPDLGA>, <LPPAGESETUPDLGA>, <PAGESETUPDLG>, <LPPAGESETUPDLG>,
	PageSetupDlg, $PSD-DEFAULTMINMARGINS, $PSD-INWININIINTLMEASURE,
	$PSD-MINMARGINS, $PSD-MARGINS, $PSD-INTHOUSANDTHSOFINCHES,
	$PSD-INHUNDREDTHSOFMILLIMETERS, $PSD-DISABLEMARGINS,
	$PSD-DISABLEPRINTER, $PSD-NOWARNING, $PSD-DISABLEORIENTATION,
	$PSD-RETURNDEFAULT, $PSD-DISABLEPAPER, $PSD-SHOWHELP,
	$PSD-ENABLEPAGESETUPHOOK, $PSD-ENABLEPAGESETUPTEMPLATE,
	$PSD-ENABLEPAGESETUPTEMPLATEHANDLE, $PSD-ENABLEPAGEPAINTHOOK,
	$PSD-DISABLEPAGEPAINTING, $PSD-NONETWORKBUTTON;

end module Win32-dialog;
