module:    Dylan-user	
Synopsis:  FFI declarations translated from Motif header files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Motif
  use functional-dylan;
  use C-FFI;
  use Xlib;
  use Xt;
  export Motif;
end library Motif;

define module Motif
  use functional-dylan;
  use C-FFI,
    export: {null-pointer, null-pointer?, pointer-address,
	     pointer-value, pointer-value-setter, size-of,
	     <C-void*>, <C-pointer>, <C-string>, <C-unicode-string>,
	     <C-string*>, <C-unicode-string*>,
	     pointer-cast, destroy,
	     pointer-value-address, c-type-cast,
	     \with-c-string, \with-stack-structure } ;
  use machine-words, 
    export: {%logior, %logand};
  use Xlib,
    export: {
    // export types used here:
	     <Atom>, <Colormap>, <Cursor>, <Pixmap>, <Time>, <Window>,
	     <Cardinal>, <Cardinal*>, <KeySym>, <Modifiers>, <Position>,
	     <Dimension>, <Dimension*>, <Dimension**>, <Display*>,
	     <GC>, <KeyCode>, <KeySym*>, <Modifiers*>, <Position*>,
	     <Screen*>, <XButtonPressedEvent*>, <XEvent*>, <XFontStruct**>,
	     <XFontStruct*>, <XIM>, <XImage*>, <XKeyPressedEvent*>,
	     <XRectangle*>,
     // export structure accessors with methods defined in both libraries.
	     background-value, background-value-setter,
	     client-data-value, client-data-value-setter,
	     data-value, data-value-setter,
	     direction-value, direction-value-setter,
	     event-value, event-value-setter,
	     foreground-value, foreground-value-setter,
	     format-value, format-value-setter,
	     length-value, length-value-setter,
	     name-value, name-value-setter,
	     pixel-value, pixel-value-setter,
	     target-value, target-value-setter,
	     text-value, text-value-setter,
	     window-value, window-value-setter,
	     res-class-value, res-class-value-setter,
	     value-value, value-value-setter};
  use Xt,
    // export types used here:
    export: {<Arg*>, <ArgList>, <Widget>, <Widget*>, <Widget**>,
	     <WidgetClass>, <X-Boolean*>, <X-String>, <X-String*>,
	     <XtPointer>, <XtPointer*>,
	     vendorShellWidgetClass}; 

  // from "Xm.h":
  export $XmVERSION, $XmREVISION, $XmUPDATE-LEVEL, $XmVERSION-STRING,
	xmUseVersion;
  export $XmUNSPECIFIED-PIXMAP, $XmSTRING-OS-CHARSET,
	$XmFALLBACK-CHARSET, $XmDEFAULT-FONT, $XmDEFAULT-BACKGROUND,
	$XmDEFAULT-DARK-THRESHOLD, $XmDEFAULT-LIGHT-THRESHOLD,
	$XmDEFAULT-FOREGROUND-THRESHOLD, %XmSDEFAULT-FONT,
	%XmSDEFAULT-BACKGROUND, $XmFONT-IS-FONT, $XmFONT-IS-FONTSET,
	$XmSTRING-DIRECTION-L-TO-R, $XmSTRING-DIRECTION-R-TO-L,
	$XmSTRING-DIRECTION-DEFAULT;
  export <XmStringDirection>, <XmString>, <XmStringTable>,
	<XmStringCharSet>, <XmStringComponentType>;
  export <XmFontListEntry>, <XmFontList>, <XmStringContext>,
	<XmFontContext>, $XmSTRING-COMPONENT-UNKNOWN,
	$XmSTRING-COMPONENT-CHARSET, $XmSTRING-COMPONENT-TEXT,
	$XmSTRING-COMPONENT-DIRECTION, $XmSTRING-COMPONENT-SEPARATOR,
	$XmSTRING-COMPONENT-LOCALE-TEXT;
  export $XmSTRING-COMPONENT-END, $XmSTRING-COMPONENT-USER-BEGIN,
	$XmSTRING-COMPONENT-USER-END;
  export xmPrimitiveWidgetClass, <XmPrimitiveWidgetClass>,
	<XmPrimitiveWidget>;
  export xmGadgetClass, <XmGadgetClass>, <XmGadget>;
  export xmManagerWidgetClass, <XmManagerWidgetClass>,
	<XmManagerWidget>;
  export XmIsPrimitive, XmIsGadget, XmIsManager;
  export $XmCHANGE-ALL, $XmCHANGE-NONE, $XmCHANGE-WIDTH,
	$XmCHANGE-HEIGHT;
  export $XmPIXELS, $Xm100TH-MILLIMETERS, $Xm1000TH-INCHES,
	$Xm100TH-POINTS, $Xm100TH-FONT-UNITS;
  export $XmDESTROY, $XmUNMAP, $XmDO-NOTHING;
  export $XmEXPLICIT, $XmPOINTER;
  export $XmNONE, $XmTAB-GROUP, $XmSTICKY-TAB-GROUP,
	$XmEXCLUSIVE-TAB-GROUP;
  export $XmDYNAMIC-DEFAULT-TAB-GROUP;
  export $XmBELL;
  export $XmNO-ORIENTATION, $XmVERTICAL, $XmHORIZONTAL;
  export $XmWORK-AREA, $XmMENU-BAR, $XmMENU-PULLDOWN, $XmMENU-POPUP,
	$XmMENU-OPTION;
  export $XmNO-PACKING, $XmPACK-TIGHT, $XmPACK-COLUMN, $XmPACK-NONE;
  export $XmALIGNMENT-CONTENTS-TOP, $XmALIGNMENT-CONTENTS-BOTTOM;
  export $XmTEAR-OFF-ENABLED, $XmTEAR-OFF-DISABLED;
  export $XmUNPOST, $XmUNPOST-AND-REPLAY;
  export $XmLAST-POSITION, $XmFIRST-POSITION;
  export $XmALIGNMENT-BEGINNING, $XmALIGNMENT-CENTER,
	$XmALIGNMENT-END;
  export $XmALIGNMENT-BASELINE-TOP, $XmALIGNMENT-BASELINE-BOTTOM,
	$XmALIGNMENT-WIDGET-TOP, $XmALIGNMENT-WIDGET-BOTTOM;
  export $XmFRAME-GENERIC-CHILD, $XmFRAME-WORKAREA-CHILD,
	$XmFRAME-TITLE-CHILD;
  export $XmN-OF-MANY, $XmONE-OF-MANY;
  export $XmATTACH-NONE, $XmATTACH-FORM, $XmATTACH-OPPOSITE-FORM,
	$XmATTACH-WIDGET, $XmATTACH-OPPOSITE-WIDGET, $XmATTACH-POSITION,
	$XmATTACH-SELF;
  export $XmRESIZE-NONE, $XmRESIZE-GROW, $XmRESIZE-ANY;
  export $XmCR-NONE, $XmCR-HELP, $XmCR-VALUE-CHANGED, $XmCR-INCREMENT,
	$XmCR-DECREMENT, $XmCR-PAGE-INCREMENT, $XmCR-PAGE-DECREMENT,
	$XmCR-TO-TOP, $XmCR-TO-BOTTOM, $XmCR-DRAG, $XmCR-ACTIVATE, $XmCR-ARM,
	$XmCR-DISARM, $XmCR-MAP, $XmCR-UNMAP, $XmCR-FOCUS,
	$XmCR-LOSING-FOCUS, $XmCR-MODIFYING-TEXT-VALUE,
	$XmCR-MOVING-INSERT-CURSOR, $XmCR-EXECUTE, $XmCR-SINGLE-SELECT,
	$XmCR-MULTIPLE-SELECT, $XmCR-EXTENDED-SELECT, $XmCR-BROWSE-SELECT,
	$XmCR-DEFAULT-ACTION, $XmCR-CLIPBOARD-DATA-REQUEST,
	$XmCR-CLIPBOARD-DATA-DELETE, $XmCR-CASCADING, $XmCR-OK, $XmCR-CANCEL,
	$XmCR-APPLY, $XmCR-NO-MATCH, $XmCR-COMMAND-ENTERED,
	$XmCR-COMMAND-CHANGED, $XmCR-EXPOSE, $XmCR-RESIZE, $XmCR-INPUT,
	$XmCR-GAIN-PRIMARY, $XmCR-LOSE-PRIMARY, $XmCR-CREATE,
	$XmCR-TEAR-OFF-ACTIVATE, $XmCR-TEAR-OFF-DEACTIVATE,
	$XmCR-OBSCURED-TRAVERSAL, $XmCR-PROTOCOLS;
  export reason-value, reason-value-setter, <XmAnyCallbackStruct>,
	<XmAnyCallbackStruct*>;
  export reason-value, reason-value-setter, click-count-value,
	click-count-value-setter, <XmArrowButtonCallbackStruct>,
	<XmArrowButtonCallbackStruct*>;
  export reason-value, reason-value-setter,
	<XmDrawingAreaCallbackStruct>, <XmDrawingAreaCallbackStruct*>;
  export reason-value, reason-value-setter, click-count-value,
	click-count-value-setter, <XmDrawnButtonCallbackStruct>,
	<XmDrawnButtonCallbackStruct*>;
  export reason-value, reason-value-setter, click-count-value,
	click-count-value-setter, <XmPushButtonCallbackStruct>,
	<XmPushButtonCallbackStruct*>;
  export reason-value, reason-value-setter, widget-value,
	widget-value-setter, callbackstruct-value,
	callbackstruct-value-setter, <XmRowColumnCallbackStruct>,
	<XmRowColumnCallbackStruct*>;
  export reason-value, reason-value-setter,
	<XmScrollBarCallbackStruct>, <XmScrollBarCallbackStruct*>;
  export reason-value, reason-value-setter, set-value,
	set-value-setter, <XmToggleButtonCallbackStruct>,
	<XmToggleButtonCallbackStruct*>;
  export reason-value, reason-value-setter, item-value,
	item-value-setter, item-length-value, item-length-value-setter,
	item-position-value, item-position-value-setter,
	selected-items-value, selected-items-value-setter,
	selected-item-count-value, selected-item-count-value-setter,
	selected-item-positions-value, selected-item-positions-value-setter,
	selection-type-value, selection-type-value-setter,
	<XmListCallbackStruct>, <XmListCallbackStruct*>;
  export reason-value, reason-value-setter,
	<XmSelectionBoxCallbackStruct>, <XmSelectionBoxCallbackStruct*>;
  export reason-value, reason-value-setter, <XmCommandCallbackStruct>,
	<XmCommandCallbackStruct*>;
  export reason-value, reason-value-setter, mask-value,
	mask-value-setter, mask-length-value, mask-length-value-setter,
	dir-value, dir-value-setter, dir-length-value,
	dir-length-value-setter, pattern-value, pattern-value-setter,
	pattern-length-value, pattern-length-value-setter,
	<XmFileSelectionBoxCallbackStruct>,
	<XmFileSelectionBoxCallbackStruct*>;
  export reason-value, reason-value-setter, <XmScaleCallbackStruct>,
	<XmScaleCallbackStruct*>;
  export $XmMULTICLICK-DISCARD, $XmMULTICLICK-KEEP;
  export $XmSHADOW-IN, $XmSHADOW-OUT;
  export $XmARROW-UP, $XmARROW-DOWN, $XmARROW-LEFT, $XmARROW-RIGHT;
  export $XmNO-LINE, $XmSINGLE-LINE, $XmDOUBLE-LINE,
	$XmSINGLE-DASHED-LINE, $XmDOUBLE-DASHED-LINE, $XmSHADOW-ETCHED-IN,
	$XmSHADOW-ETCHED-OUT, $XmSHADOW-ETCHED-IN-DASH,
	$XmSHADOW-ETCHED-OUT-DASH, $XmINVALID-SEPARATOR-TYPE;
  export $XmPIXMAP, $XmSTRING;
  export $XmWINDOW, $XmCURSOR;
  export $XmMAX-ON-TOP, $XmMAX-ON-BOTTOM, $XmMAX-ON-LEFT,
	$XmMAX-ON-RIGHT;
  export $XmSINGLE-SELECT, $XmMULTIPLE-SELECT, $XmEXTENDED-SELECT,
	$XmBROWSE-SELECT;
  export $XmSTATIC, $XmDYNAMIC;
  export $XmVARIABLE, $XmCONSTANT, $XmRESIZE-IF-POSSIBLE;
  export $XmAUTOMATIC, $XmAPPLICATION-DEFINED;
  export $XmAS-NEEDED;
  export $SW-TOP, $SW-BOTTOM, $SW-LEFT, $SW-RIGHT, $XmTOP-LEFT,
	$XmBOTTOM-LEFT, $XmTOP-RIGHT, $XmBOTTOM-RIGHT,
	$XmCOMMAND-ABOVE-WORKSPACE, $XmCOMMAND-BELOW-WORKSPACE;
  export $XmMULTI-LINE-EDIT, $XmSINGLE-LINE-EDIT;
  export $XmTEXT-FORWARD, $XmTEXT-BACKWARD;
  export <C-XmTextPosition>, <XmTextPosition>, <XmTextFormat>,
	$XmFMT-8-BIT, $XmFMT-16-BIT, $XmSELECT-POSITION,
	$XmSELECT-WHITESPACE, $XmSELECT-WORD, $XmSELECT-LINE, $XmSELECT-ALL,
	$XmSELECT-PARAGRAPH;
  export $XmHIGHLIGHT-NORMAL, $XmHIGHLIGHT-SELECTED,
	$XmHIGHLIGHT-SECONDARY-SELECTED;
  export ptr-value, ptr-value-setter, <XmTextBlockRec>,
	<XmTextBlockRec*>, <XmTextBlock>;
  export reason-value, reason-value-setter, doit-value,
	doit-value-setter, currInsert-value, currInsert-value-setter,
	newInsert-value, newInsert-value-setter, startPos-value,
	startPos-value-setter, endPos-value, endPos-value-setter,
	<XmTextVerifyCallbackStruct>, <XmTextVerifyCallbackStruct*>,
	<XmTextVerifyPtr>;
  export wcsptr-value, wcsptr-value-setter, <XmTextBlockRecWcs>,
	<XmTextBlockRecWcs*>, <XmTextBlockWcs>;
  export reason-value, reason-value-setter, doit-value,
	doit-value-setter, currInsert-value, currInsert-value-setter,
	newInsert-value, newInsert-value-setter, startPos-value,
	startPos-value-setter, endPos-value, endPos-value-setter,
	<XmTextVerifyCallbackStructWcs>, <XmTextVerifyCallbackStructWcs*>,
	<XmTextVerifyPtrWcs>, XmTextGetTopPosition, XmTextSetTopPosition,
	$XmCOPY-FAILED, $XmCOPY-SUCCEEDED, $XmCOPY-TRUNCATED, $XmDIALOG-NONE,
	$XmDIALOG-APPLY-BUTTON, $XmDIALOG-CANCEL-BUTTON,
	$XmDIALOG-DEFAULT-BUTTON, $XmDIALOG-OK-BUTTON,
	$XmDIALOG-FILTER-LABEL, $XmDIALOG-FILTER-TEXT, $XmDIALOG-HELP-BUTTON,
	$XmDIALOG-LIST, $XmDIALOG-LIST-LABEL, $XmDIALOG-MESSAGE-LABEL,
	$XmDIALOG-SELECTION-LABEL, $XmDIALOG-SYMBOL-LABEL, $XmDIALOG-TEXT,
	$XmDIALOG-SEPARATOR, $XmDIALOG-DIR-LIST, $XmDIALOG-DIR-LIST-LABEL;
  export $XmDIALOG-HISTORY-LIST, $XmDIALOG-PROMPT-LABEL,
	$XmDIALOG-VALUE-TEXT, $XmDIALOG-COMMAND-TEXT, $XmDIALOG-FILE-LIST,
	$XmDIALOG-FILE-LIST-LABEL, $XmDIALOG-MODELESS,
	$XmDIALOG-PRIMARY-APPLICATION-MODAL,
	$XmDIALOG-FULL-APPLICATION-MODAL, $XmDIALOG-SYSTEM-MODAL;
  export $XmDIALOG-APPLICATION-MODAL;
  export $XmPLACE-TOP, $XmPLACE-ABOVE-SELECTION,
	$XmPLACE-BELOW-SELECTION;
  export $XmFILE-DIRECTORY, $XmFILE-REGULAR, $XmFILE-ANY-TYPE,
	$XmDIALOG-WORK-AREA, $XmDIALOG-PROMPT, $XmDIALOG-SELECTION,
	$XmDIALOG-COMMAND, $XmDIALOG-FILE-SELECTION;
  export $XmDIALOG-TEMPLATE, $XmDIALOG-ERROR, $XmDIALOG-INFORMATION,
	$XmDIALOG-MESSAGE, $XmDIALOG-QUESTION, $XmDIALOG-WARNING,
	$XmDIALOG-WORKING;
  export $XmVISIBILITY-UNOBSCURED, $XmVISIBILITY-PARTIALLY-OBSCURED,
	$XmVISIBILITY-FULLY-OBSCURED;
  export $XmTRAVERSE-CURRENT, $XmTRAVERSE-NEXT, $XmTRAVERSE-PREV,
	$XmTRAVERSE-HOME, $XmTRAVERSE-NEXT-TAB-GROUP,
	$XmTRAVERSE-PREV-TAB-GROUP, $XmTRAVERSE-UP, $XmTRAVERSE-DOWN,
	$XmTRAVERSE-LEFT, $XmTRAVERSE-RIGHT;
  export reason-value, reason-value-setter,
	traversal-destination-value, traversal-destination-value-setter,
	<XmTraverseObscuredCallbackStruct>,
	<XmTraverseObscuredCallbackStruct*>;
  export <XmNavigationType>;
  export <XmButtonType>, <XmButtonTypeTable>, <XmKeySymTable>,
	<XmStringCharSetTable>, $XmPUSHBUTTON, $XmTOGGLEBUTTON,
	$XmRADIOBUTTON, $XmCASCADEBUTTON, $XmSEPARATOR, $XmDOUBLE-SEPARATOR,
	$XmTITLE;
  export $XmCHECKBUTTON, <XmResourceBaseProc>,
	<XmResourceBaseProc>-callback-wrapper;
  export base-proc-value, base-proc-value-setter, resources-value,
	resources-value-setter, num-resources-value,
	num-resources-value-setter, <XmSecondaryResourceDataRec>,
	<XmSecondaryResourceDataRec*>, <XmSecondaryResourceData>,
	XmGetSecondaryResourceData, XmInstallImage, XmUninstallImage,
	XmGetPixmap, XmGetPixmapByDepth, XmDestroyPixmap, XmUpdateDisplay;
  export <XmOffset>, <XmOffsetPtr>, XmResolvePartOffsets,
	XmResolveAllPartOffsets, XmWidgetGetBaselines,
	XmWidgetGetDisplayRect, XmRegisterConverters, XmCvtStringToUnitType,
	XmRegisterSegmentEncoding, XmMapSegmentEncoding, XmCvtCTToXmString,
	XmCvtTextToXmString, XmCvtXmStringToCT, XmCvtXmStringToText,
	XmConvertUnits, XmCvtToHorizontalPixels, XmCvtToVerticalPixels,
	XmCvtFromHorizontalPixels, XmCvtFromVerticalPixels, XmSetFontUnits,
	XmSetFontUnit, XmSetMenuCursor, XmGetMenuCursor,
	XmCreateSimpleMenuBar, XmCreateSimplePopupMenu,
	XmCreateSimplePulldownMenu, XmCreateSimpleOptionMenu,
	XmCreateSimpleRadioBox, XmCreateSimpleCheckBox, XmTrackingEvent,
	XmTrackingLocate, <XmColorProc>, <XmColorProc>-callback-wrapper,
	XmSetColorCalculation, XmGetColorCalculation, XmGetColors,
	XmChangeColor, XmStringCreate, XmStringCreateSimple,
	XmStringCreateLocalized, XmStringDirectionCreate,
	XmStringSeparatorCreate, XmStringSegmentCreate, XmStringLtoRCreate,
	XmStringCreateLtoR, XmStringInitContext, XmStringFreeContext,
	XmStringGetNextComponent, XmStringPeekNextComponent,
	XmStringGetNextSegment, XmStringGetLtoR, XmFontListEntryCreate,
	XmFontListEntryFree, XmFontListEntryGetFont, XmFontListEntryGetTag,
	XmFontListAppendEntry, XmFontListNextEntry, XmFontListRemoveEntry,
	XmFontListEntryLoad, XmFontListCreate, XmStringCreateFontList,
	XmFontListFree, XmFontListAdd, XmFontListCopy,
	XmFontListInitFontContext, XmFontListGetNextFont,
	XmFontListFreeFontContext, XmStringConcat, XmStringNConcat,
	XmStringCopy, XmStringNCopy, XmStringByteCompare, XmStringCompare,
	XmStringLength, XmStringEmpty, XmStringHasSubstring, XmStringFree,
	XmStringBaseline, XmStringWidth, XmStringHeight, XmStringExtent,
	XmStringLineCount, XmStringDraw, XmStringDrawImage,
	XmStringDrawUnderline, XmGetDestination, XmIsTraversable,
	XmGetVisibility, XmGetTabGroup, XmGetFocusWidget, XmProcessTraversal,
	XmAddTabGroup, XmRemoveTabGroup;
  export XmImRegister, XmImUnregister, XmImSetFocusValues,
	XmImSetValues, XmImUnsetFocus, XmImGetXIM, XmImMbLookupString;

  // from "extra.dylan":
  export <XmButtonType*>, <XmFontContext*>, <XmFontListEntry*>,
	<XmOffset*>, <XmOffsetPtr*>, <XmString*>,
	<XmStringCharSet*>, <XmStringComponentType*>, <XmStringContext*>,
	<XmStringDirection*>, <XmTextPosition*>;
  export <XmSecondaryResourceData**>, <XmSecondaryResourceData*>,
	<XmClipboardPendingList*>, <C-int**>;


  // from "ArrowB.h":
  export XmIsArrowButton, xmArrowButtonWidgetClass;
  export <XmArrowButtonWidgetClass>, <XmArrowButtonWidget>;
  export XmCreateArrowButton;

  // from "ArrowBG.h":
  export XmIsArrowButtonGadget, xmArrowButtonGadgetClass;
  export <XmArrowButtonGadgetClass>, <XmArrowButtonGadget>;
  export XmCreateArrowButtonGadget;

  // from "AtomMgr.h":
  export XmInternAtom, XmGetAtomName, XmNameToAtom;

  // from "BulletinB.h":
  export xmBulletinBoardWidgetClass;
  export <XmBulletinBoardWidgetClass>, <XmBulletinBoardWidget>;
  export XmIsBulletinBoard;
  export XmCreateBulletinBoard, XmCreateBulletinBoardDialog;

  // from "CascadeB.h":
  export xmCascadeButtonWidgetClass;
  export <XmCascadeButtonWidget>, <XmCascadeButtonWidgetClass>,
	XmIsCascadeButton, XmCreateCascadeButton, XmCascadeButtonHighlight;

  // from "CascadeBG.h":
  export xmCascadeButtonGadgetClass;
  export <XmCascadeButtonGadgetClass>, <XmCascadeButtonGadget>,
	<XmCascadeButtonGCacheObject>, XmIsCascadeButtonGadget,
	XmCreateCascadeButtonGadget, XmCascadeButtonGadgetHighlight;

  // from "Command.h":
  export xmCommandWidgetClass;
  export <XmCommandWidgetClass>, <XmCommandWidget>;
  export XmIsCommand;
  export XmCreateCommand, XmCommandGetChild, XmCommandSetValue,
	XmCommandAppendValue, XmCommandError, XmCreateCommandDialog;

  // from "CutPaste.h":
  export $XmClipboardFail, $XmClipboardSuccess, $XmClipboardTruncate,
	$XmClipboardLocked, $XmClipboardBadFormat, $XmClipboardNoData,
	$ClipboardFail, $ClipboardSuccess, $ClipboardTruncate,
	$ClipboardLocked, $ClipboardBadFormat, $ClipboardNoData,
	DataId-value, DataId-value-setter, PrivateId-value,
	PrivateId-value-setter, <XmClipboardPendingRec>,
	<XmClipboardPendingRec*>, <XmClipboardPendingList>, <XmCutPasteProc>,
	<XmCutPasteProc>-callback-wrapper, <VoidProc>,
	<VoidProc>-callback-wrapper;
  export XmClipboardBeginCopy, XmClipboardStartCopy, XmClipboardCopy,
	XmClipboardEndCopy, XmClipboardCancelCopy, XmClipboardWithdrawFormat,
	XmClipboardCopyByName, XmClipboardUndoCopy, XmClipboardLock,
	XmClipboardUnlock, XmClipboardStartRetrieve, XmClipboardEndRetrieve,
	XmClipboardRetrieve, XmClipboardInquireCount,
	XmClipboardInquireFormat, XmClipboardInquireLength,
	XmClipboardInquirePendingItems, XmClipboardRegisterFormat;

  // from "DialogS.h":
  export XmIsDialogShell, xmDialogShellWidgetClass;
  export <XmDialogShellWidgetClass>, <XmDialogShellWidget>;
  export XmCreateDialogShell;

  // from "Display.h":
  export XmIsDisplay, $XmDRAG-NONE, $XmDRAG-DROP-ONLY,
	$XmDRAG-PREFER-PREREGISTER, $XmDRAG-PREREGISTER,
	$XmDRAG-PREFER-DYNAMIC, $XmDRAG-DYNAMIC, $XmDRAG-PREFER-RECEIVER;
  export <XmDisplay>, <XmDisplayClass>, xmDisplayClass, XmGetDisplay,
	XmGetDragContext, XmGetXmDisplay;

  // from "DragC.h":
  export $XmHELP;
  export <XmID>, XmIsDragContext, $-XA-MOTIF-DROP, $-XA-DRAG-FAILURE,
	$-XA-DRAG-SUCCESS, $XmTOP-LEVEL-ENTER, $XmTOP-LEVEL-LEAVE,
	$XmDRAG-MOTION, $XmDROP-SITE-ENTER, $XmDROP-SITE-LEAVE,
	$XmDROP-START, $XmDROP-FINISH, $XmDRAG-DROP-FINISH,
	$XmOPERATION-CHANGED;
  export $XmDROP, $XmDROP-HELP, $XmDROP-CANCEL, $XmDROP-INTERRUPT;
  export $XmDROP-NOOP, $XmDROP-MOVE, $XmDROP-COPY, $XmDROP-LINK,
	$XmBLEND-ALL, $XmBLEND-STATE-SOURCE, $XmBLEND-JUST-SOURCE,
	$XmBLEND-NONE;
  export $XmDROP-FAILURE, $XmDROP-SUCCESS;
  export $XmCR-TOP-LEVEL-ENTER, $XmCR-TOP-LEVEL-LEAVE,
	$XmCR-DRAG-MOTION, $XmCR-DROP-SITE-ENTER, $XmCR-DROP-SITE-LEAVE,
	$XmCR-DROP-START, $XmCR-DROP-FINISH, $XmCR-DRAG-DROP-FINISH,
	$XmCR-OPERATION-CHANGED, $-XmNUMBER-DND-CB-REASONS;
  export <XmDragContextClass>, <XmDragContext>, xmDragContextClass;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, <XmAnyICCCallbackStruct>,
	<XmAnyICCCallbackStruct*>, <XmAnyICCCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, dragProtocolStyle-value,
	dragProtocolStyle-value-setter, iccHandle-value,
	iccHandle-value-setter, <XmTopLevelEnterCallbackStruct>,
	<XmTopLevelEnterCallbackStruct*>, <XmTopLevelEnterCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, <XmTopLevelLeaveCallbackStruct>,
	<XmTopLevelLeaveCallbackStruct*>, <XmTopLevelLeaveCallback>,
	reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, operation-value, operation-value-setter,
	operations-value, operations-value-setter, dropSiteStatus-value,
	dropSiteStatus-value-setter, <XmDropSiteEnterCallbackStruct>,
	<XmDropSiteEnterCallbackStruct*>, <XmDropSiteEnterCallback>,
	reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, <XmDropSiteLeaveCallbackStruct>,
	<XmDropSiteLeaveCallbackStruct*>, <XmDropSiteLeaveCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, operation-value, operation-value-setter,
	operations-value, operations-value-setter, dropSiteStatus-value,
	dropSiteStatus-value-setter, <XmDragMotionCallbackStruct>,
	<XmDragMotionCallbackStruct*>, <XmDragMotionCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, operation-value, operation-value-setter,
	operations-value, operations-value-setter, dropSiteStatus-value,
	dropSiteStatus-value-setter, <XmOperationChangedCallbackStruct>,
	<XmOperationChangedCallbackStruct*>, <XmOperationChangedCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, operation-value, operation-value-setter,
	operations-value, operations-value-setter, dropSiteStatus-value,
	dropSiteStatus-value-setter, dropAction-value,
	dropAction-value-setter, iccHandle-value, iccHandle-value-setter,
	<XmDropStartCallbackStruct>, <XmDropStartCallbackStruct*>,
	<XmDropStartCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, operation-value, operation-value-setter,
	operations-value, operations-value-setter, dropSiteStatus-value,
	dropSiteStatus-value-setter, dropAction-value,
	dropAction-value-setter, completionStatus-value,
	completionStatus-value-setter, <XmDropFinishCallbackStruct>,
	<XmDropFinishCallbackStruct*>, <XmDropFinishCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, <XmDragDropFinishCallbackStruct>,
	<XmDragDropFinishCallbackStruct*>, <XmDragDropFinishCallback>;
  export XmDragStart, XmDragCancel, XmTargetsAreCompatible;

  // from "DragIcon.h":
  export XmIsDragIconObjectClass, $XmATTACH-NORTH-WEST,
	$XmATTACH-NORTH, $XmATTACH-NORTH-EAST, $XmATTACH-EAST,
	$XmATTACH-SOUTH-EAST, $XmATTACH-SOUTH, $XmATTACH-SOUTH-WEST,
	$XmATTACH-WEST, $XmATTACH-CENTER, $XmATTACH-HOT;
  export <XmDragIconObject>, <XmDragIconObjectClass>,
	xmDragIconObjectClass;
  export XmCreateDragIcon;

  // from "DragOverS.h":
  export <XmDragOverShellWidget>, <XmDragOverShellWidgetClass>,
	xmDragOverShellWidgetClass;

  // from "DrawingA.h":
  export xmDrawingAreaWidgetClass;
  export <XmDrawingAreaWidgetClass>, <XmDrawingAreaWidget>;
  export XmIsDrawingArea;
  export XmCreateDrawingArea;

  // from "DrawnB.h":
  export XmIsDrawnButton, xmDrawnButtonWidgetClass;
  export <XmDrawnButtonWidgetClass>, <XmDrawnButtonWidget>;
  export XmCreateDrawnButton;

  // from "DropSMgr.h":
  export $XmCR-DROP-SITE-LEAVE-MESSAGE, $XmCR-DROP-SITE-ENTER-MESSAGE,
	$XmCR-DROP-SITE-MOTION-MESSAGE, $XmCR-DROP-MESSAGE, $XmNO-DROP-SITE,
	$XmINVALID-DROP-SITE, $XmVALID-DROP-SITE, $XmDROP-SITE-INVALID,
	$XmDROP-SITE-VALID, $XmDRAG-UNDER-NONE, $XmDRAG-UNDER-PIXMAP,
	$XmDRAG-UNDER-SHADOW-IN, $XmDRAG-UNDER-SHADOW-OUT,
	$XmDRAG-UNDER-HIGHLIGHT;
  export $XmDROP-SITE-SIMPLE, $XmDROP-SITE-COMPOSITE,
	$XmDROP-SITE-SIMPLE-CLIP-ONLY, $XmDROP-SITE-COMPOSITE-CLIP-ONLY;
  export $XmABOVE, $XmBELOW;
  export $XmDROP-SITE-ACTIVE, $XmDROP-SITE-INACTIVE;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, dragContext-value, dragContext-value-setter,
	dropSiteStatus-value, dropSiteStatus-value-setter, operation-value,
	operation-value-setter, operations-value, operations-value-setter,
	animate-value, animate-value-setter, <XmDragProcCallbackStruct>,
	<XmDragProcCallbackStruct*>, <XmDragProcCallback>;
  export reason-value, reason-value-setter, timeStamp-value,
	timeStamp-value-setter, dragContext-value, dragContext-value-setter,
	dropSiteStatus-value, dropSiteStatus-value-setter, operation-value,
	operation-value-setter, operations-value, operations-value-setter,
	dropAction-value, dropAction-value-setter,
	<XmDropProcCallbackStruct>, <XmDropProcCallbackStruct*>,
	<XmDropProcCallback>;
  export topShadowColor-value, topShadowColor-value-setter,
	topShadowPixmap-value, topShadowPixmap-value-setter,
	bottomShadowColor-value, bottomShadowColor-value-setter,
	bottomShadowPixmap-value, bottomShadowPixmap-value-setter,
	shadowThickness-value, shadowThickness-value-setter,
	highlightColor-value, highlightColor-value-setter,
	highlightPixmap-value, highlightPixmap-value-setter,
	highlightThickness-value, highlightThickness-value-setter,
	borderWidth-value, borderWidth-value-setter, <XmDropSiteVisualsRec>,
	<XmDropSiteVisualsRec*>, <XmDropSiteVisuals>;
  export xmDropSiteManagerObjectClass;
  export <XmDropSiteManagerObjectClass>, <XmDropSiteManagerObject>,
	XmIsDropSiteManager, XmDropSiteRegister, XmDropSiteUnregister,
	XmDropSiteStartUpdate, XmDropSiteUpdate, XmDropSiteEndUpdate,
	XmDropSiteRetrieve, XmDropSiteQueryStackingOrder,
	XmDropSiteConfigureStackingOrder, XmDropSiteGetActiveVisuals;

  // from "DropTrans.h":
  export $XmTRANSFER-FAILURE, $XmTRANSFER-SUCCESS,
	xmDropTransferObjectClass;
  export <XmDropTransferObjectClass>, <XmDropTransferObject>,
	XmIsDropTransfer, <XmDropTransferEntryRec>,
	<XmDropTransferEntryRec*>, <XmDropTransferEntry>,
	XmDropTransferStart, XmDropTransferAdd;

  // from "FileSB.h":
  export <XmQualifyProc>, <XmQualifyProc>-callback-wrapper,
	<XmSearchProc>, <XmSearchProc>-callback-wrapper;
  export xmFileSelectionBoxWidgetClass;
  export <XmFileSelectionBoxWidgetClass>, <XmFileSelectionBoxWidget>;
  export XmIsFileSelectionBox, XmFileSelectionBoxGetChild,
	XmFileSelectionDoSearch, XmCreateFileSelectionBox,
	XmCreateFileSelectionDialog;

  // from "Form.h":
  export xmFormWidgetClass;
  export <XmFormWidgetClass>, <XmFormWidget>;
  export XmIsForm, XmCreateForm, XmCreateFormDialog;

  // from "Frame.h":
  export XmIsFrame, xmFrameWidgetClass;
  export <XmFrameWidgetClass>, <XmFrameWidget>;
  export XmCreateFrame;

  // from "Label.h":
  export xmLabelWidgetClass;
  export <XmLabelWidgetClass>, <XmLabelWidget>, XmIsLabel,
	XmCreateLabel;

  // from "LabelG.h":
  export xmLabelGadgetClass;
  export <XmLabelGadgetClass>, <XmLabelGadget>, <XmLabelGCacheObject>,
	XmIsLabelGadget, XmCreateLabelGadget;

  // from "List.h":
  export xmListWidgetClass, $XmINITIAL, $XmADDITION, $XmMODIFICATION,
	XmIsList, <XmListWidgetClass>, <XmListWidget>;
  export XmListAddItem, XmListAddItems, XmListAddItemsUnselected,
	XmListAddItemUnselected, XmListDeleteItem, XmListDeleteItems,
	XmListDeletePositions, XmListDeletePos, XmListDeleteItemsPos,
	XmListDeleteAllItems, XmListReplaceItems, XmListReplaceItemsPos,
	XmListReplaceItemsUnselected, XmListReplaceItemsPosUnselected,
	XmListReplacePositions, XmListSelectItem, XmListSelectPos,
	XmListDeselectItem, XmListDeselectPos, XmListDeselectAllItems,
	XmListSetPos, XmListSetBottomPos, XmListSetItem, XmListSetBottomItem,
	XmListSetAddMode, XmListItemExists, XmListItemPos,
	XmListGetKbdItemPos, XmListSetKbdItemPos, XmListYToPos,
	XmListPosToBounds, XmListGetMatchPos, XmListGetSelectedPos,
	XmListSetHorizPos, XmListUpdateSelectedList, XmListPosSelected,
	XmCreateList, XmCreateScrolledList;

  // from "MainW.h":
  export XmIsMainWindow, xmMainWindowWidgetClass;
  export <XmMainWindowWidgetClass>, <XmMainWindowWidget>;
  export XmMainWindowSetAreas, XmMainWindowSep1, XmMainWindowSep2,
	XmMainWindowSep3, XmCreateMainWindow;

  // from "MenuShell.h":
  export xmMenuShellWidgetClass;
  export <XmMenuShellWidgetClass>, <XmMenuShellWidget>, XmIsMenuShell,
	XmCreateMenuShell;

  // from "MessageB.h":
  export xmMessageBoxWidgetClass;
  export <XmMessageBoxWidgetClass>, <XmMessageBoxWidget>,
	XmIsMessageBox, XmCreateMessageBox, XmCreateMessageDialog,
	XmCreateErrorDialog, XmCreateInformationDialog,
	XmCreateQuestionDialog, XmCreateWarningDialog, XmCreateWorkingDialog,
	XmCreateTemplateDialog, XmMessageBoxGetChild;

  // from "PanedW.h":
  export xmPanedWindowWidgetClass, XmIsPanedWindow,
	<XmPanedWindowWidgetClass>, <XmPanedWindowWidget>;
  export XmCreatePanedWindow;

  // from "PushB.h":
  export XmIsPushButton, xmPushButtonWidgetClass;
  export <XmPushButtonWidgetClass>, <XmPushButtonWidget>;
  export XmCreatePushButton;

  // from "PushBG.h":
  export XmIsPushButtonGadget, xmPushButtonGadgetClass;
  export <XmPushButtonGadgetClass>, <XmPushButtonGadget>,
	<XmPushButtonGCacheObject>;
  export XmCreatePushButtonGadget;

  // from "RepType.h":
  export $XmREP-TYPE-INVALID, <XmRepTypeId>;
  export rep-type-name-value, rep-type-name-value-setter,
	value-names-value, value-names-value-setter, values-value,
	values-value-setter, num-values-value, num-values-value-setter,
	reverse-installed-value, reverse-installed-value-setter,
	rep-type-id-value, rep-type-id-value-setter, <XmRepTypeEntryRec>,
	<XmRepTypeEntryRec*>, <XmRepTypeEntry>, <XmRepTypeListRec>,
	<XmRepTypeList>;
  export XmRepTypeRegister, XmRepTypeAddReverse, XmRepTypeValidValue,
	XmRepTypeGetRegistered, XmRepTypeGetRecord, XmRepTypeGetId,
	XmRepTypeGetNameList, XmRepTypeInstallTearOffModelConverter;

  // from "RowColumn.h":
  export xmRowColumnWidgetClass;
  export <XmRowColumnWidgetClass>, <XmRowColumnWidget>, XmIsRowColumn,
	XmMenuPosition, XmCreateRowColumn, XmCreateWorkArea,
	XmCreateRadioBox, XmCreateOptionMenu, XmOptionLabelGadget,
	XmOptionButtonGadget, XmCreateMenuBar, XmCreatePopupMenu,
	XmCreatePulldownMenu, XmAddToPostFromList, XmRemoveFromPostFromList,
	XmGetPostedFromWidget, XmGetTearOffControl;

  // from "Scale.h":
  export xmScaleWidgetClass, XmIsScale, <XmScaleWidgetClass>,
	<XmScaleWidget>;
  export XmScaleSetValue, XmScaleGetValue, XmCreateScale;

  // from "Screen.h":
  export XmIsScreen;
  export <XmScreen>, <XmScreenClass>, xmScreenClass;
  export XmGetXmScreen;

  // from "ScrollBar.h":
  export xmScrollBarWidgetClass;
  export <XmScrollBarWidgetClass>, <XmScrollBarWidget>, XmIsScrollBar,
	XmCreateScrollBar, XmScrollBarGetValues, XmScrollBarSetValues;

  // from "ScrolledW.h":
  export XmIsScrolledWindow, xmScrolledWindowWidgetClass;
  export <XmScrolledWindowWidgetClass>, <XmScrolledWindowWidget>;
  export XmScrolledWindowSetAreas, XmCreateScrolledWindow,
	XmScrollVisible;

  // from "SelectioB.h":
  export xmSelectionBoxWidgetClass;
  export <XmSelectionBoxWidgetClass>, <XmSelectionBoxWidget>;
  export XmIsSelectionBox;
  export XmSelectionBoxGetChild, XmCreateSelectionBox,
	XmCreateSelectionDialog, XmCreatePromptDialog;

  // from "SeparatoG.h":
  export XmIsSeparatorGadget, xmSeparatorGadgetClass;
  export <XmSeparatorGadgetClass>, <XmSeparatorGadget>,
	<XmSeparatorGCacheObject>;
  export XmCreateSeparatorGadget;

  // from "Separator.h":
  export XmIsSeparator, xmSeparatorWidgetClass;
  export <XmSeparatorWidgetClass>, <XmSeparatorWidget>;
  export XmCreateSeparator;

  // from "Text.h":
  export <XmTextSource>, <XmTextWidgetClass>, <XmTextWidget>,
	xmTextWidgetClass;
  export XmIsText, XmTextSetHighlight, XmCreateScrolledText,
	XmCreateText, XmTextGetSubstring, XmTextGetSubstringWcs,
	XmTextGetString, XmTextGetStringWcs, XmTextGetLastPosition,
	XmTextSetString, XmTextSetStringWcs, XmTextReplace, XmTextReplaceWcs,
	XmTextInsert, XmTextInsertWcs, XmTextSetAddMode, XmTextGetAddMode,
	XmTextGetEditable, XmTextSetEditable, XmTextGetMaxLength,
	XmTextSetMaxLength, XmTextGetTopCharacter, XmTextSetTopCharacter,
	XmTextGetCursorPosition, XmTextGetInsertionPosition,
	XmTextSetInsertionPosition, XmTextSetCursorPosition, XmTextRemove,
	XmTextCopy, XmTextCut, XmTextPaste, XmTextGetSelection,
	XmTextGetSelectionWcs, XmTextSetSelection, XmTextClearSelection,
	XmTextGetSelectionPosition, XmTextXYToPos, XmTextPosToXY,
	XmTextGetSource, XmTextSetSource, XmTextShowPosition, XmTextScroll,
	XmTextGetBaseline, XmTextDisableRedisplay, XmTextEnableRedisplay,
	XmTextFindString, XmTextFindStringWcs;

  // from "TextF.h":
  export <XmTextFieldWidgetClass>, <XmTextFieldWidget>;
  export xmTextFieldWidgetClass;
  export XmIsTextField;
  export ToggleCursorGC, XmTextFieldGetString,
	XmTextFieldGetSubstring, XmTextFieldGetStringWcs,
	XmTextFieldGetSubstringWcs, XmTextFieldGetLastPosition,
	XmTextFieldSetString, XmTextFieldSetStringWcs, XmTextFieldReplace,
	XmTextFieldReplaceWcs, XmTextFieldInsert, XmTextFieldInsertWcs,
	XmTextFieldSetAddMode, XmTextFieldGetAddMode, XmTextFieldGetEditable,
	XmTextFieldSetEditable, XmTextFieldGetMaxLength,
	XmTextFieldSetMaxLength, XmTextFieldGetCursorPosition,
	XmTextFieldGetInsertionPosition, XmTextFieldSetCursorPosition,
	XmTextFieldSetInsertionPosition, XmTextFieldGetSelectionPosition,
	XmTextFieldGetSelection, XmTextFieldGetSelectionWcs,
	XmTextFieldRemove, XmTextFieldCopy, XmTextFieldCut, XmTextFieldPaste,
	XmTextFieldClearSelection, XmTextFieldSetSelection,
	XmTextFieldXYToPos, XmTextFieldPosToXY, XmTextFieldShowPosition,
	XmTextFieldSetHighlight, XmTextFieldGetBaseline, XmCreateTextField;

  // from "ToggleB.h":
  export xmToggleButtonWidgetClass;
  export <XmToggleButtonWidgetClass>, <XmToggleButtonWidget>,
	XmIsToggleButton, XmToggleButtonGetState, XmToggleButtonSetState,
	XmCreateToggleButton;

  // from "ToggleBG.h":
  export xmToggleButtonGadgetClass;
  export <XmToggleButtonGadgetClass>, <XmToggleButtonGadget>,
	<XmToggleButtonGCacheObject>;
  export XmIsToggleButtonGadget, XmToggleButtonGadgetGetState,
	XmToggleButtonGadgetSetState, XmCreateToggleButtonGadget;

  // from "VendorS.h":
  export XmIsVendorShell, <XmVendorShellWidget>,
	<XmVendorShellWidgetClass>;
  export XmIsMotifWMRunning;

  // from "VirtKeys.h":
  export $osfXK-BackSpace, $osfXK-Insert, $osfXK-Delete, $osfXK-Copy,
	$osfXK-Cut, $osfXK-Paste, $osfXK-AddMode, $osfXK-PrimaryPaste,
	$osfXK-QuickPaste, $osfXK-PageLeft, $osfXK-PageUp, $osfXK-PageDown,
	$osfXK-PageRight, $osfXK-EndLine, $osfXK-BeginLine, $osfXK-Activate,
	$osfXK-MenuBar, $osfXK-Clear, $osfXK-Cancel, $osfXK-Help,
	$osfXK-Menu, $osfXK-Select, $osfXK-Undo, $osfXK-Left, $osfXK-Up,
	$osfXK-Right, $osfXK-Down, XmTranslateKey;

  // from "XmStrDefs.h":
  export %XmStrings, $XmS, $XmCAccelerator, $XmCAcceleratorText,
	$XmCAdjustLast, $XmCAdjustMargin, $XmCAlignment, $XmCAllowOverlap,
	$XmCAnimationMask, $XmCAnimationPixmap, $XmCAnimationPixmapDepth,
	$XmCAnimationStyle, $XmCApplyLabelString, $XmCArmCallback,
	$XmCArmColor, $XmCArmPixmap, $XmCArrowDirection, $XmCAttachment,
	$XmCAudibleWarning, $XmCAutoShowCursorPosition, $XmCAutoUnmanage,
	$XmCAutomaticSelection, $XmCAvailability, $XmCBackgroundPixmap,
	$XmCBlendModel, $XmCBlinkRate, $XmCBottomShadowColor,
	$XmCBottomShadowPixmap, $XmCButtonAcceleratorText,
	$XmCButtonAccelerators, $XmCButtonCount, $XmCButtonFontList,
	$XmCButtonMnemonicCharSets, $XmCButtonMnemonics, $XmCButtonSet,
	$XmCButtonType, $XmCButtons, $XmCCancelLabelString,
	$XmCChildHorizontalAlignment, $XmCChildHorizontalSpacing,
	$XmCChildPlacement, $XmCChildType, $XmCChildVerticalAlignment,
	$XmCChildren, $XmCClientData, $XmCClipWindow, $XmCColumns,
	$XmCCommandWindow, $XmCCommandWindowLocation, $XmCConvertProc,
	$XmCCursorBackground, $XmCCursorForeground, $XmCCursorPosition,
	$XmCCursorPositionVisible, $XmCDarkThreshold, $XmCDecimalPoints,
	$XmCDefaultButtonShadowThickness, $XmCDefaultButtonType,
	$XmCDefaultCopyCursorIcon, $XmCDefaultFontList,
	$XmCDefaultInvalidCursorIcon, $XmCDefaultLinkCursorIcon,
	$XmCDefaultMoveCursorIcon, $XmCDefaultNoneCursorIcon,
	$XmCDefaultPosition, $XmCDefaultSourceCursorIcon,
	$XmCDefaultValidCursorIcon, $XmCDeleteResponse, $XmCDesktopParent,
	$XmCDialogStyle, $XmCDialogTitle, $XmCDialogType,
	$XmCDirListItemCount, $XmCDirListItems, $XmCDirListLabelString,
	$XmCDirMask, $XmCDirSearchProc, $XmCDirSpec, $XmCDirectory,
	$XmCDirectoryValid, $XmCDisarmCallback, $XmCDoubleClickInterval,
	$XmCDragContextClass, $XmCDragDropFinishCallback, $XmCDragIconClass,
	$XmCDragInitiatorProtocolStyle, $XmCDragMotionCallback,
	$XmCDragOperations, $XmCDragOverMode, $XmCDragProc,
	$XmCDragReceiverProtocolStyle, $XmCDropProc, $XmCDropRectangles,
	$XmCDropSiteActivity, $XmCDropSiteEnterCallback,
	$XmCDropSiteLeaveCallback, $XmCDropSiteManagerClass,
	$XmCDropSiteOperations, $XmCDropSiteType, $XmCDropStartCallback,
	$XmCDropTransferClass, $XmCDropTransfers, $XmCEditable,
	$XmCEntryBorder, $XmCEntryClass, $XmCExportTargets,
	$XmCExposeCallback, $XmCExtensionType, $XmCFileListItemCount,
	$XmCFileListItems, $XmCFileListLabelString, $XmCFileSearchProc,
	$XmCFileTypeMask, $XmCFillOnArm, $XmCFillOnSelect,
	$XmCFilterLabelString, $XmCFontList, $XmCForegroundThreshold,
	$XmCHelpLabelString, $XmCHighlightColor, $XmCHighlightOnEnter,
	$XmCHighlightPixmap, $XmCHighlightThickness, $XmCHorizontalFontUnit,
	$XmCHorizontalScrollBar, $XmCHot, $XmCICCHandle, $XmCImportTargets,
	$XmCIncrement, $XmCIncremental, $XmCIndicatorOn, $XmCIndicatorSize,
	$XmCIndicatorType, $XmCInitialDelay, $XmCInitialFocus,
	$XmCInputCreate, $XmCInputMethod, $XmCInvalidCursorForeground,
	$XmCIsAligned, $XmCIsHomogeneous, $XmCItemCount, $XmCItems,
	$XmCKeyboardFocusPolicy, $XmCLabelFontList,
	$XmCLabelInsensitivePixmap, $XmCLabelPixmap, $XmCLabelString,
	$XmCLabelType, $XmCLightThreshold, $XmCListLabelString,
	$XmCListMarginHeight, $XmCListMarginWidth, $XmCListSizePolicy,
	$XmCListSpacing, $XmCListUpdated, $XmCLogicalParent,
	$XmCMainWindowMarginHeight, $XmCMainWindowMarginWidth,
	$XmCMappingDelay, $XmCMarginBottom, $XmCMarginHeight, $XmCMarginLeft,
	$XmCMarginRight, $XmCMarginTop, $XmCMarginWidth, $XmCMask,
	$XmCMaxItems, $XmCMaxLength, $XmCMaxValue, $XmCMaximum, $XmCMenuBar,
	$XmCMenuPost, $XmCMenuWidget, $XmCMessageProc, $XmCMessageWindow,
	$XmCMinimizeButtons, $XmCMinimum, $XmCMnemonic, $XmCMnemonicCharSet,
	$XmCMoveOpaque, $XmCMultiClick, $XmCMustMatch, $XmCMwmDecorations,
	$XmCMwmFunctions, $XmCMwmInputMode, $XmCMwmMenu, $XmCMwmMessages,
	$XmCNavigationType, $XmCNeedsMotion, $XmCNoMatchString, $XmCNoResize,
	$XmCNoneCursorForeground, $XmCNotifyProc, $XmCNumChildren,
	$XmCNumColumns, $XmCNumDropRectangles, $XmCNumDropTransfers,
	$XmCNumExportTargets, $XmCNumImportTargets, $XmCOffset,
	$XmCOkLabelString, $XmCOperationChangedCallback,
	$XmCOperationCursorIcon, $XmCOptionLabel, $XmCOptionMnemonic,
	$XmCOutputCreate, $XmCPacking, $XmCPageIncrement, $XmCPaneMaximum,
	$XmCPaneMinimum, $XmCPattern, $XmCPendingDelete, $XmCPopupEnabled,
	$XmCPositionIndex, $XmCPostFromButton, $XmCPostFromCount,
	$XmCPostFromList, $XmCPreeditType, $XmCProcessingDirection,
	$XmCPromptString, $XmCProtocolCallback, $XmCPushButtonEnabled,
	$XmCQualifySearchDataProc, $XmCRadioAlwaysOne, $XmCRadioBehavior,
	$XmCRecomputeSize, $XmCRectangles, $XmCRepeatDelay,
	$XmCResizeCallback, $XmCResizeHeight, $XmCResizePolicy,
	$XmCResizeWidth, $XmCRowColumnType, $XmCRows, $XmCRubberPositioning,
	$XmCSashHeight, $XmCSashIndent, $XmCSashWidth, $XmCScaleHeight,
	$XmCScaleMultiple, $XmCScaleWidth, $XmCScroll,
	$XmCScrollBarDisplayPolicy, $XmCScrollBarPlacement, $XmCScrollSide,
	$XmCScrolledWindowMarginHeight, $XmCScrolledWindowMarginWidth,
	$XmCScrollingPolicy, $XmCSelectColor, $XmCSelectInsensitivePixmap,
	$XmCSelectPixmap, $XmCSelectThreshold, $XmCSelectedItemCount,
	$XmCSelectedItems, $XmCSelectionArrayCount, $XmCSelectionLabelString,
	$XmCSelectionPolicy, $XmCSeparatorOn, $XmCSeparatorType, $XmCSet,
	$XmCShadowThickness, $XmCShadowType, $XmCShellUnitType,
	$XmCShowArrows, $XmCShowAsDefault, $XmCShowSeparator, $XmCShowValue,
	$XmCSimpleCheckBox, $XmCSimpleMenuBar, $XmCSimpleOptionMenu,
	$XmCSimplePopupMenu, $XmCSimplePulldownMenu, $XmCSimpleRadioBox,
	$XmCSizePolicy, $XmCSliderSize, $XmCSource, $XmCSourceCursorIcon,
	$XmCSourceIsExternal, $XmCSourcePixmapIcon, $XmCSourceWidget,
	$XmCSourceWindow, $XmCSpacing, $XmCStartTime, $XmCStateCursorIcon,
	$XmCStringDirection, $XmCTearOffModel, $XmCTextFontList,
	$XmCTextString, $XmCTextValue, $XmCTitleString, $XmCTopCharacter,
	$XmCTopItemPosition, $XmCTopLevelEnterCallback,
	$XmCTopLevelLeaveCallback, $XmCTopShadowColor, $XmCTopShadowPixmap,
	$XmCTransferProc, $XmCTransferStatus, $XmCTraversalOn,
	$XmCTraversalType, $XmCTreeUpdateProc, $XmCTroughColor, $XmCUnitType,
	$XmCUnpostBehavior, $XmCUnselectPixmap, $XmCUpdateSliderSize,
	$XmCUseAsyncGeometry, $XmCUserData, $XmCValidCursorForeground,
	$XmCValueChangedCallback, $XmCValueWcs, $XmCVerifyBell,
	$XmCVerticalAlignment, $XmCVerticalFontUnit, $XmCVerticalScrollBar,
	$XmCVisibleItemCount, $XmCVisibleWhenOff, $XmCVisualPolicy,
	$XmCWhichButton, $XmCWordWrap, $XmCWorkWindow, $XmCXmString,
	$XmNaccelerator, $XmNacceleratorText, $XmNactivateCallback,
	$XmNadjustLast, $XmNadjustMargin, $XmNalignment, $XmNallowOverlap,
	$XmNallowResize, $XmNanimationMask, $XmNanimationPixmap,
	$XmNanimationPixmapDepth, $XmNanimationStyle, $XmNapplyCallback,
	$XmNapplyLabelString, $XmNarmCallback, $XmNarmColor, $XmNarmPixmap,
	$XmNarrowDirection, $XmNattachment, $XmNaudibleWarning,
	$XmNautoShowCursorPosition, $XmNautoUnmanage, $XmNautomaticSelection,
	$XmNavailability, $XmNblendModel, $XmNblinkRate,
	$XmNbottomAttachment, $XmNbottomOffset, $XmNbottomPosition,
	$XmNbottomShadowColor, $XmNbottomShadowPixmap, $XmNbottomWidget,
	$XmNbrowseSelectionCallback, $XmNbuttonAcceleratorText,
	$XmNbuttonAccelerators, $XmNbuttonCount, $XmNbuttonFontList,
	$XmNbuttonMnemonicCharSets, $XmNbuttonMnemonics, $XmNbuttonSet,
	$XmNbuttonType, $XmNbuttons, $XmNcancelButton, $XmNcancelCallback,
	$XmNcancelLabelString, $XmNcascadePixmap, $XmNcascadingCallback,
	$XmNchildHorizontalAlignment, $XmNchildHorizontalSpacing,
	$XmNchildPlacement, $XmNchildPosition, $XmNchildType,
	$XmNchildVerticalAlignment, $XmNclientData, $XmNclipWindow,
	$XmNcolumns, $XmNcommand, $XmNcommandChangedCallback,
	$XmNcommandEnteredCallback, $XmNcommandWindow,
	$XmNcommandWindowLocation, $XmNconvertProc, $XmNcursorBackground,
	$XmNcursorForeground, $XmNcursorPosition, $XmNcursorPositionVisible,
	$XmNdarkThreshold, $XmNdecimalPoints, $XmNdecrementCallback,
	$XmNdefaultActionCallback, $XmNdefaultButton,
	$XmNdefaultButtonShadowThickness, $XmNdefaultButtonType,
	$XmNdefaultCopyCursorIcon, $XmNdefaultFontList,
	$XmNdefaultInvalidCursorIcon, $XmNdefaultLinkCursorIcon,
	$XmNdefaultMoveCursorIcon, $XmNdefaultNoneCursorIcon,
	$XmNdefaultPosition, $XmNdefaultSourceCursorIcon,
	$XmNdefaultValidCursorIcon, $XmNdeleteResponse, $XmNdesktopParent,
	$XmNdialogStyle, $XmNdialogTitle, $XmNdialogType,
	$XmNdirListItemCount, $XmNdirListItems, $XmNdirListLabelString,
	$XmNdirMask, $XmNdirSearchProc, $XmNdirSpec, $XmNdirectory,
	$XmNdirectoryValid, $XmNdisarmCallback, $XmNdoubleClickInterval,
	$XmNdragCallback, $XmNdragContextClass, $XmNdragDropFinishCallback,
	$XmNdragIconClass, $XmNdragInitiatorProtocolStyle,
	$XmNdragMotionCallback, $XmNdragOperations, $XmNdragOverMode,
	$XmNdragProc, $XmNdragReceiverProtocolStyle, $XmNdropFinishCallback,
	$XmNdropProc, $XmNdropRectangles, $XmNdropSiteActivity,
	$XmNdropSiteEnterCallback, $XmNdropSiteLeaveCallback,
	$XmNdropSiteManagerClass, $XmNdropSiteOperations, $XmNdropSiteType,
	$XmNdropStartCallback, $XmNdropTransferClass, $XmNdropTransfers,
	$XmNeditMode, $XmNeditable, $XmNentryAlignment, $XmNentryBorder,
	$XmNentryCallback, $XmNentryClass, $XmNentryVerticalAlignment,
	$XmNexportTargets, $XmNexposeCallback, $XmNextendedSelectionCallback,
	$XmNextensionType, $XmNfileListItemCount, $XmNfileListItems,
	$XmNfileListLabelString, $XmNfileSearchProc, $XmNfileTypeMask,
	$XmNfillOnArm, $XmNfillOnSelect, $XmNfilterLabelString,
	$XmNfocusCallback, $XmNfocusMovedCallback, $XmNfocusPolicyChanged,
	$XmNfontList, $XmNforegroundThreshold, $XmNfractionBase,
	$XmNgainPrimaryCallback, $XmNhelpCallback, $XmNhelpLabelString,
	$XmNhighlightColor, $XmNhighlightOnEnter, $XmNhighlightPixmap,
	$XmNhighlightThickness, $XmNhistoryItemCount, $XmNhistoryItems,
	$XmNhistoryMaxItems, $XmNhistoryVisibleItemCount,
	$XmNhorizontalFontUnit, $XmNhorizontalScrollBar,
	$XmNhorizontalSpacing, $XmNhotX, $XmNhotY, $XmNiccHandle,
	$XmNimportTargets, $XmNincrement, $XmNincrementCallback,
	$XmNincremental, $XmNindicatorOn, $XmNindicatorSize,
	$XmNindicatorType, $XmNinitialDelay, $XmNinitialFocus,
	$XmNinputCallback, $XmNinputCreate, $XmNinputMethod,
	$XmNinvalidCursorForeground, $XmNisAligned, $XmNisHomogeneous,
	$XmNitemCount, $XmNitems, $XmNkeyboardFocusPolicy, $XmNlabelFontList,
	$XmNlabelInsensitivePixmap, $XmNlabelPixmap, $XmNlabelString,
	$XmNlabelType, $XmNleftAttachment, $XmNleftOffset, $XmNleftPosition,
	$XmNleftWidget, $XmNlightThreshold, $XmNlineSpace, $XmNlistItemCount,
	$XmNlistItems, $XmNlistLabelString, $XmNlistMarginHeight,
	$XmNlistMarginWidth, $XmNlistSizePolicy, $XmNlistSpacing,
	$XmNlistUpdated, $XmNlistVisibleItemCount, $XmNlogicalParent,
	$XmNlosePrimaryCallback, $XmNlosingFocusCallback,
	$XmNmainWindowMarginHeight, $XmNmainWindowMarginWidth,
	$XmNmapCallback, $XmNmappingDelay, $XmNmargin, $XmNmarginBottom,
	$XmNmarginHeight, $XmNmarginLeft, $XmNmarginRight, $XmNmarginTop,
	$XmNmarginWidth, $XmNmask, $XmNmaxLength, $XmNmaximum,
	$XmNmenuAccelerator, $XmNmenuBar, $XmNmenuCursor, $XmNmenuHelpWidget,
	$XmNmenuHistory, $XmNmenuPost, $XmNmessageAlignment, $XmNmessageProc,
	$XmNmessageString, $XmNmessageWindow, $XmNminimizeButtons,
	$XmNminimum, $XmNmnemonic, $XmNmnemonicCharSet,
	$XmNmodifyVerifyCallback, $XmNmodifyVerifyCallbackWcs,
	$XmNmotionVerifyCallback, $XmNmoveOpaque, $XmNmultiClick,
	$XmNmultipleSelectionCallback, $XmNmustMatch, $XmNmwmDecorations,
	$XmNmwmFunctions, $XmNmwmInputMode, $XmNmwmMenu, $XmNmwmMessages,
	$XmNnavigationType, $XmNneedsMotion, $XmNnoMatchCallback,
	$XmNnoMatchString, $XmNnoResize, $XmNnoneCursorForeground,
	$XmNnotifyProc, $XmNnumColumns, $XmNnumDropRectangles,
	$XmNnumDropTransfers, $XmNnumExportTargets, $XmNnumImportTargets,
	$XmNnumRectangles, $XmNoffsetX, $XmNoffsetY, $XmNokCallback,
	$XmNokLabelString, $XmNoperationChangedCallback,
	$XmNoperationCursorIcon, $XmNoptionLabel, $XmNoptionMnemonic,
	$XmNoutputCreate, $XmNpacking, $XmNpageDecrementCallback,
	$XmNpageIncrement, $XmNpageIncrementCallback, $XmNpaneMaximum,
	$XmNpaneMinimum, $XmNpattern, $XmNpendingDelete, $XmNpopupEnabled,
	$XmNpositionIndex, $XmNpostFromButton, $XmNpostFromCount,
	$XmNpostFromList, $XmNpreeditType, $XmNprocessingDirection,
	$XmNpromptString, $XmNprotocolCallback, $XmNpushButtonEnabled,
	$XmNqualifySearchDataProc, $XmNradioAlwaysOne, $XmNradioBehavior,
	$XmNrealizeCallback, $XmNrecomputeSize, $XmNrectangles,
	$XmNrefigureMode, $XmNrepeatDelay, $XmNresizable, $XmNresizeCallback,
	$XmNresizeHeight, $XmNresizePolicy, $XmNresizeWidth,
	$XmNrightAttachment, $XmNrightOffset, $XmNrightPosition,
	$XmNrightWidget, $XmNrowColumnType, $XmNrows, $XmNrubberPositioning,
	$XmNsashHeight, $XmNsashIndent, $XmNsashShadowThickness,
	$XmNsashWidth, $XmNscaleHeight, $XmNscaleMultiple, $XmNscaleWidth,
	$XmNscrollBarDisplayPolicy, $XmNscrollBarPlacement,
	$XmNscrollHorizontal, $XmNscrollLeftSide, $XmNscrollTopSide,
	$XmNscrollVertical, $XmNscrolledWindowMarginHeight,
	$XmNscrolledWindowMarginWidth, $XmNscrollingPolicy, $XmNselectColor,
	$XmNselectInsensitivePixmap, $XmNselectPixmap, $XmNselectThreshold,
	$XmNselectedItemCount, $XmNselectedItems, $XmNselectionArrayCount,
	$XmNselectionLabelString, $XmNselectionPolicy, $XmNseparatorOn,
	$XmNseparatorType, $XmNset, $XmNshadow, $XmNshadowThickness,
	$XmNshadowType, $XmNshellUnitType, $XmNshowArrows, $XmNshowAsDefault,
	$XmNshowSeparator, $XmNshowValue, $XmNsimpleCallback,
	$XmNsingleSelectionCallback, $XmNsizePolicy, $XmNskipAdjust,
	$XmNsliderSize, $XmNsource, $XmNsourceCursorIcon,
	$XmNsourceIsExternal, $XmNsourcePixmapIcon, $XmNsourceWidget,
	$XmNsourceWindow, $XmNspacing, $XmNspotLocation, $XmNstartTime,
	$XmNstateCursorIcon, $XmNstringDirection, $XmNsubMenuId,
	$XmNsymbolPixmap, $XmNtearOffMenuActivateCallback,
	$XmNtearOffMenuDeactivateCallback, $XmNtearOffModel,
	$XmNtextAccelerators, $XmNtextColumns, $XmNtextFontList,
	$XmNtextString, $XmNtextTranslations, $XmNtextValue, $XmNtitleString,
	$XmNtoBottomCallback, $XmNtoPositionCallback, $XmNtoTopCallback,
	$XmNtopAttachment, $XmNtopCharacter, $XmNtopItemPosition,
	$XmNtopLevelEnterCallback, $XmNtopLevelLeaveCallback, $XmNtopOffset,
	$XmNtopPosition, $XmNtopShadowColor, $XmNtopShadowPixmap,
	$XmNtopWidget, $XmNtransferProc, $XmNtransferStatus,
	$XmNtraversalCallback, $XmNtraversalOn, $XmNtraversalType,
	$XmNtraverseObscuredCallback, $XmNtreeUpdateProc, $XmNtroughColor,
	$XmNunitType, $XmNunmapCallback, $XmNunpostBehavior,
	$XmNunselectPixmap, $XmNupdateSliderSize, $XmNuseAsyncGeometry,
	$XmNuserData, $XmNvalidCursorForeground, $XmNvalueChangedCallback,
	$XmNvalueWcs, $XmNverifyBell, $XmNverticalFontUnit,
	$XmNverticalScrollBar, $XmNverticalSpacing, $XmNvisibleItemCount,
	$XmNvisibleWhenOff, $XmNvisualPolicy, $XmNwhichButton, $XmNwordWrap,
	$XmNworkWindow, $XmRAlignment, $XmRAnimationMask,
	$XmRAnimationPixmap, $XmRAnimationStyle, $XmRArrowDirection,
	$XmRAtomList, $XmRAttachment, $XmRAudibleWarning, $XmRAvailability,
	$XmRBackgroundPixmap, $XmRBlendModel, $XmRBooleanDimension,
	$XmRBottomShadowPixmap, $XmRButtonType, $XmRCallbackProc, $XmRChar,
	$XmRCharSetTable, $XmRChildHorizontalAlignment, $XmRChildPlacement,
	$XmRChildType, $XmRChildVerticalAlignment, $XmRCommandWindowLocation,
	$XmRCompoundText, $XmRDefaultButtonType, $XmRDeleteResponse,
	$XmRDialogStyle, $XmRDialogType, $XmRDoubleClickInterval,
	$XmRDragInitiatorProtocolStyle, $XmRDragReceiverProtocolStyle,
	$XmRDropSiteActivity, $XmRDropSiteOperations, $XmRDropSiteType,
	$XmRDropTransfers, $XmRExtensionType, $XmRFileTypeMask, $XmRFontList,
	$XmRGadgetPixmap, $XmRHighlightPixmap, $XmRHorizontalDimension,
	$XmRHorizontalInt, $XmRHorizontalPosition, $XmRIconAttachment,
	$XmRImportTargets, $XmRIndicatorType, $XmRItemCount, $XmRItems,
	$XmRKeySym, $XmRKeySymTable, $XmRKeyboardFocusPolicy, $XmRLabelType,
	$XmRListMarginHeight, $XmRListMarginWidth, $XmRListSizePolicy,
	$XmRListSpacing, $XmRManBottomShadowPixmap, $XmRManForegroundPixmap,
	$XmRManHighlightPixmap, $XmRManTopShadowPixmap, $XmRMenuWidget,
	$XmRMnemonic, $XmRMultiClick, $XmRNavigationType, $XmRPacking,
	$XmRPrimForegroundPixmap, $XmRProc, $XmRProcessingDirection,
	$XmRRectangleList, $XmRResizePolicy, $XmRRowColumnType,
	$XmRScrollBarDisplayPolicy, $XmRScrollBarPlacement,
	$XmRScrollingPolicy, $XmRSelectedItemCount, $XmRSelectedItems,
	$XmRSelectionPolicy, $XmRSelectionType, $XmRSeparatorType,
	$XmRShadowType, $XmRShellHorizDim, $XmRShellHorizPos,
	$XmRShellUnitType, $XmRShellVertDim, $XmRShellVertPos,
	$XmRSizePolicy, $XmRStringDirection, $XmRTearOffModel,
	$XmRTopShadowPixmap, $XmRTransferStatus, $XmRTraversalType,
	$XmRUnitType, $XmRUnpostBehavior, $XmRValueWcs,
	$XmRVerticalAlignment, $XmRVerticalDimension, $XmRVerticalInt,
	$XmRVerticalPosition, $XmRVirtualBinding, $XmRVisibleItemCount,
	$XmRVisualPolicy, $XmRWhichButton, $XmRXmBackgroundPixmap,
	$XmRXmString, $XmRXmStringCharSet, $XmRXmStringTable,
	$XmVosfActivate, $XmVosfAddMode, $XmVosfBackSpace, $XmVosfBeginLine,
	$XmVosfCancel, $XmVosfClear, $XmVosfCopy, $XmVosfCut, $XmVosfDelete,
	$XmVosfDown, $XmVosfEndLine, $XmVosfHelp, $XmVosfInsert, $XmVosfLeft,
	$XmVosfMenu, $XmVosfMenuBar, $XmVosfPageDown, $XmVosfPageLeft,
	$XmVosfPageRight, $XmVosfPageUp, $XmVosfPaste, $XmVosfPrimaryPaste,
	$XmVosfQuickPaste, $XmVosfRight, $XmVosfSelect, $XmVosfUndo,
	$XmVosfUp, $XmSFONTLIST-DEFAULT-TAG-STRING,
	$XmSXmFONTLIST-DEFAULT-TAG-STRING, $XmRTopItemPosition,
	$XmSTRING-DEFAULT-CHARSET, $XmSTRING-ISO8859-1,
	$XmFONTLIST-DEFAULT-TAG, $XmFONTLIST-DEFAULT-TAG-STRING,
	$XmVaCASCADEBUTTON, $XmVaCHECKBUTTON, $XmVaDOUBLE-SEPARATOR,
	$XmVaPUSHBUTTON, $XmVaRADIOBUTTON, $XmVaSEPARATOR,
	$XmVaSINGLE-SEPARATOR, $XmVaTOGGLEBUTTON, $XmVaTITLE,
	$XtCKeyboardFocusPolicy, $XtCShellUnitType, $XtNkeyboardFocusPolicy,
	$XtNshellUnitType, $XtRKeyboardFocusPolicy,
	$XmRPrimBottomShadowPixmap, $XmRPrimHighlightPixmap,
	$XmRPrimTopShadowPixmap, $XmCAccelerators, $XmCAllowShellResize,
	$XmCArgc, $XmCArgv, $XmCBackground, $XmCBaseHeight, $XmCBaseWidth,
	$XmCBitmap, $XmCBoolean, $XmCBorderColor, $XmCBorderWidth,
	$XmCCallback, $XmCColor, $XmCColormap, $XmCCreatePopupChildProc,
	$XmCCursor, $XmCDepth, $XmCDimension, $XmCEditMode, $XmCEditType,
	$XmCEventBindings, $XmCFile, $XmCFont, $XmCFontSet, $XmCForeground,
	$XmCFraction, $XmCFunction, $XmCGeometry, $XmCHSpace, $XmCHeight,
	$XmCHeightInc, $XmCIconMask, $XmCIconName, $XmCIconNameEncoding,
	$XmCIconPixmap, $XmCIconWindow, $XmCIconX, $XmCIconY, $XmCIconic,
	$XmCIndex, $XmCInitialResourcesPersistent, $XmCInitialState,
	$XmCInput, $XmCInsertPosition, $XmCInterval, $XmCJustify, $XmCLabel,
	$XmCLength, $XmCMappedWhenManaged, $XmCMargin, $XmCMaxAspectX,
	$XmCMaxAspectY, $XmCMaxHeight, $XmCMaxWidth, $XmCMenuEntry,
	$XmCMinAspectX, $XmCMinAspectY, $XmCMinHeight, $XmCMinWidth,
	$XmCNotify, $XmCOrientation, $XmCOverrideRedirect, $XmCParameter,
	$XmCPixmap, $XmCPosition, $XmCReadOnly, $XmCResize, $XmCReverseVideo,
	$XmCSaveUnder, $XmCScreen, $XmCScrollDCursor, $XmCScrollHCursor,
	$XmCScrollLCursor, $XmCScrollProc, $XmCScrollRCursor,
	$XmCScrollUCursor, $XmCScrollVCursor, $XmCSelection,
	$XmCSelectionArray, $XmCSensitive, $XmCSpace, $XmCString,
	$XmCTextOptions, $XmCTextPosition, $XmCTextSink, $XmCTextSource,
	$XmCThickness, $XmCThumb, $XmCTitle, $XmCTitleEncoding,
	$XmCTransient, $XmCTransientFor, $XmCTranslations, $XmCVSpace,
	$XmCValue, $XmCVisual, $XmCWaitForWm, $XmCWidget, $XmCWidth,
	$XmCWidthInc, $XmCWinGravity, $XmCWindow, $XmCWindowGroup,
	$XmCWmTimeout, $XmCX, $XmCY, $XmNaccelerators, $XmNallowShellResize,
	$XmNancestorSensitive, $XmNargc, $XmNargv, $XmNbackground,
	$XmNbackgroundPixmap, $XmNbaseHeight, $XmNbaseWidth, $XmNbitmap,
	$XmNborder, $XmNborderColor, $XmNborderPixmap, $XmNborderWidth,
	$XmNcallback, $XmNchildren, $XmNcolormap, $XmNcreatePopupChildProc,
	$XmNdepth, $XmNdestroyCallback, $XmNeditType, $XmNfile, $XmNfont,
	$XmNfontSet, $XmNforceBars, $XmNforeground, $XmNfunction,
	$XmNgeometry, $XmNheight, $XmNheightInc, $XmNhighlight, $XmNiconMask,
	$XmNiconName, $XmNiconNameEncoding, $XmNiconPixmap, $XmNiconWindow,
	$XmNiconX, $XmNiconY, $XmNiconic, $XmNindex,
	$XmNinitialResourcesPersistent, $XmNinitialState, $XmNinnerHeight,
	$XmNinnerWidth, $XmNinnerWindow, $XmNinput, $XmNinsertPosition,
	$XmNinternalHeight, $XmNinternalWidth, $XmNjumpProc, $XmNjustify,
	$XmNlength, $XmNlowerRight, $XmNmappedWhenManaged, $XmNmaxAspectX,
	$XmNmaxAspectY, $XmNmaxHeight, $XmNmaxWidth, $XmNmenuEntry,
	$XmNminAspectX, $XmNminAspectY, $XmNminHeight, $XmNminWidth,
	$XmNname, $XmNnotify, $XmNnumChildren, $XmNorientation,
	$XmNoverrideRedirect, $XmNparameter, $XmNpixmap, $XmNpopdownCallback,
	$XmNpopupCallback, $XmNresize, $XmNreverseVideo, $XmNsaveUnder,
	$XmNscreen, $XmNscrollDCursor, $XmNscrollHCursor, $XmNscrollLCursor,
	$XmNscrollProc, $XmNscrollRCursor, $XmNscrollUCursor,
	$XmNscrollVCursor, $XmNselection, $XmNselectionArray, $XmNsensitive,
	$XmNshown, $XmNspace, $XmNstring, $XmNtextOptions, $XmNtextSink,
	$XmNtextSource, $XmNthickness, $XmNthumb, $XmNthumbProc, $XmNtitle,
	$XmNtitleEncoding, $XmNtop, $XmNtransient, $XmNtransientFor,
	$XmNtranslations, $XmNupdate, $XmNuseBottom, $XmNuseRight, $XmNvalue,
	$XmNvisual, $XmNwaitForWm, $XmNwidth, $XmNwidthInc, $XmNwinGravity,
	$XmNwindow, $XmNwindowGroup, $XmNwmTimeout, $XmNx, $XmNy,
	$XmRAcceleratorTable, $XmRAtom, $XmRBitmap, $XmRBool, $XmRBoolean,
	$XmRCallProc, $XmRCallback, $XmRCardinal, $XmRColor, $XmRColormap,
	$XmRCursor, $XmRDimension, $XmRDisplay, $XmREditMode, $XmREnum,
	$XmRFile, $XmRFloat, $XmRFont, $XmRFontSet, $XmRFontStruct,
	$XmRFunction, $XmRGeometry, $XmRImmediate, $XmRInitialState, $XmRInt,
	$XmRJustify, $XmRLongBoolean, $XmROrientation, $XmRObject, $XmRPixel,
	$XmRPixmap, $XmRPointer, $XmRPosition, $XmRScreen, $XmRShort,
	$XmRString, $XmRStringArray, $XmRStringTable, $XmRTextPosition,
	$XmRTranslationTable, $XmRUnsignedChar, $XmRVisual, $XmRWidget,
	$XmRWidgetClass, $XmRWidgetList, $XmRWindow;

end module Motif;
