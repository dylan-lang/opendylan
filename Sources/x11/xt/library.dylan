module:    Dylan-user	
Synopsis:  FFI declarations translated from Xt header files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Xt
  use functional-dylan;
  use C-FFI;
  use Xlib;
  export Xt;
end library Xt;

define module Xt
  use functional-dylan;
  use C-FFI;
  use Xlib, export: all;


  // from "Xutil.h":
  export $NoValue, $XValue, $YValue, $WidthValue, $HeightValue,
	$AllValues, $XNegative, $YNegative, min-width-value,
	min-width-value-setter, min-height-value, min-height-value-setter,
	max-width-value, max-width-value-setter, max-height-value,
	max-height-value-setter, width-inc-value, width-inc-value-setter,
	height-inc-value, height-inc-value-setter, min-aspect-value,
	max-aspect-value, base-width-value, base-width-value-setter,
	base-height-value, base-height-value-setter, <XSizeHints>,
	<XSizeHints*>, $USPosition, $USSize, $PPosition, $PSize, $PMinSize,
	$PMaxSize, $PResizeInc, $PAspect, $PBaseSize, $PWinGravity;
  export input-value, input-value-setter, initial-state-value,
	initial-state-value-setter, icon-pixmap-value,
	icon-pixmap-value-setter, icon-window-value,
	icon-window-value-setter, icon-x-value, icon-x-value-setter,
	icon-y-value, icon-y-value-setter, icon-mask-value,
	icon-mask-value-setter, window-group-value,
	window-group-value-setter, <XWMHints>, <XWMHints*>, $InputHint,
	$StateHint, $IconPixmapHint, $IconWindowHint, $IconPositionHint,
	$IconMaskHint, $WindowGroupHint, $AllHints, $WithdrawnState,
	$NormalState, $IconicState;
  export $DontCareState, $ZoomState, $InactiveState;
  export encoding-value, encoding-value-setter, nitems-value,
	nitems-value-setter, <XTextProperty>, <XTextProperty*>, $XNoMemory,
	$XLocaleNotSupported, $XConverterNotFound, $XStringStyle,
	$XCompoundTextStyle, $XTextStyle, $XStdICCTextStyle;
  export min-width-value, min-width-value-setter, min-height-value,
	min-height-value-setter, max-width-value, max-width-value-setter,
	max-height-value, max-height-value-setter, width-inc-value,
	width-inc-value-setter, height-inc-value, height-inc-value-setter,
	<XIconSize>, <XIconSize*>;
  export res-name-value, res-name-value-setter, <XClassHint>,
	<XClassHint*>;
  export compose-ptr-value, compose-ptr-value-setter,
	chars-matched-value, chars-matched-value-setter, <XComposeStatus>,
	<XComposeStatus*>;
  export IsKeypadKey, IsCursorKey, IsPFKey, IsFunctionKey,
	IsMiscFunctionKey, IsModifierKey;
  export <Region>, $RectangleOut, $RectangleIn, $RectanglePart;
  export colormap-size-value, colormap-size-value-setter,
	<XVisualInfo>, <XVisualInfo*>, $VisualNoMask, $VisualIDMask,
	$VisualScreenMask, $VisualDepthMask, $VisualClassMask,
	$VisualRedMaskMask, $VisualGreenMaskMask, $VisualBlueMaskMask,
	$VisualColormapSizeMask, $VisualBitsPerRGBMask, $VisualAllMask,
	red-max-value, red-max-value-setter, red-mult-value,
	red-mult-value-setter, green-max-value, green-max-value-setter,
	green-mult-value, green-mult-value-setter, blue-max-value,
	blue-max-value-setter, blue-mult-value, blue-mult-value-setter,
	base-pixel-value, base-pixel-value-setter, killid-value,
	killid-value-setter, <XStandardColormap>, <XStandardColormap*>,
	$ReleaseByFreeingColormap;
  export $BitmapSuccess, $BitmapOpenFailed, $BitmapFileInvalid,
	$BitmapNoMemory;
  export $XCSUCCESS, $XCNOMEM, $XCNOENT, <C-XContext>, <XContext>,
	<XContext*>, XUniqueContext, XStringToContext, XAllocClassHint,
	XAllocIconSize, XAllocSizeHints, XAllocStandardColormap,
	XAllocWMHints, XClipBox, XCreateRegion, XDefaultString,
	XDeleteContext, XDestroyRegion, XEmptyRegion, XEqualRegion,
	XFindContext, XGetClassHint, XGetIconSizes, XGetNormalHints,
	XGetRGBColormaps, XGetSizeHints, XGetStandardColormap,
	XGetTextProperty, XGetVisualInfo, XGetWMClientMachine, XGetWMHints,
	XGetWMIconName, XGetWMName, XGetWMNormalHints, XGetWMSizeHints,
	XGetZoomHints, XIntersectRegion, XLookupString, XMatchVisualInfo,
	XOffsetRegion, XPointInRegion, XPolygonRegion, XRectInRegion,
	XSaveContext, XSetClassHint, XSetIconSizes, XSetNormalHints,
	XSetRGBColormaps, XSetSizeHints, XSetStandardProperties,
	XSetTextProperty, XSetWMClientMachine, XSetWMHints, XSetWMIconName,
	XSetWMName, XSetWMNormalHints, XSetWMProperties, XmbSetWMProperties,
	XSetWMSizeHints, XSetRegion, XSetStandardColormap, XSetZoomHints,
	XShrinkRegion, XStringListToTextProperty, XSubtractRegion,
	XmbTextListToTextProperty, XwcTextListToTextProperty,
	XwcFreeStringList, XTextPropertyToStringList,
	XmbTextPropertyToTextList, XwcTextPropertyToTextList,
	XUnionRectWithRegion, XUnionRegion, XWMGeometry, XXorRegion;

  // from "Xresource.h":
  export Xpermalloc;
  export <C-XrmQuark>, <XrmQuark>, <XrmQuark*>, <XrmQuarkList>,
	$NULLQUARK, <XrmString>, $NULLSTRING, XrmStringToQuark,
	XrmPermStringToQuark, XrmQuarkToString, XrmUniqueQuark;
  export $XrmBindTightly, $XrmBindLoosely, XrmStringToQuarkList,
	XrmStringToBindingQuarkList;
  export <XrmName>, <XrmNameList>, XrmNameToString, XrmStringToName,
	XrmStringToNameList, <XrmClass>, <XrmClassList>, XrmClassToString,
	XrmStringToClass, XrmStringToClassList;
  export <XrmRepresentation>, XrmStringToRepresentation,
	XrmRepresentationToString, size-value, size-value-setter, addr-value,
	addr-value-setter, <XrmValue>, <XrmValue*>, <XrmValuePtr>;
  export <XrmHashBucket>, <XrmHashTable>, <XrmDatabase>;
  export XrmDestroyDatabase, XrmQPutResource, XrmPutResource,
	XrmQPutStringResource, XrmPutStringResource, XrmPutLineResource,
	XrmQGetResource, XrmGetResource, XrmQGetSearchList,
	XrmQGetSearchResource, XrmSetDatabase, XrmGetDatabase,
	XrmGetFileDatabase, XrmCombineFileDatabase, XrmGetStringDatabase,
	XrmPutFileDatabase, XrmMergeDatabases, XrmCombineDatabase,
	$XrmEnumAllLevels, $XrmEnumOneLevel, XrmEnumerateDatabase,
	XrmLocaleOfDatabase;
  export $XrmoptionNoArg, $XrmoptionIsArg, $XrmoptionStickyArg,
	$XrmoptionSepArg, $XrmoptionResArg, $XrmoptionSkipArg,
	$XrmoptionSkipLine, $XrmoptionSkipNArgs;
  export option-value, option-value-setter, specifier-value,
	specifier-value-setter, argKind-value, argKind-value-setter,
	<XrmOptionDescRec>, <XrmOptionDescRec*>, <XrmOptionDescList>;
  export XrmParseCommand;

  // from "Intrinsic.h":
  export $XtSpecificationRelease, <X-String>, <Widget>, <WidgetList>,
	<WidgetClass>, <CompositeWidget>, <XtActionList>, <XtEventTable>;
  export <XtAppContext>, <C-XtValueMask>, <XtValueMask*>,
	<XtValueMask>, <C-XtIntervalId>, <XtIntervalId*>, <XtIntervalId>,
	<C-XtInputId>, <XtInputId*>, <XtInputId>, <C-XtWorkProcId>,
	<XtWorkProcId*>, <XtWorkProcId>, <XtGeometryMask>, <C-XtGCMask>,
	<XtGCMask*>, <XtGCMask>, <C-Pixel>, <Pixel*>, <Pixel>, <XtCacheType>,
	$XtCacheNone, $XtCacheAll, $XtCacheByDisplay, $XtCacheRefCount,
	<XtArgVal>, <C-XtEnum>, <XtEnum>;
  export <C-Cardinal>, <C-Dimension>, <C-Position>, <XtPointer>,
	<Opaque>, <XtTranslations>, <XtAccelerators>, <Modifiers>;
  export <XtActionProc>, <XtActionProc>-callback-wrapper;
  export <XtBoundActions>;
  export proc-value, proc-value-setter, <XtActionsRec>,
	<XtActionsRec*>;
  export $XtAddress, $XtBaseOffset, $XtImmediate, $XtResourceString,
	$XtResourceQuark, $XtWidgetBaseOffset, $XtProcedureArg;
  export address-mode-value, address-mode-value-setter,
	address-id-value, address-id-value-setter, size-value,
	size-value-setter, <XtConvertArgRec>, <XtConvertArgRec*>,
	<XtConvertArgList>;
  export <XtConvertArgProc>, <XtConvertArgProc>-callback-wrapper;
  export request-mode-value, request-mode-value-setter,
	<XtWidgetGeometry>, <XtWidgetGeometry*>, $XtCWQueryOnly,
	$XtSMDontChange, <XtConverter>, <XtConverter>-callback-wrapper;
  export <XtTypeConverter>, <XtTypeConverter>-callback-wrapper;
  export <XtDestructor>, <XtDestructor>-callback-wrapper;
  export <XtCacheRef>;
  export <XtActionHookId>;
  export <XtActionHookProc>, <XtActionHookProc>-callback-wrapper;
  export <XtKeyProc>, <XtKeyProc>-callback-wrapper;
  export <XtCaseProc>, <XtCaseProc>-callback-wrapper;
  export <XtEventHandler>, <XtEventHandler>-callback-wrapper,
	<C-EventMask>, <EventMask*>, <EventMask>;
  export $XtListHead, $XtListTail;
  export <C-XtInputMask>, <XtInputMask*>, <XtInputMask>,
	$XtInputNoneMask, $XtInputReadMask, $XtInputWriteMask,
	$XtInputExceptMask, <XtTimerCallbackProc>,
	<XtTimerCallbackProc>-callback-wrapper;
  export <XtInputCallbackProc>,
	<XtInputCallbackProc>-callback-wrapper;
  export <Arg>, <Arg*>, <ArgList>;
  export <XtVarArgsList>;
  export <XtCallbackProc>, <XtCallbackProc>-callback-wrapper;
  export closure-value, closure-value-setter, <XtCallbackRec>,
	<XtCallbackRec*>, <XtCallbackList>;
  export $XtCallbackNoList, $XtCallbackHasNone, $XtCallbackHasSome;
  export $XtGeometryYes, $XtGeometryNo, $XtGeometryAlmost,
	$XtGeometryDone;
  export $XtGrabNone, $XtGrabNonexclusive, $XtGrabExclusive;
  export shell-widget-value, shell-widget-value-setter,
	enable-widget-value, enable-widget-value-setter, <XtPopdownIDRec>,
	<XtPopdownIDRec*>, <XtPopdownID>;
  export resource-name-value, resource-name-value-setter,
	resource-class-value, resource-class-value-setter,
	resource-type-value, resource-type-value-setter, resource-size-value,
	resource-size-value-setter, resource-offset-value,
	resource-offset-value-setter, default-type-value,
	default-type-value-setter, default-addr-value,
	default-addr-value-setter, <XtResource>, <XtResource*>,
	<XtResourceList>;
  export <XtResourceDefaultProc>,
	<XtResourceDefaultProc>-callback-wrapper;
  export <XtLanguageProc>, <XtLanguageProc>-callback-wrapper;
  export <XtErrorMsgHandler>, <XtErrorMsgHandler>-callback-wrapper;
  export <XtErrorHandler>, <XtErrorHandler>-callback-wrapper;
  export <XtCreatePopupChildProc>,
	<XtCreatePopupChildProc>-callback-wrapper;
  export <XtWorkProc>, <XtWorkProc>-callback-wrapper;
  export match-value, match-value-setter, substitution-value,
	substitution-value-setter, <SubstitutionRec>, <SubstitutionRec*>,
	<Substitution>;
  export <XtFilePredicate>, <XtFilePredicate>-callback-wrapper;
  export <XtRequestId>;
  export <XtConvertSelectionProc>,
	<XtConvertSelectionProc>-callback-wrapper;
  export <XtLoseSelectionProc>,
	<XtLoseSelectionProc>-callback-wrapper;
  export <XtSelectionDoneProc>,
	<XtSelectionDoneProc>-callback-wrapper;
  export <XtSelectionCallbackProc>,
	<XtSelectionCallbackProc>-callback-wrapper;
  export <XtLoseSelectionIncrProc>,
	<XtLoseSelectionIncrProc>-callback-wrapper;
  export <XtSelectionDoneIncrProc>,
	<XtSelectionDoneIncrProc>-callback-wrapper;
  export <XtConvertSelectionIncrProc>,
	<XtConvertSelectionIncrProc>-callback-wrapper;
  export <XtCancelConvertSelectionProc>,
	<XtCancelConvertSelectionProc>-callback-wrapper, XtConvertAndStore,
	XtCallConverter, XtDispatchEvent, XtCallAcceptFocus, XtAppPeekEvent,
	XtIsSubclass, XtIsObject, %XtCheckSubclassFlag, %XtIsSubclassOf,
	XtIsManaged, XtIsRealized, XtIsSensitive, XtOwnSelection,
	XtOwnSelectionIncremental, XtMakeResizeRequest, XtTranslateCoords,
	XtGetKeysymTable, XtKeysymToKeycodeList,
	XtDisplayStringConversionWarning, colorConvertArgs, screenConvertArg,
	XtSetTypeConverter, XtAppSetTypeConverter, XtParseTranslationTable,
	XtParseAcceleratorTable, XtOverrideTranslations,
	XtAugmentTranslations, XtInstallAccelerators,
	XtInstallAllAccelerators, XtUninstallTranslations, XtAppAddActions,
	XtAppAddActionHook, XtRemoveActionHook, XtGetActionList,
	XtCallActionProc, XtRegisterGrabAction, XtSetMultiClickTime,
	XtGetMultiClickTime, XtGetActionKeysym, XtTranslateKeycode,
	XtTranslateKey, XtSetKeyTranslator, XtRegisterCaseConverter,
	XtConvertCase, $XtAllEvents, XtAddEventHandler, XtRemoveEventHandler,
	XtAddRawEventHandler, XtRemoveRawEventHandler, XtInsertEventHandler,
	XtInsertRawEventHandler, XtBuildEventMask, XtAddGrab, XtRemoveGrab,
	XtAppProcessEvent, XtAppMainLoop, XtAddExposureToRegion,
	XtSetKeyboardFocus, XtLastTimestampProcessed, XtAppAddTimeOut,
	XtRemoveTimeOut, XtAppAddInput, XtRemoveInput, XtAppNextEvent,
	$XtIMXEvent, $XtIMTimer, $XtIMAlternateInput, $XtIMAll, XtAppPending,
	XtIsRectObj, XtIsWidget, XtIsComposite, XtIsConstraint, XtIsShell,
	XtIsOverrideShell, XtIsWMShell, XtIsTransientShell,
	XtIsTopLevelShell, XtIsApplicationShell, XtRealizeWidget,
	XtDestroyWidget, XtSetSensitive, XtSetMappedWhenManaged,
	XtNameToWidget, XtWindowToWidget;
  export XtMergeArgLists, $XtVaNestedList, $XtVaTypedArg, XtDisplay,
	XtDisplayOfObject, XtScreen, XtScreenOfObject, XtWindow,
	XtWindowOfObject, XtName, XtSuperclass, XtClass, XtParent,
	XtMapWidget, XtUnmapWidget, XtAddCallback, XtRemoveCallback,
	XtAddCallbacks, XtRemoveCallbacks, XtRemoveAllCallbacks;
  export XtCallCallbacks, XtCallCallbackList, XtHasCallbacks;
  export XtMakeGeometryRequest, XtQueryGeometry, XtCreatePopupShell,
	XtPopup, XtPopupSpringLoaded, XtCallbackNone, XtCallbackNonexclusive,
	XtCallbackExclusive, XtPopdown, XtCallbackPopdown, XtMenuPopupAction,
	XtCreateWidget, XtCreateManagedWidget, XtAppCreateShell,
	XtToolkitInitialize, XtSetLanguageProc, XtDisplayInitialize,
	XtAppInitialize, XtOpenDisplay, XtCreateApplicationContext,
	XtAppSetFallbackResources, XtDestroyApplicationContext,
	XtInitializeWidgetClass, XtWidgetToApplicationContext,
	XtDisplayToApplicationContext, XtDatabase, XtScreenDatabase,
	XtCloseDisplay, XtGetApplicationResources, XtGetSubresources,
	XtSetValues, XtGetValues, XtSetSubvalues, XtGetSubvalues,
	XtGetResourceList, XtGetConstraintResourceList, $XtUnspecifiedPixmap,
	$XtUnspecifiedShellInt, $XtUnspecifiedWindow,
	$XtUnspecifiedWindowGroup, $XtDefaultForeground,
	$XtDefaultBackground, $XtDefaultFont, $XtDefaultFontSet,
	XtAppSetErrorMsgHandler, XtAppSetWarningMsgHandler, XtAppErrorMsg,
	XtAppWarningMsg, XtAppSetErrorHandler, XtAppSetWarningHandler,
	XtAppError, XtAppWarning, XtAppGetErrorDatabase,
	XtAppGetErrorDatabaseText, XtMalloc, XtCalloc, XtRealloc, XtFree,
	XtAppAddWorkProc, XtRemoveWorkProc;
  export XtGetGC, XtAllocateGC, XtReleaseGC;
  export XtAppReleaseCacheRefs, XtCallbackReleaseCacheRef,
	XtCallbackReleaseCacheRefList, XtSetWMColormapWindows, XtFindFile,
	XtResolvePathname, $XT-CONVERT-FAIL, XtDisownSelection,
	XtGetSelectionValue, XtGetSelectionValues, XtAppSetSelectionTimeout,
	XtAppGetSelectionTimeout, XtGetSelectionRequest,
	XtGetSelectionValueIncremental, XtGetSelectionValuesIncremental,
	XtGrabKey, XtUngrabKey, XtGrabKeyboard, XtUngrabKeyboard,
	XtGrabButton, XtUngrabButton, XtGrabPointer, XtUngrabPointer,
	XtGetApplicationNameAndClass;
  export XtCvtStringToAcceleratorTable, XtCvtStringToAtom,
	XtCvtStringToBoolean, XtCvtStringToBool, XtCvtStringToCursor,
	XtCvtStringToDimension, XtCvtStringToDisplay, XtCvtStringToFile,
	XtCvtStringToFloat, XtCvtStringToFont, XtCvtStringToFontSet,
	XtCvtStringToFontStruct, XtCvtStringToInt, XtCvtStringToInitialState,
	XtCvtStringToPixel, XtCvtStringToPosition, XtCvtStringToShort,
	XtCvtStringToTranslationTable, XtCvtStringToUnsignedChar,
	XtCvtStringToVisual, XtCvtIntToBoolean, XtCvtIntToBool,
	XtCvtIntToColor, XtCvtIntToDimension, XtCvtIntToFloat,
	XtCvtIntToFont, XtCvtIntToPixel, XtCvtIntToPixmap,
	XtCvtIntToPosition, XtCvtIntToShort, XtCvtIntToUnsignedChar,
	XtCvtColorToPixel, XtCvtPixelToColor;

  // from "StringDefs.h":
  export XtStrings, $XtNaccelerators, $XtNallowHoriz, $XtNallowVert,
	$XtNancestorSensitive, $XtNbackground, $XtNbackgroundPixmap,
	$XtNbitmap, $XtNborderColor, $XtNborder, $XtNborderPixmap,
	$XtNborderWidth, $XtNcallback, $XtNchildren, $XtNcolormap, $XtNdepth,
	$XtNdestroyCallback, $XtNeditType, $XtNfile, $XtNfont, $XtNforceBars,
	$XtNforeground, $XtNfunction, $XtNheight, $XtNhighlight, $XtNhSpace,
	$XtNindex, $XtNinitialResourcesPersistent, $XtNinnerHeight,
	$XtNinnerWidth, $XtNinnerWindow, $XtNinsertPosition,
	$XtNinternalHeight, $XtNinternalWidth, $XtNjumpProc, $XtNjustify,
	$XtNknobHeight, $XtNknobIndent, $XtNknobPixel, $XtNknobWidth,
	$XtNlabel, $XtNlength, $XtNlowerRight, $XtNmappedWhenManaged,
	$XtNmenuEntry, $XtNname, $XtNnotify, $XtNnumChildren,
	$XtNorientation, $XtNparameter, $XtNpixmap, $XtNpopupCallback,
	$XtNpopdownCallback, $XtNresize, $XtNreverseVideo, $XtNscreen,
	$XtNscrollProc, $XtNscrollDCursor, $XtNscrollHCursor,
	$XtNscrollLCursor, $XtNscrollRCursor, $XtNscrollUCursor,
	$XtNscrollVCursor, $XtNselection, $XtNselectionArray, $XtNsensitive,
	$XtNshown, $XtNspace, $XtNstring, $XtNtextOptions, $XtNtextSink,
	$XtNtextSource, $XtNthickness, $XtNthumb, $XtNthumbProc, $XtNtop,
	$XtNtranslations, $XtNunrealizeCallback, $XtNupdate, $XtNuseBottom,
	$XtNuseRight, $XtNvalue, $XtNvSpace, $XtNwidth, $XtNwindow, $XtNx,
	$XtNy, $XtCAccelerators, $XtCBackground, $XtCBitmap, $XtCBoolean,
	$XtCBorderColor, $XtCBorderWidth, $XtCCallback, $XtCColormap,
	$XtCColor, $XtCCursor, $XtCDepth, $XtCEditType, $XtCEventBindings,
	$XtCFile, $XtCFont, $XtCForeground, $XtCFraction, $XtCFunction,
	$XtCHeight, $XtCHSpace, $XtCIndex, $XtCInitialResourcesPersistent,
	$XtCInsertPosition, $XtCInterval, $XtCJustify, $XtCKnobIndent,
	$XtCKnobPixel, $XtCLabel, $XtCLength, $XtCMappedWhenManaged,
	$XtCMargin, $XtCMenuEntry, $XtCNotify, $XtCOrientation,
	$XtCParameter, $XtCPixmap, $XtCPosition, $XtCReadOnly, $XtCResize,
	$XtCReverseVideo, $XtCScreen, $XtCScrollProc, $XtCScrollDCursor,
	$XtCScrollHCursor, $XtCScrollLCursor, $XtCScrollRCursor,
	$XtCScrollUCursor, $XtCScrollVCursor, $XtCSelection, $XtCSensitive,
	$XtCSelectionArray, $XtCSpace, $XtCString, $XtCTextOptions,
	$XtCTextPosition, $XtCTextSink, $XtCTextSource, $XtCThickness,
	$XtCThumb, $XtCTranslations, $XtCValue, $XtCVSpace, $XtCWidth,
	$XtCWindow, $XtCX, $XtCY, $XtRAcceleratorTable, $XtRAtom, $XtRBitmap,
	$XtRBool, $XtRBoolean, $XtRCallback, $XtRCallProc, $XtRCardinal,
	$XtRColor, $XtRColormap, $XtRCursor, $XtRDimension, $XtRDisplay,
	$XtREditMode, $XtREnum, $XtRFile, $XtRFloat, $XtRFont,
	$XtRFontStruct, $XtRFunction, $XtRGeometry, $XtRImmediate,
	$XtRInitialState, $XtRInt, $XtRJustify, $XtRLongBoolean, $XtRObject,
	$XtROrientation, $XtRPixel, $XtRPixmap, $XtRPointer, $XtRPosition,
	$XtRScreen, $XtRShort, $XtRString, $XtRStringArray, $XtRStringTable,
	$XtRUnsignedChar, $XtRTranslationTable, $XtRVisual, $XtRWidget,
	$XtRWidgetClass, $XtRWidgetList, $XtRWindow, $XtEoff, $XtEfalse,
	$XtEno, $XtEon, $XtEtrue, $XtEyes, $XtEvertical, $XtEhorizontal,
	$XtEtextRead, $XtEtextAppend, $XtEtextEdit, $XtExtdefaultbackground,
	$XtExtdefaultforeground, $XtExtdefaultfont, $XtNfontSet, $XtRFontSet,
	$XtCFontSet;

  // from "Shell.h":
  export XtShellStrings, $XtNiconName, $XtCIconName, $XtNiconPixmap,
	$XtCIconPixmap, $XtNiconWindow, $XtCIconWindow, $XtNiconMask,
	$XtCIconMask, $XtNwindowGroup, $XtCWindowGroup, $XtNvisual,
	$XtCVisual, $XtNtitleEncoding, $XtCTitleEncoding, $XtNsaveUnder,
	$XtCSaveUnder, $XtNtransient, $XtCTransient, $XtNoverrideRedirect,
	$XtCOverrideRedirect, $XtNtransientFor, $XtCTransientFor,
	$XtNiconNameEncoding, $XtCIconNameEncoding, $XtNallowShellResize,
	$XtCAllowShellResize, $XtNcreatePopupChildProc,
	$XtCCreatePopupChildProc, $XtNtitle, $XtCTitle, $XtNargc, $XtCArgc,
	$XtNargv, $XtCArgv, $XtNiconX, $XtCIconX, $XtNiconY, $XtCIconY,
	$XtNinput, $XtCInput, $XtNiconic, $XtCIconic, $XtNinitialState,
	$XtCInitialState, $XtNgeometry, $XtCGeometry, $XtNbaseWidth,
	$XtCBaseWidth, $XtNbaseHeight, $XtCBaseHeight, $XtNwinGravity,
	$XtCWinGravity, $XtNminWidth, $XtCMinWidth, $XtNminHeight,
	$XtCMinHeight, $XtNmaxWidth, $XtCMaxWidth, $XtNmaxHeight,
	$XtCMaxHeight, $XtNwidthInc, $XtCWidthInc, $XtNheightInc,
	$XtCHeightInc, $XtNminAspectY, $XtCMinAspectY, $XtNmaxAspectY,
	$XtCMaxAspectY, $XtNminAspectX, $XtCMinAspectX, $XtNmaxAspectX,
	$XtCMaxAspectX, $XtNwmTimeout, $XtCWmTimeout, $XtNwaitForWm,
	$XtCWaitForWm;
  export <ShellWidgetClass>, <OverrideShellWidgetClass>,
	<WMShellWidgetClass>, <TransientShellWidgetClass>,
	<TopLevelShellWidgetClass>, <ApplicationShellWidgetClass>,
	shellWidgetClass, overrideShellWidgetClass, wmShellWidgetClass,
	transientShellWidgetClass, topLevelShellWidgetClass,
	applicationShellWidgetClass;

  // from "Composite.h":
  export <CompositeWidgetClass>;
  export <XtOrderProc>, <XtOrderProc>-callback-wrapper,
	XtManageChildren, XtManageChild, XtUnmanageChildren, XtUnmanageChild,
	compositeWidgetClass;

  // from "Constraint.h":
  export <ConstraintWidgetClass>, constraintWidgetClass;

  // from "Core.h":
  export <CoreWidgetClass>, <CoreWidget>, coreWidgetClass,
	widgetClass;

  // from "Object.h":
  export <X-Object>, <ObjectClass>, objectClass;

  // from "RectObj.h":
  export <RectObj>, <RectObjClass>, rectObjClass;

  // from "Vendor.h":
  export <VendorShellWidgetClass>, vendorShellWidgetClass;

  // from "extra.dylan":
  export <X-Boolean>, <X-Boolean*>;
  export <Widget*>, <Widget**>, <XrmDatabase*>,
	<XrmHashBucket*>, <XrmRepresentation*>,
	<XtActionList*>, <XtActionProc*>, <XtAppContext*>,
	<XtCacheRef*>, <XtPointer*>, <XtResourceList*>; 
  export <XIconSize**>, <XStandardColormap**>;
  export <X-String*>;
  export <XrmBindingList>, <XrmSearchList>;
  export XtSetArg, XrmStringsEqual;
 
end module Xt;
