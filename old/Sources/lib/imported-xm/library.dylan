Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library imported-xm
  use dylan;
  use c-ffi;
  export imported-xm;
end library;

define module imported-xm
  use dylan;
  use c-ffi;
  export
    <C-boolean>, <XtArgVal>, <XtEnum>,
    <Cardinal>, <Dimension>, <Position>,
    <XtPointer>, <XtString>,

    <Arg>, <ArgList>, 
    x-name, x-name-setter, 
    x-value, x-value-setter,
    XtSetArg,

    <Widget>,
    <XrmOptionDesc>, <XrmOptionDescList>,
    <XtAppContext>,

    XtAppInitialize, XtAppMainLoop,
    XtRealizeWidget, XtCreateManagedWidget, XtManageChild,

    // xmPushButtonWidgetClass,
    XmCreatePushButton,

    XmStringCreateSimple,

    XmNlabelString, XmNwidth, XmNheight;
end module imported-motif;

// eof
