Module: imported-xm
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-type-alias <C-boolean> = <C-char>;
define C-type-alias <XtArgVal>  = <C-long>;
define C-type-alias <XtEnum>    = <C-unsigned-char>;

define C-type-alias <Cardinal>  = <C-unsigned-int>;
define C-type-alias <Dimension> = <C-unsigned-short>;
define C-type-alias <Position>  = <C-short>;

define C-type-alias <XtPointer> = <C-char>.pointer-type;

define C-type-alias <XtString> = <C-string>;

// Xt Arg:

define C-struct <Arg>
  slot x-name  :: <C-string>;
  slot x-value :: <XtArgVal>;
end C-struct;

define C-type-alias <ArgList> = <Arg>.pointer-type;

define method XtSetArg (arg :: <Arg>.pointer-type, name, value)
  arg.x-name  := name;
  arg.x-value := value;
end method;

// Xt Widget:

define C-struct <WidgetStruct> 
  // should be filled in
end C-struct;

define C-type-alias <Widget> = <WidgetStruct>.pointer-type;

// Xt Widget classes:

define C-struct <WidgetClassRec>
  // should be filled in
end C-struct;

define C-type-alias <WidgetClass> = <WidgetClassRec>.pointer-type;

// Options:

define C-struct <XrmOptionDesc>
  // should be filled in
end C-struct;

define C-type-alias <XrmOptionDescList> = <XrmOptionDesc>.pointer-type;

// App context:

define C-struct <XtAppStruct> 
  // should be filled in
end;

define C-type-alias <XtAppContext> = <XtAppStruct>.pointer-type;

// Functions:

define C-function XtAppInitialize
  /* output */ parameter app-context-return :: <XtAppContext>.pointer-type;
         parameter application-class  :: <XtString>;
         parameter options            :: <XrmOptionDescList>;
         parameter num-options        :: <Cardinal>;
  /* in-out */ parameter argc-in-out        :: <C-int>.pointer-type;
  /* in-out */ parameter argv-in-out        :: <C-string>.pointer-type;
         parameter fallback-resources :: <C-string>.pointer-type;
         parameter args               :: <ArgList>;
         parameter num-args           :: <Cardinal>;
  result widget :: <Widget>;
  c-name: "XtAppInitialize";
end C-function;

define C-function XtAppMainLoop
  parameter app-context :: <XtAppContext>;
  c-name: "XtAppMainLoop";
end C-function;

define C-function XtRealizeWidget
  parameter widget :: <Widget>;
  c-name: "XtRealizeWidget";
end C-function;

define C-function XtCreateManagedWidget
  parameter name         :: <XtString>;
  parameter widget-class :: <WidgetClass>;
  parameter parent       :: <Widget>;
  parameter args         :: <ArgList>;
  parameter num-args     :: <Cardinal>;
  result widget :: <Widget>;
  c-name: "XtCreateManagedWidget";
end C-function;

define C-function XtManageChild
  parameter widget :: <Widget>;
  c-name: "XtManageChild";
end C-function;

// Motif widget classes:

/*
define C-value xmPushButtonWidgetClass :: <WidgetClass>,
  c-name: "xmPushButtonWidgetClass";
*/

define C-function XmCreatePushButton
  parameter parent   :: <Widget>;
  parameter name     :: <XtString>;
  parameter args     :: <ArgList>;
  parameter num-args :: <Cardinal>;
  result widget :: <Widget>;
  c-name: "XmCreatePushButton";
end C-function;

define C-function XmStringCreateSimple
  parameter text    :: <C-string>;
  result    xstring :: <C-string>;
  c-name: "XmStringCreateSimple";
end C-function;

// Resource strings:

define constant XmNlabelString = as(<C-string>, "labelString");
define constant XmNwidth       = as(<C-string>, "width");
define constant XmNheight      = as(<C-string>, "height");

// eof
