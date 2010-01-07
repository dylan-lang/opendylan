Module: motif-ffi-example
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

format-out("Play time.~%");

define C-function XtAddCallback
  parameter widget        :: <Widget>;
  parameter callback-name :: <C-string>;
  parameter callback      :: <C-function>.pointer-type;
  parameter client-data   :: <C-long>;
  c-name: "XtAddCallback";
end C-function;

/*
define C-variable application-argc :: <C-int>
  c-name: "application_argc";
end C-variable;

define C-variable application-argv :: <C-string>.pointer-type
  c-name: "application_argv";
end C-variable;
*/

define variable &app = make(<XtAppContext>.pointer-type);
define variable &argc = make(<C-int>.pointer-type);
define variable &argv = null-pointer(<C-string>.pointer-type);
                        // application-argv();
pointer-value(&argc) := 0;
                        // application-argc();

define method main ()
  let shell
    = XtAppInitialize
        (&app, "Mini Motif", 
         null-pointer(<XrmOptionDescList>), 0,
         &argc, &argv,
         null-pointer(<C-string>.pointer-type),
         null-pointer(<ArgList>), 0);
  make-demo-child(shell);
  XtRealizeWidget(shell);
  XtAppMainLoop(pointer-value(&app));
end method;

define method make-demo-child (shell)
  let button-text = XmStringCreateSimple("Don't Push Me!");
  let args = make(<ArgList>, element-count: 10);
  let n = 0;
  XtSetArg(args[n], XmNlabelString, button-text); n := n + 1;
  XtSetArg(args[n], XmNwidth, 250); n := n + 1;
  XtSetArg(args[n], XmNheight, 250); n := n + 1;
  let button = XmCreatePushButton(shell, "button", args, n);
  XtManageChild(button);
  XtAddCallback(button, "activateCallback", PushedCallbackRef, 0);
end method;

define method pushed-callback (widget, client-data, callback-struct)
  format-out("Ack!!!\n");
end method;

define C-callable-wrapper PushedCallbackRef of pushed-callback
  parameter widget      :: <Widget>;
  parameter client-data :: <C-long>;
  parameter callback    :: <C-long>;
  c-name: "dumdumdum";
end C-callable-wrapper;

main();
