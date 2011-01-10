Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Hardcopying

define open generic frame-hardcopy-document (frame :: <frame>) => ();
define open generic frame-hardcopy-object (frame :: <frame>, object) => ();
define open generic frame-page-setup (frame :: <frame>) => ();

define function not-yet-implemented (frame :: <frame>) => ()
  notify-user("Not yet implemented!", owner: frame);
end function not-yet-implemented;

define method frame-hardcopy-document (frame :: <frame>) => ()
  let selection = frame-selection(frame);
  if (instance?(selection, <sequence>))
    do(curry(frame-hardcopy-object, frame), selection)
  else
    frame-hardcopy-object(frame, selection)
  end
end method frame-hardcopy-document;

define method frame-hardcopy-object
    (frame :: <frame>, object :: <object>) => ()
  #f
end method frame-hardcopy-object;

define method frame-page-setup (frame :: <frame>) => ()
  not-yet-implemented(frame)
end method frame-page-setup;


/// Print command table

//--- Where should this really be defined?
define constant <label-type> = false-or(type-union(<string>, <image>));

define variable $page-setup-bitmap :: <label-type> = "Setup";
define variable $print-bitmap      :: <label-type> = "Print";

define command-table *print-command-table* (*global-command-table*)
//---*** andrewa: this doesn't work yet!
//  menu-item "Page Setup..." = frame-page-setup,
//    documentation: "Enables you to change the page setup.";
  menu-item "Print..."      = frame-hardcopy-document,
    accelerator:   make-keyboard-gesture(#"p", #"control"),
    documentation: "Prints the document.";
end command-table *print-command-table*;
