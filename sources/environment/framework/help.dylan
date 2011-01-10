Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Jason Trenouth, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Help menu implementation

define open abstract class <frame-help-mixin> (<frame>)
end class <frame-help-mixin>;

define variable $help-bitmap :: <label-type> = "?";

define open generic frame-help-contents-and-index (frame :: <frame>) => ();
define open generic frame-help-on-keyword (frame :: <frame>, keyword) => ();

define constant $help-topics-title   = "Contents and Index";

define command-table *help-command-table* (*global-command-table*)
  menu-item $help-topics-title = frame-help-contents-and-index,
  //        $help-topics-title = <help-on-topics>,
    accelerator: make-keyboard-gesture(#"f1"),	// ---*** F1 really be context sensitive help
    documentation: "Opens Help.";
  // Comment out these until there is some content for them to work on
  // menu-item "What's This?" = (<help-on-context>, pane: #"by-focus"),
  //   documentation: "Displays help about the current context.";
  // menu-item "Search..."    = (<help-on-keyword>, pane: #"by-focus"),
  //   documentation: "Displays help about the selected text.";
end command-table *help-command-table*;
