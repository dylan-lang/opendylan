Module:       DUIM-Presentations-Internals
Synopsis:     DUIM presentation system
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presentation type translators macros

define macro presentation-translator-definer
end macro presentation-translator-definer;

define macro presentation-action-definer
end macro presentation-action-definer;

define macro command-translator-definer
end macro command-translator-definer;


define macro drag-and-drop-translator-definer
end macro drag-and-drop-translator-definer;


/// Basic global presentation translators

define presentation-translator identity-translator
    (object :: <object>, #key presentation) => (<bottom>)
    (command-table: XXX,
     tester: identity-translator-applicable?,
     tester-definitive: #t,
     documentation: XXX,
     gesture: XXX)
  values(object, presentation-type(presentation))
end presentation-translator identity-translator;

define function identity-translator-applicable? (presentation context-type)
end function identity-translator-applicable?;


define presentation-action presentation-menu-action
    (object :: <object>, #key presentation, frame, sheet, x, y) => (<bottom>)
    (command-table: XXX,
     documentation: "Menu",
     gesture: XXX)
  call-presentation-menu(presentation, *input-context*, frame,
			 sheet: sheet, x: x, y: y,
			 for-menu?: #t
end presentation-action presentation-menu-action;

define method call-presentation-menu
    (presentation, input-context, frame :: <frame>,
     #key sheet, x, y, for-menu?, label)
end method call-presentation-menu;

