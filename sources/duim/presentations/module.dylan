Module:	      Dylan-User
Synopsis:     DUIM presentation system
Author:	      Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-presentations
  create *input-context*,
	 <presentation-event-handler>,
	 <presentation-pane>,
	 <presentation-record>,
	 <presentation-sheet>,
	 <presentation-translator>,
	 accept, do-accept,
         \command-translator-definer,
	 describe-presentation-type, do-describe,
         \drag-and-drop-translator-definer,
         highlight-presentation, do-highlight,
	 input-context-type, input-context-tag,
	 present, do-present,
	 presentation-position-test, do-position-test,
	 presentation-type,   presentation-type-setter,
	 presentation-object, presentation-object-setter,
         \presentation-action-definer,
         \presentation-translator-definer,
	 \with-input-context, do-with-input-context,
	 \with-output-as-presentation, do-with-output-as-presentation;
end module duim-presentations;

define module duim-presentations-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals;
  use duim-layouts-internals;
  use duim-gadgets-internals;
  use duim-frames-internals;
  use duim-recording-internals;
  use duim-presentations, export: all

  export <basic-presentation-record>,
	 call-presentation-menu,
         find-applicable-presentation,
         find-applicable-translators,
         find-presentation-translators,
         highlight-applicable-presentation,
	 sheet-accept,
	 sheet-present;
end module duim-presentations-internals;

