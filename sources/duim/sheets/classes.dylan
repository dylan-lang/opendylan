Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sheet/medium/port protocol classes

define protocol-class event-handler (<object>) end;

// "Primary" sheet classes should inherit from <sheet>
// "Mixin" sheet classes should inherit from <abstract-sheet>
define open abstract class <abstract-sheet> (<event-handler>) end;
define protocol-class sheet (<abstract-sheet>) end;

define open abstract class <abstract-medium> (<object>) end;
define protocol-class medium (<abstract-medium>) end;

define open abstract class <abstract-port> (<object>) end;
define protocol-class port (<abstract-port>) end;

define open abstract class <abstract-display> (<abstract-sheet>) end;
define protocol-class display (<sheet>, <abstract-display>) end;

define protocol-class mirror (<object>) end;

define protocol-class event (<object>) end;

define protocol-class pointer (<object>) end;

define protocol-class caret (<object>) end;

define protocol-class clipboard (<object>) end;

define open abstract class <abstract-frame-manager> (<object>) end;
define protocol-class frame-manager (<abstract-frame-manager>) end;

define open abstract class <abstract-frame> (<event-handler>) end;
define protocol-class frame (<abstract-frame>) end;


// This isn't an abstract superclass of <sheet> and <medium> because
// they don't otherwise have much to do with each other.  <drawable> is
// really just a convenience for the functions (such as the drawing
// functions) that trampoline from sheets to mediums.
define constant <drawable> = type-union(<abstract-sheet>, <abstract-medium>);


// A cursor is a thing that tells what the pointer's cursor should look like.
// Don't confuse this with a "caret", which is a little indicator in a text
// pane that shows you where the next character will be inserted.
define constant <cursor> = type-union(<symbol>, <image>);
