Module:       DUIM-Presentations-Internals
Synopsis:     DUIM presentation system
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Presentation record protocol classes

define open abstract class <abstract-presentation-record> (<abstract-sheet>) end;

// The protocol class for all output records
define protocol-class presentation-record (<sheet>, <abstract-presentation-record>) end;


// The protocol class for views
define protocol-class view (<object>) end;
