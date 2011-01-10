Module:       duim-extended-geometry-internals
Synopsis:     DUIM extended geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Extended region protocol classes

define protocol-class polyline (<path>) end;

define protocol-class polygon (<area>) end;

define protocol-class line (<path>) end;

define protocol-class rectangle (<area>) end;

define protocol-class elliptical-arc (<path>) end;

define protocol-class ellipse (<area>) end;
