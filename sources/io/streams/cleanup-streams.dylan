Module:       streams-internals
Synopsis:     Close open external streams on application exit
Author:       Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Make sure this file is last end the streams lid file.
// The function close-external-streams is defined in external-streams.dylan

register-application-exit-function(close-external-streams);
