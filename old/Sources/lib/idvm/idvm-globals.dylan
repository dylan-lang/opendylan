Module:    IDVM
Synopsis:  Macros for the IDVM MK 3 (23/8/94)
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *use-generic-functions* = #f;
define variable bodies = make(<table>);
define variable normal-to-debug-map = make(<table>);
define variable debug-to-normal-map = make(<table>);
define variable code = make(<table>);

