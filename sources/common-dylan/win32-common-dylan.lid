Library:      common-dylan
Module:       dylan-user
Synopsis:     Common Dylan library definition
Author:       Andy Armstrong
Executable:   DxCMNDYL
Base-Address: 0x66DC0000
RC-Files:     version.rc
Target-Type:  dll
Files: library
       macros
       common-extensions
       win32-common-extensions
       locators-protocol
       streams-protocol
       random
       numerics
       format
       byte-vector
       timers
       transcendentals
       machine-words/utilities
       machine-words/machine-word
       machine-words/logicals
       machine-words/arithmetic
       machine-words/division
       machine-words/shift
       machine-words/unsigned
       machine-words/signal-overflow
       machine-words/double
       machine-words/unsigned-double
C-Source-Files: timer_helpers.c
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

