Module:    Win32-kernel
Synopsis:  Declarations needed before "winbase.dylan".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Slot acessors defined both here and in "com" and "ole-automation".

define open-accessor cBytes-value;
define open-accessor dwPlatformId-value;

// Slot accessors defined both here and in "winsock2".

define open-accessor wVersion-value;
