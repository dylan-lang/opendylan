module:    harp-x86-windows-rtg
Synopsis:  Constant definitions for the Dylan X86 Windows runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $not-yet-relocated-data = #x3;  // Invalid tag value

define runtime-literal not-yet-relocated-string 
  = "not-yet-relocated-string", data: "Data not yet relocated.";
