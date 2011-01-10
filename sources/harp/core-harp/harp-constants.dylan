module:    base-harp
Synopsis:  Constant definitions for the HARP data structures
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  "The offset from the start of the instruction of the ops code"
define constant instruction-op-index =  0;

//  "The offset from the start of the instruction of the special slot"
define constant instruction-special-index = 1;

//  "The offset of the defines slot"
define constant instruction-defs-index = 2;

//  "The number of slots in an instruction for the defs"
define constant instruction-defs-slots = 1;

//  "The offset of the start of the uses slot"
define constant instruction-uses-index =
  instruction-defs-index + instruction-defs-slots;

//  "The number of slots in an instruction for the uses"
define constant instruction-uses-slots = 2;

//  "The total size of an instruction"
define constant instruction-size =
  2 + instruction-defs-slots + instruction-uses-slots;

define constant empty-rset = 0;

