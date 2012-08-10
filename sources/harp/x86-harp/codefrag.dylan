module:    x86-harp
Synopsis:  Code fragments for the Intel architecture
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Code fragments used by the 386
///
/// (c) Copyright Functional Objects, Inc. 1991
///
/// Started 20/11/91 (TonyM)


// Direction flag - used for marking multiple values as well as directions
define constant std = #xfd;
define constant cld = #xfc;

// Push & pop flags
define constant pushfd = #x9c;
define constant popfd  = #x9d;

// Block copies
// define constant repne = #xf2;
define constant repe = #xf3;
define constant rep = #xf3;
define constant movsb = #xa4;
define constant movsd = #xa5;
define constant stosb = #xaa;
define constant stosd = #xab;

// Block comparisons
define constant cmpsd = #xa7;
define constant cmpsb = #xa6;


// shifts
define constant asl2 = #b100000;
define constant asr2 = #b111000;
define constant lsr2 = #b101000;
define constant rol2 = #b000000;
define constant ror2 = #b001000;
define constant rcl2 = #b010000;
// define constant rcr2 = #b011000;

// size conversions
define constant cwd = #x99;
define constant cdq = cwd;

// control transfer
define constant leave = #xc9;
define constant call = #xe8;
define constant jmp = #xe9;
define constant ret = #xc3;
define constant ret-drop = #xc2;

// escapes
define constant grp5 = #xff;
define constant lock = #xf0;

