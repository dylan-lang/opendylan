module:    threads-primitives
Synopsis:  Return codes from the portable threads layer
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $success = 0;

define constant $general-error = -1;

define constant $timeout = 1;

define constant $unlocked = 2;

define constant $pre-locked = 2;

define constant $count-exceeded = 3;

define constant $creation-error = 1;

define constant $priority-error = 2;

define constant $false = 0;

define constant $true = 1;

