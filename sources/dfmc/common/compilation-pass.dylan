Synopsis: compilation-pass definition and creation
Module:   dfmc-common
Author:   Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// public interface

define open generic run-compilation-passes (code) => code;
  // method definition in management module


//// optimization levels

define constant $optimization-mandatory = 0;
define constant $optimization-low       = 1;
define constant $optimization-medium    = 2;
define constant $optimization-high      = 3;

define constant $optimization-default   = $optimization-low;

define thread variable *optimization-level* = $optimization-default;

