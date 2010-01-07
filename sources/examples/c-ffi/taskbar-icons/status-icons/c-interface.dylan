Module:   status-icons
Synopsis: A library abstracting persistent status icon display
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The exported C interface.

// TODO: Define constants for the status codes.

define constant $STATUS-NONE    = 0;
define constant $STATUS-OK      = 1;
define constant $STATUS-WARNING = 2;
define constant $STATUS-ERROR   = 3;

// TODO: Define a Dylan implementation of DisplayStatus.

define method DisplayStatus 
    (status :: <integer>, tip :: <C-string>) => (code :: <integer>)
  select (status)
    $STATUS-NONE    => stop-status-display();
    $STATUS-OK      => display-status-ok(tip: tip);
    $STATUS-WARNING => display-status-warning(tip: tip);
    $STATUS-ERROR   => display-status-error(tip: tip);
  end;
  0
end method;

// TODO: Define an exported, __stdcall, C callable wrapper around DisplayStatus.

define C-callable-wrapper of DisplayStatus
  parameter status :: <C-int>;
  parameter tip :: <C-string>;
  result code :: <C-int>;
  c-name: "DisplayStatus", export: #t, c-modifiers: "__stdcall";
end C-callable-wrapper;
