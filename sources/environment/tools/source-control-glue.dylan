Module:    environment-tools
Synopsis:  Environment tools
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Glue to Source Control Manager

define function claim-unit
    (compound :: <string>, unit :: false-or(<string>),
     #key reason :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-claim-command>,
                       compound: compound, unit: unit,
                       reason: reason));
  #t
end function claim-unit;

define function check-unit-out
    (compound :: <string>, unit :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-check-out-command>,
                       compound: compound, unit: unit));
  #t
end function check-unit-out;

define function check-unit-in
    (compound :: <string>, unit :: false-or(<string>),
     #key reason :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-check-in-command>,
                       compound: compound, unit: unit,
                       reason: reason));
  #t
end function check-unit-in;

define function abandon-unit
    (compound :: <string>, unit :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-abandon-command>,
                       compound: compound, unit: unit));
  #t
end function abandon-unit;

define function merge-unit
    (compound :: <string>, unit :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-merge-command>,
                       compound: compound, unit: unit));
  #t
end function merge-unit;

define function diff-unit
    (compound :: <string>, unit :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-diff-command>,
                       compound: compound, unit: unit));
  #t
end function diff-unit;

define function report-unit
    (compound :: <string>, unit :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-report-command>,
                       compound: compound, unit: unit));
  #t
end function report-unit;

define function add-unit
    (compound :: <string>, unit :: <string>,
     #key reason :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-add-command>,
                       compound: compound, unit: unit,
                       reason: reason));
  #t
end function add-unit;

define function remove-unit
    (compound :: <string>, unit :: <string>,
     #key reason :: false-or(<string>))
 => (succeeded? :: <boolean>)
  execute-command(make(<sccs-remove-command>,
                       compound: compound, unit: unit,
                       reason: reason));
  #t
end function remove-unit;
