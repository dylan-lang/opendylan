Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Protocol classes

define abstract class <bp> (<object>) end;                // _not_ open!

define open abstract class <line> (<object>) end;

define open abstract class <interval> (<object>) end;

define open abstract class <node> (<interval>) end;

define open abstract class <source-container> (<object>) end;

define open abstract class <section> (<object>) end;

define open abstract class <command-table> (<object>) end;

define open abstract class <command-set> (<object>) end;

define open abstract class <mode> (<object>) end;

define open abstract class <buffer> (<interval>) end;

define open abstract class <window> (<object>) end;

define open abstract class <editor> (<object>) end;

define open abstract class <editor-frame> (<object>) end;

define open abstract class <presentation> (<object>) end;


define open abstract primary class <command-error> (<condition>)
  sealed constant slot command-error-window :: <window>,
    required-init-keyword: window:;
  sealed constant slot command-error-format-string :: false-or(<string>) = #f,
    init-keyword: format-string:;
  sealed constant slot command-error-format-arguments :: <sequence> = #[],
    init-keyword: format-arguments:;
end class <command-error>;
