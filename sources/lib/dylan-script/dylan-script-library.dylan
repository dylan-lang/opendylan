Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-script
  use common-dylan;
  use io;
  use system;
  use collections;

  use network;
  use http-client;
  use smtp-client;
  use pop-client;
  use nntp-client;

  use c-ffi;
  use ole-automation;
  use lotus-notes;

  use dom;
  use html;

  export dylan-script;
end library;

define module dylan-script
  use common-dylan, 
    exclude: { format-out, format-to-string },
    export: all;
  use streams, export: all;
  use print, export: all;
  use format, export: all;
  use format-out, export: all;
  use standard-io, export: all;
  use locators, export: all;
  use file-system, export: all;

  use sockets, export: all;
  use http-client, export: all;
  use smtp-client, export: all;
  use pop-client, export: all;
  use nntp-client, export: all;

  use dom, export: all;
  use html-dom, export: all;
  use html, export: all;

  // Protocol
  create
    contents, contents-setter, contents-as, 
    find,
    open;

  // Strings
  create
    text-parser;

  // Files
  create
    path-parser, p-parser;

  // Generic TCP
  create
    <tcp-locator>, tcp-parser;

  // Mail & SMTP
  create
    set-smtp-defaults,
    send, send-using,
    <message>,
      message-property;

  // POP
  create
    <pop-locator>, pop-parser;

  // Notes
  create
    <notes-locator>, notes-parser;

end module;

define module dylan-script-internals
  use dylan-script;
  use collectors;
  use c-ffi;
  use ole-automation;
  use lotus-notes;
end module;
