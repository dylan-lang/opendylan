Module:       helpmate
Author:       Jason Trenouth
Synopsis:     Example DUIM Help code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// COMMAND TABLES

define command-table *help-command-table* ()
  menu-item "&Context..."     = (<help-on-context>, pane: #"by-focus");
  menu-item "&Keyword..."     = (<help-on-keyword>, pane: #"by-focus");
  menu-item "&Topics..."      = <help-on-topics>;
  menu-item "&Help on Help"   = <help-on-help>;
  separator;
  menu-item "&About HelpMate" = <help-on-version>;
end command-table *help-command-table*;

define command-table *helpmate-command-table* (*global-command-table*)
  menu-item "Help" = *help-command-table*;
end command-table *project-browser-command-table*;

/// <HELPMATE>
  
define frame <helpmate> (<simple-frame>)
  pane helpmate-locator (frame)
    make(<text-field>, text: "c:\\hqbin\\win32\\msoffice\\winword\\winword.hlp");
  pane helpmate-context (frame)
    make(<text-field>, text: "0");
  pane helpmate-keyword (frame)
    make(<text-field>, text: "wizard");
  layout (frame)
    vertically ()
      horizontally ()
        make(<label>, label: "Locator:");
        frame.helpmate-locator;
        make(<push-button>, label: "Browse ..", activate-callback: helpmate-update-locator);
      end;
      horizontally ()
        make(<label>, label: "Context:");
        frame.helpmate-context;
      end;
      horizontally ()
        make(<label>, label: "Keyword:");
        frame.helpmate-keyword;
      end;
    end;
  command-table (frame)
    *helpmate-command-table*;
end frame <helpmate>;

define method start-helpmate ()
  start-frame(make(<helpmate>, title: "HelpMate"));
end method start-helpmate;

/// CONTEXT, KEYWORD, AND TOPICS

define help-source helpmate #f
end;

define method frame-help-topic-id (frame :: <helpmate>, command :: <help-on-context>)
  => (result)
  frame.helpmate-context.gadget-value
end method frame-help-topic-id;

define method frame-help-source-locator (frame :: <helpmate>, source :: <help-source>)
  => (locator)
  frame.helpmate-locator.gadget-value
end method frame-help-source-locator;

define method frame-help-source (frame :: <helpmate>, command :: <help-on-topics>)
  => (locator :: <symbol>)
  #"helpmate"
end method frame-help-source;

define method frame-help-source (frame :: <helpmate>, command :: <help-on-context>)
  => (locator :: <symbol>)
  #"helpmate"
end method frame-help-source;

define method frame-help-source (frame :: <helpmate>, command :: <help-on-keyword>)
  => (locator :: <symbol>)
  #"helpmate"
end method frame-help-source;

define method frame-help-context (frame :: <helpmate>, command :: <help-on-context>)
  => (context :: <symbol>)
  as(<symbol>, frame.helpmate-context.gadget-value);
end method frame-help-context;

define method frame-help-keyword (frame :: <helpmate>, command :: <help-on-keyword>)
  => (keyword :: <string>)
  frame.helpmate-keyword.gadget-value;
end method frame-help-keyword;

/// ABOUT BOX

define variable *application-name*    = "HelpMate";
define variable *application-version* = "1.0.0 (internal Functional Developer test app)";
define variable *application-copyright* = "Copyright (c) 1997-2000 Functional Objects, Inc. All rights reserved.";
define variable *application-credits* = #("jason");

define method do-execute-command
    (frame :: <helpmate>, command :: <help-on-version>) => ()
  with-frame-manager (frame-manager(frame))
    let about-layout =
      vertically ()
        make(<label>, label: format-to-string("%s %s", *application-name*, *application-version*));
        make(<label>, label: *application-copyright*);
      end;
    let credits-button =
      make(<push-button>, label: "Credits", activate-callback: about-box-credits);
    start-frame(
      make(<dialog-frame>,
        owner: frame,
        title: "About DylanWorks",
        layout: with-border (type: #"groove") about-layout end with-border,
        cancel-button: credits-button));
  end with-frame-manager;
end method do-execute-command;

define method about-box-credits (pane :: <object>) => ()
  local method labelize (x) make(<label>, label: x) end method;
  with-frame-manager (frame-manager(pane))
    let credits-layout =
      with-border (type: #"groove") 
        make(<table-layout>,
          rows: 10,
          x-spacing: 10,
          children: map(labelize, sort(*application-credits*)))
      end with-border;
    start-frame(
      make(<dialog-frame>,
        owner: pane.sheet-frame,
        title: "HelpMate Credits",
        layout: credits-layout,
        cancel-callback: #f));
  end with-frame-manager;
end method about-box-credits;

// UPDATE-LOCATOR

define method helpmate-update-locator (pane :: <object>)
  let locator = choose-file(title: "Help Locator: ", direction: #"input");
  if (locator)
    pane.sheet-frame.helpmate-locator.gadget-value := locator;
  end if;
end method helpmate-update-locator;
