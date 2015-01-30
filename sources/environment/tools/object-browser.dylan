Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// View state
//
// This is the class of objects that remember the state of a particular
// object's view in the browser history.

define class <view-state> (<object>)
  constant slot state-object :: <environment-object>,
    required-init-keyword: object:;
  slot state-page :: false-or(<symbol>) = #f;
  slot state-properties :: false-or(<environment-property-pane-state>) = #f;
end class <view-state>;

/*---*** andrewa: try taking this out again
//---*** andrewa: temporary hack until this is working properly
define method state-object
    (object) => (object)
  object
end method state-object;
*/


/// Object Browser

define frame <object-browser>
    (<frame-primary-object-mixin>,
     <frame-refresh-mixin>,
     <frame-module-gadget-mixin>,
     <frame-cascading-window-mixin>,
     <environment-project-tool>)
  constant slot object-browser-page = #f,
    init-keyword: page:;
//---*** cpage: 1997.11.10 Experiment with Explorer Bars
/*
  pane object-hierarchy-pane (frame)
    make(<definition-names-displayer>, max-width: 50, frame: frame);
/*
    make(<tree-control>,
         max-width: 200,
         selection-mode: #"single",
         depth: 1,
         children-generator: method (object)
                               object-browser-children-generator(frame, object)
                             end,
         label-key: curry(frame-default-object-name, frame),
         popup-menu-callback: display-environment-popup-menu,
         value-changed-callback: method (sheet)
                                   let frame = sheet-frame(sheet);
                                   note-frame-selection-updated(frame);
                                   refresh-frame-view(frame);
                                   frame-primary-object(sheet-frame(sheet))
                                     := gadget-value(sheet)
                                 end);
*/
/*,
         activate-callback: method (sheet)
                              let target = gadget-value(sheet);
                              environment-activate-callback(sheet, target)
                            end);
*/
*/
//---*** cpage: 1997.11.10 Removing the primary object displayer in anticipation
//              of having an "explorer bar" to the left of the property pages,
//              which would display (among other things) the primary object.
/*
  pane object-browser-primary-object-displayer (frame)
    make(<list-control>,
         view: #"small-icon",
         scroll-bars: #"none",
         //--- cpage: 1997.09.14 This is a hack; when there is no object, the list
         //           goes to "infinite" height. I don't know the right way to do
         //           this. I want the "list" to always be the height of one item.
         max-height: 12,
         label-key: curry(frame-object-unique-name, frame),
         activate-callback: execute-displayer-gadget-activate-callback,
         popup-menu-callback: display-environment-popup-menu);
*/
  pane object-browser-object-bar (frame)
    make(<tool-bar>,
         child: vertically (spacing: 2)
                  make(<separator>);
                  make-frame-primary-object-selector(frame, label: "Object");
                end);
  pane tab-layout (frame)
    make(<environment-property-pane>,
         class: frame-primary-object-class(frame),
         frame: frame,
         page:  object-browser-page(frame),
         value-changed-callback: method (pane)
                                   let state :: false-or(<view-state>)
                                     = frame-raw-primary-object(frame);
                                   if (state)
                                     let page
                                       = environment-property-pane-page(pane);
                                     state-page(state) := page
                                   end
                                 end,
         activate-callback: environment-activate-callback);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.object-browser-object-bar;
      make(<separator>);
//---*** cpage: 1997.11.10 Removing the primary object displayer in anticipation
//              of having an "explorer bar" to the left of the property pages,
//              which would display (among other things) the primary object.
//    frame.object-browser-primary-object-displayer;
      horizontally (spacing: 2)
//---*** cpage: 1997.11.10 Experiment with Explorer Bars
/*
        frame.object-hierarchy-pane;
*/
        frame.tab-layout;
      end
    end;
  layout        (frame) frame.main-layout;
  tool-bar      (frame) make-environment-tool-bar(frame);
  status-bar    (frame) make-environment-status-bar(frame);
  command-table (frame) *object-browser-command-table*;
  keyword width:  = $default-environment-frame-width;
  keyword height: = $default-environment-frame-height;
  keyword icon: = $browser-window-small-icon;
  keyword frame-class-name:, init-value: #"object-browser";
end frame <object-browser>;

define cascading-window-settings browser :: <object-browser> = "Browser";

define method frame-coerce-raw-object
    (frame :: <object-browser>, object :: <view-state>)
 => (object)
  object.state-object
end method frame-coerce-raw-object;


// Frame initialization

define method reinitialize-frame
    (frame :: <object-browser>,
     #key project :: <project-object>, page :: false-or(<symbol>))
 => ()
  next-method();
//---*** cpage: 1997.11.15 We probably want to remove the project parameter
//              since object browsers are tied to a single project.
  frame.%frame-project := project;
  when (page)
    environment-property-pane-page(frame.tab-layout) := page;
  end;
  frame-ensure-project-database(frame);
end method reinitialize-frame;

define method handle-event
    (frame :: <object-browser>, event :: <frame-mapped-event>)
 => ()
  with-busy-cursor (frame)
    next-method();
    frame-ensure-project-database(frame);
  end
end method handle-event;


/// Toolbars etc

//---*** cpage: 1997.11.10 Temporary(?) kludge: This is a modified copy of
//              the code in the framework, so that we can have a custom
//              tool bar (the Object selector). Eventually, we need to
//              expand the framework protocol to allow for this.
/// Bar options (tool bar, status bar etc)

define command-table *object-browser-bar-options-command-table* (*global-command-table*)
end command-table *object-browser-bar-options-command-table*;

add-command-table-menu-item
  (*object-browser-bar-options-command-table*,
   "", <check-box>, vector(#"tool-bar", #"object-bar", #"status-bar"),
   items: #[#["Toolbar",    #"tool-bar"],
            #["Object Bar", #"object-bar"],
            #["Status Bar", #"status-bar"]],
   label-key: first, value-key: second,
   callback: method (menu-box)
               frame-show-bars?(sheet-frame(menu-box), gadget-value(menu-box))
             end);

define method frame-show-bars?
    (frame :: <object-browser>, bars :: <sequence>) => ()
  let top-sheet   = top-level-sheet(frame);
  let tool-bar    = frame-tool-bar(frame);
  let object-bar  = object-browser-object-bar(frame);
  let status-bar  = frame-status-bar(frame);
  let tool-bar?   = member?(#"tool-bar",   bars);
  let object-bar? = member?(#"object-bar", bars);
  let status-bar? = member?(#"status-bar", bars);
  let relayout?   = #f;
  local method show-or-hide (sheet, present?) => ()
          // Work extra hard to ensure that everything gets re-layed out,
          // since bars can have associated "decorations"
          when (sheet & sheet-withdrawn?(sheet) == present?)
            sheet-withdrawn?(sheet) := ~present?;
            for (s = sheet then sheet-parent(s),
                 until: s == top-sheet)
              sheet-layed-out?(s) := #f
            end;
            relayout? := #t
          end
        end method show-or-hide;
  show-or-hide(tool-bar,   tool-bar?);
  show-or-hide(object-bar, object-bar?);
  show-or-hide(status-bar, status-bar?);
  when (relayout?)
    relayout-children(top-sheet);
    relayout-parent(tool-bar | object-bar | status-bar);
    sheet-mapped?(tool-bar)   := tool-bar?;
    sheet-mapped?(object-bar) := object-bar?;
    sheet-mapped?(status-bar) := status-bar?;
  end
end method frame-show-bars?;


/// Clipboard support

define method clipboard-object-to-browse
    (frame :: <object-browser>) => (object)
  let object = dylan-clipboard-object(frame);
  select (object by instance?)
    <environment-object> =>
      let project = frame.frame-project;
      if (environment-object-home-server?(project, object))
        object
      else
        //---*** Note that environment-object-id ignores the project,
        //---*** which is good because this is not its owner project!
        //---*** Really environment-object-id shouldn't take a server.
        let id = environment-object-id(project, object);
        id & find-environment-object(project, id)
      end;
    <string> =>
      find-named-object(frame, object);
    otherwise =>
      #f;
  end
end method clipboard-object-to-browse;

define method paste-object?
    (frame :: <object-browser>, object) => (paste? :: <boolean>)
  ignore(object);
  clipboard-object-to-browse(frame) ~= #f
end method paste-object?;

define method paste-object
    (frame :: <object-browser>, object) => ()
  ignore(object);
  let object = clipboard-object-to-browse(frame);
  if (object)
    frame-browse-object(frame, object)
  else
    let object-name
      = with-clipboard (clipboard = top-level-sheet(frame))
          if (clipboard-data-available?(<string>, clipboard))
            get-clipboard-data-as(<string>, clipboard)
          end
        end;
    let message
      = if (object-name & size(object-name) < $maximum-object-name-length)
          format-to-string
            ("Cannot paste '%s' as it was not found in this project",
             object-name)
        else
          "Cannot browse the current clipboard contents"
        end;
    environment-error-message(message, owner: frame)
  end
end method paste-object;


/// Tool-bar buttons

define method make-browse-tool-bar-buttons
    (frame :: <object-browser>)
 => (buttons :: <sequence>)
  let project = frame.frame-project;
  vector(make(<button>,
              label: $home-bitmap,
              documentation: "Browse library",
              command: frame-browse-project-library,
              activate-callback: method (sheet)
                                   let frame = sheet-frame(sheet);
                                   frame-browse-project-library(frame)
                                 end))
end method make-browse-tool-bar-buttons;

define method make-environment-tool-bar-buttons
    (frame :: <object-browser>)
 => (buttons :: <sequence>)
  with-frame-manager (frame-manager(frame))
    let history-buttons = make-history-tool-bar-buttons(frame);
    let browse-buttons = make-browse-tool-bar-buttons(frame);
    let search-buttons = make-search-tool-bar-buttons(frame);
    let clipboard-buttons = make-clipboard-tool-bar-buttons(frame);
    let clone-buttons = make-clone-tool-bar-buttons(frame);
    let module-buttons = make-module-tool-bar-buttons(frame);
    vector(make(<row-layout>, children: history-buttons,   spacing: 0),
           make(<row-layout>, children: browse-buttons,    spacing: 0),
           make(<row-layout>, children: clipboard-buttons, spacing: 0),
           make(<row-layout>, children: search-buttons,    spacing: 0),
           make(<row-layout>, children: clone-buttons,     spacing: 0),
           make(<row-layout>, children: module-buttons,    spacing: 0))
  end
end method make-environment-tool-bar-buttons;


/// Browser options

define method frame-edit-options (frame :: <object-browser>) => ()
//---*** cpage: 1997.11.10 Not yet implemented.
  not-yet-implemented(owner: frame);
end method frame-edit-options;


///*---*** cpage: Disabled these, as they are not currently used because the
//              hierarchy pane is currently disabled. Don't delete yet,
//              though, as the hierarchy pane may come back.

// Object hierarchy

/*
define method frame-secondary-object
    (frame :: <object-browser>) => (object :: false-or(<environment-object>))
  frame-primary-object(frame)
  //---*** Experiment with removing the tree control
/*
  let selected-objects = gadget-value(frame.object-hierarchy-pane);
  unless (empty?(selected-objects)) selected-objects[0] end
*/
end method frame-secondary-object;

define method object-browser-children-generator
    (frame :: <object-browser>, object :: <environment-object>)
 => (objects :: <sequence>)
  #[]
end method object-browser-children-generator;

define method object-browser-children-generator
    (frame :: <object-browser>, class :: <class-object>)
 => (objects :: <sequence>)
  class-direct-subclasses(frame.frame-project, class)
end method object-browser-children-generator;

define method object-browser-children-generator
    (frame :: <object-browser>, function :: <generic-function-object>)
 => (objects :: <sequence>)
  generic-function-object-methods(frame.frame-project, function)
end method object-browser-children-generator;

*/


/// Frame title

define method generate-frame-title
    (frame :: <object-browser>) => (title :: <string>)
  let project = frame.frame-project;
  let object = frame.frame-primary-object;
  concatenate(if (object & object ~= project)
                concatenate(frame-object-unique-name(frame, object), " - ")
              else
                ""
              end,
              "Browsing ",
              frame-default-object-name(frame, project),
              " - ", release-product-name())
end method generate-frame-title;


/// Primary object handling

define method frame-primary-object-name
    (frame :: <object-browser>, object :: <environment-object>)
 => (name :: <string>)
  frame-object-unique-name(frame, object)
end method frame-primary-object-name;

define method frame-primary-object-class
    (frame :: <object-browser>) => (class :: <class>)
  let object = frame-primary-object(frame);
  if (object)
    object-class(object)
  else
    <environment-object>
  end
end method frame-primary-object-class;

define method frame-default-primary-object
    (frame :: <object-browser>)
 => (object :: false-or(<environment-object>))
  let project = frame.frame-current-project;
  let library = project.project-library;
  library
end method frame-default-primary-object;

define method frame-coerce-primary-object
    (frame :: <object-browser>, state :: <view-state>)
 => (coerced-object :: <view-state>)
  state
end method frame-coerce-primary-object;

define method frame-coerce-primary-object
    (frame :: <object-browser>, object :: <environment-object>)
 => (coerced-object :: <view-state>)
  // frame-raw-primary-object might not be a view-state, e.g. before
  // frame is mapped.
  let state = frame-raw-primary-object(frame);
  if (instance?(state, <view-state>) & state.state-object == object)
    state
  else
    make(<view-state>, object: object)
  end
end method frame-coerce-primary-object;

define method frame-coerce-primary-object
    (frame :: <object-browser>, name :: <name-object>)
 => (coerced-object :: false-or(<view-state>))
  let project = frame.frame-project;
  let object = name-value(project, name);
  object & frame-coerce-primary-object(frame, object)
end method frame-coerce-primary-object;

define method frame-coerce-primary-object
    (frame :: <object-browser>, name :: <string>)
 => (coerced-object :: false-or(<view-state>))
  let object = find-named-object(frame, name);
  object & frame-coerce-primary-object(frame, object)
end method frame-coerce-primary-object;

define method note-raw-primary-object-replaced
    (frame :: <object-browser>, state :: <view-state>) => ()
  next-method();
  let pane = frame.tab-layout;
  // Remember the old state of the pages so that we can restore them
  // when switching back to this object in the history.
  state.state-properties := environment-property-pane-state(pane);
end method note-raw-primary-object-replaced;

define method note-primary-object-changed
    (frame :: <object-browser>,
     old-object :: false-or(<environment-object>))
 => ()
  ignore(old-object);
  next-method();
  let project = frame.frame-project;
  let state = frame-raw-primary-object(frame);
  let page = state & state-page(state);
  let object = frame-primary-object(frame);
  frame-title(frame) := generate-frame-title(frame);
  let has-type?
    = environment-object-type(project, object)
        & #t;
  let has-gf?
    = instance?(object, <method-object>)
        & method-generic-function(project, object)
        & #t;
  command-enabled?(frame-browse-primary-object-type, frame)
    := has-type?;
  command-enabled?(frame-browse-primary-object-generic-function, frame)
    := has-gf?;
//---*** cpage: 1997.11.10 Removing this in anticipation of having an
//              "explorer bar" to the left of the property pages, which
//              would display (among other things) the primary object.
// gadget-items(frame.object-browser-primary-object-displayer) := vector(object);
  environment-property-pane-object
    (frame.tab-layout, page: page, state: state.state-properties) := object
end method note-primary-object-changed;

define method frame-sheet-with-selection
    (frame :: <object-browser>) => (sheet :: false-or(<sheet>))
  pane-sheet-with-selection(frame.tab-layout)
end method frame-sheet-with-selection;

define method refresh-frame (frame :: <object-browser>) => ()
  next-method();
  // If there is a project database and the primary object is #f, browse
  // the project's library. The primary object should never be #f unless
  // the project database didn't exist when the browser was created.
  // refresh-frame is called at the end of parsing, so this should cause
  // the library to be browsed once parsing is complete.
  when (frame-ensure-project-database(frame) & ~frame-primary-object(frame))
    let project = frame.frame-project;
    let library = project.project-library;
    frame-browse-object(frame, library);
  end;
  //---*** cpage: 1997.11.10 Experiment with Explorer Bars
  /*
    let object = frame-primary-object(frame);
    let tree-control = frame.object-hierarchy-pane;
    if (object)
      tree-control-roots(tree-control) := vector(object);
      gadget-value(tree-control) := vector(object)
    else
      tree-control-roots(tree-control) := #[]
    end;
  */
  /*
    let project    = frame.frame-project;
    let object     = frame.frame-primary-object;
    let displayer  = frame.object-hierarchy-pane;
    let names-pane = displayer-names-pane(displayer);
    let new-roots  = definition-contents(displayer, project);
    if (tree-control-roots(names-pane) ~= new-roots)
      tree-control-roots(names-pane) := new-roots;
    else
      update-gadget(names-pane);
    end;
    gadget-value(names-pane) := if (object)
                                  vector(object)
                                else
                                  #[]
                                end;
  */
  refresh-frame-view(frame)
end method refresh-frame;

define method refresh-frame-view
    (frame :: <object-browser>,
     #key pages, new-thread? = #t, refresh-all? = #f)
 => ()
  frame-status-message(frame) := "";
  refresh-environment-property-pane
    (frame.tab-layout, pages: pages,
     clean?: #t, new-thread?: new-thread?, refresh-all?: refresh-all?)
end method refresh-frame-view;


define method frame-browse-object?
    (frame :: <frame>, object :: <object>)
 => (browse? :: <boolean>)
  #f
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <number-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <character-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <string-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <symbol-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <source-form-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <thread-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <environment-object-breakpoint-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <composite-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;

define method frame-browse-object?
    (frame :: <frame>, object :: <foreign-object>)
 => (browse? :: <boolean>)
  #t
end method frame-browse-object?;


define sideways method browse-object
    (project :: <project-object>, object :: <environment-object>, #key page)
 => (success? :: <boolean>)
  find-environment-frame(default-port(),
                         <object-browser>,
                         project: project,
                         object: object,
                         page: page);
  #t
end method browse-object;

define sideways method browse-object-type
    (project :: <project-object>, object :: <environment-object>, #key page)
 => (success? :: <boolean>)
  let type = environment-object-type(project, object);
  if (type)
    find-environment-frame(default-port(),
                           <object-browser>,
                           project: project,
                           object: type,
                           page: page);
    #t
  end
end method browse-object-type;

define sideways method browse-object-generic-function
    (project :: <project-object>, function :: <generic-function-object>, #key page)
 => (success? :: <boolean>)
  find-environment-frame(default-port(),
                         <object-browser>,
                         project: project,
                         object: function,
                         page: page);
  #t
end method browse-object-generic-function;

define sideways method browse-object-generic-function
    (project :: <project-object>, object :: <method-object>, #key page)
 => (success? :: <boolean>)
  let function = method-generic-function(project, object);
  if (function)
    find-environment-frame(default-port(),
                           <object-browser>,
                           project: project,
                           object: function,
                           page: page);
    #t
  end
end method browse-object-generic-function;

define sideways method browse-object-generic-function
    (project :: <project-object>, object :: <environment-object>, #key page)
 => (success? :: <boolean>)
  #f
end method browse-object-generic-function;


/// Frame refreshing

define method frame-note-project-rebuilt
    (frame :: <object-browser>) => ()
  next-method();
  refresh-frame-view(frame, pages: #[#"definitions", #"names"])
end method frame-note-project-rebuilt;

define method frame-note-project-warnings-updated
    (frame :: <object-browser>) => ()
  next-method();
  refresh-frame-view(frame, pages: #[#"warnings"])
end method frame-note-project-warnings-updated;

define method frame-note-application-threads-changed
    (frame :: <object-browser>) => ()
  next-method();
  let object = frame-primary-object(frame);
  if (instance?(object, <application>)
        & object.server-project == frame-project(frame))
    refresh-frame-view(frame, pages: #[#"threads"])
  end
end method frame-note-application-threads-changed;

define method frame-note-all-breakpoints-changed
    (frame :: <object-browser>, state :: <breakpoint-state>)
 => ()
  next-method();
  // We don't need to refresh the sources page because Deuce will do
  // this for us. Are there any other pages that need refreshing?
  refresh-frame-view(frame, pages: #[#"breakpoints"])
end method frame-note-all-breakpoints-changed;

define method frame-note-breakpoint-state-changed
    (frame :: <object-browser>, breakpoint :: <breakpoint-object>,
     state :: <breakpoint-state>)
 => ()
  next-method();
  // We don't need to refresh the sources page because Deuce will do
  // this for us. Are there any other pages that need refreshing?
  refresh-frame-view(frame, pages: #[#"breakpoints"])
end method frame-note-breakpoint-state-changed;


/// Object Browser command table

define command-table *object-browser-file-io-command-table* (*global-command-table*)
  menu-item "Browse..."  = frame-select-primary-object,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Browses an object in this window.";
  include *export-command-table*;
  //---*** andrewa: remove printing options for 1.0
  // include *print-command-table*;
  separator;
  menu-item "Close"      = frame-close-file,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Closes the window.";
end command-table *object-browser-file-io-command-table*;

define command-table *object-browser-basic-view-command-table* (*global-command-table*)
  include *view-refresh-command-table*;
/* ---*** hughg, 1998/02/11: Hide for 1.0, since there are no options here!
  menu-item "Browser Options..." = frame-edit-options,
    documentation: "Enables you to change browser options.";
*/
end command-table *object-browser-basic-view-command-table*;

define command-table *object-browser-view-command-table* (*global-command-table*)
  include *object-browser-bar-options-command-table*;
  include *object-browser-basic-view-command-table*;
end command-table *object-browser-view-command-table*;

define command-table *object-browser-go-command-table* (*global-command-table*)
  include *history-command-table*;
  include *browse-locations-command-table*;
end command-table *object-browser-go-command-table*;

define command-table *object-browser-command-table* (*global-command-table*)
  menu-item "File"    = *object-browser-file-io-command-table*;
  menu-item "Edit"    = *edit-command-table*;
  menu-item "View"    = *object-browser-view-command-table*;
  menu-item "Go"      = *object-browser-go-command-table*;
  menu-item "Object"  = *primary-object-command-table*;
  menu-item "Window"  = *windows-command-table*;
  menu-item "Help"    = *environment-help-command-table*;
end command-table *object-browser-command-table*;
