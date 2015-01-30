Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constants

define constant $playground-description
  = #["This option starts a simple application that you",
      "can use to evaluate Dylan code interactively."];

define constant $examples-description
  = #["This option displays a list of example projects",
      "that you can use to get started using Dylan."];

/*---*** Not used currently
define constant $tutorial-description
  = #["This option starts a tutorial that describes",
      "Open Dylan and the Dylan language."];
*/


/// Variables
///---*** These really need to be read from a registry

define variable *last-project* :: false-or(<project-object>) = #f;

//--- cpage: 1997.10.25 This is a bit hackish. We use this variable to pass the
//           filename or project parameter from start-environment to the
//           handle-event method on (<environment-primary-frame>, <frame-mapped-event>).
//           Perhaps there's a better way? I explicitly didn't add a slot to
//           <environment-primary-frame> yet because I'm not sure this functionality
//           is even needed or if it is a remnant of the emulator environment.
define variable *startup-open-file* = #f;


/// Start environment

define method start-environment
    (#key filename :: false-or(<file-locator>),
          project)
 => (status-code :: <integer>)
  //--- cpage: 1997.09.26 This is a kludge for the emulator, which can restart
  //           the environment without initializing module variables.
  *application-exiting?* := #f;
  block()
    broadcast($environment-channel, make(<environment-starting-message>));
    *startup-open-file* := project | filename;
    let primary-frame
      = make-environment-frame(default-port(), <environment-primary-frame>);
    start-environment-frame(primary-frame);
    // hughg, 1998/09/25: See bug 4072 for why we don't WAIT-FOR-SHUTDOWN any
    // more.  We should really remove that and related stuff from ENVIRONMENT-
    // FRAMEWORK, but it won't hurt to leave it in for now.
    0
  cleanup
    broadcast($environment-channel, make(<environment-stopped-message>))
  end block
end method start-environment;

define method handle-environment-startup
    (frame :: <environment-frame>) => ()
  broadcast($environment-channel, make(<environment-started-message>));
  let (process, id) = just-in-time-debugging-arguments();
  let filename = *startup-open-file*;
  case
    process & id =>
      frame-open-just-in-time-project(frame, process, id);
    filename =>
      let location = as(<file-locator>, filename);
      let full-location = merge-locators(location, working-directory());
      open-file(owner: frame, filename: as(<string>, full-location));
      // Reset this, for the emulator, which can start the environment more than once.
      *startup-open-file* := #f;
    otherwise =>
      select (environment-start-action())
        #"start-dialog" => initial-dialog(frame);
        #"open-file"    => open-file(owner: frame);
        #"no"           => /* Do nothing */;
      end select;
  end
end method handle-environment-startup;

/// Project browser startup

//---*** cpage: 97.08.13 Note that status codes will eventually be replaced by
//              properly defined exceptions. I've temporarily added this code
//              to assist in reporting errors to the user.
define constant $project-browser-not-found-status = 1;

define method find-project-browser
    (project :: <object>, #rest args, #key, #all-keys)
 => (status-code :: false-or(<integer>))
  let project = coerce-project(project);
  if (project)
    *last-project* := project;
    apply(ensure-project-browser-showing-project, project, args);
    0
  else
    $project-browser-not-found-status
  end
end method find-project-browser;


/// Initial dialog

define frame <initial-dialog> (<dialog-frame>)
  slot initial-dialog-action :: <symbol>,
    required-init-keyword: action:;
  /*---*** Temporarily remove this until we have a Tutorial
  pane %dialog-tutorial-button (frame)
    make(<radio-button>,
         label: "Tutorial",
         id: #"tutorial",
         activate-callback: exit-dialog);
  */
  pane %dialog-example-button (frame)
    make(<radio-button>,
         label: "Example project",
         id: #"example",
         activate-callback: exit-dialog);
  pane %dialog-playground-button (frame)
    make(<radio-button>,
         label: "Dylan playground",
         id: #"playground",
         activate-callback: exit-dialog);
  pane %dialog-new-project-button (frame)
    make(<radio-button>,
         label: "Project",
         id: #"new-project",
         activate-callback: exit-dialog);
  pane %dialog-new-text-button (frame)
    make(<radio-button>,
         label: "Text File",
         id: #"new-text",
         activate-callback: exit-dialog);
  pane %dialog-open-button (frame)
    make(<radio-button>,
         label: "Open",
         id: #"open",
         activate-callback: exit-dialog);
  /*---*** Temporarily remove this until we have a Tutorial
  pane %dialog-tutorial-layout (frame)
    vertically (spacing: 0)
      make(<label>,
           label: $tutorial-large-bitmap,
           width:  32, min-width:  32, max-width:  32,
           height: 32, min-height: 32, max-height: 32);
      frame.%dialog-tutorial-button;
      make(<null-pane>,
           width:  32, min-width:  32, max-width:  32,
           height: 32, min-height: 32, max-height: 32);
      make-labels-layout($tutorial-description)
    end;
  */
  pane %dialog-example-layout (frame)
    make(<table-layout>,
         columns: 2,
         y-alignment: #"center",
         y-spacing: 2,
         children:
           vector(make(<label>,
                       label: $examples-large-bitmap,
                       width:  32, min-width:  32, max-width:  32,
                       height: 32, min-height: 32, max-height: 32),
                  frame.%dialog-example-button,
                  make(<null-pane>,
                       width:  32, min-width:  32, max-width:  32,
                       height: 32, min-height: 32, max-height: 32),
                  make-labels-layout($examples-description)));
  pane %dialog-playground-layout (frame)
    make(<table-layout>,
         columns: 2,
         y-alignment: #"center",
         y-spacing: 2,
         children:
           vector(make(<label>,
                       label: $playground-large-bitmap,
                       width:  32, min-width:  32, max-width:  32,
                       height: 32, min-height: 32, max-height: 32),
                  frame.%dialog-playground-button,
                  make(<null-pane>,
                       width:  32, min-width:  32, max-width:  32,
                       height: 32, min-height: 32, max-height: 32),
                  make-labels-layout($playground-description)));
  pane %dialog-buttons (frame)
    vertically (spacing: 5, equalize-widths?: #t)
      /*---*** 1997.02.09 Temporarily omit the tutorial option until implemented.
      grouping ("Start the Open Dylan tutorial")
        make(<table-layout>,
             columns: 2,
             y-alignment: #"center",
             children vector(%dialog-tutorial-layout))
      end grouping;
      */
      grouping ("Open an example project")
        vertically (spacing: 2)
          frame.%dialog-example-layout;
          frame.%dialog-playground-layout;
        end
      end grouping;
      grouping ("Create a new project")
        make(<table-layout>,
             columns: 2,
             y-alignment: #"center",
             y-spacing: 4,
             children:
               vector(make(<label>,
                           label: $project-file-large-bitmap,
                           width:  32, min-width:  32, max-width:  32,
                           height: 32, min-height: 32, max-height: 32),
                      frame.%dialog-new-project-button /*,
                      make(<label>,
                           label: $text-file-large-bitmap,
                           width:  32, min-width:  32, max-width:  32,
                           height: 32, min-height: 32, max-height: 32),
                      frame.%dialog-new-text-button) */))
      end grouping;
      grouping ("Open an existing project")
        make(<table-layout>,
             columns: 2,
             y-alignment: #"center",
             children:
               vector(make(<label>,
                           label: $open-large-bitmap,
                           width:  32, min-width:  32, max-width:  32,
                           height: 32, min-height: 32, max-height: 32),
                      frame.%dialog-open-button))
      end grouping;
    end vertically;
  pane %dialog-radio-box (frame)
    make(<radio-box>,
         child: frame.%dialog-buttons,
         value: frame.initial-dialog-action,
         value-changed-callback: method (gadget)
                                   let action = gadget.gadget-value;
                                   frame.initial-dialog-action := action
                                 end);
  layout (frame)
    frame.%dialog-radio-box;
  keyword title: = release-product-name();
  keyword exit-buttons-position: = #"bottom";
  keyword center?: = #t;
end frame <initial-dialog>;

define method initial-dialog (frame :: <frame>) => (status-code :: <integer>)
  let status-code :: <integer>
    = block (return)
        while (#t)
          with-abort-restart ()
            let dialog
              = make(<initial-dialog>,
                     owner: frame,
                     action: environment-start-dialog-action());
            unless (start-dialog(dialog))
              return(0)
            end;
            let action = dialog.initial-dialog-action;
            environment-start-dialog-action() := action;
            let status-code
              = select (action)
                  #"tutorial"    => frame-start-tutorial(frame);
                  #"example"     => frame-open-example(frame);
                  #"playground"  => frame-open-playground(frame);
                  #"new-project" => create-new-project(frame: frame) & 0;
                  #"new-text"    => editor-new-file() & 0;
                  #"open"        => open-file(owner: frame);
                  otherwise      => #f;
                end;
            status-code & return(status-code)
          end
        end
      end;
  // ---*** hughg, 1997/08/04: What if status code is #f (which is
  // not an <integer>, which is the return type here)?  Return 0?
  status-code
end method initial-dialog;

define function frame-start-tutorial
    (frame :: <frame>) => (status-code :: false-or(<integer>))
  not-yet-implemented(message: "The tutorial is not in this early release.", owner: frame);
  #f
end function frame-start-tutorial;


/// Examples

define frame <examples-dialog> (<dialog-frame>)
  slot dialog-children-function :: <function> = info-examples;
  pane %category-pane (dialog)
    make(<option-box>,
         items: #[#["examples",                    #"examples"],
                  #["examples by category",        #"examples-by-category"],
                  #["examples by library pack",    #"library-pack-examples"],
                  #["test suites",                 #"test-suites"],
                  #["test suites by category",     #"test-suites-by-category"],
                  #["test suites by library pack", #"library-pack-test-suites"]],
         value: #"examples-by-category",
         label-key: first,
         value-key: second,
         documentation: "Select example view",
         value-changed-callback: method (gadget)
                                   let frame = sheet-frame(gadget);
                                   let category = gadget-value(gadget);
                                   note-example-category-changed(frame, category)
                                 end);
  pane %examples-pane (dialog)
    make(<tree-control>,
         icon-function: method (info)
                          select (info by instance?)
                            <library-category-info> => $examples-bitmap;
                            <library-pack-info>     => $examples-bitmap;
                            <library-info>          => $project-bitmap;
                          end select;
                        end method,
         children-predicate: method (info)
                               select (info by instance?)
                                 <library-category-info> =>
                                   #t;
                                 <library-pack-info> =>
                                   ~empty?(dialog.dialog-children-function(info));
                                 otherwise =>
                                   #f;
                               end
                             end,
         children-generator: method (info)
                               select (info by instance?)
                                 <library-category-info> =>
                                   concatenate(sort-release-info(info.info-subcategories),
                                               sort-release-info(info.info-libraries));
                                 <library-pack-info> =>
                                   sort-release-info(dialog.dialog-children-function(info));
                                 otherwise =>
                                   #[];
                               end
                             end,
         use-buttons-only?: #f,
         activate-callback: method (gadget)
                              let dialog = sheet-frame(gadget);
                              exit-dialog(dialog)
                            end,
         value-changed-callback: method (gadget)
                                   let dialog = sheet-frame(gadget);
                                   let info = gadget-value(gadget);
                                   note-example-selected(dialog, info)
                                 end method,
         label-key: info-title,
         width: 250,
         height: 250);
  pane %description-pane (dialog)
    make(<text-editor>,
         //---*** Should use a functional space requirement
         min-width: 400, max-width: $fill,
         lines: 4, fixed-height?: #t,
         read-only?: #f, tab-stop?: #f,
         scroll-bars: #"none");
  layout (dialog)
    vertically (spacing: 5)
      grouping ("Open an example project")
        vertically (spacing: 8)
          horizontally (spacing: 4)
            make(<label>, label: "View:");
            dialog.%category-pane;
          end;
          dialog.%examples-pane;
          dialog.%description-pane;
        end
      end
    end;
  input-focus (dialog)
    dialog.%examples-pane;
  keyword title: = "Open Example Project";
  keyword center?: = #t;
end frame <examples-dialog>;

define method initialize
    (dialog :: <examples-dialog>, #key) => ()
  next-method();
  note-example-category-changed(dialog, gadget-value(dialog.%category-pane))
end method initialize;

/*---*** andrewa: do we still need this?
define method handle-event
    (dialog :: <examples-dialog>, event :: <frame-focus-in-event>) => ()
  next-method();
  frame-input-focus(dialog) := dialog.%examples-pane
end method handle-event;
*/

define method note-example-category-changed
    (dialog :: <examples-dialog>, category :: <symbol>) => ()
  let category-pane = dialog.%category-pane;
  let examples-pane = dialog.%examples-pane;
  dialog.dialog-children-function
    := select (category)
         #"examples", #"examples-by-category", #"library-pack-examples" =>
           info-examples;
         #"test-suites", #"test-suites-by-category", #"library-pack-test-suites" =>
           info-test-suites;
       end;
  let roots
    = select (category)
        #"examples", #"test-suites" =>
          let libraries = make(<stretchy-object-vector>);
          for (library-pack-info :: <library-pack-info> in installed-library-packs())
            if (instance?(library-pack-info, <basic-library-pack-info>))
              let pack-libraries
                = select (category)
                    #"examples"    => library-pack-info.info-examples;
                    #"test-suites" => library-pack-info.info-test-suites;
                  end;
              for (info :: <library-info> in pack-libraries)
                add!(libraries, info)
              end
            end
          end;
          sort-release-info(libraries);
        #"examples-by-category", #"test-suites-by-category" =>
          sort-release-info(installed-library-categories(dialog.dialog-children-function));
        #"library-pack-examples", #"library-pack-test-suites" =>
          remove(installed-library-packs(), #f,
                 test: method (info :: <library-pack-info>, dummy == #f)
                         ~instance?(info, <basic-library-pack-info>)
                           | empty?(dialog.dialog-children-function(info))
                       end);
      end;
  gadget-value(category-pane) := category;
  tree-control-roots(examples-pane) := roots;
  note-example-selected(dialog, gadget-value(examples-pane));
end method note-example-category-changed;

define method sort-release-info
    (info :: <sequence>) => (sorted :: <sequence>)
  sort(info,
       test: method (info1 :: <release-info>, info2 :: <release-info>)
               info1.info-title < info2.info-title
             end)
end method sort-release-info;

define method note-example-selected
    (dialog :: <examples-dialog>, info :: false-or(<release-info>)) => ()
  gadget-text(dialog.%description-pane)
    := if (info)
         format-to-string("%s\n", info.info-description)
       else
         ""
       end;
  dialog-exit-enabled?(dialog) := instance?(info, <library-info>)
end method note-example-selected;

define method frame-open-example
    (frame :: <frame>) => (status-code :: false-or(<integer>))
  let dialog = make(<examples-dialog>, owner: frame, exit-enabled?: #f);
  let examples-pane = dialog.%examples-pane;
  frame-input-focus(dialog) := examples-pane;
  when (start-dialog(dialog))
    let info :: <library-info> = gadget-value(examples-pane);
    let location = info.info-location;
    let project-name = as(<string>, info.info-name);
    let project :: false-or(<project-object>)
      = case
          location =>
            if (file-exists?(location))
              open-project(location)
            end;
          otherwise =>
            find-project(project-name);
        end;
    if (project)
      find-project-browser(project)
    else
      environment-error-message
        (concatenate
           ("The example project '", project-name, "' could not be opened."
            " Make sure the example projects are installed."),
         owner: frame);
      #f
    end if
  end
end method frame-open-example;
