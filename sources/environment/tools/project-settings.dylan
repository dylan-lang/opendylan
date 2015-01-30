Module:    environment-tools
Synopsis:  Project settings
Author:    Chris Page, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project settings

define constant $compilation-mode-descriptions
 = #["This setting controls trade-offs between speed and efficiency",
     "at compile-time and run-time."];

define constant $loose-mode-descriptions
  = #["Recommended during initial development of a project.",
      "Faster compilation, less efficient generated code.",
      "Fewer errors detected at compile-time.",
      "More information available when debugging.",
      "Allows both interactive evaluation and redefinition."];

define constant $tight-mode-descriptions
  = #["Recommended for delivered applications.",
      "Slower compilation, more efficient generated code.",
      "More error-detection at compile-time.",
      "Less information available when debugging.",
      "Allows limited interactive evaluation, but not redefinition."];

define pane <compile-settings-page> ()
  sealed constant slot %enabled? :: <boolean> = #t,
    init-keyword: enabled?:;
  sealed slot %compilation-mode :: <compilation-mode>,
    required-init-keyword: compilation-mode:;
  pane %loose-mode-button-layout (pane)
    vertically ()
      make(<radio-button>,
           enabled?: pane.%enabled?,
           id: #"loose",
           label: "Interactive development mode");
      make-description-layout($loose-mode-descriptions)
    end;
  pane %tight-mode-button-layout (pane)
    vertically ()
      make(<radio-button>,
           enabled?: pane.%enabled?,
           id: #"tight",
           label: "Production mode");
      make-description-layout($tight-mode-descriptions)
    end;
  pane %compilation-mode-pane (pane)
    make(<radio-box>,
         child: vertically (spacing: 4)
                  make-labels-layout($compilation-mode-descriptions);
                  pane.%loose-mode-button-layout;
                  pane.%tight-mode-button-layout;
                end,
         max-width: $fill,
         value: pane.%compilation-mode,
         value-changed-callback: method (box)
                                   pane.%compilation-mode := gadget-value(box)
                                 end);
  layout (pane)
    grouping ("Compilation mode", max-width: $fill)
      pane.%compilation-mode-pane
    end grouping;
end pane <compile-settings-page>;

define function make-description-layout
    (descriptions :: <sequence>) => (layout :: <layout>)
  make(<row-layout>,
       children: vector(make(<null-pane>, width: 12, fixed-width?: #t),
                        make-labels-layout(descriptions,
                                           prefix: "* ",
                                           y-spacing: 2,
                                           border: 2)),
       fixed-height?: #t)
end function make-description-layout;

define pane <link-settings-page> ()
  sealed constant slot %enabled? :: <boolean> = #t,
    init-keyword: enabled?:;
  sealed slot %target-name :: false-or(<file-locator>),
    required-init-keyword: target-name:;
  sealed slot %target-type :: <project-target-type>,
    required-init-keyword: target-type:;
  sealed slot %interface-type :: <project-interface-type>,
    required-init-keyword: interface-type:;
  sealed slot %base-address :: false-or(<machine-word>),
    required-init-keyword: base-address:;
  sealed slot %major-version :: false-or(<integer>),
    required-init-keyword: major-version:;
  sealed slot %minor-version :: false-or(<integer>),
    required-init-keyword: minor-version:;
  pane %target-name-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value: as(<string>, pane.%target-name),
         value-changed-callback: method (gadget :: <text-field>)
                                   let full-name = gadget.gadget-value;
                                   let locator = as(<file-locator>, full-name);
                                   let base = locator.locator-base;
                                   let type = locator.environment-locator-type;
                                   let type
                                     = select (type by \=)
                                         #"dll"    => #"dll";
                                         #"exe"    => #"executable";
                                         otherwise => pane.%target-type;
                                       end;
                                   update-link-settings(pane, full-name, type)
                                 end);
  pane %target-type-pane (pane)
    make(<option-box>,
         enabled?: pane.%enabled?,
         items: #[#["&Executable",                 #"executable"],
                  #["&Dynamic Link Library (DLL)", #"DLL"]],
         value: pane.%target-type,
         label-key: first,
         value-key: second,
         value-changed-callback: method (gadget :: <option-box>)
                                   let type = gadget.gadget-value;
                                   let old-name
                                     = pane.%target-name.locator-base;
                                   let type-name
                                     = select (type)
                                         #"dll"        => ".dll";
                                         #"executable" => ".exe";
                                       end;
                                   let name
                                     = concatenate(old-name, type-name);
                                   update-link-settings(pane, name, type)
                                 end);
  pane %interface-type-pane (pane)
    make(<option-box>,
         enabled?: pane.%enabled?,
         items: #[#["Windows GUI",     #"gui"],
                  #["Windows Console", #"console"]],
         value: pane.%interface-type,
         label-key: first,
         value-key: second,
         value-changed-callback: method (gadget)
                                   pane.%interface-type := gadget.gadget-value
                                 end);
  pane %base-address-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value-type: <machine-word>,
         value: pane.%base-address,
         value-changing-callback: method (gadget)
                                    pane.%base-address := gadget.gadget-value
                                  end);
  pane %major-version-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value-type: <integer>,
         value: pane.%major-version,
         value-changing-callback: method (gadget)
                                    pane.%major-version := gadget.gadget-value
                                  end);
  pane %minor-version-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value-type: <integer>,
         value: pane.%minor-version,
         value-changing-callback: method (gadget)
                                    pane.%minor-version := gadget.gadget-value
                                  end);
  layout (pane)
    vertically (spacing: 8)
      grouping ("Target file", max-width: $fill)
        make(<table-layout>,
             y-spacing: 2,
             contents:
               vector(vector(make(<label>, label: "Name:"),
                             pane.%target-name-pane),
                      vector(make(<label>, label: "Type:"),
                             pane.%target-type-pane)))
      end grouping;
      grouping ("Base address", max-width: $fill)
        make(<table-layout>,
             y-spacing: 2,
             contents:
               vector(vector(make(<label>, label: "Address:"),
                             pane.%base-address-pane)));
      end grouping;
      grouping ("Version information", max-width: $fill)
        make(<table-layout>,
             y-spacing: 2,
             contents:
               vector(vector(make(<label>, label: "Major (definition version):"),
                             pane.%major-version-pane),
                      vector(make(<label>, label: "Minor (implementation version):"),
                             pane.%minor-version-pane)))
      end grouping;
      grouping ("Windows subsystem", max-width: $fill)
        horizontally ()
          make(<label>, label: "Subsystem:");
          pane.%interface-type-pane;
        end horizontally
      end grouping;
    end vertically;
end pane <link-settings-page>;

define function update-link-settings
    (pane :: <link-settings-page>, name :: <string>, type :: <symbol>)
 => ()
  gadget-value(pane.%target-name-pane) := name;
  gadget-value(pane.%target-type-pane) := type;
  pane.%target-type := type;
  pane.%target-name := as(<file-locator>, name)
end function update-link-settings;

define pane <debug-settings-pane> ()
  sealed constant slot %enabled? :: <boolean> = #t,
    init-keyword: enabled?:;
  sealed slot %debug-filename :: false-or(<file-locator>),
    required-init-keyword: filename:;
  sealed slot %debug-arguments :: false-or(<string>),
    required-init-keyword: arguments:;
  sealed slot %debug-start-function-name :: false-or(<string>),
    required-init-keyword: start-function-name:;
  sealed slot %debug-machine :: false-or(<machine>),
    required-init-keyword: machine:;
  pane %debug-filename-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value: begin
                  let filename = pane.%debug-filename;
                  filename & as(<string>, filename)
                end,
         value-changing-callback: method (gadget)
                                    //--- Need some error checking...
                                    let value = gadget.gadget-value;
                                    pane.%debug-filename
                                      := unless (empty?(value))
                                           as(<file-locator>, value)
                                         end;
                                  end);
  pane %debug-arguments-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value: pane.%debug-arguments,
         value-changing-callback: method (gadget)
                                    pane.%debug-arguments := gadget.gadget-value;
                                  end);
  pane %debug-start-function-name-pane (pane)
    make(<text-field>,
         enabled?: pane.%enabled?,
         value: pane.%debug-start-function-name,
         value-changing-callback: method (gadget)
                                    pane.%debug-start-function-name
                                      := gadget.gadget-value;
                                  end);
  pane %debug-machine-pane (pane)
    make(<option-box>,
         enabled?: pane.%enabled?,
         items: available-machines(),
         label-key: method (machine :: <machine>)
                      if (machine == environment-host-machine())
                        concatenate(machine.machine-hostname,
                                    " (Local machine)")
                      else
                        machine.machine-hostname
                      end
                    end,
         value: pane.%debug-machine,
         value-changed-callback: method (gadget)
                                    pane.%debug-machine := gadget.gadget-value
                                  end method);
  pane %open-remote-connection-button (pane)
    make(<button>,
         label: "Open New Connection...",
         enabled?: pane.%enabled?,
         activate-callback: method (gadget)
                              let frame = sheet-frame(gadget);
                              let machine
                                = open-remote-connection(owner: frame);
                              if (machine)
                                let gadget = pane.%debug-machine-pane;
                                gadget-items(gadget) := available-machines();
                                gadget-value(gadget, do-callback?: #t)
                                  := machine
                              end
                            end);
  layout (pane)
    vertically (spacing: 8)
      grouping ("Command line", max-width: $fill)
        make(<table-layout>,
             y-spacing: 2,
             contents:
               vector(vector(make(<label>, label: "Command:"),
                             pane.%debug-filename-pane),
                      vector(make(<label>, label: "Arguments:"),
                             pane.%debug-arguments-pane)))
      end grouping;
      grouping ("Start function", max-width: $fill)
        horizontally ()
          make(<label>, label: "Name:");
          pane.%debug-start-function-name-pane;
        end
      end grouping;
      grouping ("Remote machine", max-width: $fill)
        vertically (spacing: 8)
          horizontally ()
            make(<label>, label: "Machine:");
            pane.%debug-machine-pane;
          end;
          pane.%open-remote-connection-button
        end
      end grouping;
    end vertically;
end pane <debug-settings-pane>;

define frame <project-settings-dialog> (<property-frame>)
  sealed constant slot %project :: <project-object>,
    required-init-keyword: project:;
  sealed constant slot %enabled? :: <boolean> = #t,
    init-keyword: enabled?:;
  pane project-settings-compile-page (dialog)
    begin
      let project = dialog.%project;
      make(<compile-settings-page>,
           enabled?:         dialog.%enabled?,
           compilation-mode: if (project.project-can-be-built?)
                               project.project-compilation-mode
                             else
                               #"tight"
                             end)
    end;
  pane project-settings-link-page (dialog)
    begin
      let project = dialog.%project;
      let can-be-built? = project.project-can-be-built?;
      make(<link-settings-page>,
           enabled?:       can-be-built?,
           target-name:    project.project-build-filename,
           target-type:    project.project-target-type,
           interface-type: project.project-interface-type,
           base-address:   can-be-built? & project.project-base-address,
           major-version:  can-be-built? & project.project-major-version,
           minor-version:  can-be-built? & project.project-minor-version)
    end;
  pane project-settings-debug-page (dialog)
    begin
      let project = dialog.%project;
      make(<debug-settings-pane>,
           enabled?:            project.project-can-be-built?,
           filename:            project.project-debug-filename,
           arguments:           project.project-debug-arguments,
           start-function-name: project.project-start-function-name,
           machine:             project.project-debug-machine)
    end;
  pages (dialog)
    vector(make(<property-page>,
                child: dialog.project-settings-compile-page,
                label: "Compile"),
           make(<property-page>,
                child: dialog.project-settings-link-page,
                label: "Link"),
           make(<property-page>,
                child: dialog.project-settings-debug-page,
                label: "Debug"));
  keyword title:  = "Project Settings";
  keyword mode:   = #"modal";
/*---*** andrewa: Try removing kludges...
  keyword width:  = 400; //--- cpage: 1997.09.29 Temporary [emulator?] kludge
  keyword height: = 250; //           to prevent dialog from being too small.
*/
  keyword exit-callback:  = method (dialog)
                              when (valid-project-settings?(dialog))
                                update-project-settings(dialog);
                                exit-dialog(dialog);
                              end;
                            end;
end frame <project-settings-dialog>;

define method initialize
    (dialog :: <project-settings-dialog>, #key) => ()
  next-method();
  dialog-exit-enabled?(dialog) := dialog.%enabled?
end method initialize;

// Handle <machine-word> for text entry fields
define sealed sideways method gadget-text-parser
    (type == <machine-word>, text :: <string>)
 => (value :: false-or(<machine-word>))
  block ()
    let (value, next) = string-to-machine-word(text);
    next = size(text) & value
  exception (<error>)
    #f
  end
end method gadget-text-parser;

define sealed sideways method gadget-value-printer
    (type == <machine-word>, value :: false-or(<machine-word>))
 => (text :: <string>)
  if (value)
    machine-word-to-string(value, prefix: "#x")
  else
    ""
  end
end method gadget-value-printer;

define method update-project-settings
    (dialog :: <project-settings-dialog>) => ()
  let project = dialog.%project;
  if (project.project-can-be-built?)
    let compile-page = dialog.project-settings-compile-page;
    let link-page    = dialog.project-settings-link-page;
    project.project-compilation-mode := compile-page.%compilation-mode;
    project.project-build-filename   := link-page.%target-name;
    project.project-target-type      := link-page.%target-type;
    project.project-interface-type   := link-page.%interface-type;
    project.project-base-address     := link-page.%base-address;
    project.project-major-version    := link-page.%major-version;
    project.project-minor-version    := link-page.%minor-version;
  end;
  let debug-page = dialog.project-settings-debug-page;
  project.project-debug-filename      := debug-page.%debug-filename;
  project.project-debug-arguments     := debug-page.%debug-arguments;
  project.project-start-function-name := debug-page.%debug-start-function-name;
  project.project-debug-machine       := debug-page.%debug-machine;
end method update-project-settings;

define method valid-project-settings?
    (dialog :: <project-settings-dialog>)
 => (valid? :: <boolean>)
  let link-page = dialog.project-settings-link-page;
  block (return)
    local method validate (valid? :: <boolean>, message :: <string>)
            unless (valid?)
              environment-error-message(message, owner: dialog);
              return(#f);
            end;
          end method validate;
    // Validate target name.
    let target-name = link-page.%target-name;
    validate(target-name ~== #f,
             "The target file name must not be empty.");
    // Validate DLL base address. Check the text size first so we can
    // distinguish between no setting and bogus text.
    let base-address = link-page.%base-address;
    if (size(link-page.%base-address-pane.gadget-text) > 0)
      validate(base-address ~= #f,
               "The base address must be an unsigned hexadecimal integer,"
                 " or you may leave it blank to have one automatically assigned.");
    end;
    // Validate version information.
    let major-version = link-page.%major-version;
    let minor-version = link-page.%minor-version;
    validate(major-version & ~negative?(major-version)
               & minor-version & ~negative?(minor-version),
             "The major and minor versions must be positive decimal integers.");
    // If we got this far, all's well.
    #t
  end block;
end method valid-project-settings?;

define method frame-edit-project-settings
    (frame :: <environment-frame>, #key project = frame.frame-current-project) => ()
  let framem = frame.frame-manager;
  with-frame-manager (framem)
    let filename = project.project-filename;
    let enabled? = filename & file-property(filename, #"writeable?");
    let dialog
      = make(<project-settings-dialog>,
             owner:    frame,
             project:  project,
             enabled?: enabled?);
    start-dialog(dialog)
  end
end method frame-edit-project-settings;
