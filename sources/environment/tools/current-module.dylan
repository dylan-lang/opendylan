Module:    environment-tools
Synopsis:  Environment tools library
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Current module handling

define open abstract class <frame-module-mixin> (<environment-frame>)
  slot %module :: false-or(<module-object>) = #f,
    init-keyword: module:;
end class <frame-module-mixin>;

define open generic frame-current-module
    (frame :: <frame-module-mixin>) => (module :: false-or(<module-object>));

define open generic frame-current-library
    (frame :: <frame-module-mixin>) => (library :: false-or(<library-object>));

define open generic frame-current-module-setter
    (module :: false-or(<module-object>), frame :: <frame-module-mixin>)
 => (module :: false-or(<module-object>));

define method frame-current-module
    (frame :: <frame-module-mixin>) => (module :: false-or(<module-object>))
  let project = frame.frame-current-project;
  if (project)
    let module = frame.%module;
    if (module)
      environment-object-exists?(project, module) & module
    else
      let library = project.project-library;
      let module = library & library-default-module(project, library);
      frame-current-module(frame) := module
    end
  end
end method frame-current-module;

define method frame-current-module-setter
    (module :: false-or(<module-object>), frame :: <frame-module-mixin>)
 => (module :: false-or(<module-object>))
  frame.%module := module
end method frame-current-module-setter;

define method frame-current-library
    (frame :: <frame-module-mixin>) => (module :: false-or(<library-object>))
  let project = frame.frame-current-project;
  if (project)
    let module  = frame.frame-current-module;
    (module & environment-object-library(project, module))
      | project.project-library
  end
end method frame-current-library;

define variable $current-module-dialog-width :: <integer> = 500;

define method frame-choose-current-module
    (frame :: <frame-module-mixin>)
 => (module :: <module-object>)
  with-frame-manager (frame-manager(frame))
    let text-field
      = make(<text-field>,
             documentation: "Name of a module in the current project.",
             value-changing-callback:
               method (gadget)
                 let dialog = sheet-frame(gadget);
                 let module = find-module(frame.frame-project, gadget-value(gadget));
                 dialog-exit-enabled?(dialog) := module & #t;
               end method,
             value-changed-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>,
             owner: frame,
             title: "Choose a module",
             layout: text-field,
             input-focus: text-field,
             width: max($current-module-dialog-width, 250));
    dialog-exit-enabled?(dialog) := #f;
    when (start-dialog(dialog))
      let (width, height) = frame-size(dialog);
      $current-module-dialog-width := width;
      let module = find-module(frame.frame-project, gadget-value(text-field));
      when (module)
        frame-current-module(frame) := module
      end;
    end
  end;
  frame-current-module(frame)
end method frame-choose-current-module;


/// Module Gadget

define open abstract class <frame-module-gadget-mixin> (<frame>)
  slot %module-gadget;
end class <frame-module-gadget-mixin>;

define constant $module-gadget-none-label = "(None)";

define generic module-gadget-label-key
    (frame :: <frame-module-gadget-mixin>, value :: false-or(<module-object>))
 => (name :: <string>);

define method module-gadget-label-key
    (frame :: <frame-module-gadget-mixin>, value == #f)
 => (name :: <string>)
  ignore(frame, value);
  $module-gadget-none-label
end method module-gadget-label-key;

define method module-gadget-label-key
    (frame :: <frame-module-gadget-mixin>, module :: <module-object>)
 => (name :: <string>)
  let project = frame.ensure-frame-project;
  let library = project.project-library;
  let home-library = environment-object-library(project, module);
  if (library == home-library)
    frame-default-object-name(frame, module)
  else
    environment-object-display-name(project, module, #f, qualify-names?: #t)
  end
end method module-gadget-label-key;


define open generic frame-available-modules
    (frame :: <frame-module-gadget-mixin>)
 => (items :: <sequence>); // limited(<sequence>, of: <module-object>)

define method frame-available-modules
    (frame :: <frame-module-gadget-mixin>)
 => (items :: <sequence>) // limited(<sequence>, of: <module-object>)
  let project = frame.ensure-frame-project;
  let library = project.project-library;
  if (library)
    library-modules(project, library, imported?: #t)
  else
    #[]
  end if;
end method frame-available-modules;

define function frame-sorted-available-modules
    (frame :: <frame-module-gadget-mixin>)
 => (items :: <sequence>) // limited(<sequence>, of: <module-object>)
  let project = frame.ensure-frame-project;
  let modules = frame-available-modules(frame);
  let library = project.project-library;
  let sorted-modules
    = sort(modules,
           test: method
                     (module1 :: <module-object>, module2 :: <module-object>)
                  => (less-than? :: <boolean>)
                   let library1 = environment-object-library(project, module1);
                   let library2 = environment-object-library(project, module2);
                   let main-library1? = library1 == library;
                   let main-library2? = library2 == library;
                   case
                     main-library1? & ~main-library2? =>
                       #t;
                     ~main-library1? & main-library2? =>
                       #f;
                     otherwise =>
                       let name1
                         = environment-object-primitive-name(project, module1)
                             | $n/a;
                       let name2
                         = environment-object-primitive-name(project, module2)
                         | $n/a;
                       name1 < name2;
                   end
                 end);
  concatenate(vector(#f), sorted-modules)
end function frame-sorted-available-modules;

//---*** cpage: 97.07.15 This is not currently called. It should be called
//              when the available list of modules may have changed. My
//              initial attempt to call this in refresh-frame produced odd
//              results, so I'm leaving it out for now.
define method update-module-gadget
    (frame :: <frame-module-gadget-mixin>)
 => ()
  let project = frame.ensure-frame-project;
  let module-gadget = frame.%module-gadget;
  gadget-items(module-gadget) := frame-sorted-available-modules(frame);
  gadget-value(module-gadget) := frame-current-module(frame);
end method update-module-gadget;

define method make-module-tool-bar-buttons
    (frame :: <frame-module-gadget-mixin>)
 => (buttons :: <sequence>)
  let project = frame.ensure-frame-project;
  let module-gadget
    = make(<option-box>,
           label: "Module",
           items: frame-sorted-available-modules(frame),
           label-key: curry(module-gadget-label-key, frame),
           value: frame-current-module(frame),
           documentation: "Select View Module",
           min-width: 100,
           max-width: 200,
           value-changed-callback: method (gadget) => ()
                                     let frame = sheet-frame(gadget);
                                     let module = gadget-value(gadget);
                                     frame-current-module(frame) := module;
    //---*** cpage: Perhaps this should call refresh-frame-view, instead, in order
    //              to minimize redrawing, and so that we can refresh the module
    //              gadget inside refresh-frame, which currently causes a potential
    //              circularity. In order for this to be true, nothing outside the
    //              property page can display namespace suffixes.
                                     refresh-frame(frame);
                                   end method);
  frame.%module-gadget := module-gadget;
  vector(module-gadget)
end method make-module-tool-bar-buttons;

