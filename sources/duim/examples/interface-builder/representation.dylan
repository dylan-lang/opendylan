Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// We need a few workarounds for DUIM version 1.0
define constant $duim-version = 1;

/// Representation Frame

define class <representation-frame> (<simple-frame>)
  slot frame-model :: <model-frame>,
    required-init-keyword: model:;
  slot frame-interface-builder :: <simple-frame>,
    required-init-keyword: interface-builder:;
end class <representation-frame>;

define method handle-event
    (frame :: <representation-frame>, event :: <frame-mapped-event>) => ()
  next-method();
  raise-frame(frame.frame-interface-builder)
end method handle-event;

define method handle-event
    (frame :: <representation-frame>, event :: <frame-exited-event>) => ()
  next-method();
  let model = frame.frame-model;
  if (frame == model.model-representation)
    destroy-representation(model)
  end
end method handle-event;


/// Interface builder frame handling

define method find-parent-representation
    (model :: <model-sheet>)
 => (representation :: <representation>)
  let parent = model-parent(model);
  if (parent)
    model-representation(parent)
      | find-parent-representation(parent)
  end
end method find-parent-representation;

define method find-parent-representation
    (model :: <model-frame>)
 => (representation :: false-or(<representation>))
  #f
end method find-parent-representation;


// Ensure a representation

define method ensure-representation
    (frame :: <frame>, model :: <model-object>)
 => (representation :: false-or(<representation>))
  model-representation(model)
    | make-representation(frame, model)
end method ensure-representation;

define method ensure-representation
    (frame :: <frame>, model == #f)
 => (representation :: false-or(<representation>))
  #f
end method ensure-representation;


// Make a representation

define method make-representation 
    (frame :: <frame>, model :: <model-object>, #key add-to-parent? = #t)
 => (representation :: <representation>)
  do-make-representation(frame, model, add-to-parent?: add-to-parent?);
  for (child in model-children(model))
    make-representation(frame, child, add-to-parent?: add-to-parent?)
  end;
  model.model-representation
end method make-representation;

define method do-make-representation 
    (frame :: <frame>, model :: <model-frame>, #key)
 => (new-frame :: <frame>)
  let (x, y) = frame-position(frame);
  let (width, height) = frame-size(frame);
  let (screen-width, screen-height) = sheet-size(display(frame));
  let x = min(x + width + 20, screen-width - 200);
  model.model-representation
    := make(<representation-frame>,
            interface-builder: frame,
            model: model,
            x: x, y: y,
            title: model-title(model),
            layout: if ($duim-version == 1) make(<column-layout>) end)
end method do-make-representation;

define method do-make-representation
    (frame :: <frame>, model :: <model-sheet>, #key add-to-parent? = #t)
 => (sheet :: <sheet>)
  with-frame-manager (frame-manager(frame))
    let info = model-class(model);
    let class = info.info-class;
    let arguments 
      = concatenate(model-arguments(model),
                    representation-extra-arguments(model, class));
    let parent-rep = find-parent-representation(model);
    let pane = apply(make, class, arguments);
    model.model-representation := pane;
    add-to-parent? & update-representation-parent(pane, parent-rep);
    pane
  end
end method do-make-representation;

define method representation-extra-arguments
    (model :: <model-object>, class :: <class>)
 => (arguments :: <sequence>)
  #[]
end method representation-extra-arguments;

//---*** This should really be on <labelled-gadget-mixin> but it
//---*** isn't exported. Should it be?
define method representation-extra-arguments
    (model :: <model-gadget>, class :: subclass(<gadget>))
 => (arguments :: <sequence>)
  vector(label: model.model-label | model.model-name | class.class-name)
end method representation-extra-arguments;

define method representation-extra-arguments
    (model :: <model-gadget>, class :: subclass(<collection-gadget>))
 => (arguments :: <sequence>)
  vector(items: range(from: 1, to: 5))
end method representation-extra-arguments;

define method representation-extra-arguments
    (model :: <model-gadget>, class :: subclass(<tree-control>))
 => (arguments :: <sequence>)
  vector(roots: #(1),
         children-generator: method (x :: <integer>)
                               vector(x * 2, 1 + (x * 2))
                             end)
end method representation-extra-arguments;

define method update-representation-parent 
    (sheet :: <sheet>, representation :: <representation-frame>) => ()
  select (sheet by instance?)
    <menu-bar>   => representation.frame-menu-bar   := sheet;
    <tool-bar>   => representation.frame-tool-bar   := sheet;
    <status-bar> => representation.frame-status-bar := sheet;
    // <layout> must be last, as some of the other classes are layouts
    <layout>     => representation.frame-layout     := sheet;
  end;
end method update-representation-parent;

define method update-representation-parent
    (sheet :: <sheet>, parent :: <sheet>) => ()
  sheet-parent(sheet) := parent;
  sheet-mapped?(sheet) := sheet-mapped?(parent)
end method update-representation-parent;

define method update-representation-parent
    (sheet :: <sheet>, parent :: <tab-control>) => ()
  tab-control-pages(parent)
    := concatenate(tab-control-pages(parent), vector(sheet));
  sheet-mapped?(sheet) := sheet-mapped?(parent)
end method update-representation-parent;

define method resize-representation
    (model :: <model-sheet>, representation :: <sheet>)
  relayout-parent(representation)
end method resize-representation;

define method resize-representation
    (model :: <model-sheet>, representation :: <menu-bar>)
  #f
end method resize-representation;

    
define method resize-representation
    (model :: <model-sheet>, representation :: <menu>)
  #f
end method resize-representation;

define method resize-representation
    (model :: <model-sheet>, representation :: <menu-box>)
  #f
end method resize-representation;

define method resize-representation
    (model :: <model-sheet>, representation :: <menu-button>)
  #f
end method resize-representation;

define method update-representation
    (frame :: <frame>, model :: <model-object>) => ()
  let model-frame = find-model-frame(model);
  let representation = model-frame.model-representation;
  if (representation)
    if (frame-needs-refreshing?(frame, model-frame))
      refresh-representation(frame, model-frame)
    else
      call-in-frame(representation, do-update-representation, frame, model)
    end
  else
    make-representation(frame, model)
  end
end method update-representation;

define method do-update-representation
    (frame :: <frame>, model :: <model-frame>) => ()
  let rep = model-representation(model);
  frame-title(rep) := model-title(model);
  let menu-bar   = ensure-representation(frame, model-menu-bar(model));
  let tool-bar   = ensure-representation(frame, model-tool-bar(model));
  let layout     = ensure-representation(frame, model-layout(model));
  let status-bar = ensure-representation(frame, model-status-bar(model));
  frame-menu-bar(rep)   := menu-bar;
  frame-tool-bar(rep)   := tool-bar;
  frame-layout(rep)     := layout;
  frame-status-bar(rep) := status-bar;
end method do-update-representation;

define method do-update-representation
    (frame :: <frame>, model :: <model-sheet>) => ()
  let representation = model-representation(model);
  if (representation)
    do-update-representation-2(frame, model, representation)
  else
    make-representation(frame, model)
  end;
  resize-representation(model, model-representation(model));
end method do-update-representation;

define method do-update-representation-2
    (frame :: <frame>, model :: <model-sheet>,
     representation :: <sheet>) => ()
  let representation = model-representation(model);
  let parent-rep = find-parent-representation(model);
  let new-representation
    = make-representation(frame, model, add-to-parent?: #f);
  select (parent-rep by instance?)
    <simple-frame> =>
      update-representation-parent(new-representation, parent-rep);
      relayout-parent(top-level-sheet(parent-rep));
      sheet-mapped?(new-representation) := frame-mapped?(parent-rep);
    <basic-composite-pane> =>
      replace-child(parent-rep, representation, new-representation);
      relayout-parent(parent-rep);
      sheet-mapped?(new-representation) := sheet-mapped?(parent-rep);
  end;
end method do-update-representation-2;

define method destroy-representation
    (model :: <model-object>) => ()
  model-representation(model) := #f;
  do(destroy-representation, model-children(model))
end method destroy-representation;

define method destroy-representation
    (model :: <model-frame>) => ()
  let representation = model.model-representation;
  if (representation & representation.frame-model == model)
    next-method();
    exit-frame(representation)
  end
end method destroy-representation;

define method refresh-representation
    (frame :: <frame>, model :: <model-frame>) => ()
  let old-representation = model.model-representation;
  let (x, y, width, height)
    = if (old-representation)
        let (x, y) = frame-position(old-representation);
        let (width, height) = frame-size(old-representation);
        values(x, y, width, height)
      end;
  destroy-representation(model);
  let representation = make-representation(frame, model);
  if (old-representation)
    set-frame-position(representation, x, y);
    set-frame-size(representation, width, height)
  end;
  make(<thread>,
       name: frame-title(representation),
       function: method ()
                   start-frame(representation)
                 end)
end method refresh-representation;

/// DUIM can't dynamically update tool bars etc (yet), so instead we
/// have to use the refreshing code.
define method frame-needs-refreshing?
    (frame :: <frame>, model :: <model-frame>)
 => (needs-refreshing? :: <boolean>)
  local method representation-needs-refreshing?
           (model :: false-or(<model-sheet>), representation) => ()
        if (model)
          ~representation | (representation ~= model.model-representation)
        else
          representation
        end
      end method representation-needs-refreshing?;
  let representation = model.model-representation;
  $duim-version == 1
    | representation-needs-refreshing?(model.model-menu-bar, representation.frame-menu-bar)
    | representation-needs-refreshing?(model.model-tool-bar, representation.frame-tool-bar)
    | representation-needs-refreshing?(model.model-layout, representation.frame-layout)
    | representation-needs-refreshing?(model.model-status-bar, representation.frame-status-bar)
end method frame-needs-refreshing?;


/// Class information

define method class-has-children? 
    (class :: <class>) => (has-children? :: <boolean>)
  subtype?(class, <layout>)
    | subtype?(class, <menu-bar>)
    | subtype?(class, <menu>)
    | subtype?(class, <tool-bar>)
    | subtype?(class, <status-bar>)
    | subtype?(class, <tab-control>)
end method class-has-children?;

