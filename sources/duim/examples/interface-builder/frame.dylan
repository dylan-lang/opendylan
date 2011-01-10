Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Interface builder frame handling

define frame <interface-builder> (<simple-frame>)
  slot frame-model-frame :: false-or(<model-frame>) = #f;
  pane hierarchy-pane (frame)
    make(<tree-control>,
	 label-key: method (model :: <model-object>)
                      let name = model.model-name;
                      if (name)
                        as(<string>, name)
                      else
                        model.model-class.info-name
                      end
                    end,
         children-predicate: method (model :: <model-object>)
                               ~empty?(model-children(model))
                             end,
	 children-generator: model-children,
         value-changed-callback: method (gadget :: <tree-control>)
                                   let value = gadget-value(gadget);
                                   frame-selected-model(frame) := value
                                 end,
         popup-menu-callback: method (gadget :: <tree-control>, #rest args)
                                apply(frame-popup-menu, frame, args)
                              end,
         always-show-selection?: #t,
	 width: 300, height: 400);
  pane status (frame)
    make(<status-bar>);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.hierarchy-pane
    end;
  layout (frame) frame.main-layout;
  command-table (frame) *frame-command-table*;
  status-bar (frame) frame.status;
end frame <interface-builder>;

define method initialize
    (frame :: <interface-builder>, #key) => ()
  next-method();
  update-interface-builder(frame);
  frame-selected-model(frame) := #f;
end method initialize;

define method frame-current-model 
    (frame :: <interface-builder>) 
 => (model :: false-or(<model-object>))
  gadget-value(hierarchy-pane(frame))
end method frame-current-model;

define method frame-popup-menu
    (frame :: <interface-builder>, target :: false-or(<model-object>),
     #key x :: <integer>, y :: <integer>)
 => ()
  let framem = frame-manager(frame);
  let command-table = *model-command-table*;
  let menu 
    = make-menu-from-command-table-menu
        (command-table-menu(command-table), frame, framem,
         command-table: command-table,
         owner: frame);
  display-menu(menu, x: x, y: y)
end method frame-popup-menu;

define method frame-add-sheet 
    (frame :: <interface-builder>) => ()
  let parent = frame-current-model(frame);
  let classes = sheet-potential-child-classes(parent);
  if (~empty?(classes))
    let info
      = choose-from-dialog(classes,
			   label-key: info-title,
			   owner: frame,
			   title: "Select a class of sheet to add:");
    if (info)
      let new-child = add-model-child(parent, info);
      update-interface-builder(frame);
      if (class-has-children?(new-child.model-class))
        frame-selected-model(frame) := new-child
      end;
      if (instance?(parent, <model-frame>)
            | class-has-children?(parent.model-class))
        frame-ensure-node-expanded(frame, parent)
      end;
      update-representation(frame, new-child);
    end
  else
    beep(frame)
  end
end method frame-add-sheet;

define method frame-remove-sheet 
    (frame :: <interface-builder>) => ()
  let model = frame-current-model(frame);
  select (model by instance?)
    <model-frame> =>
      destroy-representation(model);
      frame-model-frame(frame) := #f;
      update-interface-builder(frame);
      frame-selected-model(frame) := #f;
    <model-sheet> =>
      let parent = model-parent(model);
      remove-model-child(parent, model);
      update-interface-builder(frame);
      frame-selected-model(frame) := parent;
      update-representation(frame, parent);
    otherwise =>
      beep(frame);
  end
end method frame-remove-sheet;

define method frame-selected-model 
    (frame :: <interface-builder>)
 => (model :: false-or(<model-object>))
  let gadget = hierarchy-pane(frame);
  gadget-value(gadget)
end method frame-selected-model;

define method frame-selected-model-setter
    (model :: false-or(<model-object>), frame :: <interface-builder>)
 => (model :: false-or(<model-object>))
  let gadget = hierarchy-pane(frame);
  let selection? = model ~== #f;
  let can-add? = model & ~empty?(sheet-potential-child-classes(model));
  command-enabled?(frame-add-sheet, frame) := can-add?;
  command-enabled?(frame-remove-sheet, frame) := selection?;
  command-enabled?(frame-sheet-properties, frame) := selection?;
  gadget-value(gadget) := model
end method frame-selected-model-setter;

define method frame-ensure-node-expanded
    (frame :: <interface-builder>, model :: <model-object>) => ()
  let gadget = frame.hierarchy-pane;
  let node = find-node(gadget, model);
  node & expand-node(gadget, node)
end method frame-ensure-node-expanded;

define method update-interface-builder 
    (frame :: <interface-builder>) => ()
  let tree-control = hierarchy-pane(frame);
  let model-frame = frame-model-frame(frame);
  let roots = tree-control-roots(tree-control);
  let old-child = ~empty?(roots) & roots[0];
  let frame? = model-frame ~== #f;
  command-enabled?(frame-save-file, frame) := frame?;
  command-enabled?(frame-save-as-file, frame) := frame?;
  command-enabled?(frame-refresh-representation, frame) := frame?;
  if (old-child = model-frame)
    update-gadget(tree-control)
  else
    tree-control-roots(tree-control)
      := if (frame?) vector(model-frame) else #[] end;
    gadget-value(tree-control) := model-frame
  end
end method update-interface-builder;

define method frame-new-frame 
    (frame :: <interface-builder>) => ()
  let old-model = frame.frame-model-frame;
  old-model & destroy-representation(old-model);
  let model-frame 
    = make(<model-frame>, name: "my-frame", title: "My Frame");
  frame-model-frame(frame) := model-frame;
  refresh-representation(frame, model-frame);
  update-interface-builder(frame);
  frame-selected-model(frame) := model-frame;
end method frame-new-frame;

define method frame-refresh-representation
    (frame :: <interface-builder>) => ()
  let model = frame.frame-model-frame;
  refresh-representation(frame, model)
end method frame-refresh-representation;


/// File saving

define method frame-save-file
    (frame :: <interface-builder>) => ()
  let model = frame.frame-model-frame;
  frame-save-as-file(frame, filename: model.model-filename)
end method frame-save-file;

define method frame-open-file
    (frame :: <interface-builder>) => ()
  notify-user("Not yet implemented!", owner: frame)
end method frame-open-file;

define method frame-save-as-file
    (frame :: <interface-builder>, #key filename) => ()
  let filename
    = filename 
        | choose-file(owner: frame,
                      title: "Choose a filename for your code",
                      direction: #"output");
  if (filename)
    let model = frame.frame-model-frame;
    model.model-filename := filename;
    save-model(frame.frame-model-frame, #"duim")
  end
end method frame-save-as-file;


/// Properties

define method frame-sheet-properties
    (frame :: <interface-builder>) => ()
  let model = frame.frame-selected-model;
  if (show-model-properties(model, owner: frame))
    frame-refresh-representation(frame);
    update-interface-builder(frame)
  end
end method frame-sheet-properties;


/// Start it up
define method start-interface-builder () => ()
  let frame = make(<interface-builder>, title: "Interface Builder");
  start-frame(frame)
end method start-interface-builder;
