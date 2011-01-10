Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <model-property-frame> (<dialog-frame>)
  slot dialog-model :: <model-object>,
    required-init-keyword: model:;
  pane dialog-name-field (frame)
    make(<text-field>,
         value: frame.dialog-model.model-name);
  pane dialog-label-field (frame)
    make(<text-field>,
         value: frame.dialog-model.model-label);
  pane dialog-general-page (frame)
    make(<table-layout>,
         columns: 2,
         x-alignment: #[#"right", #"left"],
         children: make-property-gadgets(frame));
  pane dialog-property-page (frame)
    make(<tab-control>,
         pages: vector(make(<tab-control-page>,
                            label: "General",
                            child: frame.dialog-general-page)));
  layout (frame)
    frame.dialog-property-page;
  keyword title: = "Properties";
end frame <model-property-frame>;

define method make-property-gadgets
    (dialog :: <model-property-frame>) => (children :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  let model = dialog.dialog-model;
  for (type :: <string> in model.model-property-types)
    add!(children, make-label-for-property-type(dialog, type));
    add!(children, make-gadget-for-property-type(dialog, type));
  end;
  children
end method make-property-gadgets;

define method model-property-types
    (model :: <model-object>) => (types :: <sequence>)
  #["Name"]
end method model-property-types;

define method model-property-types
    (model :: <model-frame>) => (types :: <sequence>)
  concatenate(next-method(), #["Module", "Superclasses", "Title"])
end method model-property-types;

define method model-property-types
    (model :: <model-gadget>) => (types :: <sequence>)
  concatenate(next-method(), #["Label"])
end method model-property-types;

define method make-label-for-property-type
    (dialog :: <model-property-frame>, type :: <string>)
 => (label :: <label>)
  make(<label>, label: format-to-string("%s:", type))
end method make-label-for-property-type;

define method make-gadget-for-property-type
    (dialog :: <model-property-frame>, type :: <string>)
 => (gadget :: <text-field>)
  let id = as(<symbol>, type);
  make(<text-field>, id: id)
end method make-gadget-for-property-type;

define method gadget-for-property-type
    (dialog :: <model-property-frame>, type :: <string>)
 => (gadget :: false-or(<gadget>))
  let children = dialog.dialog-general-page.sheet-children;
  let id = as(<symbol>, type);
  block (return)
    for (child in children)
      if (child.gadget-id == id)
        return(child)
      end
    end;
    #f
  end
end method gadget-for-property-type;

define method show-model-properties
    (model :: <model-object>, #key owner = error("Owner not supplied!"))
 => (updated? :: <boolean>)
  let dialog = make(<model-property-frame>, model: model, owner: owner);
  update-property-gadgets(dialog, model);
  if (start-frame(dialog))
    set-model-properties(dialog, model);
    #t
  end
end method show-model-properties;

define method update-property-gadgets
    (dialog :: <model-property-frame>, model :: <model-object>) => ()
  let gadget = gadget-for-property-type(dialog, "Name");
  gadget.gadget-value := model.model-name | "";
end method update-property-gadgets;

define method update-property-gadgets
    (dialog :: <model-property-frame>, model :: <model-frame>) => ()
  next-method();
  let gadget = gadget-for-property-type(dialog, "Module");
  gadget.gadget-value := model.model-module-name;
  let gadget = gadget-for-property-type(dialog, "Title");
  gadget.gadget-value := model.model-title;
  let gadget = gadget-for-property-type(dialog, "Superclasses");
  gadget.gadget-value := model.model-superclasses;
end method update-property-gadgets;

define method update-property-gadgets
    (dialog :: <model-property-frame>, model :: <model-gadget>) => ()
  next-method();
  let gadget = gadget-for-property-type(dialog, "Label");
  gadget.gadget-value := model.model-label | "";
end method update-property-gadgets;

define method set-model-properties
    (dialog :: <model-property-frame>, model :: <model-object>) => ()
  let name = gadget-for-property-type(dialog, "Name").gadget-value;
  model.model-name := ~empty?(name) & name;
end method set-model-properties;

define method set-model-properties
    (dialog :: <model-property-frame>, model :: <model-frame>) => ()
  next-method();
  let module = gadget-for-property-type(dialog, "Module").gadget-value;
  model.model-module-name := module;
  let title = gadget-for-property-type(dialog, "Title").gadget-value;
  model.model-title := title;
  let superclasses = gadget-for-property-type(dialog, "Superclasses").gadget-value;
  model.model-superclasses := superclasses;
end method set-model-properties;

define method set-model-properties
    (dialog :: <model-property-frame>, model :: <model-gadget>) => ()
  next-method();
  let label = gadget-for-property-type(dialog, "Label").gadget-value;
  model.model-label := ~empty?(label) & label;
end method set-model-properties;

