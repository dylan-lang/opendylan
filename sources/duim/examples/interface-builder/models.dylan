Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Model representations

define constant <representation> = type-union(<sheet>, <frame>);

define class <model-object> (<object>)
  slot model-representation :: false-or(<representation>) = #f,
    init-keyword: representation:;
  slot model-name :: false-or(<string>) = #f,
    init-keyword: name:;
end class <model-object>;

define method model-arguments
    (model :: <model-object>) => (arguments :: <sequence>)
  #[]
end method model-arguments;

/// Model sheet handling

define abstract class <model-sheet> (<model-object>)
  slot model-class :: <sheet-info>,
    required-init-keyword: class:;
  slot model-arguments :: <sequence> = #[],
    init-keyword: arguments:;
  slot model-parent :: false-or(<model-object>) = #f,
    init-keyword: parent:;
  slot model-children :: <stretchy-vector> = make(<stretchy-vector>),
    init-keyword: children:;
end class <model-sheet>;

define class <model-gadget> (<model-sheet>)
  slot model-label :: false-or(<string>) = #f,
    init-keyword: label:;
end class <model-gadget>;

define method model-arguments
    (model :: <model-gadget>) => (arguments :: <sequence>)
  let label = model.model-label;
  concatenate(next-method(),
              if (label) vector(label: label) else #[] end)
end method model-arguments;

define class <model-layout> (<model-sheet>)
end class <model-layout>;

define method find-model-frame
    (sheet :: <model-sheet>)
 => (frame :: false-or(<model-frame>))
  let parent = model-parent(sheet);
  if (parent)
    find-model-frame(parent)
  end
end method find-model-frame;

define method generate-next-name
    (sheet :: <model-sheet>, name :: <string>, #key remove-number?)
 => (name :: <string>)
  generate-next-name(find-model-frame(sheet), name,
                     remove-number?: remove-number?)
end method generate-next-name;

define method add-model-child 
    (sheet :: <model-sheet>, info :: <gadget-info>)
 => (child :: <model-gadget>)
  let new-child 
    = make(<model-gadget>,
           class: info,
           parent: sheet,
           name: generate-next-name(sheet, info.info-name));
  add!(model-children(sheet), new-child);
  new-child
end method add-model-child;

define method add-model-child 
    (sheet :: <model-sheet>, info :: <layout-info>)
 => (child :: <model-layout>)
  let new-child 
    = make(<model-layout>,
           class: info,
           parent: sheet);
  add!(model-children(sheet), new-child);
  new-child
end method add-model-child;

define method remove-model-child 
    (sheet :: <model-sheet>, child :: <model-sheet>)
 => (child :: <model-sheet>)
  remove!(model-children(sheet), child);
  child
end method remove-model-child;


/// Model frame handling

define class <model-frame> (<model-object>)
  slot model-filename :: false-or(<string>) = #f,
    init-keyword: filename:;
  slot model-module-name :: false-or(<string>) = "my-module",
    init-keyword: module-name:;
  slot model-superclasses :: <string> = "<simple-frame>",
    init-keyword: superclasses:;
  slot model-title :: <string> = "Untitled",
    init-keyword: title:;
  slot model-slots :: <sequence> = #(),
    init-keyword: slots:;
  slot model-menu-bar :: false-or(<model-sheet>) = #f,
    init-keyword: menu-bar:;
  slot model-tool-bar :: false-or(<model-sheet>) = #f,
    init-keyword: tool-bar:;
  slot model-layout :: false-or(<model-sheet>) = #f,
    init-keyword: layout:;
  slot model-status-bar :: false-or(<model-sheet>) = #f,
    init-keyword: status-bar:;
  slot model-name-generator = make(<name-generator>);
end class <model-frame>;

define method find-model-frame
    (frame :: <model-frame>) => (frame :: <model-frame>)
  frame
end method find-model-frame;

define method generate-next-name
    (frame :: <model-frame>, name :: <string>, #key remove-number?)
 => (name :: <string>)
  generate-next-name(model-name-generator(frame), name,
                     remove-number?: remove-number?)
end method generate-next-name;

define method add-model-child 
    (frame :: <model-frame>, info :: <gadget-info>)
 => (child :: <model-gadget>)
  let model = make(<model-gadget>, class: info, parent: frame);
  select (info.info-class)
    <menu-bar>   => frame.model-menu-bar := model;
    <tool-bar>   => frame.model-tool-bar := model;
    <status-bar> => frame.model-status-bar := model;
  end
end method add-model-child;

define method add-model-child 
    (frame :: <model-frame>, info :: <layout-info>)
 => (child :: <model-layout>)
  frame.model-layout := make(<model-layout>, class: info, parent: frame)
end method add-model-child;

define method remove-model-child 
    (frame :: <model-frame>, child :: <model-sheet>)
 => (child :: <model-sheet>)
  select (child)
    frame.model-menu-bar   => frame.model-menu-bar   := #f;
    frame.model-tool-bar   => frame.model-tool-bar   := #f;
    frame.model-layout     => frame.model-layout     := #f;
    frame.model-status-bar => frame.model-status-bar := #f;
  end;
  child
end method remove-model-child;

define method model-children
    (frame :: <model-frame>) => (children :: <sequence>)
  let vector = make(<stretchy-vector>);
  let menu-bar   = frame.model-menu-bar;
  let tool-bar   = frame.model-tool-bar;
  let status-bar = frame.model-status-bar;
  let layout     = frame.model-layout;
  menu-bar   & add!(vector, menu-bar);
  tool-bar   & add!(vector, tool-bar);
  layout     & add!(vector, layout);
  status-bar & add!(vector, status-bar);
  vector
end method model-children;

