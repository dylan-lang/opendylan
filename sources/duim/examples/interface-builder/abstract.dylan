Module:    interface-builder
Author:    Andy Armstrong
Synopsis:  DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Model representations

define class <model-object> (<object>)
  slot model-representation = #f,
    init-keyword: representation:;
  slot model-name :: false-or(<string>),
    init-keyword: name:;
end class <model-object>;

/*---*** Probably not needed anymore
define method flatten-model-hierarchy (object :: <model-object>)
  let vector = vector(object);
  for (child in model-children(object))
    vector := concatenate(vector, flatten-model-hierarchy(child))
  end;
  vector
end method flatten-model-hierarchy;
*/

/// Model sheet handling

define class <model-sheet> (<model-object>)
  slot model-sheet-class :: <class>,
    required-init-keyword: class:;
  slot model-sheet-arguments :: <sequence> = #[],
    init-keyword: arguments:;
  slot model-sheet-parent :: false-or(<model-sheet>) = #f,
    init-keyword: parent:;
  slot model-children :: <stretchy-vector> = make(<stretchy-vector>),
    init-keyword: children:;
end class <model-sheet>;

define method find-model-frame
    (sheet :: <model-sheet>)
 => (frame :: false-or(<model-frame>))
  let parent = model-sheet-parent(sheet);
  if (parent)
    find-model-frame(parent)
  end
end method find-model-frame;

define method generate-next-name
    (sheet :: <model-sheet>, name :: <string>, #key remove-number?)
  generate-next-name(find-model-frame(sheet), name,
                     remove-number?: remove-number?)
end method generate-next-name;

define method add-model-child 
    (sheet :: <model-sheet>, class :: subclass(<sheet>))
 => (child :: <model-sheet>)
  let new-child 
    = make(<model-sheet>,
           class: class,
           parent: sheet,
           name: generate-next-name(sheet, "sheet"));
  add!(model-children(sheet), new-child);
  new-child
end method add-model-child;

define method add-model-child 
    (sheet :: <model-sheet>, class :: subclass(<gadget>))
 => (child :: <model-sheet>)
  let new-child 
    = make(<model-sheet>,
           class: class,
           parent: sheet,
           name: generate-next-name(sheet, "gadget"));
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
  slot model-frame-superclasses :: <sequence> = vector(<simple-frame>),
    init-keyword: superclasses:;
  slot model-frame-menu-bar :: false-or(<model-sheet>) = #f,
    init-keyword: menu-bar:;
  slot model-frame-tool-bar :: false-or(<model-sheet>) = #f,
    init-keyword: tool-bar:;
  slot model-frame-layout :: false-or(<model-sheet>) = #f,
    init-keyword: layout:;
  slot model-frame-status-bar :: false-or(<model-sheet>) = #f,
    init-keyword: status-bar:;
  slot model-frame-title :: <string> = "Untitled",
    init-keyword: title:;
  slot model-frame-name-generator = make(<name-generator>);
  slot model-frame-slots :: <sequence> = #(),
    init-keyword: slots:;
end class <model-frame>;

define method find-model-frame
    (frame :: <model-frame>) => (frame :: <model-frame>)
  frame
end method find-model-frame;

define method generate-next-name
    (frame :: <model-frame>, name :: <string>, #key remove-number?)
  generate-next-name(model-frame-name-generator(frame), name,
                     remove-number?: remove-number?)
end method generate-next-name;

define method add-model-child 
    (frame :: <model-frame>, class :: subclass(<menu-bar>))
  model-frame-menu-bar(frame)
    := make(<model-sheet>,
            class: class,
            parent: frame,
            name: "menu-bar");          
end method add-model-child;

define method add-model-child 
    (frame :: <model-frame>, class :: subclass(<tool-bar>))
  model-frame-tool-bar(frame)
    := make(<model-sheet>,
            class: class,
            parent: frame,
            name: "tool-bar");            
end method add-model-child;

define method add-model-child 
    (frame :: <model-frame>, class :: subclass(<layout-pane>))
  model-frame-layout(frame)
    := make(<model-sheet>,
            class: class,
            parent: frame,
            name: "layout");
end method add-model-child;

define method add-model-child 
    (frame :: <model-frame>, class :: subclass(<status-bar>))
  model-frame-status-bar(frame)
    := make(<model-sheet>,
            class: class,
            parent: frame,
            name: "status-bar");            
end method add-model-child;

define method remove-model-child 
    (frame :: <model-frame>, child :: <model-sheet>)
 => (child :: <model-sheet>)
  case
    child = model-frame-menu-bar(frame) =>
      model-frame-menu-bar(frame) := #f;
    child = model-frame-layout(frame) =>
      model-frame-layout(frame) := #f;
    child = model-frame-tool-bar(frame) =>
      model-frame-tool-bar(frame) := #f;
    child = model-frame-status-bar(frame) =>
      model-frame-menu-bar(frame) := #f;
  end;
  child
end method remove-model-child;

define method model-children (frame :: <model-frame>)
  let menu-bar   = model-frame-menu-bar(frame);
  let tool-bar   = model-frame-tool-bar(frame);
  let status-bar = model-frame-status-bar(frame);
  let layout     = model-frame-layout(frame);
  let vector     = make(<stretchy-vector>);
  if (menu-bar)   add!(vector, menu-bar)   end;
  if (tool-bar)   add!(vector, tool-bar)   end;
  if (layout)     add!(vector, layout)     end;
  if (status-bar) add!(vector, status-bar) end;
  vector
end method model-children;

