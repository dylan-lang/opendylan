Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $gadget-classes = make(<table>);
define constant $layout-classes = make(<table>);

define class <sheet-info> (<object>)
  slot info-class :: <class>,
    required-init-keyword: class:;
  slot info-name :: <string>,
    required-init-keyword: name:;
  slot info-title :: <string>,
    required-init-keyword: title:;
end class <sheet-info>;

define class <gadget-info> (<sheet-info>)
end class <gadget-info>;

define class <menu-gadget-info> (<gadget-info>)
end class <menu-gadget-info>;

define class <layout-info> (<sheet-info>)
end class <layout-info>;

define function class-name
    (class :: <class>) => (name :: <string>)
  let info
    = element($gadget-classes, class, default: #f)
        | element($layout-classes, class, default: #f);
  assert(info, "Class info not found for %=", class);
  info.info-name
end function class-name;

define method class-has-children?
    (class :: <layout-info>) => (children? :: <boolean>)
  #t
end method class-has-children?;

define method class-has-children?
    (class :: <gadget-info>) => (children? :: <boolean>)
  #t
end method class-has-children?;

define function sheet-classes
    () => (classes :: <sequence>)
  local method info<
            (info1 :: <sheet-info>, info2 :: <sheet-info>)
          info1.info-title < info2.info-title
        end method info<;
  let layout-classes = sort(as(<vector>, $layout-classes), test: info<);
  let gadget-classes = sort(as(<vector>, $gadget-classes), test: info<);
  concatenate-as(<vector>, layout-classes, gadget-classes)
end function sheet-classes;

define constant $frame-gadget-children
    = vector(<menu-bar>, <tool-bar>, <status-bar>);

define constant $gadgets-with-children
    = vector(<tool-bar>, <status-bar>, <tab-control>);

define method sheet-potential-child-classes
    (model :: <model-frame>) => (classes :: <sequence>)
  let classes = make(<stretchy-vector>);
  for (info in sheet-classes())
    let class = info.info-class;
    unless (instance?(info, <gadget-info>)
              & ~member?(class, $frame-gadget-children))
      add!(classes, info)
    end
  end;
  classes
end method sheet-potential-child-classes;

define method sheet-potential-child-classes
    (model :: <model-sheet>) => (classes :: <sequence>)
  let parent-info = model.model-class;
  sheet-potential-child-classes(parent-info)
end method sheet-potential-child-classes;

define method sheet-potential-child-classes
    (info :: <layout-info>) => (classes :: <sequence>)
  let classes = make(<stretchy-vector>);
  for (info in sheet-classes())
    let class = info.info-class;
    unless (instance?(info, <menu-gadget-info>))
      add!(classes, info)
    end
  end;
  classes
end method sheet-potential-child-classes;

define method sheet-potential-child-classes
    (info :: <gadget-info>) => (classes :: <sequence>)
  if (member?(info.info-class, $gadgets-with-children))
    let classes = make(<stretchy-vector>);
    for (info in sheet-classes())
      let class = info.info-class;
      unless (instance?(info, <menu-gadget-info>))
        add!(classes, info)
      end
    end;
    classes
  else
    #[]
  end
end method sheet-potential-child-classes;

define method sheet-potential-child-classes
    (info :: <menu-gadget-info>) => (classes :: <sequence>)
  if (subtype?(info.info-class, <menu-button>))
    #[]
  else
    let classes = make(<stretchy-vector>);
    for (info in sheet-classes())
      let class = info.info-class;
      if (instance?(info, <menu-gadget-info>))
        add!(classes, info)
      end
    end;
    classes
  end
end method sheet-potential-child-classes;

define macro gadget-definer
  { define gadget ?name:name = ?title:expression }
 => { begin
        let class = "<" ## ?name ## ">";
        let info
          = make(<gadget-info>,
                 class: class,
                 name: ?"name",
                 title: ?title);
        $gadget-classes[class] := info
      end }
end macro gadget-definer;

define macro menu-gadget-definer
  { define menu-gadget ?name:name = ?title:expression }
 => { begin
        let class = "<" ## ?name ## ">";
        let info
          = make(<menu-gadget-info>,
                 class: class,
                 name: ?"name",
                 title: ?title);
        $gadget-classes[class] := info
      end }
end macro menu-gadget-definer;

define macro layout-definer
  { define layout ?name:name = ?title:expression }
 => { begin
        let class = "<" ## ?name ## ">";
        let info
          = make(<layout-info>,
                 class: class,
                 name: ?"name",
                 title: ?title);
        $layout-classes[class] := info
      end }
end macro layout-definer;

define menu-gadget menu-bar = "Menu Bar";
define menu-gadget menu = "Menu";
define menu-gadget push-menu-box = "Push Menu Box";
define menu-gadget radio-menu-box = "Radio Menu Box";
define menu-gadget check-menu-box = "Check Menu Box";
define menu-gadget push-menu-button = "Push Menu Button";
define menu-gadget radio-menu-button = "Radio Menu Button";
define menu-gadget check-menu-button = "Check Menu Button";

define gadget tool-bar = "Tool Bar";
define gadget status-bar = "Status Bar";

define gadget label = "Label";
define gadget push-button = "Push Button";
define gadget radio-button = "Radio Button";
define gadget check-button = "Check Button";
define gadget password-field = "Password Field";
define gadget text-field = "Text Field";
define gadget text-editor = "Text Editor";

define gadget push-box = "Push Box";
define gadget radio-box = "Radio Box";
define gadget check-box = "Check Box";
define gadget list-box = "List Box";
define gadget option-box = "Option Box";
define gadget spin-box = "Spin Box";
define gadget list-control = "List Control";
define gadget table-control = "Table Control";
define gadget tree-control = "Tree Control";

define gadget scroll-bar = "Scroll Bar";
define gadget slider = "Slider";
define gadget progress-bar = "Progress Bar";

define gadget tab-control = "Tab Control";
define gadget tab-control-page = "Tab Control Page";

define layout column-layout = "Column Layout";
define layout row-layout = "Row Layout";
define layout table-layout = "Table Layout";
define layout pinboard-layout = "Pinboard Layout";
define layout stack-layout = "Stack Layout";
