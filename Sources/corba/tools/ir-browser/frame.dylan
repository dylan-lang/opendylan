Module:    ir-browser
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *interface-repository* :: corba/<repository>
  = make-nil(corba/<repository>);

define frame <ir-browser-frame> (<simple-frame>)

  pane info-pane (frame)
    make(<table-control>,
         items: #(),
         headings: #["Attribute", "Value"],
         generators: vector(first, second));

  pane main-pane (frame)
    make(<tree-control>,
         value-changed-callback: update-info-table,
         children-generator: generate-children,
         children-predicate: method (node) ~empty?(generate-children(node)) end,
         label-key: irobject-name,
         value-key: identity,
         width: 200, height: 100);

  layout (frame)
    horizontally ()
      frame.main-pane;
      frame.info-pane;
    end;

//  command-table (frame)
//    *template-command-table*;

//   tool-bar (frame)
//     make-command-tool-bar(frame-manager(frame), frame);

  status-bar (frame)
    make(<status-bar>);

  keyword title: = $application-name;
end frame;

define method initialize (frame :: <ir-browser-frame>, #key)
  next-method();
  let orb = corba/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  let repository = as(corba/<repository>, corba/orb/resolve-initial-references(orb, "InterfaceRepository"));
  *interface-repository* := repository;
  unless (corba/object/is-nil(*interface-repository*))
    let tree = frame.main-pane;
    tree.tree-control-roots := vector(*interface-repository*);
  end unless;
end method;

define method do-refresh-ir-browser-frame (frame :: <ir-browser-frame>)
 => ()
  let tree-control = frame.main-pane;
  let irobject = gadget-value(tree-control);
  let items = info-table-items(irobject);
  let list-box = frame.info-pane;
  if (gadget-items(list-box) == items)
    update-gadget(list-box);
  else
    gadget-items(list-box) := items;
  end if;
end method;

define method update-info-table (gadget)
 => ()
  let frame = gadget.sheet-frame;
  do-refresh-ir-browser-frame(frame);
end method;

define method generate-children (object :: <object>)
 => (children :: <sequence>)
  #[];
end method;

define method generate-children (object :: corba/<container>)
 => (children :: <sequence>)
  corba/container/contents(object, #"dk-all", #t);
end method;

define method generate-children (object :: type-union(corba/<structdef>, corba/<uniondef>, corba/<exceptiondef>))
 => (children :: <sequence>)
  #[]
end method;

define method irobject-name (object :: corba/<irobject>)
 => (name :: <string>)
  "unknown";
end method;

define method irobject-name (object :: corba/<contained>)
 => (name :: <string>)
  object.corba/contained/name;
end method;

define method irobject-name (object :: corba/<repository>)
 => (name :: <string>)
  "Root";
end method;

define method start-ir-browser () => ()
  start-frame(make(<ir-browser-frame>));
end method;

