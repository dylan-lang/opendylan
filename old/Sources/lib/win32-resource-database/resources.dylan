Module:    win32-resource-database-internal 
Synopsis:  resource classes
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <resource-wrapper> (<resource>)
  slot the-resource :: <resource>, required-init-keyword: the-resource:;
end;

define abstract class <win32-resource> (<resource>)
  slot resource-handle :: <HANDLE>, init-keyword: resource-handle:;
  slot resource-id :: <raw-resource-id>, init-keyword: resource-id:;
  slot resource-size :: <integer>, init-keyword: resource-size:;
end;

define method describe-resource(r :: <win32-resource>) => ();
  print-resource-id(decode-resource(r.resource-id), "");
  debug-out("\tsize = %d\n", r.resource-size);
end;

define class <resource-description> (<win32-resource>) 
  slot resource-type-value :: <resource-type>, 
    required-init-keyword: resource-type:;
end;

define method resource-type(r :: <resource-description>)
 => (resource-type :: <resource-type>);
  r.resource-type-value
end;

define abstract class <loaded-resource> (<win32-resource>) 
  slot memory-handle, init-keyword: memory-handle:;
end;

define method initialize(resource :: <loaded-resource>, 
			 #key resource-description,
			 #all-keys)
 => (r :: <loaded-resource>);
  next-method();
  debug-out("Initializing loaded resource\n");
  resource.resource-handle := resource-description.resource-handle;
  resource.resource-id := resource-description.resource-id;
  resource.resource-size := resource-description.resource-size;
  resource;
end;

define abstract class <pseudo-resource> (<win32-resource>) end;

define class <simple-resource> (<loaded-resource>, <win32-resource>) end;

define class <dialog-resource> (<loaded-resource>, <top-window-resource>)
  slot dialog-template :: <LPCDLGTEMPLATEA>, required-init-keyword: template:;
  slot dialog-menu :: <raw-resource-id>;
  slot dialog-title :: <raw-resource-id>;
  slot dialog-font-size :: <integer> = 0;
  slot dialog-font :: <raw-resource-id> = encode-resource(0);
  slot dialog-children = make(<resource-table>);
end;

define method describe-resource(r :: <dialog-resource>) => ();
  debug-out("Dialog resource: \n");
  next-method();
  debug-out("Number of gadgets: %d\n", number-of-gadgets(r));
  debug-out("Number of accessible gadgets: %d\n\n", size(r.dialog-children));
end;

define generic register-child(dialog :: <dialog-resource>, 
			      child :: <window-resource>,
			      id :: <resource-id>) => ();

define method register-child(dialog :: <dialog-resource>,
			     child :: <control-resource>,
			     id :: <integer>) => ();
  dialog.dialog-children[encode-resource(id)] := child;
end;

define method register-child(dialog :: <dialog-resource>,
			     child :: <control-resource>,
			     id :: <byte-string>) => ();
  dialog.dialog-children[encode-resource(id)] := child;
end;

define method number-of-gadgets(dialog :: <dialog-resource>) 
 => (number :: <integer>);
  dialog.dialog-template.cdit-value;
end;

define method window-position(dialog :: <dialog-resource>)
 => (x :: <integer>, y :: <integer>);
  values(dialog.dialog-template.x-value, dialog.dialog-template.y-value);
end;

define method window-size(dialog :: <dialog-resource>)
 => (width :: <integer>, height :: <integer>);
  values(dialog.dialog-template.cx-value, dialog.dialog-template.cy-value); 
end;

// this is a pseudo resource
// not sure if we need to keep this info around
define class <control-resource> (<pseudo-resource>, <window-resource>)
  slot control-template :: <LPCDLGITEMTEMPLATEA>, 
    required-init-keyword: template:;
  slot control-text :: <raw-resource-id>;
  slot creation-data-size :: <integer>;
  slot creation-data :: <LPWORD>; 
end;

define method initialize(resource :: <control-resource>, 
			 #key, #all-keys)
 => (control-resource :: <control-resource>);
  next-method();
  resource.resource-id := encode-resource(resource.control-template.id-value);
  resource;
end;

define method describe-resource(r :: <control-resource>) => ();
  debug-out("Control resource: \n");
  next-method();
end;

define method window-position(control :: <control-resource>)
 => (x :: <integer>, y :: <integer>);
  values(control.control-template.x-value, control.control-template.y-value);
end;

define method window-size(control :: <control-resource>)
 => (width :: <integer>, height :: <integer>);
  values(control.control-template.cx-value, control.control-template.cy-value); 
end;

define method get-resource-id(control :: <control-resource>)
 => (rid :: <resource-id>);
  control.control-template.id-value;
end;


define class <toolbar-resource> (<loaded-resource>, <window-resource>)
end class <toolbar-resource>;

