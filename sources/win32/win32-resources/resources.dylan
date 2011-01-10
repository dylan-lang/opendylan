Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Resource classes

define sealed class <resource-wrapper>
    (<resource>)
  slot %resource :: <resource>,
    required-init-keyword: resource:;
end class <resource-wrapper>;

define sealed domain make (singleton(<resource-wrapper>));
define sealed domain initialize (<resource-wrapper>);


define abstract class <win32-resource>
    (<resource>)
  slot resource-handle :: <HANDLE>,
    init-keyword: resource-handle:;
  slot resource-id :: <raw-resource-id>,
    init-keyword: resource-id:;
  slot resource-size :: <integer>,
    init-keyword: resource-size:;
end class <win32-resource>;

define method describe-resource
    (resource :: <win32-resource>) => ()
  print-resource-id(decode-resource(resource-id(resource)), "");
  format-out("\tsize = %d\n", resource-size(resource))
end method describe-resource;


// Stand-in for a loaded resource so we can load it lazily...
define sealed class <resource-description>
    (<win32-resource>) 
  constant slot resource-type-value :: <resource-type>, 
    required-init-keyword: resource-type:;
end class <resource-description>;

define sealed domain make (singleton(<resource-description>));
define sealed domain initialize (<resource-description>);

define method resource-type
    (resource :: <resource-description>) => (resource-type :: <resource-type>)
  resource-type-value(resource)
end method resource-type;


// A resource once it as been loaded into memory
define abstract class <loaded-resource>
    (<win32-resource>) 
  constant slot memory-handle :: <HANDLE>,
    init-keyword: memory-handle:;
end class <loaded-resource>;

define method initialize
    (resource :: <loaded-resource>, #key resource-description)
  next-method();
  resource-handle(resource) := resource-handle(resource-description);
  resource-id(resource)     := resource-id(resource-description);
  resource-size(resource)   := resource-size(resource-description);
end method initialize;

//---*** So who should call this?
define generic unload-resource
    (resource :: <resource>) => ();

define method unload-resource
    (resource :: <resource>) => ()
  #f
end method unload-resource;


define abstract class <pseudo-resource>
    (<win32-resource>)
end class <pseudo-resource>;


define sealed class <bitmap-resource>
    (<loaded-resource>)
end class <bitmap-resource>;

define sealed domain make (singleton(<bitmap-resource>));
define sealed domain initialize (<bitmap-resource>);


define sealed class <icon-resource>
    (<loaded-resource>)
end class <icon-resource>;

define sealed domain make (singleton(<icon-resource>));
define sealed domain initialize (<icon-resource>);


define sealed class <cursor-resource>
    (<loaded-resource>)
end class <cursor-resource>;

define sealed domain make (singleton(<cursor-resource>));
define sealed domain initialize (<cursor-resource>);


/// Dialog resources

define sealed class <dialog-resource>
    (<loaded-resource>, <top-window-resource>)
  constant slot dialog-template :: <LPCDLGTEMPLATEA>,
    required-init-keyword: template:;
  slot dialog-menu :: <raw-resource-id>;
  slot dialog-title :: <raw-resource-id>;
  slot dialog-font-name :: <raw-resource-id> = encode-resource(0);
  slot dialog-font-size :: <integer> = 0;
  constant slot dialog-children = make(<resource-table>);
end class <dialog-resource>;

define sealed domain make (singleton(<dialog-resource>));
define sealed domain initialize (<dialog-resource>);


define sealed method window-position
    (dialog :: <dialog-resource>) => (x :: <integer>, y :: <integer>)
  values(dialog-template(dialog).x-value, dialog-template(dialog).y-value)
end method window-position;

define sealed method window-size
    (dialog :: <dialog-resource>) => (w :: <integer>, h :: <integer>)
  values(dialog-template(dialog).cx-value, dialog-template(dialog).cy-value)
end method window-size;

define sealed method gadget-count
    (dialog :: <dialog-resource>) => (n :: <integer>)
  dialog-template(dialog).cdit-value
end method gadget-count;


define generic register-child
    (dialog :: <dialog-resource>, child :: <window-resource>, id :: <resource-id>) => ();

define sealed method register-child
    (dialog :: <dialog-resource>, child :: <control-resource>, id :: <unsigned-int>) => ()
  control-parent(child) := dialog;
  dialog-children(dialog)[encode-resource(id)] := child;
end method register-child;

define sealed method register-child
    (dialog :: <dialog-resource>, child :: <control-resource>, id :: <byte-string>) => ()
  control-parent(child) := dialog;
  dialog-children(dialog)[encode-resource(id)] := child
end method register-child;


define sealed method describe-resource
    (resource :: <dialog-resource>) => ()
  format-out("Dialog resource: \n");
  next-method();
  format-out("Number of gadgets: %d\n", gadget-count(resource));
  format-out("Number of accessible gadgets: %d\n\n", size(dialog-children(resource)))
end method describe-resource;


/// Control resources

define sealed class <control-resource> 
    (<pseudo-resource>, <window-resource>)
  constant slot control-template :: <LPCDLGITEMTEMPLATEA>, 
    required-init-keyword: template:;
  slot control-text :: <raw-resource-id>;
  slot control-creation-data-size :: <integer>;
  slot control-creation-data :: <LPWORD>; 
  // The dialog that contains this control...
  slot control-parent :: false-or(<dialog-resource>) = #f;
end class <control-resource>;

define method initialize
    (resource :: <control-resource>, #key)
  next-method();
  resource-id(resource) := encode-resource(control-template(resource).id-value)
end method initialize;

define sealed domain make (singleton(<control-resource>));
define sealed domain initialize (<control-resource>);


define sealed method window-position
    (control :: <control-resource>) => (x :: <integer>, y :: <integer>)
  values(control-template(control).x-value, control-template(control).y-value)
end method window-position;

define sealed method window-size
    (control :: <control-resource>) => (w :: <integer>, h :: <integer>)
  values(control-template(control).cx-value, control-template(control).cy-value)
end method window-size;


define sealed method get-resource-id
    (control :: <control-resource>) => (id :: <resource-id>)
  control-template(control).id-value
end method get-resource-id;


define method control-parent
    (resource :: <resource>) => (parent :: false-or(<dialog-resource>))
  #f
end method control-parent;


define method describe-resource
    (r :: <control-resource>) => ()
  format-out("Control resource: \n");
  next-method()
end method describe-resource;


/// Toolbar resources

define sealed class <toolbar-resource>
    (<loaded-resource>, <window-resource>)
end class <toolbar-resource>;

define sealed domain make (singleton(<toolbar-resource>));
define sealed domain initialize (<toolbar-resource>);
