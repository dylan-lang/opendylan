Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dlg-template-size  :: <integer> = 18;
define constant $item-template-size :: <integer> = 18;

define function grok-dialog
    (resource :: <resource-description>, handle :: <HANDLE>)
 => (dialog :: <dialog-resource>)
  let resource-address
    = pointer-address(LoadResource(handle, resource-handle(resource)));
  let template :: <LPCDLGTEMPLATEA>
    = make(<LPCDLGTEMPLATEA>, address: resource-address);
  let dialog = make(<dialog-resource>, 
		    resource-description: resource,
		    template: template);
  let template-size :: <integer> = 0;
  unless (null-pointer?(template))
    debug-message("Dialog %s", 
		  print-resource-id-to-string(resource-id(resource), "Dialog"));
    debug-message("    x= %d y= %d, width= %d height= %d",
		  template.x-value, template.y-value,
		  template.cx-value, template.cy-value);
    debug-message("    Number of controls= %d", template.cdit-value);
    let offset :: <integer> = $dlg-template-size;
    let menu-test-pointer :: <LPWORD>
      = make(<LPWORD>, address: \%+(resource-address, offset));
    let (menu-resource-id, menu-resource-id-size)
      = grok-resource-id(menu-test-pointer);
    debug-message("    Menu id %d, size %d", menu-resource-id, menu-resource-id-size);
    offset := offset + menu-resource-id-size;
    dialog-menu(dialog) := encode-resource(menu-resource-id);
    debug-message("%s", print-resource-id-to-string(menu-resource-id, "Menu"));
    let class-test-pointer :: <LPWORD>
      = make(<LPWORD>, address: \%+(resource-address, offset));
    let (class-resource-id, class-resource-id-size)
      = grok-resource-id(class-test-pointer);
    offset := offset + class-resource-id-size;
    window-class(dialog) := encode-resource(class-resource-id);
    let title-string-pointer :: <C-void*>
      = make(<C-void*>, address: \%+(resource-address, offset));
    let (title-string, title-size)
      = grok-resource-string(title-string-pointer);
    offset := offset + title-size;
    dialog-title(dialog) := encode-resource(title-string);
    let font-resource-size :: <integer> = 0;
    when (logand(template.style-value, $DS-SETFONT) ~= 0)
      let font-size-pointer :: <LPWORD>
	= make(<LPWORD>, address: \%+(resource-address, offset));
      offset := offset + size-of(<WORD>);
      dialog-font-size(dialog) := pointer-value(font-size-pointer);
      debug-message("    Font size %d", pointer-value(font-size-pointer));
      let font-name-pointer :: <C-pointer>
	= pointer+(font-size-pointer, 1);
      let (font-name, font-name-size)
	= grok-resource-string(font-name-pointer);
      dialog-font-name(dialog) := encode-resource(font-name);
      debug-message("    Font name %s", as(<byte-string>, font-name));
      offset := offset + font-name-size;
    end;
    let addr = \%+(resource-address, offset);
    for (i :: <integer> from 1 below template.cdit-value,
	 item = make(<LPCDLGITEMTEMPLATEA>, address: pointer-address-32(addr))
	   then make(<LPCDLGITEMTEMPLATEA>, address: pointer-address-32(addr)))
      let (item-size, control-resource)
	= grok-item-template(item);
      register-child(dialog, control-resource, get-resource-id(control-resource));
      addr := \%+(pointer-address(item), item-size);
    finally
      let (item-size, control-resource)
	= grok-item-template(item);
      register-child(dialog, control-resource, get-resource-id(control-resource));
      addr := \%+(pointer-address(item), item-size);
      template-size := as(<integer>, \%-(addr, resource-address));
      debug-message("    Dialog resource size %d, expected size %d\n",
		    template-size, resource-size(dialog))
    end
  end;
  assert(resource-size(dialog) = template-size,
	 "Incorrect dialog resource size retrieved");
  dialog
end function grok-dialog;

*grok-resource-table*[$RT-DIALOG] := grok-dialog;

define method grok-item-template
    (template :: <LPCDLGITEMTEMPLATEA>)
 => (resource-size :: <integer>, resource :: <control-resource>)
  debug-message("Control id %d", template.id-value);
  debug-message("    x= %d y= %d, width= %d height= %d",
		template.x-value, template.y-value,
		template.cx-value, template.cy-value);
  let control = make(<control-resource>,
		     template: template);
  let class-test-pointer :: <LPWORD>
    = make(<LPWORD>, address: \%+(pointer-address(template), $item-template-size));
  let (class-resource-id, class-resource-id-size)
    = grok-resource-id(class-test-pointer);
  window-class(control) := encode-resource(class-resource-id);
  debug-message("    Class id size %d", class-resource-id-size);
  debug-message("    %s", print-resource-id-to-string(class-resource-id, "Control Class"));
  let text-address
    = \%+(pointer-address(class-test-pointer), class-resource-id-size);
  let text-resource-pointer :: <LPWORD>
    = make(<LPWORD>, address: pointer-address-16(text-address));
  let (text-resource, text-size)
    = grok-resource-id(text-resource-pointer);
  control-text(control) := encode-resource(text-resource);
  debug-message("    %s", print-resource-id-to-string(text-resource, "Control's Text"));
  let creation-data-pointer
    = make(<LPWORD>, address: \%+(text-address, text-size));
  let data-size = grok-creation-data(creation-data-pointer);
  debug-message("    Creation data size %d", data-size);
  let template-size
    = as(<integer>,		// skip overflow check
	 \%-(\%+(pointer-address(creation-data-pointer), data-size),
	     pointer-address(template)));
  control-creation-data-size(control) := data-size;
  // What to do with creation data?  The only useful thing is to keep
  // it around in C format to be used for explicit window creation?
  // Unless we have our own custom controls...
  control-creation-data(control) := creation-data-pointer;
  debug-message("    Template size %d\n", template-size);
  resource-size(control) := template-size;
  values(template-size, control)
end method grok-item-template;

define method grok-resource-id
    (p :: <LPWORD>) 
 => (id :: type-union(<unsigned-int>, <string>), resource-id-size :: <integer>)
  let word-pointer = as(<WORD*>, p);
  let word-value   = pointer-value(word-pointer);
  case
    word-value = #x0000 =>
      debug-message("    ID %d, size %d", 0, size-of(<WORD>));
      values(0, size-of(<WORD>));
    word-value = #xFFFF =>
      let id = pointer-value(word-pointer, index: 1);
      let sz = 2 * size-of(<WORD>);
      debug-message("    ID %d, size %d", id, sz);
      values(id, sz);
    otherwise =>
      grok-resource-string(p);
  end
end method grok-resource-id;

define method grok-resource-string
    (p :: <C-pointer>)
 => (string :: <string>, resource-size :: <integer>)
  let resource-string :: <C-unicode-string>
    = make(<C-unicode-string>, address: pointer-address(p));
  //---*** (size(resource-string) + 1) * size-of(referenced-type(resource-string));
  let resource-size :: <integer> = (size(resource-string) + 1) * 2;
  debug-message("    String %s, size %d", as(<byte-string>, resource-string), resource-size);
  values(resource-string, resource-size)
end method grok-resource-string;

define method grok-creation-data
    (p :: <LPWORD>)
 => (data-size :: <integer>)
  let data-value :: <integer> = as(<integer>, pointer-value(p));
  if (data-value = 0)
    size-of(<WORD>)
  else
    size-of(<WORD>) + data-value
  end
end method grok-creation-data;
