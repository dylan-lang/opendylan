Module:    win32-resource-database-internal 
Synopsis:  grok the dialog resource in memory
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant dialog-debug-out = 
    method(#rest args) 
	apply(debug-out-internal, args)
    end;

define constant $dlg-template-size = 18;
define constant $item-template-size = 18;

*grok-resource-table*[$RT-DIALOG] := grok-dialog;

define function grok-dialog(r :: <resource-description>, m :: <HANDLE>)
 => (resource :: <dialog-resource>);

  let resource-address = LoadResource(m,r.resource-handle).pointer-address;
  let offset = 0;
  let template :: <LPCDLGTEMPLATEA> = 
    make(<LPCDLGTEMPLATEA>, address: resource-address);
  let new-dialog = make(<dialog-resource>, 
			resource-description: r,
			template: template);

  let template-size = 0;

  if(~null-pointer?(template))
    dialog-debug-out("x= %d y= %d width= %d height= %d\n",
	       template.x-value, template.y-value,
	       template.cx-value, template.cy-value);
    dialog-debug-out("no controls= %d\n", template.cdit-value);

    offset := offset + $dlg-template-size; 
    let menu-test-pointer :: <LPWORD> = 
      make(<LPWORD>, address: \%+(resource-address, offset));

    let (menu-resource-id, menu-resource-id-size) = 
      grok-resource-id(menu-test-pointer);
    dialog-debug-out("menu-id: %d, %d\n", menu-resource-id, menu-resource-id-size);
    offset := offset + menu-resource-id-size;

    new-dialog.dialog-menu := encode-resource(menu-resource-id);

    print-resource-id(menu-resource-id, "Menu");

    let class-test-pointer :: <LPWORD> =
      make(<LPWORD>, address: \%+(resource-address, offset));

    let (class-resource-id, class-resource-id-size) = 
      grok-resource-id(class-test-pointer);
    offset := offset + class-resource-id-size;

    new-dialog.window-class := encode-resource(class-resource-id);
    
    print-resource-id(class-resource-id, "Window Class");

    let title-string-pointer :: <C-void*> =
      make(<C-void*>,
	   address: \%+(resource-address, offset));

    let (title-string, title-size) = grok-resource-string(title-string-pointer);

    offset := offset + title-size;

    new-dialog.dialog-title := encode-resource(title-string);

    let font-resource-size = 0;

    if(logand(template.style-value, $DS-SETFONT) ~= 0)
      let font-size-pointer :: <LPWORD> = make(<LPWORD>, 
					       address: \%+(resource-address, offset));

      offset := offset + size-of(<WORD>);
      new-dialog.dialog-font-size := font-size-pointer.pointer-value;
      dialog-debug-out("We have a font size: %d\n", font-size-pointer.pointer-value);

      let font-name-pointer :: <C-pointer> = pointer+(font-size-pointer, 1);
      let (font-name, font-name-size) = grok-resource-string(font-name-pointer);

      new-dialog.dialog-font  := encode-resource(font-name);

      dialog-debug-out("Font name: %s\n", as(<byte-string>, font-name));
      offset := offset + font-name-size;
    end;

    let addr = \%+(resource-address, offset);
    for(i from 1 below template.cdit-value,
	item =  make(<LPCDLGITEMTEMPLATEA>, address: 
		       pointer-address-32(addr))
	  then
	  make(<LPCDLGITEMTEMPLATEA>, address: 
		 pointer-address-32(addr)))

      let (item-size, control-resource) = grok-item-template(item);
      register-child(new-dialog, control-resource, 
		     control-resource.get-resource-id);
      addr := \%+(item.pointer-address, item-size);
      
    finally
      let (item-size, control-resource) = grok-item-template(item);
      register-child(new-dialog, control-resource, control-resource.get-resource-id);
      addr := \%+(item.pointer-address, item-size);
      template-size := as(<integer>, \%-(addr, resource-address));
      dialog-debug-out("Dialog resource size = %d\n\n", template-size);
    end;
  end;
  if(new-dialog.resource-size ~= template-size)
    ErrorHandler("Incorrect dialog resource size retrieved");
  end;
  new-dialog;
end;

define method grok-item-template(template :: <LPCDLGITEMTEMPLATEA>)
 => (resource-size :: <integer>, resource :: <control-resource>);

  dialog-debug-out("\nControl id = %d\n", template.id-value);

  dialog-debug-out("x= %d y= %d width= %d height= %d\n",
	     template.x-value, template.y-value,
	     template.cx-value, template.cy-value);
 
  let new-control = make(<control-resource>,
			 template: template);

  let class-test-pointer :: <LPWORD> =
    make(<LPWORD>, 
	 address: \%+(template.pointer-address, $item-template-size));

  let (class-resource-id, class-resource-id-size) = 
    grok-resource-id(class-test-pointer);

  new-control.window-class := encode-resource(class-resource-id);

  dialog-debug-out("class id size= %d\n", class-resource-id-size);
  print-resource-id(class-resource-id, "Control Class");

  let text-address = \%+(class-test-pointer.pointer-address, 
    class-resource-id-size);

  let text-resource-pointer :: <LPWORD> = 
    make(<LPWORD>, address: pointer-address-16(text-address));

  let (text-resource, text-size) = grok-resource-id(text-resource-pointer);

  new-control.control-text := encode-resource(text-resource);

  print-resource-id(text-resource, "Control's Text");

  let creation-data-pointer = 
    make(<LPWORD>, address: \%+(text-address, text-size));

  let data-size = grok-creation-data(creation-data-pointer);

  dialog-debug-out("Creation data size = %d\n", data-size);

  let template-size = \%-(\%+(creation-data-pointer.pointer-address, data-size),
			 template.pointer-address);

  new-control.creation-data-size := data-size;
// what to do with creation data ? the only useful thing is 
// to keep it around in C format to be used for explicit window creation ?
// unless we have our own custom controls

  new-control.creation-data := creation-data-pointer;

//  dialog-debug-out("Template size: %d\n", template-size);

  new-control.resource-size := as(<integer>, template-size); // skip overflow check

  values(template-size, new-control);

end;


define method grok-resource-id(p :: <LPWORD>) 
 => (id :: type-union(<integer>, <string>), resource-id-size :: <integer>);
  let word-pointer = as(<WORD*>, p);
  let word-value = word-pointer.pointer-value;
  if(word-value = #x0000)
    dialog-debug-out("g-r-id: %d, %d\n", 0, size-of(<WORD>));
    values(0, size-of(<WORD>));
  elseif(word-value = #xFFFF)
    let id = pointer-value(word-pointer, index: 1);
    let sz = 2 * size-of(<WORD>);
    dialog-debug-out("g-r-id: %d, %d\n", id, sz);
    values(id, sz);
  else
    grok-resource-string(p);
  end;
end;

define method grok-resource-string(p :: <C-pointer>)
 => (string :: <string>, resource-size :: <integer>);
  let resource-string :: <C-unicode-string> = 
    make(<C-unicode-string>, address: p.pointer-address);
  let resource-size :: <integer> = 
//    (resource-string.size + 1) * size-of(resource-string.referenced-type);
    (resource-string.size + 1) * 2;
  dialog-debug-out("g-r-string: %s, %d\n", as(<byte-string>, resource-string), resource-size);
  values(resource-string, resource-size);
end;

define method grok-creation-data(p :: <LPWORD>)
 => ( data-size :: <integer>);
  let data-value :: <integer> = as(<integer>, p.pointer-value);
  if(data-value = 0)
    size-of(<WORD>)
  else
    size-of(<WORD>) + data-value
  end if;
end;

define method pointer-address-16(addr)
 => ( addr-16)
  %logand(\%+(addr, 1), as(<machine-word>, #xFFFFFFFE));
end;

define method pointer-address-32(addr)
 => ( addr-32)
  let new-addr = %logand(\%+(addr, 3), as(<machine-word>, #xFFFFFFFC)); 
//    debug-out("=>Adjusting pointer to 32 from %x to %x\n", 
//	       addr, new-addr);
  new-addr
end;
