Module:    button-ocx
Synopsis:  Demonstrate using a DUIM button gadget as an OLE Control.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <my-frame> (<ocx-frame>)
  slot my-dialog = #f;
  slot my-text :: <string> = "\r\nYour message here!\r\n";
  pane my-gadget (frame)
    make(<button>, label: "Show text",
	 accelerator: make(<gesture>, keysym: 't', modifiers: #[#"control"]),
	 activate-callback: activated);
  layout (frame) frame.my-gadget;
end frame;

define function activated ( button )
  display-box(sheet-frame(button));
end;


define method display-box (frame :: <my-frame>) => ()
  let box = frame.my-dialog;
  if ( box & frame-mapped?(box) ) // dialog exists and not exited.
    deiconify-frame(box);
    raise-frame(box);
  else
    with-frame-manager (frame-manager(frame))
      let text = make(<text-editor>,
		      value: frame.my-text,
		      read-only?: #f,
		      nlines: 6, ncolumns: 40);
      let dialog = make(<dialog-frame>,
			mode: #"modeless", owner: frame,
			title: "Look at me",
 			layout: text,
			exit-callback: exit-my-dialog);
      frame.my-dialog := dialog;
      start-frame(dialog);
    end with-frame-manager;
  end if;
end method;

define function exit-my-dialog (dialog :: <dialog-frame>)
  let frame = frame-owner(dialog);
  let new-text = dialog.frame-layout.gadget-value;
  unless (new-text = frame.my-text)
    frame.my-text := new-text;
    frame.embedded-data-changed? := #t;
  end unless;
  exit-dialog(dialog);
  frame.my-dialog := #f;
end;
