Module:    environment-debugger
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <register-window>
    (<frame-refresh-mixin>,
     <frame-window-settings-mixin>,
     <environment-project-tool>)
  sealed constant slot register-window-thread :: <thread-object>,
    required-init-keyword: remote-thread:;
  pane register-window-thread-pane (frame)
    make(<table-control-displayer>,
	 element-label: "register",
	 children-generator: curry(frame-thread-registers, frame),
	 headings: #["Name", "Value"],
	 widths: #[50, 800],
	 generators: vector(wrapper-register,
			    wrapper-object),
	 sort-orders: #[#"name", #"value"],
	 sort-order:  #"name",
	 sort-function: curry(frame-sort-thread-registers, frame),
	 label-key: curry(register-window-label, frame),
	 activate-callback: curry(environment-activate-callback, frame));
  layout (frame)
    frame.register-window-thread-pane;
  keyword icon: = $debugger-window-small-icon;
  keyword frame-class-name: = #"register-window";
  keyword width: = 400;
  keyword mode:  = #"modeless";
  keyword exit-callback:   = #f;
  keyword cancel-callback: = #f;
end frame <register-window>;

define sealed domain make (subclass(<register-window>));
define sealed domain initialize (<register-window>);

define window-settings
  register-window :: <register-window> = "Register Window";

define method handle-event
    (frame :: <register-window>, event :: <frame-mapped-event>)
 => ()
  with-busy-cursor (frame)
    next-method();
    refresh-frame(frame)
  end
end method handle-event;

define method debugger-show-registers
    (debugger :: <debugger>) => ()
  let register-window = debugger.debugger-register-window;
  if (register-window)
    raise-frame(register-window, activate?: #f);
    update-debugger-register-window(debugger)
  else
    let project = debugger.ensure-frame-project;
    let thread = debugger.debugger-thread;
    let dialog
      = make(<register-window>,
	     owner: debugger,
	     remote-thread: thread,
	     project: project);
    start-frame(dialog)
  end
end method debugger-show-registers;

define method refresh-frame
    (frame :: <register-window>) => ()
  next-method();
  let displayer = frame.register-window-thread-pane;
  let thread = frame.register-window-thread;
  if (displayer.displayer-object == thread)
    refresh-displayer(displayer)
  else
    displayer.displayer-object := thread
  end
end method refresh-frame;

define method update-debugger-register-window
    (debugger :: <debugger>) => ()
  let dialog = debugger.debugger-register-window;
  dialog & call-in-frame(dialog, refresh-frame, dialog)
end method update-debugger-register-window;

define method debugger-register-window
    (debugger :: <debugger>) => (window :: false-or(<register-window>))
  block (return)
    let dialogs = debugger.frame-owned-frames;
    for (dialog in dialogs)
      if (instance?(dialog, <register-window>))
	return(dialog)
      end
    end
  end
end method debugger-register-window;

define method generate-frame-title
    (frame :: <register-window>) => (title :: <string>)
  let debugger :: <debugger> = frame.frame-owner;
  let project = debugger.ensure-frame-project;
  let thread = frame.register-window-thread;
  concatenate("Registers - ",
	      frame-default-object-name(frame, thread))
end method generate-frame-title;


/// Register handling

define sealed class <register-wrapper> (<object-wrapper>)
  constant sealed slot wrapper-register :: <register-object>,
    required-init-keyword: register:;
end class <register-wrapper>;
  
define sealed domain make (singleton(<register-wrapper>));
define sealed domain initialize (<register-wrapper>);


define method frame-thread-registers
    (frame :: <register-window>, thread :: <thread-object>)
 => (registers :: <sequence>)
  let debugger :: <debugger> = frame.frame-owner;
  let project = debugger.ensure-frame-project;
  map(method (register :: <register-object>)
	let object = frame-register-contents(frame, register);
	make(<register-wrapper>,
	     object:   object,
	     register: register)
      end,
      project.application-registers)
end method frame-thread-registers;

define method register-window-label
    (frame :: <register-window>, object :: <environment-object>)
 => (label :: <string>)
  let debugger :: <debugger> = frame.frame-owner;
  let project = debugger.ensure-frame-project;
  let module = debugger.frame-current-module;
  print-environment-object-to-string(project, object, namespace: module)
end method register-window-label;

define method frame-register-contents
    (frame :: <register-window>, register :: <register-object>)
 => (contents :: false-or(<environment-object>))
  let debugger :: <debugger> = frame.frame-owner;
  let project = debugger.ensure-frame-project;
  let thread = debugger.debugger-thread;
  let stack-frame = debugger.debugger-current-stack-frame;
  register-contents(project, register, thread,
		    stack-frame-context: stack-frame)
end method frame-register-contents;

define method frame-sort-thread-registers
    (frame :: <register-window>, registers :: <sequence>,
     order :: <symbol>)
 => (registers :: <sequence>)
  let project = frame.ensure-frame-project;
  frame-sort-items
    (frame, registers,
     key: select (order)
	    #"name"  => wrapper-register;
	    #"value" => wrapper-object;
	  end)
end method frame-sort-thread-registers;
