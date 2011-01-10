Module:    task-list
Synopsis:  Task List Manager.
Author:    Functional Objects, Inc.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $priority-items
  = #(#("Low", #"low"),
      #("Medium", #"medium"),
      #("High", #"high"));

define frame <task-frame> (<simple-frame>)
  slot frame-task-list :: <task-list> = make(<task-list>);

// definition of menu bar
  pane task-menu-bar (frame)
    make(<menu-bar>,
         children: vector(frame.file-menu, 
                          frame.edit-menu,  
                          frame.task-menu,  
                          frame.help-menu));
// definition of menus
  pane file-menu (frame)
    make(<menu>, label: "File",
         children: vector(frame.open-menu-button,
                          frame.save-menu-button,
                          frame.save-as-menu-button,
                          frame.exit-menu-button));
  pane edit-menu (frame)
    make(<menu>, label: "Edit",
         children: vector(frame.cut-menu-button,
                          frame.copy-menu-button,
                          frame.paste-menu-button));
  pane task-menu (frame)
    make(<menu>, label: "Task",
         children: vector(frame.add-menu-button,
                          frame.remove-menu-button));
  pane help-menu (frame)
    make(<menu>, label: "Help",
         children: vector(frame.about-menu-button));

// definition of menu buttons

  // Commands in the File menu
  pane open-menu-button (frame)
    make(<menu-button>, label: "Open...",
         activate-callback: open-file,
         accelerator: make-keyboard-gesture(#"o", #"control"),
         documentation: "Opens an existing file.");
  pane save-menu-button (frame)
    make(<menu-button>, label: "Save",
         activate-callback: save-file,
         accelerator: make-keyboard-gesture(#"s", #"control"),
         documentation: "Saves the current file to disk.");
  pane save-as-menu-button (frame)
    make(<menu-button>, label: "Save As...",
         activate-callback: save-as-file,
         documentation: 
           "Saves the current file with a new name.");
  pane exit-menu-button (frame)
    make(<menu-button>, label: "Exit",
         activate-callback: exit-task,
         accelerator: make-keyboard-gesture(#"f4", #"alt"),
         documentation: "Exits the application.");

  //Commands in the Edit menu
  pane cut-menu-button (frame)
    make(<menu-button>, label: "Cut",
         activate-callback: not-yet-implemented,
         accelerator: make-keyboard-gesture(#"x", #"control"),
         documentation: "Cut the selection to the clipboard.");
  pane copy-menu-button (frame)
    make(<menu-button>, label: "Copy",
         activate-callback: not-yet-implemented,
         accelerator: make-keyboard-gesture(#"c", #"control"),
         documentation: "Copy the selection to the clipboard.");
  pane paste-menu-button (frame)
    make(<menu-button>, label: "Paste",
         activate-callback: not-yet-implemented,
         accelerator: make-keyboard-gesture(#"v", #"control"),
         documentation: "Paste the selection in the clipboard at the current position.");

  //Commands in the Task menu
  pane add-menu-button (frame)
    make(<menu-button>, label: "Add...",
         activate-callback: frame-add-task,
         accelerator: make-keyboard-gesture
                        (#"a", #"control", #"shift"),
         documentation: "Add a new task.");
  pane remove-menu-button (frame)
    make(<menu-button>, label: "Remove",
         activate-callback: frame-remove-task,
         accelerator: make-keyboard-gesture
                        (#"d", #"control", #"shift"),
         documentation: 
           "Remove the selected task from the list.");

  //Commands in the Help menu
  pane about-menu-button (frame)
    make(<menu-button>, label: "About",
         activate-callback: about-task,
         accelerator: make-keyboard-gesture(#"f1"),
         documentation: 
           "Display information about the application.");

// definition of buttons
  pane add-button (frame)
    make(<push-button>, label: "Add task",
         activate-callback: frame-add-task);
  pane remove-button (frame)
    make(<push-button>, label: "Remove task",
         activate-callback: frame-remove-task);
  pane open-button (frame)
    make(<push-button>, label: "Open file",
         activate-callback: open-file);
  pane save-button (frame)
    make(<push-button>, label: "Save file",
         activate-callback: save-file);

// definition of radio box
  pane priority-box (frame)
    make (<radio-box>,
          items: $priority-items,
          orientation: #"horizontal",
          label-key: first,
          value-key: second,
          value: #"medium",
          activate-callback: not-yet-implemented);

// definition of tool bar
  pane task-tool-bar (frame)
    make(<tool-bar>,
         child: horizontally ()
                  frame.open-button;
                  frame.save-button;
                  frame.add-button;
                  frame.remove-button
                end); 

// definition of status bar
  pane task-status-bar (frame)
    make(<status-bar>, label: "Task Manager");

// definition of list
  pane task-list (frame)
    make (<list-box>,
          items: frame.frame-task-list.task-list-tasks,
          label-key: task-name,
          lines: 15,
          value-changed-callback: note-task-selection-change);

// main layout
  pane task-layout (frame)
    vertically ()
      frame.task-list; 
      frame.priority-box;
    end;

// activation of frame elements
  layout (frame) frame.task-layout;
  tool-bar (frame) frame.task-tool-bar;
  status-bar (frame) frame.task-status-bar;
  menu-bar (frame) frame.task-menu-bar;

// frame title
  keyword title: = "Task List Manager";
end frame <task-frame>;

define method initialize
    (frame :: <task-frame>, #key) => ()
  next-method();
  refresh-task-frame(frame);
end method initialize;

define method prompt-for-task 
   (#key title = "Type text of new task", owner)
 => (name :: false-or(<string>), 
     priority :: false-or(<priority>))
  let task-text
    = make(<text-field>, 
           label: "Task text:",
           activate-callback: exit-dialog);
  let priority-field
    = make(<radio-box>,
           items: $priority-items,
           label-key: first,
           value-key: second,
           value: #"medium");
  let frame-add-task-dialog
    = make(<dialog-frame>, 
           title: title,
           owner: owner,
           layout: vertically ()
                     task-text;
                     priority-field
                   end,
           input-focus: task-text);
  if (start-dialog(frame-add-task-dialog))
    values(gadget-value(task-text), gadget-value(priority-field))
  end
end method prompt-for-task;

define function make-keyboard-gesture
    (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

define function not-yet-implemented (gadget :: <gadget>) => ()
  notify-user("Not yet implemented!", owner: sheet-frame(gadget))
end function not-yet-implemented;

define method start-task () => ()
  let frame
    = make(<task-frame>);
  start-frame(frame);
end method start-task;

define method frame-add-task (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let task-list = frame-task-list(frame);
  let (name, priority) = prompt-for-task(owner: frame);
  if (name & priority)
    let new-task = make(<task>, name: name, priority: priority);
    add-task(task-list, new-task);
    refresh-task-frame(frame);
    frame-selected-task(frame) := new-task
  end
end method frame-add-task;

define method frame-remove-task (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let task = frame-selected-task(frame);
  let task-list = frame-task-list(frame);
  if (notify-user(format-to-string
                    ("Really remove task %s", task.task-name),
                  owner: frame, style: #"question"))
    frame-selected-task(frame) := #f;
    remove-task(task-list, task);
    refresh-task-frame(frame)
  end
end method frame-remove-task;

define method frame-selected-task
    (frame :: <task-frame>) => (task :: false-or(<task>))
  let list-box = task-list(frame);
  gadget-value(list-box)
end method frame-selected-task;

define method frame-selected-task-setter
    (task :: false-or(<task>), frame :: <task-frame>)
 => (task :: false-or(<task>))
  let list-box = task-list(frame);
  gadget-value(list-box) := task;
  note-task-selection-change(frame);
  task
end method frame-selected-task-setter;

define method refresh-task-frame
    (frame :: <task-frame>) => ()
  let list-box = frame.task-list;
  let task-list = frame.frame-task-list;
  let modified? = task-list.task-list-modified?;
  let tasks = task-list.task-list-tasks;
  if (gadget-items(list-box) == tasks)
    update-gadget(list-box)
  else
    gadget-items(list-box) := tasks
  end;
  gadget-enabled?(frame.save-button) := modified?;
  gadget-enabled?(frame.save-menu-button) := modified?;
  note-task-selection-change(frame);
end method refresh-task-frame;

define method note-task-selection-change
    (gadget :: <gadget>) => ()
  let frame = gadget.sheet-frame;
  note-task-selection-change(frame)
end method note-task-selection-change;

define method note-task-selection-change
    (frame :: <task-frame>) => ()
  let task = frame-selected-task(frame);
  if (task)
    frame.priority-box.gadget-value := task.task-priority;
  end;
  let selection? = (task ~= #f);
  frame.remove-button.gadget-enabled? := selection?;
  frame.remove-menu-button.gadget-enabled? := selection?;
end method note-task-selection-change;

define method open-file
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let task-list = frame-task-list(frame);
  let filename
    = choose-file(frame: frame,
                  default: task-list.task-list-filename,
                  direction: #"input");
  if (filename)
    let task-list = load-task-list(filename);
    if (task-list)
      frame.frame-task-list := task-list;
      refresh-task-frame(frame)
    else
      notify-user(format-to-string("Failed to open file %s", filename),
                  owner: frame)
    end
  end
end method open-file;

define method save-file
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let task-list = frame-task-list(frame);
  save-as-file(gadget, filename: task-list.task-list-filename)
end method save-file;

define method save-as-file
    (gadget :: <gadget>, #key filename) => ()
  let frame = sheet-frame(gadget);
  let task-list = frame-task-list(frame);
  let filename
    = filename
        | choose-file(frame: frame,
                      default: task-list.task-list-filename,
                      direction: #"output");
  if (filename)
    if (save-task-list(task-list, filename: filename))
      frame.frame-task-list := task-list;
      refresh-task-frame(frame)
    else
      notify-user(format-to-string
                    ("Failed to save file %s", filename),
                  owner: frame)
    end
  end
end method save-as-file;

define function about-task (gadget :: <gadget>) => ()
  notify-user("Task List Manager", owner: sheet-frame(gadget))
end function about-task;

define method exit-task (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let task-list = frame-task-list(frame);
  save-file (gadget);
  exit-frame(frame)
end method exit-task;

define method main (arguments :: <sequence>) => ()
// handle the arguments
  start-task();
end method main;

begin
  main(application-arguments()) // Start the application!
end;
