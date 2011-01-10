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

// Note: no definition of menu buttons in this implementation,
// See definition of command tables instead.

// definition of buttons
  pane add-button (frame)
    make(<push-button>, label: "Add task",
	 command: frame-add-task,
	 activate-callback: method (gadget) frame-add-task(frame) end);
  pane remove-button (frame)
    make(<push-button>, label: "Remove task",
	 command: frame-remove-task,
         activate-callback:  method (gadget) frame-remove-task(frame) end);
  pane open-button (frame)
    make(<push-button>, label: "Open file",
	 command: open-file,
         activate-callback: method (gadget) open-file(frame) end);
  pane save-button (frame)
    make(<push-button>, label: "Save file",
	 command: save-file,
         activate-callback: method (gadget) save-file(frame) end);

// definition of radio box
  pane priority-box (frame)
    make(<radio-box>,
	 items: $priority-items,
	 orientation: #"horizontal",
	 label-key: first,
	 value-key: second,
	 value: #"medium",
	 activate-callback: method (gadget) not-yet-implemented(frame) end);

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
          value-changed-callback: method (gadget) note-task-selection-change(frame) end);

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
  command-table (frame) *task-list-command-table*;

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

define function not-yet-implemented (frame :: <task-frame>) => ()
  notify-user("Not yet implemented!", owner: frame)
end function not-yet-implemented;

define method start-task () => ()
  let frame
    = make(<task-frame>);
  start-frame(frame);
end method start-task;

define method frame-add-task (frame :: <task-frame>) => ()
  let task-list = frame-task-list(frame);
  let (name, priority) = prompt-for-task(owner: frame);
  if (name & priority)
    let new-task = make(<task>, name: name, priority: priority);
    add-task(task-list, new-task);
    refresh-task-frame(frame);
    frame-selected-task(frame) := new-task
  end
end method frame-add-task;

define method frame-remove-task (frame :: <task-frame>) => ()
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
  command-enabled?(save-file, frame) := modified?;
  note-task-selection-change(frame);
end method refresh-task-frame;

define method note-task-selection-change
    (frame :: <task-frame>) => ()
  let task = frame-selected-task(frame);
  if (task)
    frame.priority-box.gadget-value := task.task-priority;
  end;
  command-enabled?(frame-remove-task, frame) := task ~= #f;
end method note-task-selection-change;

define method open-file
    (frame :: <task-frame>) => ()
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
    (frame :: <task-frame>) => ()
  let task-list = frame-task-list(frame);
  if (task-list.task-list-modified?)
    save-as-file(frame, filename: task-list.task-list-filename)
  end
end method save-file;

define method save-as-file
    (frame :: <task-frame>, #key filename) => ()
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

define function about-task (frame :: <task-frame>) => ()
  notify-user("Task List Manager", owner: frame)
end function about-task;

define method exit-task (frame :: <task-frame>) => ()
  let task-list = frame-task-list(frame);
  save-file(frame);
  exit-frame(frame)
end method exit-task;

define function make-keyboard-gesture
    (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

// Definition of the File menu

define command-table *file-command-table* (*global-command-table*)
  menu-item "Open" = open-file,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Opens an existing file.";
  menu-item "Save" = save-file,
    accelerator:   make-keyboard-gesture(#"s", #"control"),
    documentation: "Saves the current file to disk.";
  menu-item "Save As..." = save-as-file,
    documentation: "Saves the current file with a new name.";
  separator;
  menu-item "Exit" = exit-task,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Exits the application.";
end command-table *file-command-table*;

// Definition of the Edit menu

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Cut" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"x", #"control"),
    documentation: "Cut the selection to the clipboard.";
  menu-item "Copy" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"c", #"control"),
    documentation: "Copy the selection to the clipboard.";
  menu-item "Paste" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"v", #"control"),
    documentation: "Paste the selection in the clipboard at the current position.";
end command-table *edit-command-table*;

// Definition of the Task menu 

define command-table *task-command-table* 
    (*global-command-table*)
  menu-item "Add..." = frame-add-task,
    accelerator: make-keyboard-gesture(#"a", #"control", #"shift"),
    documentation: "Add a new task.";
  menu-item "Remove" = frame-remove-task,
    accelerator: make-keyboard-gesture(#"d", #"control", #"shift"),
    documentation: "Remove the selected task from the list.";
end command-table *task-command-table*;

// Definition of the Help menu

define command-table *help-command-table* (*global-command-table*)
  menu-item "About" = about-task,
    accelerator:   make-keyboard-gesture(#"f1"),
    documentation: "Display information about the application.";
end command-table *help-command-table*;

// Definition of the overall menu bar

define command-table *task-list-command-table* 
    (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Task" = *task-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *task-list-command-table*;


define method main (arguments :: <sequence>) => ()
// handle the arguments
  start-task();
end method main;

begin
  main(application-arguments()) // Start the application!
end;
