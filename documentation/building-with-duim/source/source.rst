*************************************
Source Code For The Task List Manager
*************************************

For completeness, here is the full source code for both versions of the
task list manager. If you have followed the example given in :doc:`design`
through :doc:`commands` from the beginning, then your code should be the
same as the code given in `A task list manager using menu gadgets`_. The
source code for the second version of the task list manager, using command
tables, is given in `A task list manager using command tables`_.

.. note: Please note that both projects have the same name within the source
   code— *task-list* —and you should not load them both into the environment
   at the same time.*

A task list manager using menu gadgets
--------------------------------------

This section contains the complete source code to the first complete
design of the task list manager, described in Chapters :doc:`improve`
to :doc:`callbacks`. To load this code into the environment, choose
**Tools > Open Example Project** from any window in the environment. The
code in this section can be loaded by choosing Task List 1 in the
Documentation category of the Open Example Project dialog.

Contents of the file *frame.dylan* :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: dylan

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

Contents of the file *task-list.dylan* :

.. code-block:: dylan

    Module:    task-list
    Synopsis:  Task List Manager.
    Author:    Functional Objects, Inc.
    Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
                  All rights reserved.
    License:      See License.txt in this distribution for details.
    Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

    define class <task-list> (<object>)
      constant slot task-list-tasks = make(<stretchy-vector>),
        init-keyword: tasks:;
      slot task-list-filename :: false-or(<string>) = #f,
        init-keyword: filename:;
      slot task-list-modified? :: <boolean> = #f;
    end class <task-list>;

    define constant <priority> = one-of(#"low", #"medium", #"high");

    define class <task> (<object>)
      slot task-name :: <string>,
        required-init-keyword: name:;
      slot task-priority :: <priority>,
        required-init-keyword: priority:;
    end class <task>;

    define function add-task
        (task-list :: <task-list>, task :: <task>) => ()
      add!(task-list.task-list-tasks, task);
      task-list.task-list-modified? := #t
    end function add-task;

    define function remove-task
        (task-list :: <task-list>, task :: <task>) => ()
      remove!(task-list.task-list-tasks, task);
      task-list.task-list-modified? := #t
    end function remove-task;

    define function save-task-list
        (task-list :: <task-list>, #key filename)
     => (saved? :: <boolean>)
      let filename = filename | task-list-filename(task-list);
      with-open-file (stream = filename, direction: #"output")
        for (task in task-list.task-list-tasks)
          format(stream, "%s\n%s\n",
                 task.task-name, as(<string>, task.task-priority))
        end
      end;
      task-list.task-list-modified? := #f;
      task-list.task-list-filename := filename;
      #t
    end function save-task-list;

    define function load-task-list
       (filename :: <string>) => (task-list :: false-or(<task-list>))
      let tasks = make(<stretchy-vector>);
      block (return)
        with-open-file (stream = filename, direction: #"input")
          while (#t)
            let name = read-line(stream, on-end-of-stream: #f);
            unless (name) return() end;
            let priority = read-line(stream, on-end-of-stream: #f);
            unless (priority)
              error("Unexpectedly missing priority!")
            end;
            let task = make(<task>, name: name,
                            priority: as(<symbol>, priority));
            add!(tasks, task)
          end
        end
      end;
      make(<task-list>, tasks: tasks, filename: filename)
    end function load-task-list;

A task list manager using command tables
----------------------------------------

This section contains the complete source code of the task list manager
when command tables have been used to implement the menu system, rather
than explicit menu gadgets. To load this code into the environment,
choose **Tools > Open Example Project** from any window in the
environment. The code in this section can be loaded by choosing Task
List 2 in the Documentation category of the Open Example Project dialog.

The command tables used in this implementation are described in
:doc:`commands`. You should refer to :doc:`improve` and :doc:`callbacks`,
for a full description of the rest of the code shown here. Note that,
apart from code specific to command tables and callbacks, the code listed
in this section is a repeat of code listed in `A task list manager using
menu gadgets`_.

Contents of the file *frame.dylan* :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: dylan

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

Contents of the file *task-list.dylan* :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The file *task-list.dylan* is identical to the listing shown in `A
task list manager using menu gadgets`_, and so is not repeated here.
