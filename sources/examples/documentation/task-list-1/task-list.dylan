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
//        format-out("Read %=\n", name);
        unless (name) return() end;
        let priority = read-line(stream, on-end-of-stream: #f);
//        format-out("Priority %=\n", priority);
        unless (priority) 
          error("Unexpectedly missing priority!") 
        end;
//        format-out("Read: %= %=\n", name, priority);
        let task = make(<task>, name: name, 
                        priority: as(<symbol>, priority));
        add!(tasks, task)
      end
    end
  end;
//  debug-message("Tasks: %=", tasks);
  make(<task-list>, tasks: tasks, filename: filename)
end function load-task-list;
