Module:       standalone-deuce-internals
Synopsis:     Standalone wrapper for DUIM-Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM back-end for a top-level Deuce editor

define sealed class <deuce-editor> (<basic-editor>)
end class <deuce-editor>;


/// Random test commands

define sealed class <random-sections-buffer>
    (<definition-browsing-buffer>)
  keyword format-string: = "Random sections of %s";
end class <random-sections-buffer>;

define function edit-random-sections (frame :: <editor-state-mixin>) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode   :: <major-mode>   = buffer-major-mode(buffer);
  let node-class = if (instance?(mode, <dylan-mode>)) <dylan-definition-node>
                   else <definition-node> end;
  local method random-sections (buffer) => (sections :: <vector>)
          let sections :: <stretchy-object-vector> = make(<stretchy-vector>);
          for (i :: <integer> from 0,
               node = buffer-start-node(buffer) then node-next(node),
               until: ~node)
            when (even?(i))
              add!(sections, node-section(node))
            end
          end;
          sections
        end method;
  let buffer = make-empty-buffer(<random-sections-buffer>,
                                 definition: buffer,
                                 name-key:   buffer-name,
                                 generator:  random-sections,
                                 major-mode: mode,
                                 node-class: node-class);
  revert-buffer(buffer);
  select-buffer-in-appropriate-window(window, buffer);
  frame-last-command-type(frame) := #"file"
end function edit-random-sections;


/* ---*** The mail sending hack...
/// Glue to simple SMTP client

define variable *smtp-mail-host* :: <string> = "mailhost";

define sideways method do-send-mail
    (window :: <basic-window>,
     to :: <string>, subject :: <string>, body :: <string>,
     #key from :: false-or(<string>), cc :: false-or(<string>), other-headers)
 => (success? :: <boolean>, message :: false-or(<string>))
  start-sockets();
  let from
    = from
      | begin
          let name = login-name();
          let host = local-host-name();
          name & host & concatenate-as(<string>, name, "@", host)
        end;
  let success? :: <boolean> = #t;
  let message  :: false-or(<string>) = #f;
  block ()
    with-smtp-message-stream (stream to *smtp-mail-host*,
                              from: from,
                              recipients: vector(to))
      // Write the other headers
      when (cc)
        write(stream, format-to-string("cc: %s\r\n", cc))
      end;
      write(stream, format-to-string("Subject: %s\r\n", subject));
      for (header in other-headers)
        let key   = header[0];
        let name  = header[1];
        let value = header[2];
        write(stream, format-to-string("%s: %s\r\n", name, value))
      end;
      write(stream, "\r\n");
      // Write the message body
      let line-start :: <integer> = 0;
      for (i :: <integer> from 0 below size(body))
        let c = body[i];
        when (c == '\n')
          write(stream, body, start: line-start, end: i);
          write(stream, "\r\n");
          line-start := i + 1;
        end;
      finally
        unless (i = line-start)
          write(stream, "\r\n")
        end;
      end
    end;
  exception (c :: <smtp-error>)
    success? := #f;
    message  := smtp-error-response(c)
  end;
  values(success?, message)
end method do-send-mail;
*/


/// DUIM command tables for Deuce

define function make-deuce-command
    (function :: <function>) => (command :: <function>)
  let command = method (frame)
                  // Prefer the Deuce frame to the DUIM frame...
                  let frame = *editor-frame* | frame;
                  execute-command-in-frame(frame, function)
                end method;
  command
end function make-deuce-command;


define command-table *deuce-file-comtab* ()
  menu-item "New..."     = make-deuce-command(new-file);
  menu-item "Open..."    = make-deuce-command(find-file);
  menu-item "Close"      = make-deuce-command(close-file);
  separator;
  menu-item "Print..."   = make-deuce-command(print-file);
  separator;
  menu-item "Save"       = make-deuce-command(save-file);
  menu-item "Save As..." = make-deuce-command(save-file-as);
  menu-item "Save All"   = make-deuce-command(save-all-files);
  separator;
  menu-item "Compile"    = make-deuce-command(compile-file);
  menu-item "Load"       = make-deuce-command(load-file);
  separator;
  menu-item "Exit"       = exit-frame;
end command-table *deuce-file-comtab*;

define command-table *deuce-edit-comtab* ()
  menu-item "Undo"       = make-deuce-command(deuce/undo-command);
  menu-item "Redo"       = make-deuce-command(deuce/redo-command);
  separator;
  menu-item "Cut"        = make-deuce-command(cut-region);
  menu-item "Copy"       = make-deuce-command(copy-region);
  menu-item "Paste"      = make-deuce-command(paste);
  menu-item "Delete"     = make-deuce-command(delete-region);
  separator;
  menu-item "Find..."    = make-deuce-command(find-string);
  menu-item "Find Next"  = make-deuce-command(find-next-string);
  menu-item "Replace..." = make-deuce-command(replace-string);
  separator;
  menu-item "Find Definition..." = make-deuce-command(edit-definition);
  menu-item "Random Sections"    = make-deuce-command(edit-random-sections);
end command-table *deuce-edit-comtab*;

define command-table *deuce-view-comtab* ()
  menu-item "Configure..." = make-deuce-command(choose-configuration);
end command-table *deuce-view-comtab*;

define command-table *deuce-region-comtab* ()
  menu-item "Evaluate"      = make-deuce-command(evaluate-region);
  menu-item "Macroexpand"   = make-deuce-command(macroexpand-region);
  separator;
  menu-item "Browse"        = make-deuce-command(browse-object);
  menu-item "Browse Class"  = make-deuce-command(browse-class);
  separator;
  menu-item "Arglist"       = make-deuce-command(show-arglist);
  menu-item "Documentation" = make-deuce-command(show-documentation);
  separator;
  menu-item "Upcase"        = make-deuce-command(upcase-region);
  menu-item "Downcase"      = make-deuce-command(downcase-region);
  menu-item "Indent"        = make-deuce-command(indent-region);
end command-table *deuce-region-comtab*;

define command-table *deuce-buffer-comtab* ()
  menu-item "Evaluate"   = make-deuce-command(evaluate-buffer);
  separator;
  menu-item "Sectionize" = make-deuce-command(sectionize-file);
  menu-item "Revert"     = make-deuce-command(revert-file);
  menu-item "Kill"       = make-deuce-command(close-file);
  separator;
  menu-item "Buffers..." = make-deuce-command(choose-buffer);
end command-table *deuce-buffer-comtab*;

define command-table *deuce-help-comtab* ()
  menu-item "Key Bindings" = make-deuce-command(editor-key-bindings);
end command-table *deuce-help-comtab*;

define command-table *deuce-command-table* ()
  menu-item "File"   = *deuce-file-comtab*;
  menu-item "Edit"   = *deuce-edit-comtab*;
  menu-item "View"   = *deuce-view-comtab*;
  menu-item "Region" = *deuce-region-comtab*;
  menu-item "Buffer" = *deuce-buffer-comtab*;
  menu-item "Help"   = *deuce-help-comtab*;
end command-table *deuce-command-table*;


/// Toolbar icons

define variable $undo-bitmap          = "Undo";
define variable $redo-bitmap          = "Redo";
define variable $cut-bitmap           = "X";
define variable $copy-bitmap          = "C";
define variable $paste-bitmap         = "P";
define variable $find-bitmap          = "Find";
define variable $replace-bitmap       = "Replace";
define variable $find-next-bitmap     = ">";
define variable $find-previous-bitmap = "<";
define variable $new-bitmap           = "New";
define variable $open-bitmap          = "Open";
define variable $save-bitmap          = "Save";


/// DUIM back-end for a top-level Deuce editor frame

define variable $deuce-small-icon = #f;
define variable $deuce-large-icon = #f;

define frame <deuce-frame>
    (<basic-editor-frame>, <simple-frame>)
  constant slot %lines   :: false-or(<integer>) = #f,
    init-keyword: lines:;
  constant slot %columns :: false-or(<integer>) = #f,
    init-keyword: columns:;
  pane %window (frame)
    make(<deuce-pane>,
         frame: frame,
         lines:   frame.%lines,
         columns: frame.%columns);
  layout (frame)
    scrolling (scroll-bars: #"both")
      frame.%window
    end;
  status-bar (frame)
     make-deuce-status-bar(frame);
  command-table (frame)
     *deuce-command-table*;
  keyword icon: = $deuce-small-icon;
end frame <deuce-frame>;

define method initialize
    (frame :: <deuce-frame>, #key) => ()
  next-method();
  frame-input-focus(frame) := frame.%window;
  frame-window(frame) := frame.%window
end method initialize;

define method frame-top-level
    (frame :: <deuce-frame>) => (#rest values)
  dynamic-bind (*editor-frame* = frame)
    let buffer = make-initial-buffer();
    dynamic-bind (*buffer* = buffer)
      select-buffer(frame-window(frame), buffer);
      let top-sheet = top-level-sheet(frame);
      while (#t)
        let event = read-event(top-sheet);
        block ()
          handle-event(event-handler(event-client(event)), event);
        exception (e :: <command-error>)
          when (command-error-format-string(e))
            apply(deuce/display-error-message,
                  command-error-window(e),
                  command-error-format-string(e), command-error-format-arguments(e))
          end;
          #f
        end
      end
    end
  end
end method frame-top-level;

define method handle-event
    (frame :: <deuce-frame>, event :: <frame-created-event>) => ()
  next-method();
  // Set up thread variables and initial buffer
  let window :: false-or(<basic-window>) = frame-window(frame);
  let buffer :: false-or(<basic-buffer>) = frame-buffer(frame);
  *editor-frame* := frame;
  *buffer*       := buffer;
  let policy = editor-policy(frame-editor(frame));
  window-note-policy-changed(frame-window(frame), policy, #f);
  unless (window & buffer == window-buffer(window))
    select-buffer(window, buffer);
    queue-redisplay(window, $display-all);
    redisplay-window(window)
  end
end method handle-event;

define method exit-editor (frame :: <deuce-frame>) => ()
  exit-frame(frame)
end method exit-editor;

define method handle-event
    (frame :: <deuce-frame>, event :: <frame-exit-event>) => ()
  block (return)
    let window :: false-or(<basic-window>) = frame-window(frame);
    when (window)
      let buffers = save-buffers-dialog(window, exit-label: "&Close");
      select (buffers)
        #f        => #f;
        #"cancel" => return();
        otherwise =>
          do-save-all-files(frame, buffers, curry(deuce/display-message, window));
      end
    end;
    when (event-destroy-frame?(event))
      // If this frame is going away, stop tracking it
      let frames = editor-frames(frame-editor(frame));
      remove!(frames, frame)
    end;
    next-method()
  end
end method handle-event;

define method handle-event
    (frame :: <deuce-frame>, event :: <frame-focus-in-event>) => ()
  frame-input-focus(frame) := frame.%window
end method handle-event;
