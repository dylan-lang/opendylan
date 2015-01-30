Module:    editor-deuce-backend
Synopsis:  Environment-Deuce Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce back-end

define sealed class <deuce-editor> (<editor>)
  keyword name:  = #"deuce";
  keyword title: = "Deuce";
end class <deuce-editor>;

define sealed domain make (singleton(<deuce-editor>));
define sealed domain initialize (<deuce-editor>);


define /* open */ abstract class <deuce-editor-command>
    (<editor-command>, <basic-command>)
end class <deuce-editor-command>;

define macro deuce-command-definer
  { define deuce-command ?:name; }
    => { define sealed class "<deuce-" ## ?name ## "-command>"
             ("<editor-" ## ?name ## "-command>", <deuce-editor-command>)
         end class;
         define sealed domain make (singleton("<deuce-" ## ?name ## "-command>"));
         define sealed domain initialize ("<deuce-" ## ?name ## "-command>");
         define sealed method class-for-editor-command
             (editor :: <deuce-editor>, class == "<editor-" ## ?name ## "-command>")
          => (class == "<deuce-" ## ?name ## "-command>")
           "<deuce-" ## ?name ## "-command>"
         end method class-for-editor-command; }
end macro deuce-command-definer;


define deuce-command open;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-open-command>)
 => (#rest values)
  find-deuce-frame()
end method do-execute-command;


define deuce-command close;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-close-command>)
 => (#rest values)
  let frame = editor-command-frame(command);
  when (frame)
    exit-frame(frame)
  end
end method do-execute-command;


define deuce-command new-file;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-new-file-command>)
 => (#rest values)
  let policy = editor-policy($environment-editor);
  find-deuce-frame(new-file?: #t, choose-file?: new-file-buffer?(policy))
end method do-execute-command;


define deuce-command open-file;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-open-file-command>)
 => (#rest values)
  let pathname = editor-command-pathname(command);
  local method open-file (frame :: <basic-editor-frame>)
          let frame-showing-pathname? :: <boolean>
            = begin
                let buffer = frame-buffer(frame);
                buffer
                & file-buffer?(buffer)
                & as(<string>, buffer-pathname(buffer))
                  = as(<string>, as(<file-locator>, pathname))
              end;
          if (frame-showing-pathname?)
            let window :: <basic-window> = frame-window(frame);
            let buffer :: <basic-buffer> = frame-buffer(frame);
            let start-line = max(editor-command-start-line(command),   0);
            let start-col  = max(editor-command-start-column(command), 0);
            let end-line   = max(editor-command-end-line(command),   0);
            let end-col    = max(editor-command-end-column(command), 0);
            // Offer to revert the buffer if it has changed on disk
            revert-buffer-if-necessary(buffer, window: window);
            // Move the point and mark to the positions specified, if any
            let line  = line-index->line(buffer, start-line);
            let index = line & min(start-col, line-length(line));
            clear-mark!(window: window);
            push-point-pdl!(window, window-point(window));
            when (line)
              move-point!(line, index: index, window: window);
              when (end-line ~= start-line | end-col ~= start-col)
                let line  = line-index->line(buffer, end-line);
                let index = line & min(end-col, line-length(line));
                when (line)
                  move-mark!(line, index: index, window: window)
                end
              end
            end;
            queue-redisplay(window, $display-all, centering: 0);
            redisplay-window(window)
          else
            command-error("Couldn't find file %s!", pathname);
          end
        end method;
  // Find a frame displaying a buffer based on pathname, or if there
  // isn't one, use any old frame
  find-and-call-in-deuce-frame(method (frame)
                                 open-file(frame);
                                 raise-frame(frame);
                                 deiconify-frame(frame)
                               end method,
                               buffer-pathname: pathname,
                               deuce-frame: editor-command-frame(command))
end method do-execute-command;


define deuce-command close-file;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-close-file-command>)
 => (#rest values)
  //---*** Do this
end method do-execute-command;


define deuce-command insert-text;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-insert-text-command>)
 => (#rest values)
  //---*** Do this
end method do-execute-command;


define deuce-command delete-text;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-delete-text-command>)
 => (#rest values)
  //---*** Do this
end method do-execute-command;


define deuce-command edit-definitions;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-edit-definitions-command>)
 => (#rest values)
  let buffer
    = make-empty-buffer(<definitions-editing-buffer>,
                        name:        editor-command-title(command),
                        project:     editor-command-project(command),
                        definitions: editor-command-definitions(command),
                        major-mode:  find-mode(<dylanworks-mode>),
                        editor:      $environment-editor);
  local method edit-definitions (frame :: <basic-editor-frame>)
          let window = frame-window(frame);
          with-busy-cursor (window)
            revert-buffer(buffer)
          end;
          select-buffer-in-appropriate-window(window, buffer);
          clear-mark!(window: window);
          move-point!(interval-start-bp(buffer), window: window);
          queue-redisplay(window, $display-all, centering: 0);
          redisplay-window(window)
        end method;
  find-and-call-in-deuce-frame(method (frame)
                                 edit-definitions(frame);
                                 raise-frame(frame);
                                 deiconify-frame(frame)
                               end method,
                               buffer: buffer,
                               deuce-frame: editor-command-frame(command))
end method do-execute-command;


define deuce-command save-files;

define sealed method do-execute-command
    (editor :: <deuce-editor>, command :: <deuce-save-files-command>)
 => (#rest values)
  let owner-frame = editor-command-frame(command);
  let pathnames   = editor-command-pathnames(command);
  let reason      = editor-command-reason(command);
  let exit-label  = editor-command-exit-label(command);
  when (empty?(exit-label))
    exit-label := #f;
  end;
  // Some or all of the pathnames may not be open in buffers
  let buffers = map(curry(find-buffer-from-pathname, $environment-editor), pathnames);
  let buffers = choose(method (buffer)
                         buffer & buffer-modified?(buffer) & ~buffer-anonymous?(buffer)
                       end method, buffers);
  if (empty?(buffers))
    // No files needed to be saved, so no need to cancel
    #t
  else
    // Save the buffers
    let (buffers) = do-save-buffers-dialog(owner-frame, $environment-editor,
                                           buffers: buffers,
                                           exit-label: exit-label,
                                           reason: reason);
    select (buffers)
      #f        => #t;                // don't save, but build anyway
      #"cancel" => #f;                // neither save nor build
      otherwise =>                // try to save; don't build if it fails
        let results
          = with-asynchronous-results (id)
              find-and-call-in-deuce-frame
                (method (frame)
                   let window :: <basic-window> = frame-window(frame);
                   let buffer :: <basic-buffer> = frame-buffer(frame);
                   dynamic-bind (*editor-frame* = frame,
                                 *buffer*       = buffer)
                     // Make sure the frame is visible
                     raise-frame(frame);
                     deiconify-frame(frame);
                     let all-saved? :: <boolean> = #f;
                     block ()
                       do-save-all-files(frame, buffers, curry(display-message, window));
                       all-saved? := #t;
                     cleanup
                       //---*** Following fails due to weird compiler(?) bug.
                       // %provide-results(id, vector(all-saved?));
                       %provide-results(id, make(<vector>, size: 1, fill: all-saved?));
                       // Update window titles, in case user did a "Save As".
                       display-buffer-name-everywhere(buffer)
                     end
                   end
                 end method,
                 buffer: buffers[0])
            end;
        let all-saved? :: <boolean> = results & apply(values, results);
        unless (all-saved?)
          all-saved? := environment-question("Not all files could be saved.\n"
                                             "Build anyway?",
                                             owner: owner-frame)
        end;
        all-saved?;
    end
  end
end method do-execute-command;


/// Definitions editing buffers

define sealed class <definitions-editing-buffer>
    (<composite-buffer-mixin>,
     <basic-special-purpose-buffer>)
  sealed constant slot %project :: <project-object>,
    required-init-keyword: project:;
  sealed constant slot %definitions :: <sequence>,
    required-init-keyword: definitions:;
end class <definitions-editing-buffer>;

define method buffer-project
    (buffer :: <definitions-editing-buffer>,
     #key on-error :: false-or(singleton(#"signal")) = #f)
 => (project :: <project-object>)
  buffer.%project
end method buffer-project;

define sealed method revert-buffer
    (buffer :: <definitions-editing-buffer>,
     #key buffer-filler :: false-or(<function>) = fill-definitions-editing-buffer, major-mode)
 => (reverted? :: <boolean>)
  ignore(major-mode);
  // Reset the timestamps on the buffer
  // Since the undo history is per-section, we don't need to reset it
  let tick = tick();
  buffer-modification-tick(buffer) := tick;
  buffer-save-tick(buffer) := tick;
  // Now go read the contents of the buffer
  when (buffer-filler)
    buffer-filler(buffer)
  end;
  #t
end method revert-buffer;

define sealed method fill-definitions-editing-buffer
    (buffer :: <definitions-editing-buffer>) => ()
  local method find-section (definition)
          find-section-for-definition(buffer.%project, definition, source-type: #"newest")
        end method;
  let sections = map(find-section, buffer.%definitions);
  // 'find-section-for-definition' can return #f if no section was found
  let sections = remove!(sections, #f);
  // Some definitions might share the same section
  let sections = remove-duplicates!(sections);
  buffer-start-node(buffer) := #f;
  buffer-end-node(buffer)   := #f;
  if (empty?(sections))
    // No definitions, so make an empty section node for the buffer
    let node = make-empty-section-node(buffer);
    add-node!(buffer, node)
  else
    for (section in sections)
      let node = make-section-node(buffer, section,
                                   node-class: <dylan-definition-node>);
      add-node!(buffer, node)
    end
  end
end method fill-definitions-editing-buffer;
