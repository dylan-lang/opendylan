/// "File" command table

define command-table *file-command-table* (*global-command-table*)
  menu-item "New..."     = new-document,
    documentation: "Make a new document";
  menu-item "Open..."    = open-document,
    documentation: "Open an existing document";
  menu-item "Close"      = close-document,
    documentation: "Close the current document";
  separator;
  menu-item "Save"       = save-document,
    documentation: "Save the current document";
  menu-item "Save As..." = save-document-as,
    documentation: "Save to a new document";
  menu-item "Save All"   = save-all-documents,
    documentation: "Save all changed document";
  separator;
  menu-item "Exit"       = exit-frame,
    documentation: "Exit the application";
end command-table *file-command-table*;


/// "Edit" command table

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Undo"  = command-undo,
    documentation: "Undo the last change";
  menu-item "Redo"  = command-redo,
    documentation: "Redo the last undone change";
  separator;
  menu-item "Cut"   = clipboard-cut,
    documentation: "Cut to the clipboard";
  menu-item "Copy"  = clipboard-copy,
    documentation: "Copy to the clipboard";
  menu-item "Paste" = clipboard-paste,
    documentation: "Paste from the clipboard";
end command-table *edit-command-table*;


/// "Help" command table

define command-table *help-command-table* (*global-command-table*)
  menu-item "Topics..." = <help-on-topics>,
    documentation: "Show documentation for a topic";
  separator;
  menu-item "About..."  = <help-on-version>,
    documentation: "Show the About box for this application";
end command-table *help-command-table*;


/// Application command table

define command-table *template-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *template-command-table*;


/// Application frame

//*** FILL THE REST OF THIS IN ***
define class <template-document> (<object>)
  slot document-pathname = #f,
    init-keyword: pathname:;
  slot document-modified? :: <boolean> = #f,
    init-keyword: modified?:;
end class <template-document>;


//*** FILL THE REST OF THIS IN ***
define frame <template-frame> (<simple-frame>)
  //*** FILL IN THE FRAMES PANES ***
  pane main-pane (frame)
    make(<text-editor>, enabled?: #f,
         width: 200, height: 100);
  layout (frame)
    vertically () frame.main-pane end;
  command-table (frame)
    *template-command-table*;
  //*** MAYBE INCLUDE A TOOL BAR ***
  /* tool-bar (frame)
       make-command-tool-bar(frame-manager(frame), frame); */
  status-bar (frame)
    make(<status-bar>);
  keyword title: = $application-name;
end frame <template-frame>;


/// File open commands

define method frame-document-setter
    (document :: false-or(<template-document>), frame :: <template-frame>)
 => (document :: false-or(<template-document>))
  gadget-value(frame.main-pane) := "";
  gadget-enabled?(frame.main-pane) := (document & #t);
  next-method()
end method frame-document-setter;

define method new-document
    (frame :: <template-frame>) => (document :: <template-document>)
  //*** FILL THE REST OF THIS IN ***
  frame-document(frame) := make(<template-document>, modified?: #t);
end method new-document;

define method open-document
    (frame :: <template-frame>, #key document) => (document :: <template-document>)
  let file = choose-file(direction: #"input", owner: frame);
  //*** FILL THE REST OF THIS IN ***
  make(<template-document>,
       pathname: file)
end method open-document;

define method close-document
    (frame :: <template-frame>, #key document) => ()
  let document = frame-document(frame);
  when (document)
    let (save?, close?)
      = if (document-modified?(document))
	  let (value, type)
	    = notify-user("The document is modified; save it first?",
			  owner: frame,
			  style: #"warning", exit-style: #"yes-no-cancel");
	  values(value == #t, type ~== #"cancel")
	else
	  values(#f, #t)
	end;
    when (save?)
      save-document(frame, document: document)
    end;
    when (close?)
      //*** FILL THE REST OF THIS IN ***
      frame-document(frame) := #f
    end
  end
end method close-document;


/// File save commands

define method save-document
    (frame :: <template-frame>, #key document) => ()
  let document = document | frame-document(frame);
  when (document & document-modified?(document))
    //*** FILL THE REST OF THIS IN ***
  end
end method save-document;

define method save-document-as
    (frame :: <template-frame>, #key document) => ()
  let document = document | frame-document(frame);
  if (document)
    let file = choose-file(direction: #"output", owner: frame);
    if (file)
      document-pathname(document) := file;
      //*** FILL THE REST OF THIS IN ***
    end
  else
    notify-user("You must create a new document or open an existing one first",
		owner: frame,
		style: #"information", exit-style: #"ok");
  end
end method save-document-as;

define method save-all-documents
    (frame :: <template-frame>) => ()
  //*** FILL THE REST OF THIS IN ***
end method save-all-documents;

define method note-document-changed
    (frame :: <template-frame>, document :: <template-document>) => ()
  document-modified?(document) := #t;
  //*** FILL THE REST OF THIS IN ***
end method note-document-changed;


/// Undo/redo commands

define method command-undo
    (frame :: <template-frame>) => ()
  //*** FILL THE REST OF THIS IN ***
end method command-undo;

define method command-redo
    (frame :: <template-frame>) => ()
  //*** FILL THE REST OF THIS IN ***
end method command-redo;


/// Clipboard commands

define method clipboard-cut
    (frame :: <template-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO CUT FROM ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      add-clipboard-data(clipboard, gadget-value(gadget))
    end
  end
  //*** NOW REMOVE THE DATA FROM THE GADGET ***
end method clipboard-cut;

define method clipboard-copy
    (frame :: <template-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO COPY FROM ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      add-clipboard-data(clipboard, gadget-value(gadget))
    end
  end
end method clipboard-copy;

define method clipboard-paste
    (frame :: <template-frame>) => ()
  let gadget = frame.main-pane;	// *** GADGET TO PASTE INTO ***
  with-clipboard (clipboard = gadget)
    when (clipboard)
      let text = get-clipboard-data-as(<string>, clipboard);
      when (text)
	gadget-value(gadget) := text
      end
    end
  end
end method clipboard-paste;


/// Help commands

define method do-execute-command
    (frame :: <template-frame>, command :: <help-on-topics>) => ()
  //*** REMOVE THIS IF YOU ADD A HELP SOURCE ***
  notify-user("No help available.", owner: frame)
end method do-execute-command;

define method do-execute-command
    (frame :: <template-frame>, command :: <help-on-version>) => ()
  //*** PUT UP A NICE ABOUT BOX ***
  notify-user(application-full-name(), owner: frame)
end method do-execute-command;


/// Start the template

define method start-template
    () => ()
  start-frame(make(<template-frame>));
end method start-template;
