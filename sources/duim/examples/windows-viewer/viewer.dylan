Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <viewer-options> (<object>)
  constant slot viewer-show-invisible? :: <boolean> = #f;
end class <viewer-options>;

define frame <windows-viewer> (<simple-frame>)
  constant slot viewer-options :: <viewer-options> = make(<viewer-options>);
  constant slot viewer-hook-dll :: <HMODULE>,
    required-init-keyword: hook-dll:;
  pane viewer-tree-control (frame)
    make(<tree-control>,
	 roots: vector($desktop-window),
	 depth: 1,
	 label-key: window-name,
	 children-predicate: window-has-children?,
	 children-generator: curry(viewer-window-children, frame),
	 always-show-selection?: #t,
	 value-changed-callback:
	   method (gadget :: <tree-control>)
	     note-selected-window(frame, gadget.gadget-value)
	   end);
  pane viewer-info (frame)
    make(<text-editor>,
	 text-style: make(<text-style>, family: #"fix"),
	 read-only?: #t);
  pane viewer-styles (frame)
    make(<text-editor>,
	 text-style: make(<text-style>, family: #"fix"),
	 read-only?: #t);
  pane messages-pane (frame)
    make(<messages-pane>,
	 hook-dll: frame.viewer-hook-dll);
  pane viewer-window-properties (frame)
    make(<tab-control>,
	 pages: vector(make(<tab-control-page>,
			    label: "General",
			    child: frame.viewer-info),
		       make(<tab-control-page>,
			    label: "Styles",
			    child: frame.viewer-styles),
		       make(<tab-control-page>,
			    label: "Messages",
			    child: frame.messages-pane)));
  layout (frame)
    make(<row-splitter>,
	 ratios: #[1, 4],
	 children: vector(frame.viewer-tree-control, 
			  frame.viewer-window-properties));
  command-table (frame)
    *windows-viewer-command-table*;
  status-bar (frame)
    make(<status-bar>);
  input-focus (frame)
    frame.viewer-tree-control;
  keyword title: = $application;
  keyword width:  = 800;
  keyword height: = 700;
end frame <windows-viewer>;

define method handle-event
    (frame :: <windows-viewer>, event :: <frame-created-event>) => ()
  next-method();
  update-window-info(frame, $desktop-window)
end method handle-event;

define method refresh-windows-viewer
    (frame :: <windows-viewer>) => ()
  let tree = frame.viewer-tree-control;
  let handle = tree.gadget-value;
  update-gadget(tree);
  handle & note-selected-window(frame, handle)
end method refresh-windows-viewer;

define method about-windows-viewer
    (frame :: <windows-viewer>) => ()
  let message
    = format-to-string("%s %s\n%s\n",
		       $application, $version,
		       $copyright);
  notify-user(message, owner: frame)
end method about-windows-viewer;

define method start-windows-viewer () => ()
  let hook-dll :: false-or(<HMODULE>) = #f;
  block ()
    hook-dll := LoadLibrary($dll-name);
    create-hook-thread();
    start-frame(make(<windows-viewer>, hook-dll: hook-dll))
  cleanup
    uninstall-windows-hooks();
    hook-dll & FreeLibrary(hook-dll)
  end
end method start-windows-viewer;

define method viewer-window-children
    (frame :: <windows-viewer>, handle :: <HWND>)
 => (children :: <sequence>)
  let all-children = window-children(handle);
  let show-invisible? = frame.viewer-options.viewer-show-invisible?;
  let children :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  for (child :: <HWND> in all-children)
    if (show-invisible? | IsWindowVisible(child))
      add!(children, child)
    end
  end;
  children
end method viewer-window-children;

define method note-selected-window
    (frame :: <windows-viewer>, handle :: <HWND>) => ()
  update-window-info(frame, handle);
  update-window-styles(frame, handle)
end method note-selected-window;


/// Window information

define method update-window-info
    (frame :: <windows-viewer>, handle :: <HWND>) => ()
  let stream = make(<string-stream>, direction: #"output");
  print-window-info(stream, handle);
  let info = stream-contents(stream);
  gadget-text(frame.viewer-info) := info;
  pane-handle(frame.messages-pane) := handle
end method update-window-info;

define method print-window-info
    (stream :: <stream>, handle :: <HWND>) => ()
  let (style, extstyle) = window-styles(handle);
  format(stream, "Label:      %=\n",
	 window-label(handle) | "");
  format(stream, "Handle:     %s\n",
	 number-to-hex-string(handle.pointer-address));
  format(stream, "Type:       %s\n",
	 window-class-description(handle));
  format(stream, "Id:         %s\n",
	 number-to-hex-string(window-id(handle)));
  format(stream, "Enabled?:   %s\n",
	 if (IsWindowEnabled(handle)) "yes" else "no" end);
  format(stream, "Visible?:   %s\n",
	 case
	   IsIconic(handle)        => "minimized";
	   IsWindowVisible(handle) => "yes";
	   otherwise               => "no";
	 end);
  format(stream, "Maximized?: %s\n",
	 if (IsZoomed(handle)) "yes" else "no" end);
  let (left, top, right, bottom) = get-window-edges(handle);
  format(stream, "Geometry:   %dx%d at %d,%d\n",
	 right - left, bottom - top, left, top);
  let (left, top, right, bottom) = get-client-edges(handle);
  format(stream, "Client:     %dx%d at %d,%d\n",
	 right - left, bottom - top, left, top);
  format(stream, "\n");
  let owner = window-owner(handle);
  let parent = window-parent(handle);
  let next-window = window-next-window(handle);
  let previous-window = window-previous-window(handle);
  let top-child = window-top-child(handle);
  format(stream, "Owner:      %s\n",
	 if (owner) window-name(owner) else "" end);
  format(stream, "Parent:     %s\n",
	 if (parent) window-name(parent) else "" end);
  format(stream, "Previous:   %s\n",
	 if (previous-window) window-name(previous-window) else "" end);
  format(stream, "Next:       %s\n",
	 if (next-window) window-name(next-window) else "" end);
  format(stream, "Top child:  %s\n",
	 if (top-child) window-name(top-child) else "" end);
end method print-window-info;


/// Window styles

define method update-window-styles
    (frame :: <windows-viewer>, handle :: <HWND>) => ()
  let stream = make(<string-stream>, direction: #"output");
  print-window-styles(stream, handle);
  let styles = stream-contents(stream);
  gadget-text(frame.viewer-styles) := styles;
  pane-handle(frame.messages-pane) := handle
end method update-window-styles;

define method print-window-styles
    (stream :: <stream>, handle :: <HWND>) => ()
  local
    method value-label
	(value) => (label :: <string>)
      select (value)
	#f        => "No";
	#t        => "Yes";
	<string>  => value;
	otherwise => format-to-string("%s", value)
      end
    end method value-label,

    method name-and-value-maximums
	(names-and-values :: <sequence>)
     => (max-name :: <integer>, max-value :: <integer>)
      let max-name :: <integer> = 0;
      let max-value :: <integer> = 0;
      for (i :: <integer> from 0 below names-and-values.size by 2)
	let name   = names-and-values[i];
	let value  = names-and-values[i + 1];
	max-name  := max(max-name, name.size);
	max-value := max(max-value, value-label(value).size)
      end;
      values(max-name, max-value)
    end method name-and-value-maximums,

    method print-names-and-values
	(title1 :: <string>, names-and-values1 :: <sequence>,
	 title2 :: <string>, names-and-values2 :: <sequence>,
	 title3 :: <string>, names-and-values3 :: <sequence>)
     => ()
      let inner-spacing = 2;
      let outer-spacing = 3;
      let size1 :: <integer> = names-and-values1.size;
      let size2 :: <integer> = names-and-values2.size;
      let size3 :: <integer> = names-and-values3.size;
      let (max-name1, max-value1) = name-and-value-maximums(names-and-values1);
      let (max-name2, max-value2) = name-and-value-maximums(names-and-values2);
      let (max-name3, max-value3) = name-and-value-maximums(names-and-values3);
      let column1 :: <integer> = 0;
      let column2 :: <integer> = column1 + max(title1.size, max-name1 + max-value1 + inner-spacing) + outer-spacing;
      let column3 :: <integer> = column2 + max(title2.size, max-name2 + max-value2 + inner-spacing) + outer-spacing;
      let column4 :: <integer> = column3 + max(title3.size, max-name3 + max-value3 + inner-spacing) + outer-spacing;
      let spaces = make(<byte-string>, size: column4, fill: ' ');
      let total-size :: <integer> = max(size1, size2, size3);
      write(stream, title1);
      write(stream, spaces, end: column2 - column1 - title1.size);
      write(stream, title2);
      if (size3 > 0)
	write(stream, spaces, end: column3 - column2 - title2.size);
	write(stream, title3)
      end;
      new-line(stream);
      for (i :: <integer> from 0 below total-size by 2)
	let last-column :: <integer> = 0;
	if (i + 1 < size1)
	  let name1  = names-and-values1[i];
	  let value1 = names-and-values1[i + 1];
	  let value1-label = value-label(value1);
	  write(stream, name1);
	  write(stream, spaces, end: max-name1 - name1.size + inner-spacing);
	  write(stream, value1-label);
	  last-column := max-name1 + value1-label.size + inner-spacing;
	end;
	if (i + 1 < size2)
	  write(stream, spaces, end: column2 - last-column);
	  let name2  = names-and-values2[i];
	  let value2 = names-and-values2[i + 1];
	  let value2-label = value-label(value2);
	  write(stream, name2);
	  write(stream, spaces, end: max-name2 - name2.size + inner-spacing);
	  write(stream, value2-label);
	  last-column := column2 + max-name2 + value2-label.size + inner-spacing;
	end;
	if (i + 1 < size3)
	  write(stream, spaces, end: column3 - last-column);
	  let name3  = names-and-values3[i];
	  let value3 = names-and-values3[i + 1];
	  let value3-label = value-label(value3);
	  write(stream, name3);
	  write(stream, spaces, end: max-name3 - name3.size + inner-spacing);
	  write(stream, " ");
	  write(stream, value3-label)
	end;
	new-line(stream)
      end
    end method print-names-and-values;
  format(stream, "Label:    %=\n",
	 window-label(handle) | "");
  format(stream, "Handle:   %s\n",
	 number-to-hex-string(handle.pointer-address));
  format(stream, "Type:     %s\n",
	 window-class-description(handle));
  format(stream, "Class:    %s\n",
	 window-class-name(handle));
  format(stream, "\n");
  let (style, ext-style) = window-raw-styles(handle);
  let title1
    = format-to-string("Styles: [%s]",
		       number-to-hex-string(style));
  let title2
    = format-to-string("Extended styles: [%s]",
		       number-to-hex-string(ext-style));
  let title3
    = format-to-string("%s styles:",
		       window-class-name(handle));
  print-names-and-values
    (title1, window-styles(handle),
     title2, window-extended-styles(handle),
     title3, window-class-styles(handle))
end method print-window-styles;
