Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce-based Dylan source code viewers

define sealed class <code-viewer>
    (<primary-object-interval-mixin>, <environment-deuce-gadget>)
  constant slot code-viewer-project :: <project-object>,
    required-init-keyword: project:;
  slot code-viewer-definition :: false-or(<environment-object>) = #f,
    init-keyword: definition:,
    setter: %code-viewer-definition-setter;
  slot code-viewer-source-type :: <code-viewer-source-type> = $code-viewer-default-source-type,
    setter: %code-viewer-source-type-setter;
  // We don't store a #"current-location" property in <line>s, as they
  // might have different current locations in different code-viewers.
  // Instead the code-viewer caches a pointer to the line (which is
  // flushed when we change the code-viewer's definition).
  //--- Put the current location functionality into a "<debugger-code-viewer>" subclass?
  slot code-viewer-current-location :: false-or(<source-location>) = #f,
    init-keyword: current-location:,
    setter: %code-viewer-current-location-setter;
  slot code-viewer-current-line :: false-or(<basic-line>) = #f,
    init-keyword: current-line:;
end class <code-viewer>;

define sealed domain make (singleton(<code-viewer>));
define sealed domain initialize (<code-viewer>);


define variable *code-viewer-buffer-count* :: <integer> = 0;

define sealed class <code-viewer-buffer>
    (<composite-buffer-mixin>, <non-file-buffer-mixin>, <basic-buffer>)
end class <code-viewer-buffer>;

define sealed method buffer-pathname
    (buffer :: <code-viewer-buffer>) => (pathname :: false-or(<pathname>))
  let node      = buffer-start-node(buffer);
  let section   = node    & node-section(node);
  let container = section & section-container(section);
  container & container-pathname(container)
end method buffer-pathname;

define sealed domain make (singleton(<code-viewer-buffer>));
define sealed domain initialize (<code-viewer-buffer>);

define method buffer-section-separator-style
    (buffer :: <code-viewer-buffer>) => (style :: <section-separator-style>)
  #"never"
end method buffer-section-separator-style;


define pane <code-viewer-displayer> (<displayer-mixin>)
  constant slot %project :: <project-object>,
    required-init-keyword: project:;
  constant slot %read-only? :: <boolean> = #t,
    init-keyword: read-only?:;
  pane %code-viewer (pane)
    begin
      let window = make(<code-viewer>,		// no tooltip, it's too intrusive
                        project: pane.%project);
      dynamic-bind (*editor-frame* = window)
        inc!(*code-viewer-buffer-count*);
        let name   = format-to-string("Gadget %d", *code-viewer-buffer-count*);
        let buffer = make-empty-buffer(<code-viewer-buffer>,
                                       name: name,
                                       major-mode: find-mode(<dylanworks-mode>),
                                       read-only?: pane.%read-only?,
                                       anonymous?: #t);
        dynamic-bind (*buffer* = buffer)
          select-buffer(window, buffer)
        end
      end;
      window
    end;
  pane %edit-source-button (pane)
    make(<push-button>,
         label: $edit-source-bitmap,
         documentation: "Edit newest source",
         activate-callback:
           method (button)
             // let frame  = sheet-frame(button);
             // let viewer = pane.%code-viewer;
             // let object = code-viewer-definition(viewer);
             // frame-edit-object(frame, object)
             let frame      = sheet-frame(button);
             let viewer     = pane.%code-viewer;
             let definition = code-viewer-definition(viewer);
             when (definition)
               // This is more complicated than it should be so that we
               // get to the right interactive source if somebody compiled
               // a definition from an editor buffer
               //---*** 'frame-edit-object' should do this somehow...
               let project  = ensure-frame-project(frame);
               let location = environment-object-source-location(project, definition);
               let record   = location & source-location-source-record(location);
               let library  = record & find-source-record-library(project, record);
               let project  = library & library-project(project, library);
               let section
                 = project
                   & find-section-for-definition(project, definition, source-type: #"newest");
               let buffer
                 = section & section-home-buffer(section, editor: $environment-editor);
               let locator = buffer & buffer-locator(buffer);
               if (locator)
                 dynamic-bind (*buffer* = buffer)
                   let start-line
                     = if (code-viewer-current-line(viewer))
                         line->line-index(buffer, code-viewer-current-line(viewer))
                       else
                         //--- The '+ 1' to get past the Dylan divider line...
                         line->line-index(buffer, section-start-line(section)) + 1
                       end;
                 editor-open-file(locator, start-line: start-line)
                 end
               else
                 frame-edit-object(frame, definition)
               end
             end
           end method);
  pane %source-type-box (pane)
    make(<radio-box>,
         value: $code-viewer-default-source-type,
         value-changed-callback:
           method (box)
             let frame  = sheet-frame(box);
             let viewer = pane.%code-viewer;
             code-viewer-source-type(viewer) := gadget-value(box);
             update-current-location-gadget(frame, pane, code-viewer-definition(viewer))
           end method,
         child: make(<row-layout>,
                     spacing: 0,
                     children:
                       vector(pane.%newest-source-button
                                := make(<radio-button>,
                                        label: $current-source-bitmap | "N",
                                        value: #"newest", id: #"newest",
                                        button-style: #"push-button",
                                        documentation: "Show the newest source"),
                              pane.%canonical-source-button
                                := make(<radio-button>,
                                        label: $canonical-source-bitmap | "C",
                                        value: #"canonical", id: #"canonical",
                                        button-style: #"push-button",
                                        documentation: "Show the canonical source"))));
  pane %current-location-gadget (pane)
    make(<text-field>,
         read-only?: #t, text: "",
         documentation: "Source location");
  slot %newest-source-button    = #f;
  slot %canonical-source-button = #f;
  layout (pane)
    vertically (spacing: 2)
      if (environment-default-copy-sources())
        horizontally (spacing: 8)		// same as $tool-bar-group-spacing
          pane.%source-type-box;
          pane.%edit-source-button;
          pane.%current-location-gadget;
        end;
      else
        horizontally (spacing: 8)		// same as $tool-bar-group-spacing
          pane.%edit-source-button;
          pane.%current-location-gadget;
        end;
      end;
      scrolling (scroll-bars: #"both")
        pane.%code-viewer
      end;
    end;
end pane <code-viewer-displayer>;

define sideways method make-code-viewer
    (#rest initargs,
     #key project, frame, read-only? = #t,
          foreground, background, text-style,
          class = <code-viewer>, scroll-bars = #"vertical", #all-keys)
 => (displayer :: <code-viewer-displayer>, viewer :: <code-viewer>)
  ignore(frame, foreground, background, text-style, scroll-bars);
  let displayer
    = make(<code-viewer-displayer>,
           project: project, read-only?: read-only?);
  values(displayer, displayer.%code-viewer)
end method make-code-viewer;

define method displayer-default-input-focus
    (displayer :: <code-viewer-displayer>)
 => (code-viewer :: <code-viewer>)
  displayer.%code-viewer
end method displayer-default-input-focus;

define method pane-sheet-with-selection
    (displayer :: <code-viewer-displayer>)
 => (sheet :: <sheet>)
  displayer.%code-viewer
end method pane-sheet-with-selection;


// When the contents of the code viewer are changed, update the pane contents.
// Note that we call on Deuce to read in and sectionize the file containing
// the requested definition.  This means that we get a free ride for such
// things as active breakpoints, etc.
define method code-viewer-definition-setter
    (definition :: false-or(<environment-object>), window :: <code-viewer>,
     #key redisplay? = #t)
 => (definition :: false-or(<environment-object>))
  // Don't do anything if the definition is unchanged
  unless (definition = code-viewer-definition(window))
    window.%code-viewer-definition := definition;
    update-code-viewer-definition(window, redisplay?: redisplay?)
  end;
  definition
end method code-viewer-definition-setter;

define method update-code-viewer-definition
    (window :: <code-viewer>, #key redisplay? = #t) => ()
  with-editor-state-bound (buffer = window)
    let project    = code-viewer-project(window);
    let definition = code-viewer-definition(window);
    let section
      = definition
        & find-section-for-definition(project, definition,
                                      source-type: code-viewer-source-type(window));
    let old-section
      = buffer-start-node(buffer) & node-section(buffer-start-node(buffer));
    // Two different stack frames might be running the same function,
    // but their definition objects will be different (since stack frames
    // themselves get treated as definitions), so we also avoid doing
    // anything if the section is unchanged
    unless (section == old-section)
      // Reset the current location and line
      window.%code-viewer-current-location := #f;
      code-viewer-current-line(window)     := #f;
      // Build the code viewer's buffer up from scratch
      // Start by cleaning out all current nodes
      for (node = buffer-start-node(buffer) then buffer-start-node(buffer),
           while: node)
        let section = node-section(node);
        when (section)
          section-nodes(section) := remove!(section-nodes(section), node);
        end;
        remove-node!(buffer, node)
      end;
      let node
        = if (section)
            make-section-node(buffer, section, node-class: <dylan-definition-node>)
          else
            make-empty-section-node(buffer)
          end;
      node-buffer(node)         := buffer;
      buffer-start-node(buffer) := node;
      buffer-end-node(buffer)   := node;
      // Ensure the window gets completely redisplayed
      initialize-redisplay-for-buffer(window, buffer);
      queue-redisplay(window, $display-all);
      when (redisplay? & sheet-mapped?(window))
        redisplay-window(window)
      end
    end
  end
end method update-code-viewer-definition;

// NB: this assumes that the location is in the definition given by
// 'code-viewer-definition', but nothing checks for it!
define method code-viewer-current-location-setter
    (location :: false-or(<source-location>), window :: <code-viewer>)
 => (location :: false-or(<source-location>))
  // Don't do anything if the location is unchanged
  unless (location = code-viewer-current-location(window))
    with-editor-state-bound (buffer = window)
      // Remember the old line so we can clear its marker,
      // then find the line that corresponds to the new location
      let project  = code-viewer-project(window);
      let old-line = code-viewer-current-line(window);
      let record   = location & source-location-source-record(location);
      let (section, new-line)
        = record & find-section-for-source-location(project, record, location);
      ignore(section);
      window.%code-viewer-current-location := location;
      code-viewer-current-line(window)     := new-line;
      // Flush the marker for the old line, if it's visible
      let dline = old-line & find-display-line(window, old-line);
      when (dline)
        queue-redisplay(window, $display-line, line: old-line, index: 0);
        redisplay-window(window)
      end;
      // Move the point to ensure the new line gets scrolled into view
      when (new-line)
        move-point!(new-line, index: 0, window: window);
        queue-redisplay(window, $display-line, line: new-line, index: 0, centering: 0);
        redisplay-window(window)
      end
    end
  end;
  location
end method code-viewer-current-location-setter;

define method code-viewer-source-type-setter
    (source-type :: <code-viewer-source-type>, window :: <code-viewer>)
 => (source-type :: <code-viewer-source-type>)
  window.%code-viewer-source-type := source-type;
  update-code-viewer-definition(window);
  source-type
end method code-viewer-source-type-setter;


define method deuce/line-breakpoint-icon
    (mode :: <dylanworks-mode>, line :: <basic-line>, window :: <code-viewer>)
 => (image :: false-or(<object>))
  if (line == code-viewer-current-line(window))
    //--- We could distinguish top-of-stack from other frames
    $current-location
  else
    next-method()
  end
end method deuce/line-breakpoint-icon;

define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <code-viewer>,
     line :: <basic-line>, type == deuce/<dylan-breakpoint>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
          menu-function = dylanworks-breakpoint-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, line, type,
        menu-function: menu-function, keys)
end method do-handle-presentation-event;

define method do-handle-presentation-event
    (mode :: <dylanworks-mode>, window :: <code-viewer>,
     nothing, type == <blank-area>,
     #rest keys,
     #key bp, x, y, button, modifiers, event-type,
          menu-function = dylanworks-default-editor-menu) => ()
  ignore(bp, x, y, button, modifiers, event-type);
  apply(next-method, mode, window, nothing, type,
        menu-function: menu-function, keys)
end method do-handle-presentation-event;


/// Environment frame protocol

define constant $code-viewer-no-source-message :: <byte-string>
  = "No source available";

define constant $code-viewer-interactive-code-message :: <byte-string>
  = "Interactive source code";

define method refresh-frame-property-page
    (frame :: <environment-frame>, displayer :: <code-viewer-displayer>,
     definition :: <environment-object>, type == #"source",
     #key clean? = #f, new-thread? = #t) => ()
  let window   = displayer.%code-viewer;
  let changed? = (code-viewer-definition(window) ~== definition);
  let project  = ensure-frame-project(frame);
  let location = instance?(definition, <stack-frame-object>)
                 & stack-frame-source-location(project, definition);
  code-viewer-definition(window, redisplay?: #f) := definition;
  code-viewer-current-location(window)           := location;
  redisplay-window(window);
  let clean? = ~displayer.displayer-valid? | clean?;
  when (clean? | changed?)
    update-current-location-gadget(frame, displayer, definition)
  end
end method refresh-frame-property-page;

define method update-current-location-gadget
    (frame :: <environment-frame>, displayer :: <code-viewer-displayer>,
     definition :: false-or(<environment-object>)) => ()
  when (definition)
    let window   = displayer.%code-viewer;
    let type     = code-viewer-source-type(window);
    let project  = ensure-frame-project(frame);
    let location = environment-object-source-location(project, definition);
    let record   = location & source-location-source-record(location);
    let interactive?
      = select (record by instance?)
          <file-source-record> => #f;
          <source-record>      => #t;
          otherwise            => #f;
        end;
    // This next bit is cribbed from 'find-section-for-source-location'
    let library = record  & find-source-record-library(project, record);
    let project = library & library-project(project, library);
    let (newest, canonical)
      = case
          interactive? =>
            // If it's a piece of interactive code that came from using c-sh-C
            // out of a file buffer, go to the file buffer
            let definition = code-viewer-definition(window);
            let section
              = definition
                  & find-section-for-definition(project, definition, source-type: #"newest");
            let buffer
              = section & section-home-buffer(section, editor: $environment-editor);
            let container
              = buffer & file-buffer?(buffer) & buffer-source-container(buffer);
            values(container & as(<string>, container-pathname(container)),
                   #f);
          record =>
            let locator = source-record-location(record);
            let newest
              = locator & merge-locators(locator, project-directory(project));
            let canonical = newest & project-canonical-filename(project, newest);
            values(newest, canonical);
          otherwise =>
            values(#f, #f);
        end;
    let newest?    = newest    & file-exists?(newest);
    let canonical? = canonical & file-exists?(canonical);
    let (filename, edit?)
      = select (type)
          #"newest"    => values(newest, newest?);
          #"canonical" => if (canonical?) values(canonical, canonical?)
                          else values(newest, newest?) end;
        end;
    displayer.%edit-source-button
      & (gadget-enabled?(displayer.%edit-source-button)      := edit?);
    displayer.%newest-source-button
      & (gadget-enabled?(displayer.%newest-source-button)    := newest?);
    displayer.%canonical-source-button
      & (gadget-enabled?(displayer.%canonical-source-button) := canonical?);
    gadget-text(displayer.%current-location-gadget)
      := case
           edit?        => as(<string>, filename);
           interactive? => $code-viewer-interactive-code-message;
           otherwise    => $code-viewer-no-source-message;
         end
  end
end method update-current-location-gadget;

// Default method
define method refresh-frame-property-page
    (frame :: <environment-frame>, displayer :: <code-viewer-displayer>,
     definition == #f, type == #"source",
     #key clean?, new-thread? = #t) => ()
  ignore(clean?);
  let window = displayer.%code-viewer;
  code-viewer-definition(window, redisplay?: #f) := #f;
  code-viewer-current-location(window)           := #f;
  redisplay-window(window);
  displayer.%edit-source-button
    & (gadget-enabled?(displayer.%edit-source-button) := #f);
  gadget-text(displayer.%current-location-gadget)
    := $code-viewer-no-source-message;
end method refresh-frame-property-page;
