Module:    environment-interactor
Synopsis:  Environment interactor
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A pane upon which we can build Interactors

// The children of an <interactor-control> will be a set of <presentation>s
define pane <interactor-control> (<standard-input-mixin>)
  slot interactor-popup-menu-callback = #f,
    init-keyword: popup-menu-callback:;
  //---*** The next two should either use a pinboard layout,
  //---*** or we should fix row/column layouts to be smart when adding to the end
  slot %highlighted-presentation = #f;
  slot interactor-next-variable-number = 1;
  pane %presentations (pane)
    make(<column-layout>);
  pane %prompts (pane)
    make(<column-layout>, width: 20, min-width: 20, max-width: 20);
  layout (pane)
    begin
      let hscroll = make(<scroll-bar>, orientation: #"horizontal");
      let vscroll = make(<scroll-bar>, orientation: #"vertical");
      make(<table-layout>,
	   children: vector(// The first row...
			    make(<viewport>, 
				 child: pane.%prompts,
				 width: 20, min-width: 20, max-width: 20,
				 vertical-scroll-bar: vscroll),
			    make(<viewport>, 
				 child: pane.%presentations,
				 vertical-scroll-bar:   vscroll,
				 horizontal-scroll-bar: hscroll),
			    vscroll,
			    // The second row...
			    make(<null-pane>,
				 width:  1, min-width:  1, max-width:  1,
				 height: 1, max-height: 1, max-height: 1),
			    hscroll,
			    make(<null-pane>,
				 width:  1, min-width:  1, max-width:  1,
				 height: 1, max-height: 1, max-height: 1)),
	     columns: 3)
    end;
end pane <interactor-control>;

define method interactor-last-values
    (control :: <interactor-control>) 
 => (values :: false-or(<sequence>))
  //---*** Should return the last 'values'
  #f
end method interactor-last-values;

/// Simple "presentations"

// The children of a <presentation> will be a set of <text-line>s
define sealed class <presentation>
    (<standard-input-mixin>, <column-layout>)
  slot presentation-object,
    required-init-keyword: object:;
  slot presentation-type,
    required-init-keyword: type:;
end class <presentation>;

//---*** Need to insert the prompt in the prompt strip
define method present
    (string :: <string>, interactor :: <interactor-control>,
     #key object = #f, type = <object>,
          foreground, background, text-style, prompt)
 => (presentation :: <presentation>)
  let s = make(<string-stream>, contents: string, direction: #"input");
  let lines = make(<stretchy-vector>);
  while (stream-input-available?(s))
    let line = read-line(s);
    add!(lines, make(<text-line>,
		     string: line,
		     foreground: foreground,
		     background: background,
		     text-style: text-style))
  end;
  // Make the presentation and add it to the parent
  let presentations = interactor.%presentations; 
  let presentation = make(<presentation>,
			  children: lines,
			  object: object, type: type);
  add-child(presentations, presentation, index: #"end");
  sheet-mapped?(presentation) := sheet-mapped?(presentations);
  //---*** The following is way too slow -- we should use a pinboard layout,
  //---*** or we should fix row/column layout to be faster
  // Put it in the right place
  let space-req = compose-space(presentation);
  let width  = space-requirement-width(space-req);
  let height = space-requirement-height(space-req);
  let (l, t, r, b) = sheet-edges(presentations);
  let (x, y) = values(l, b + 2);
  set-sheet-edges(presentation, x, y, x + width, y + height);
  // Hack to get the screen to redisplay
  clear-box*(presentations, sheet-region(presentations));
  // Force relayout of all items, then notify up the sheet tree
  // if anything changed
  relayout-parent(presentations);
  repaint-sheet(presentations, $everywhere);
  force-display(presentations);
  presentation
end method present;

//--- Is this good enough?
define open generic handle-presentation-event
    (sheet :: <sheet>, object, type, gesture, #key presentation, event) => ();

define method handle-presentation-event
    (sheet :: <sheet>, object, type, gesture, #key presentation, event) => ()
  #f
end method handle-presentation-event;

define method handle-event
    (sheet :: <presentation>, event :: <button-press-event>) => ()
  let interactor = find-parent-of-class(sheet, <interactor-control>);
  interactor.%highlighted-presentation := sheet;
  highlight-presentation(sheet, #t)	// on...
end method handle-event;

define method handle-event
    (sheet :: <presentation>, event :: <button-release-event>) => ()
  let interactor = find-parent-of-class(sheet, <interactor-control>);
  assert(sheet == interactor.%highlighted-presentation);
  interactor.%highlighted-presentation := #f;
  highlight-presentation(sheet, #f);	// off...
  handle-presentation-event(interactor,
                            presentation-object(sheet), presentation-type(sheet),
                            make(<pointer-gesture>,
                                 button: event-button(event),
                                 modifier-state: event-modifier-state(event)),
                            presentation: sheet, event: event)
end method handle-event;

define method handle-event
    (sheet :: <presentation>, event :: <pointer-drag-event>) => ()
  let interactor = find-parent-of-class(sheet, <interactor-control>);
  unless (sheet == interactor.%highlighted-presentation)
    interactor.%highlighted-presentation := sheet;
    // Pointer exit event will have already unhighlighted the old one...
    highlight-presentation(sheet, #t)	// on...
  end
end method handle-event;

define method handle-event
    (sheet :: <presentation>, event :: <pointer-exit-event>) => ()
  let interactor = find-parent-of-class(sheet, <interactor-control>);
  let old = interactor.%highlighted-presentation;
  when (old)
    interactor.%highlighted-presentation := #f;
    highlight-presentation(old, #f)	// off...
  end
end method handle-event;

define method highlight-presentation
    (presentation :: <presentation>, highlight? :: <boolean>) => ()
  ignore(highlight?);
  with-sheet-medium (medium = presentation)
    let (left, top, right, bottom) = box-edges(presentation);
    with-drawing-options (medium, brush: $xor-brush)
      draw-rectangle(medium, left, top, right, bottom, filled?: #t)
    end
  end
end method highlight-presentation;


/// Simple text lines

define sealed class <text-line>
    (<standard-repainting-mixin>,
     <cached-space-requirement-mixin>,
     <basic-sheet>)
  slot %string :: <string>,
    required-init-keyword: string:;
end class <text-line>;

define method do-compose-space
    (pane :: <text-line>, #key width, height)
 => (space-requirement :: <space-requirement>)
  ignore(width, height);
  with-sheet-medium (medium = pane)
    let text-style = get-default-text-style(port(pane), pane);
    let (width, height)
      = text-size(medium, pane.%string, text-style: text-style, do-newlines?: #f);
    make(<space-requirement>,
	 width: width, height: height)
  end
end method do-compose-space;

define method handle-repaint
    (sheet :: <text-line>, medium :: <medium>, region :: <region>) => ()
  with-sheet-medium (medium = sheet)
    with-drawing-options (medium,
			  brush: default-foreground(sheet),
			  text-style: default-text-style(sheet))
      draw-text(medium, sheet.%string, 0, 0, align-y: #"top")
    end
  end
end method handle-repaint;


/// Support for Dylan forms and objects

//---*** We need a class called <source-form> which is the superclass of
//---*** both <interactor-source-form> and all <definition> classes
define class <interactor-source-form> (<compiler-object>)
  slot %string, init-keyword: string:;	//--- kludge
  slot %form, init-keyword: form:;	//--- kludge
end class <interactor-source-form>;

define open generic frame-reexecute-source-form
    (frame :: <frame>, form :: <interactor-source-form>) => ();

define method frame-reexecute-source-form
    (frame :: <frame>, form :: <interactor-source-form>) => ()
  #f
end method frame-reexecute-source-form;

define open generic frame-describe-object
    (frame :: <frame>, object :: <application-object>) => ();

define method frame-describe-object
    (frame :: <frame>, object :: <application-object>) => ()
  #f
end method frame-describe-object;

define constant $menu-gesture = make(<gesture>, button: $right-button);
define constant $describe-gesture = make(<gesture>, button: $middle-button);

/*---*** This is what we really want
define method handle-presentation-event
    (sheet :: <interactor-control>, object, type :: subclass(<environment-object>),
     gesture == $menu-gesture,
     #key presentation, event) => ()
  ignore(presentation, event);
  interactor-popup-menu-callback(sheet)(sheet, object)
end method handle-presentation-event;

// For all you old CLIM users...
define method handle-presentation-event
    (sheet :: <interactor-control>, object, type :: subclass(<application-object>),
     gesture == $describe-gesture,
     #key presentation, event) => ()
  ignore(presentation, event);
  interactor-describe-object(sheet, object)
end method handle-presentation-event;
*/

//---*** This is what we have to use before the "Incremental Release"
define method handle-presentation-event
    (sheet :: <interactor-control>, object, type :: subclass(<environment-object>),
     gesture :: <pointer-gesture>,
     #key presentation, event) => ()
  handle-presentation-event-1(sheet, object, type,
			      gesture-button(gesture),
			      gesture-modifier-state(gesture),
			      presentation: presentation, event: event)
end method handle-presentation-event;

//---*** More of same...
define method handle-presentation-event-1
    (sheet :: <interactor-control>, object, type :: subclass(<environment-object>),
     button == $right-button, modifier-state == 0,
     #key presentation, event) => ()
  ignore(presentation, event);
  interactor-popup-menu-callback(sheet)(sheet, object)
end method handle-presentation-event-1;

//---*** More of same...
define method handle-presentation-event-1
    (sheet :: <interactor-control>, object, type :: subclass(<application-object>),
     button == $middle-button, modifier-state == 0,
     #key presentation, event) => ()
  ignore(presentation, event);
  interactor-describe-object(sheet, object)
end method handle-presentation-event-1;


/// Describe object

//--- Dummy method that does nothing for non-composite objects
define method interactor-describe-object
    (pane :: <interactor-control>, object :: <application-object>)
 => ()
  let frame   = sheet-frame(pane);
  let project = frame-project(frame);
  let module  = frame-current-module(frame);
  let object-name
    = present-environment-object(project, object, <application-object>, module);
  present(format-to-string("Describe %s", object-name),
          pane,
          text-style: make(<text-style>, weight: #"bold"));
end method interactor-describe-object;

define method interactor-describe-object
    (pane :: <interactor-control>, object :: <composite-object>)
 => ()
  let frame   = sheet-frame(pane);
  let project = frame-project(frame);
  let module  = frame-current-module(frame);
  let object-name = present-environment-object(project, object, <application-object>, module);
  present(format-to-string("Describe %s", object-name),
          pane,
          text-style: make(<text-style>, weight: #"bold"));
  let (names, values)
    = composite-object-contents(project, object);
  for (name in names, value in values)
    let string = 
      format-to-string("%s -> %s",
                       if (instance?(name, <environment-object>)) 
                         present-environment-object(project, name, <environment-object>, module)
                       else
                         name
                       end,
                       present-environment-object(project, value, <application-object>, module));
    present(string, pane,
            object: value, type: <application-object>)
  end
end method interactor-describe-object;

//---*** A fake thread until we know what thread to do execution in!
define constant $dummy-thread 
  = make(<thread-object>, application-object-proxy: #f);

define method interactor-current-thread
    (pane :: <interactor-control>) => (thread :: <thread-object>)
  $dummy-thread;
end method interactor-current-thread;

define method interactor-execute-code
    (pane :: <interactor-control>, code :: <string>) => ()
  let frame   = sheet-frame(pane);
  let project = frame-project(frame);
  let module  = frame-current-module(frame);
  let code = copy-sequence(code);
  present(code, pane,
          object: make(<interactor-source-form>, string: code,
                       compiler-object-proxy: #f),
          type: <interactor-source-form>,
          text-style: make(<text-style>, weight: #"bold"));
  let thread = interactor-current-thread(pane);
  let (values, error?)
    = project-execute-code(project, code, thread, module: module);
  let variable-number = interactor-next-variable-number(pane);
  //---*** Should handle the error
  for (value in values)
    let string = present-environment-object(project, value, <application-object>, module);
    let variable-name = format-to-string("$%d", variable-number);
    project-bind-variable(project, variable-name, value, module: module);
    present(format-to-string("%s = %s", variable-name, string),
            pane,
            object: value, type: <application-object>);
    variable-number := variable-number + 1;
  end;
  interactor-next-variable-number(pane) := variable-number
end method interactor-execute-code;
