Module:    environment-code-viewer
Synopsis:  Environment code viewer
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Convenience functions for colors
//---*** Perhaps there are appropriate functions already in DUIM?

// Get a brighter or darker shade of a base color

define function brighter-color
    (color :: <color>, brighter :: <float>)
 => (new-color :: <color>)
  let (r, g, b) = color-rgb(color);
  make-rgb-color(max(min(r + brighter, 1), 0),
                 max(min(g + brighter, 1), 0),
                 max(min(b + brighter, 1), 0))
end function brighter-color;

// Create highlight and shadow colors from a base color

define method highlight-color (color :: <color>) => (new-color :: <color>)
  brighter-color(color, 0.50)
end method highlight-color;

//---*** Currently unused. We (possibly) need to move all these color
//       utilities into another library, eventually.
/*
define method highlight-edge-color (color :: <color>) => (new-color :: <color>)
  brighter-color(color, 0.20)
end method highlight-edge-color;

define method shadow-edge-color (color :: <color>) => (new-color :: <color>)
  brighter-color(color, -0.20)
end method shadow-edge-color;
*/

define method shadow-color (color :: <color>) => (new-color :: <color>)
  brighter-color(color, -0.30)
end method shadow-color;


// Code viewer: displays source code with breakpoints and current execution
// location

define pane <code-viewer> (<standard-input-mixin>, <standard-repainting-mixin>)
  slot code-viewer-project :: <project-object>,
    required-init-keyword: project:;
  slot code-viewer-definition :: false-or(<definition-object>) = #f,
    init-keyword: definition:,
    setter: %code-viewer-definition-setter;
  slot code-viewer-current-location :: false-or(<source-location>) = #f,
    init-keyword: current-location:,
    setter: %code-viewer-current-location-setter;
  pane code-viewer-table (pane)
    make(<table-layout>,
         columns: 4,
         children: make-code-viewer-lines(pane));
  pane code-viewer-breakpoint-popup-menu (pane)
    make-breakpoint-popup-menu(pane);
  layout (pane)
    scrolling ()
      pane.code-viewer-table
    end;
end pane <code-viewer>;


// Pane layout

define constant $code-viewer-line-height = 17; //---*** Testing: this depends on fonts, etc.


// When the contents of the code viewer are changed, update the pane contents

define method code-viewer-definition-setter
    (definition :: <definition-object>, code-viewer :: <code-viewer>)
 => (definition :: <definition-object>)
  code-viewer.%code-viewer-definition := definition;
  sheet-children(code-viewer-table(code-viewer))
    := make-code-viewer-lines(code-viewer);
  definition
end method code-viewer-definition-setter;

define method code-viewer-current-location-setter
    (location :: false-or(<source-location>), code-viewer :: <code-viewer>)
 => (location :: false-or(<source-location>))
  code-viewer.%code-viewer-current-location := location;
  //---*** This should be optimized so that only the current location display
  //       is updated, instead of rebuilding the entire code viewer.
  sheet-children(code-viewer-table(code-viewer))
    := make-code-viewer-lines(code-viewer);
  location
end method code-viewer-current-location-setter;


// Some sample data for testing
/* ---*** I don't think this currently works. Hopefully, real data will soon
//        be available and this can be deleted altogether.
define constant $fake-definition
  = make(<method-object>,
	 name: "fake-method-definition",
	 application-object-proxy: #f,
         compiler-object-proxy: #f);

define constant $fake-definition-text
  // Definition source with carriage-returns (source locations are marked with '^')
  //           1         2         3         4
  // 01234567890123456789012345678901234567890
  = "define method foo ()\n"
    "  // Do that voodoo that you do so well.\n"
    "  let x = bar(1);\n"
  //   ^64        ^75
    "  values(x, x + 1)\n"
  //         ^88   ^94
    "end method foo;"; //\n"; //---*** Test with and without trailing '\n'
  //               ^113  ^114

define constant $fake-locations
  = vector(0, 75, 64, 94, 88, 113 /*114*/);
  //---*** These are character offsets from the start of the definition source,
  //       in execution order.

define constant $fake-breakpoints
  = vector(make(<breakpoint-object>,
                definition: $fake-definition,
                location: 0),
           make(<breakpoint-object>,
                definition: $fake-definition,
                location: 3),
           make(<breakpoint-object>,
                definition: $fake-definition,
                location: 5));
*/


// Split a sequence into a vector of sequences. Sequence elements are passed to
// predicate which returns true if the sequence should be split at that element
// (the "split" element). Elements between split elements (or preceding the first
// split or trailing the last split) are placed in a new sequence which is placed
// in the result vector. Split elements are excluded from the result vector. If
// there are no elements between split elements, empty-sequence is placed in the
// result vector.
//
// The second value returned is a sequence whose elements are the keys of the
// split elements.
//
// An example application of this is to break a string of text into lines.
// To do this, predicate tests each <character> element against '\n'. The result
// is a vector of strings, one for each line, with carriage-returns omitted.

//---*** This needs more work: it doesn't handle the case where the first element
//       is a match for predicate. Also, what  should be the default value of
//       empty-sequence? Or should this key exist at all?

define method split-sequence
    (sequence :: <sequence>, predicate :: <function>, #key empty-sequence = #())
 => (sequences :: <stretchy-vector>, element-keys :: <stretchy-vector>)
  let sequences = make(<stretchy-vector>);
  let element-keys = make(<stretchy-vector>);
  let start-key = 0;
  let last-key = size(sequence) - 1;
  for (key from 0 to last-key)
    let next-key = key + 1;
    //---*** Is there a better way to express this? There's some extra
    //       checking (e.g. 'unless') for the last element.
    if (key = last-key | predicate(sequence[next-key]))
      if (start-key ~= next-key)
        add!(sequences, copy-sequence(sequence, start: start-key, end: next-key));
      else
        unless (key = last-key)
          add!(sequences, empty-sequence)
        end
      end;
      unless (key = last-key)
        add!(element-keys, next-key);
      end;
      start-key := next-key + 1
    end if
  end for;
  values(sequences, element-keys)
end method split-sequence;


// Map character locations to line numbers. character-locations is a sequence
// of character offsets from the start of some text (not necessarily in sorted
// order). line-breaks is a sequence of character offsets where line breaks
// occur, not including the end of the last line. text-size is the size of the
// source text (used to determine the end of the last line).
//
//---*** NOTE: this is a bit ugly. Perhaps the line-breaks should contain
//       the end of the last line (text-length).
//
// The result is a sequence of line numbers, one for each line that
// contains at least one character location.

define function map-character-locations-to-lines
    (character-locations :: <sequence>,
     line-breaks :: <sequence>,
     text-size :: <integer>)
 => (character-location-lines :: <sequence>)
  let character-location-lines = make(<stretchy-vector>);
  let line-number = 0;
  let line-end = line-breaks[0];
  let location-number = 0;
  while (location-number < size(character-locations))
    if (character-locations[location-number] <= line-end)
      add-new!(character-location-lines, line-number);
      location-number := location-number + 1;
    else
      line-number := line-number + 1;
      line-end := element(line-breaks, line-number, default: text-size);
    end
  end while;
  character-location-lines
end function map-character-locations-to-lines;


// Make code viewer subpanes to display the source code and breakpoints of
// its definition (and current execution location, if appropriate).

define method make-code-viewer-lines
    (pane :: <code-viewer>)
 => (lines :: <sequence>)
  let lines = #f;
  let definition = code-viewer-definition(pane);
  if (~definition) //---*** Testing
//debug-message("make-code-viewer-lines: Definition unavailable");
//  definition := $fake-definition;
  end;
  if (definition)
   block () //---*** For testing purposes, we need to catch exceptions
//  let text = $fake-definition-text; //---*** Used to test instead of the following line
    let text = environment-object-source(pane.code-viewer-project, definition);
    if (text)
      //---*** environment-object-source() is returning <byte-vector> from
      //       copy-source-location-contents() -- I think this should raise a
      //       type check error in environment-object-source(), but it doesn't.
      //       In any case, coerce to a <string> for now. Perhaps e-o-s()
      //       should do this.
      text := as(<string>, text);
      
      // Split the source text into lines
      
      let text-size = size(text);
      let (text-lines, line-breaks)
        = split-sequence(text, curry(\=, '\n'), empty-sequence: "");
      //*/ format-out("text-lines           = %=\n", text-lines);
      //*/ format-out("line-breaks          = %=\n", line-breaks);
      
      // Map source locations to lines
      
      let locations = #f; //$fake-locations;
      //*/ format-out("code-locations       = %=\n", code-locations);
      let location-lines
        = locations
           & map(method (location :: <source-location>) => (line :: <integer>)
                   location.source-location-start-line
                 end,
                 locations);
      //*/ format-out("location-lines  = %=\n", location-lines);
      
      // Get breakpoint locations, then map to lines
      
      let breakpoints = #f; //$fake-breakpoints;
      let breakpoint-lines
        = #f; /* breakpoints
           & map(method (breakpoint :: <breakpoint-object>) => (line :: <integer>)
                   breakpoint.breakpoint-source-location.source-location-start-line
                 end,
                 breakpoints); */
      //*/ format-out("breakpoint-lines     = %=\n", breakpoint-lines);
      
      // Map current execution location to a line
      
      let current-location = code-viewer-current-location(pane);
      let current-location-line
        = current-location
           & current-location.source-location-start-line;
      
      // Make panes for each line
      
      lines := make(<stretchy-vector>);
      for (line in text-lines,
           line-number from 0)
        //---*** We probably want to change this so we have a sorted sequence with one
        //       entry per line that contains the code location and breakpoint, if any.
        let has-location? = #f; //member?(line-number, location-lines);
        let has-breakpoint? = #f; //member?(line-number, breakpoint-lines);
        add!(lines, 
             if (has-location?)
               make(<breakpoint-button>,
                    client: pane.code-viewer-breakpoint-popup-menu,
                    value: has-breakpoint?, // & $fake-breakpoints[0],
                    project: pane.code-viewer-project,
                    definition: pane.code-viewer-definition,
                    location: breakpoint-source-location($fake-breakpoints[0]))
             else
               make(<null-pane>,
                    width: $breakpoint-button-column-width,
                    height: $code-viewer-line-height)
             end);
        add!(lines, make(<separator-pane>,
                         orientation: #"vertical",
                         max-height: $code-viewer-line-height));
        if (line-number = current-location-line)
          add!(lines, make(<current-location-indicator>))
        else
          add!(lines, make(<null-pane>,
                           width: $breakpoint-button-column-width,
                           height: $code-viewer-line-height))
        end;
        add!(lines, make(<label-pane>, label: line))
      end for
    end if;
//   exception (<condition>) //---*** Testing
//     lines := vector(make(<label-pane>,
//                          label: "<No source code available: exception raised>"))
   end block
  end if;
  lines
    | vector(make(<label-pane>, label: "<No source code available>"))
end method make-code-viewer-lines;


// Draw a vertical line separating breakpoints from code
/*
define method handle-repaint
    (pane :: <code-viewer>, medium :: <medium>, region :: <region>) => ()
  let background = medium-background(medium);
  with-drawing-options (medium, brush: shadow-color(background))
    draw-line(medium, 1, 0, 1, $code-viewer-line-height - 1)
  end;
  with-drawing-options (medium, brush: highlight-color(background))
    draw-line(medium, 2, 0, 2, $code-viewer-line-height - 1)
  end
end method handle-repaint;
*/


/// Breakpoint button popup menu


// Breakpoint menu constructor

define method make-breakpoint-popup-menu
    (owner :: <sheet>)
 => (menu :: <menu>)
  local method get-button (menu-box) => (button :: <breakpoint-button>)
    menu-box.gadget-client.gadget-client // menu box => menu => button
  end method get-button;
  let type-box
    = make(<radio-menu-box>,
           items: #(#("None",    #f),
                    #("Break",   #"break"),
                    #("Trace",   #"trace"),
                    #("Profile", #"profile")),
           label-key: first,
           value-key: second,
           value-changed-callback:
             method (client)
               breakpoint-button-type(get-button(client))
		 := gadget-value(client)
             end method);
  let enabled?-box
    = make(<check-menu-box>,
           items: #(#("Enabled", #t)),
           label-key: first,
           value-key: second,
           value-changed-callback:
             method (client)
	       let enabled? = gadget-value(client);
               breakpoint-button-enabled?(get-button(client)) := enabled?
             end method);
  let run-to-box
    = make(<menu-box>,
           items: #("Run to Here"),
           activate-callback:
             method (client)
               breakpoint-button-type(get-button(client)) := #"step"
             end method);
  let menu = make(<menu>,
                  owner: owner,
                  label: "Breakpoint",
                  children: vector(type-box, enabled?-box, run-to-box));
  type-box.gadget-client
    := enabled?-box.gadget-client
    := run-to-box.gadget-client
    := menu
end method make-breakpoint-popup-menu;


// Breakpoint menu state accessors
//---*** Since there is no specific class for the breakpoint menu, there is no
//       type checking performed on the 'menu' parameter. Perhaps we should
//       provide a way to identify the breakpoint menu for error checking
//       purposes?

define function breakpoint-popup-menu-type
    (menu :: <menu>)
 => (type :: false-or(<breakpoint-type>))
  gadget-value(menu.sheet-children[0])
end function breakpoint-popup-menu-type;

define function breakpoint-popup-menu-type-setter
    (type :: false-or(<breakpoint-type>), menu :: <menu>)
 => (type :: false-or(<breakpoint-type>))
  gadget-value(menu.sheet-children[0]) := type;
  gadget-enabled?(menu.sheet-children[1].sheet-children[0])
    := select (type)
         #"break", #"trace" => #t;
         otherwise          => #f;
       end select;
end function breakpoint-popup-menu-type-setter;

define function breakpoint-popup-menu-enabled?
    (menu :: <menu>)
 => (enabled? :: <boolean>)
  let value = gadget-value(menu.sheet-children[1]);
  (size(value) = 1) & value[0]
end function breakpoint-popup-menu-enabled?;

define function breakpoint-popup-menu-enabled?-setter
    (enabled? :: <boolean>, menu :: <menu>)
 => (enabled? :: <boolean>)
  gadget-value(menu.sheet-children[1]) := vector(enabled?);
  enabled?
end function breakpoint-popup-menu-enabled?-setter;



/// Breakpoint button: displays a breakpoint

define class <breakpoint-button> (<push-button>, <simple-pane>)
  constant slot breakpoint-button-project :: <project-object>,
    required-init-keyword: project:;
  constant slot breakpoint-button-definition :: <definition-object>,
    required-init-keyword: definition:;
  constant slot breakpoint-button-source-location :: <source-location>,
    required-init-keyword: location:;
  virtual slot breakpoint-button-type :: false-or(<breakpoint-type>);
  virtual slot breakpoint-button-enabled? :: <boolean>;
end class <breakpoint-button>;


// Breakpoint button mouse-click gestures for setting different breakpoint types

define constant $breakpoint-trace-gesture
  = make(<gesture>, button: #"left", modifiers: #[#"shift"]);
define constant $breakpoint-step-gesture
  = make(<gesture>, button: #"left", modifiers: #[#"control"]);
define constant $breakpoint-profile-gesture
  = make(<gesture>, button: #"left", modifiers: #[#"shift", #"control"]);


// Icon layout

define constant $breakpoint-icon-margin = 4;
define constant $breakpoint-icon-offset = truncate/($breakpoint-icon-margin, 2);
define constant $breakpoint-button-column-width = 11 + $breakpoint-icon-margin;
  //---*** Should this (11) be calculated from the image, or is it valid to
  //       put this here and assume that I will make icons the right size?


// Breakpoint button icons

define constant $breakpoint-red    = make-rgb-color(0.80, 0.20, 0.20);
define constant $breakpoint-green  = make-rgb-color(0.10, 0.80, 0.10);
define constant $breakpoint-yellow = make-rgb-color(0.90, 0.90, 0.20);

define pattern $breakpoint-none-icon
    (list($background, $foreground))
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0;
  0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0;
  0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0;
  0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0;
  0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0;
  0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $breakpoint-none-icon;

define pattern $breakpoint-break-icon
    (list($background,
          highlight-color($breakpoint-red),
          $breakpoint-red,
          shadow-color($breakpoint-red)))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  0, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $breakpoint-break-icon;

define pattern $breakpoint-break-test-icon
    (list($background,
          highlight-color($red),
          $red,
          shadow-color($red),
          $black))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 4, 2, 2, 4, 2, 2, 0;
  1, 2, 2, 2, 4, 2, 2, 4, 2, 2, 3;
  1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 3;
  1, 2, 2, 2, 4, 2, 4, 2, 2, 2, 3;
  1, 2, 4, 4, 4, 4, 4, 4, 4, 2, 3;
  1, 2, 2, 4, 2, 2, 4, 2, 2, 2, 3;
  0, 2, 2, 4, 2, 2, 4, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $breakpoint-break-test-icon;

define pattern $breakpoint-break-disabled-icon
    (list($background,
          highlight-color($breakpoint-red),
          $breakpoint-red,
          shadow-color($breakpoint-red)))
  0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 3, 3, 3, 3, 2, 2, 2, 0;
  1, 2, 3, 3, 0, 0, 0, 2, 2, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3;
  1, 2, 2, 2, 0, 0, 0, 1, 1, 2, 3;
  0, 2, 2, 2, 1, 1, 1, 1, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $breakpoint-break-disabled-icon;

define pattern $breakpoint-step-icon
    (list($background,
          highlight-color($breakpoint-green),
          $breakpoint-green,
          shadow-color($breakpoint-green)))
   0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3;
  0, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0;
end pattern $breakpoint-step-icon;

define pattern $breakpoint-trace-icon
    (list($background,
          highlight-color($breakpoint-yellow),
          $breakpoint-yellow,
          shadow-color($breakpoint-yellow)))
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 2, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 2, 0, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 2, 0, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;
end pattern $breakpoint-trace-icon;

define pattern $breakpoint-trace-disabled-icon
    (list($background,
          highlight-color($breakpoint-yellow),
          $breakpoint-yellow,
          shadow-color($breakpoint-yellow)))
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  0, 0, 0, 1, 3, 0, 2, 2, 0, 0, 0;
  0, 0, 0, 1, 3, 0, 2, 2, 0, 0, 0;
  0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 0;
  0, 0, 1, 3, 0, 0, 0, 2, 2, 0, 0;
  0, 1, 3, 0, 0, 0, 0, 0, 2, 2, 0;
  0, 1, 2, 1, 1, 1, 1, 1, 1, 2, 0;
  1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3;
end pattern $breakpoint-trace-disabled-icon;

define pattern $breakpoint-profile-icon
    (list($background,
          highlight-color($blue),
          $blue,
          shadow-color($blue)))
  1, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 1, 2, 2, 2, 2, 2, 3, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 1, 2, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0;
end pattern $breakpoint-profile-icon;


// Map a breakpoint's type to an icon

define function breakpoint-button-type-icon
    (pane :: <breakpoint-button>)
 => (icon :: <image>)
  let breakpoint = gadget-value(pane);
  let breakpoint-type = breakpoint & breakpoint-type(breakpoint);
  select (breakpoint-type)
    #f         => $breakpoint-none-icon;
    #"break"   => if (breakpoint-enabled?(breakpoint))
                    $breakpoint-break-icon
                  else
                    $breakpoint-break-disabled-icon
                  end;
    #"step"    => $breakpoint-step-icon;
    #"trace"   => if (breakpoint-enabled?(breakpoint))
                    $breakpoint-trace-icon
                  else
                    $breakpoint-trace-disabled-icon
                  end;
    #"profile" => $breakpoint-profile-icon;
  end
end function breakpoint-button-type-icon;


//---*** Currently using initialize to set the gadget label. However, this label is
//       entirely derived from the gadget value. Is there a way to set the label
//       when the value is initialized? (e.g. is there a way to get a call-back when
//       value is initialized, just like when value is set?)

define method initialize (pane :: <breakpoint-button>, #key)
  next-method();
  gadget-label(pane) := breakpoint-button-type-icon(pane);
end method initialize;

define method do-compose-space
    (pane :: <breakpoint-button>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  make(<space-requirement>,
       width:  width  + $breakpoint-icon-margin,
       height: height + $breakpoint-icon-margin)
end method do-compose-space;

define method handle-repaint
    (pane :: <breakpoint-button>, medium :: <medium>, region :: <region>) => ()
  draw-image(medium,
	     gadget-label(pane),
	     $breakpoint-icon-offset,
	     $breakpoint-icon-offset);
end method handle-repaint;

define method note-gadget-value-changed
    (pane :: <breakpoint-button>) => ()
  gadget-label(pane) := breakpoint-button-type-icon(pane);
  when (sheet-mapped?(pane))
    clear-box*(pane, sheet-region(pane));
    repaint-sheet(pane, $everywhere)
  end
end method note-gadget-value-changed;


// Set/clear/change breakpoint for breakpoint button

define method breakpoint-button-type
    (pane :: <breakpoint-button>)
 => (type :: false-or(<breakpoint-type>))
  let breakpoint = gadget-value(pane);
  breakpoint & breakpoint-type(breakpoint)
end method breakpoint-button-type;

define method breakpoint-button-type-setter
    (type :: false-or(<breakpoint-type>), pane :: <breakpoint-button>)
 => (type :: false-or(<breakpoint-type>))
  let breakpoint = gadget-value(pane);
  if (type)
    let (definition, location) = if (breakpoint)
                                   values(breakpoint-definition(breakpoint),
                                          breakpoint-source-location(breakpoint))
                                 else
                                   values(breakpoint-button-definition(pane),
                                          breakpoint-button-source-location(pane))
                                 end;
    let new-breakpoint = make(<breakpoint-object>,
                              definition: definition,
                              location: location,
                              type: type);
    gadget-value(pane) := new-breakpoint;
    project-add-breakpoint(pane.breakpoint-button-project, new-breakpoint);
    //---*** Issue a run/continue?
    // when (type = #"step")
    //   do-continue()
    // end
  else
    gadget-value(pane) := #f;
    project-remove-breakpoint(pane.breakpoint-button-project, breakpoint);
  end;
  type
end method breakpoint-button-type-setter;

define method breakpoint-button-enabled?
    (pane :: <breakpoint-button>)
 => (enabled? :: <boolean>)
  let breakpoint = gadget-value(pane);
  breakpoint & breakpoint-enabled?(breakpoint)
end method breakpoint-button-enabled?;

define method breakpoint-button-enabled?-setter
    (enabled? :: <sequence>, pane :: <breakpoint-button>)
 => (enabled? :: <sequence>)
  let breakpoint = gadget-value(pane);
  /**/ assert(breakpoint, "Can't enable/disable because there is no breakpoint");
  breakpoint-enabled?(breakpoint) := (size(enabled?) = 1) & enabled?[0];
  gadget-value(pane) := #f; //---*** Work-around to force gadget to update
  gadget-value(pane) := breakpoint;
  project-breakpoint-changed(pane.breakpoint-button-project, breakpoint);
  enabled?
end method breakpoint-button-enabled?-setter;


// Set/clear breakpoint when button clicked

define method handle-event
    (pane :: <breakpoint-button>, event :: <button-release-event>) => ()
  when (gadget-enabled?(pane))
    let old-breakpoint = gadget-value(pane);
    let old-breakpoint-type = old-breakpoint & breakpoint-type(old-breakpoint);
    select (event-button(event))
      $left-button
        => let modifiers = event-modifier-state(event);
           let new-breakpoint-type
             = select (modifiers by modifier-state-matches-gesture?)
		 $breakpoint-trace-gesture   => #"trace";
		 $breakpoint-profile-gesture => #"profile";
		 $breakpoint-step-gesture    => #"step";
		 otherwise
		   => if (old-breakpoint-type) #f else #"break" end;
	       end select;
           if (new-breakpoint-type = old-breakpoint-type)
             new-breakpoint-type := #f
           end;
           breakpoint-button-type(pane) := new-breakpoint-type;
       $right-button
        => let menu = gadget-client(pane);
           gadget-client(menu) := pane;
           breakpoint-popup-menu-type(menu) := old-breakpoint-type;
           breakpoint-popup-menu-enabled?(menu) := old-breakpoint
                                                    & breakpoint-enabled?(old-breakpoint);
           sheet-mapped?(menu) := #t;
    end select
  end when
end method handle-event;


// Current location indicator: displays a marker for the current code location
//---*** Currently a subclass of <push-button> because we may want some kind of
//       interaction. If not, change to something higher up in the food chain.

define class <current-location-indicator> (<push-button>, <simple-pane>)
end class <current-location-indicator>;

define pattern $current-location-indicator-icon
    (list($background,
          highlight-color($breakpoint-green),
          $breakpoint-green,
          shadow-color($breakpoint-green)))
  0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 2, 0, 0, 0, 0;
  1, 1, 1, 1, 1, 2, 2, 2, 0, 0, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0;
  1, 2, 2, 2, 2, 2, 2, 2, 3, 0, 0;
  1, 3, 3, 3, 1, 2, 2, 3, 0, 0, 0;
  0, 0, 0, 0, 1, 2, 3, 0, 0, 0, 0;
  0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0;
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0;
end pattern $current-location-indicator-icon;

define method initialize (pane :: <current-location-indicator>, #key)
  next-method();
  gadget-label(pane) := $current-location-indicator-icon;
end method initialize;

define method do-compose-space
    (pane :: <current-location-indicator>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let (width, height) = gadget-label-size(pane);
  make(<space-requirement>,
       width:  width  + $breakpoint-icon-margin,
       height: height + $breakpoint-icon-margin)
end method do-compose-space;

define method handle-repaint
    (pane :: <current-location-indicator>, medium :: <medium>, region :: <region>) => ()
  draw-image(medium,
	     gadget-label(pane),
	     $breakpoint-icon-offset,
	     $breakpoint-icon-offset);
end method handle-repaint;
