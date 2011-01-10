Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DC classes

define sideways method make-test-instance
    (class == <contrasting-color>) => (instance :: <contrasting-color>)
  make(<contrasting-color>, how-many: 5, which-one: 1)
end method make-test-instance;

define sideways method make-test-instance
    (class == <color-not-found>) => (instance :: <color-not-found>)
  make(<color-not-found>, color: $red)
end method make-test-instance;

define sideways method make-test-instance
    (class == <palette-full>) => (instance :: <palette-full>)
  make(<palette-full>, palette: make(<palette>, port: find-test-port()))
end method make-test-instance;

define sideways method make-test-instance
    (class == <palette>) => (instance :: <palette>)
  make(<palette>, port: find-test-port())
end method make-test-instance;

define sideways method make-test-instance
    (class == <dynamic-color>) => (instance :: <dynamic-color>)
  make(<dynamic-color>, color: $red)
end method make-test-instance;

define sideways method make-test-instance
    (class == <pattern>) => (instance :: <pattern>)
  make(<pattern>, array: make(<array>, dimensions: #(2, 2)), colors: #[])
end method make-test-instance;

define sideways method make-test-instance
    (class == <stencil>) => (instance :: <stencil>)
  make(<stencil>, array: make(<array>, dimensions: #(2, 2)))
end method make-test-instance;

define sideways method make-test-instance
    (class == <device-font>) => (instance :: <device-font>)
  make(<device-font>, port: find-test-port(), font: #"test-font")
end method make-test-instance;
           

/// DUIM-Sheets classes

define constant $dummy-sheet  = make-test-pane(<simple-pane>);
define constant $dummy-gadget = make-test-pane(<push-button>);
define constant $dummy-frame  = make-test-frame(<test-frame>);

define sideways method make-test-instance
    (class == <undefined-text-style-mapping>) => (instance :: <undefined-text-style-mapping>)
  make(<undefined-text-style-mapping>,
       port: find-test-port(),
       text-style: $default-text-style)
end method make-test-instance;

define sideways method make-test-instance
    (class == <application-exited-event>) => (instance :: <application-exited-event>)
  make(<application-exited-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <button-press-event>) => (instance :: <button-press-event>)
  make(<button-press-event>, sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <button-release-event>) => (instance :: <button-release-event>)
  make(<button-release-event>,
       sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <double-click-event>) => (instance :: <double-click-event>)
  make(<double-click-event>, sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <key-press-event>) => (instance :: <key-press-event>)
  make(<key-press-event>, sheet: $dummy-sheet)
end method make-test-instance;

define sideways method make-test-instance
    (class == <key-release-event>) => (instance :: <key-release-event>)
  make(<key-release-event>, sheet: $dummy-sheet)
end method make-test-instance;

define sideways method make-test-instance
    (class == <frame-created-event>) => (instance :: <frame-created-event>)
  make(<frame-created-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <frame-destroyed-event>) => (instance :: <frame-destroyed-event>)
  make(<frame-destroyed-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <frame-exit-event>) => (instance :: <frame-exit-event>)
  make(<frame-exit-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <frame-exited-event>) => (instance :: <frame-exited-event>)
  make(<frame-exited-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <pointer-boundary-event>) => (instance :: <pointer-boundary-event>)
  make(<pointer-boundary-event>, 
       sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <pointer-drag-event>) => (instance :: <pointer-drag-event>)
  make(<pointer-drag-event>, sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <pointer-enter-event>) => (instance :: <pointer-enter-event>)
  make(<pointer-enter-event>,
       sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <pointer-exit-event>) => (instance :: <pointer-exit-event>)
  make(<pointer-exit-event>, sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <pointer-motion-event>) => (instance :: <pointer-motion-event>)
  make(<pointer-motion-event>, 
       sheet: $dummy-sheet, pointer: find-test-pointer())
end method make-test-instance;

define sideways method make-test-instance
    (class == <port-terminated-event>) => (instance :: <port-terminated-event>)
  make(<port-terminated-event>,
       frame: $dummy-frame,
       condition: make(<type-error>, type: <object>, value: 10))
end method make-test-instance;

define sideways method make-test-instance
    (class == <timer-event>) => (instance :: <timer-event>)
  make(<timer-event>, frame: $dummy-frame)
end method make-test-instance;

define sideways method make-test-instance
    (class == <window-configuration-event>)
 => (instance :: <window-configuration-event>)
  make(<window-configuration-event>, sheet: $dummy-sheet, region: $everywhere)
end method make-test-instance;

define sideways method make-test-instance
    (class == <window-repaint-event>) => (instance :: <window-repaint-event>)
  make(<window-repaint-event>, sheet: $dummy-sheet, region: $everywhere)
end method make-test-instance;

define duim-sheets class-test <caret> ()
  //---*** Fill this in...
end class-test <caret>;

define duim-sheets class-test <clipboard> ()
  //---*** Fill this in...
end class-test <clipboard>;

define duim-sheets class-test <display> ()
  //---*** Fill this in...
end class-test <display>;

define duim-sheets class-test <medium> ()
  //---*** Fill this in...
end class-test <medium>;

define duim-sheets class-test <pointer> ()
  //---*** Fill this in...
end class-test <pointer>;

define duim-sheets class-test <port> ()
  //---*** Fill this in...
end class-test <port>;

define duim-sheets class-test <sheet> ()
  //---*** Fill this in...
end class-test <sheet>;

define duim-sheets class-test <undefined-text-style-mapping> ()
  //---*** Fill this in...
end class-test <undefined-text-style-mapping>;


/// Frame classes

define sideways method make-test-instance
    (class == <functional-command>) => (instance :: <functional-command>)
  make(<functional-command>, function: identity)
end method make-test-instance;

define sideways method make-test-instance
    (class == <command-table>) => (instance :: <command-table>)
  make(<command-table>, name: #"test instance")
end method make-test-instance;


/// Extended Geometry classes

define sideways method make-test-instance
    (class == <ellipse>) => (instance :: <ellipse>)
  make(<ellipse>,
       center-point: make-point(100, 100),
       center-x: 100, center-y: 100,
       radius-1-dx: 100, radius-1-dy: 100,
       radius-2-dx: 100, radius-2-dy: 100)
end method make-test-instance;

define sideways method make-test-instance
    (class == <elliptical-arc>) => (instance :: <elliptical-arc>)
  make(<elliptical-arc>,
       center-point: make-point(100, 100),
       center-x: 100, center-y: 100,
       radius-1-dx: 100, radius-1-dy: 100,
       radius-2-dx: 100, radius-2-dy: 100)
end method make-test-instance;

define sideways method make-test-instance
    (class == <line>) => (instance :: <line>)
  make(<line>,
       start-point: make-point(0, 0),
       end-point: make-point(100, 100))
end method make-test-instance;

define sideways method make-test-instance
    (class == <polygon>) => (instance :: <polygon>)
  make(<polygon>, points: #())
end method make-test-instance;

define sideways method make-test-instance
    (class == <polyline>) => (instance :: <polyline>)
  make(<polyline>, points: #())
end method make-test-instance;

define sideways method make-test-instance
    (class == <rectangle>) => (instance :: <rectangle>)
  make(<rectangle>,
       min-point: make-point(0, 0),
       max-point: make-point(100, 100))
end method make-test-instance;
           

/// Gadget pane classes

/*---*** Removed duim-gadget-panes tests
define sideways method make-test-instance
    (class == <table-control-pane>) => (instance :: <table-control-pane>)
  make(<table-control-pane>,
       headings: #["Heading"], generators: vector(identity))
end method make-test-instance;

define sideways method make-test-instance
    (class == <tree-control-pane>) => (instance :: <tree-control-pane>)
  make(<tree-control-pane>, children-generator: always(#[]))
end method make-test-instance;
*/
