Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The DUIM library specification

define library-spec duim ()
  module duim-utilities;
  module duim-geometry;
  module duim-dcs;
  module duim-sheets;
  module duim-graphics;
  module duim-layouts;
  module duim-gadgets;
  module duim-frames;
  module duim-extended-geometry;
end library-spec duim;


/// Utilities

define module-spec duim-utilities ()
end module-spec duim-utilities;


/// Geometry

define module-spec duim-geometry ()
  // Coordinates
  constant $largest-coordinate :: <integer>;
  constant $smallest-coordinate :: <integer>;
  function do-coordinates (<function>, limited(<sequence>, of: <integer>)) => ();
  function do-endpoint-coordinates (<function>, limited(<sequence>, of: <integer>)) => ();
  function fix-coordinate (<real>) => (<integer>);

  // Bounding boxes
  open abstract instantiable class <bounding-box> (<region>);
  function bounding-box (<object>, #"key", #"into") => (<bounding-box>);
  function bounding-box? (<object>) => (<boolean>);
  open generic-function box-edges
       (<bounding-box>)
    => (<integer>, <integer>, <integer>, <integer>);
  open generic-function set-box-edges
       (<bounding-box>, <integer>, <integer>, <integer>, <integer>)
    => (<bounding-box>);
  open generic-function box-position
       (<bounding-box>) => (<integer>, <integer>);
  open generic-function set-box-position 
       (<bounding-box>, <integer>, <integer>) => (<bounding-box>);
  open generic-function box-size
       (<bounding-box>) => (<integer>, <integer>);
  open generic-function set-box-size 
       (<bounding-box>, <integer>, <integer>) => (<bounding-box>);
  function box-left   (<bounding-box>) => (<integer>);
  function box-top    (<bounding-box>) => (<integer>);
  function box-right  (<bounding-box>) => (<integer>);
  function box-bottom (<bounding-box>) => (<integer>);
  function box-width  (<bounding-box>) => (<integer>);
  function box-height (<bounding-box>) => (<integer>);
  function make-bounding-box 
      (<real>, <real>, <real>, <real>) => (<bounding-box>);

  // Regions
  constant $everywhere :: <region>;
  constant $nowhere    :: <region>;
  open abstract class <region> (<object>);
  open abstract class <area> (<region>);
  open abstract class <path> (<region>);
  open abstract class <region-set> (<region>);
  open abstract instantiable class <point> (<region>);
  sealed instantiable class <standard-point> (<point>);

  // Transforms
  constant $identity-transform :: <transform>;
  open abstract instantiable class <transform> (<object>);
  open abstract class <transform-error> (<error>);
  sealed instantiable class <singular-transform> (<transform-error>);
end module-spec duim-geometry;


/// DCs

define module-spec duim-dcs ()
  // Colors
  constant $foreground :: <ink>;
  constant $background :: <ink>;
  constant $black :: <color>;
  constant $blue :: <color>;
  constant $cyan :: <color>;
  constant $green :: <color>;
  constant $magenta :: <color>;
  constant $red :: <color>;
  constant $white :: <color>;
  constant $yellow :: <color>;
  open abstract instantiable class <color> (<ink>);
  open abstract instantiable class <contrasting-color> (<color>);
  open generic-function color-ihs (<color>) => (<real>, <real>, <real>, <real>);
  open generic-function color-rgb (<color>) => (<real>, <real>, <real>);
  open generic-function color-luminosity (<color>) => (<real>);
  function color? (<object>) => (<boolean>);
  function contrasting-colors-limit (<object>) => (<integer>);
  function make-contrasting-colors (<integer>, #"key", #"k") => (<object>);
  function make-gray-color (<real>, #"key", #"opacity") => (<color>);
  function make-ihs-color
      (<real>, <real>, <real>, #"key", #"opacity") => (<color>);
  function make-rgb-color
      (<real>, <real>, <real>, #"key", #"opacity") => (<color>);

  open abstract instantiable class <brush> (<object>);
  sealed instantiable class <color-not-found> (<error>);
  open abstract instantiable class <device-font> (<text-style>);
  open abstract instantiable class <dynamic-color> (<color>);
  open abstract class <image> (<ink>);
  open abstract class <ink> (<object>);
  sealed instantiable class <palette-full> (<error>);
  open abstract instantiable class <palette> (<object>);
  open abstract instantiable class <pattern> (<stencil>);
  open abstract instantiable class <pen> (<object>);
  open abstract instantiable class <stencil> (<image>);
  open abstract instantiable class <text-style> (<object>);
end module-spec duim-dcs;


/// DUIM-Sheets classes

define module-spec duim-sheets ()
  open abstract class <caret> (<object>);
  open abstract class <display> (<sheet>);
  open abstract class <clipboard> (<object>);
  open abstract class <frame> (<object>);
  open abstract class <frame-manager> (<object>);
  open abstract instantiable class <medium> (<object>);
  open abstract instantiable class <pointer> (<object>);
  open abstract class <port> (<object>);
  open abstract class <sheet> (<object>);
  sealed instantiable class <undefined-text-style-mapping> (<error>);
  protocol duim-events;
end module-spec duim-sheets;
           
define protocol-spec duim-events ()
  open abstract class <event> (<object>);
  open abstract class <sheet-event> (<event>);
  open abstract class <device-event> (<sheet-event>);
  open abstract class <pointer-event> (<device-event>);
  sealed instantiable class <pointer-motion-event> (<pointer-event>);
  sealed instantiable class <pointer-drag-event> (<object>);
  sealed instantiable class <port-terminated-event> (<frame-event>);
  sealed instantiable class <pointer-boundary-event> (<pointer-motion-event>);
  sealed instantiable class <pointer-enter-event> (<object>);
  sealed instantiable class <pointer-exit-event> (<pointer-event>);
  open abstract class <pointer-button-event> (<pointer-event>);
  sealed instantiable class <button-press-event> (<pointer-button-event>);
  sealed instantiable class <button-release-event> (<pointer-button-event>);
  sealed instantiable class <double-click-event> (<pointer-button-event>);
  open abstract class <keyboard-event> (<device-event>);
  sealed instantiable class <key-press-event> (<keyboard-event>);
  sealed instantiable class <key-release-event> (<keyboard-event>);
  open abstract class <window-event> (<sheet-event>);
  sealed instantiable class <window-configuration-event> (<window-event>);
  sealed instantiable class <window-repaint-event> (<window-event>);
  open abstract class <frame-event> (<event>);
  sealed instantiable class <timer-event> (<frame-event>);
end protocol-spec duim-events;
           

/// Graphics

define module-spec duim-graphics ()
  open abstract instantiable class <pixmap> (<image>);
  open abstract instantiable class <pixmap-medium> (<medium>);
end module-spec duim-graphics;
           

/// Layouts

define module-spec duim-layouts ()
  open abstract class <layout> (<abstract-sheet>);
  open abstract class <leaf-pane> (<sheet>);
  open abstract class <basic-composite-pane> (<sheet>);
  open abstract class <single-child-composite-pane> (<basic-composite-pane>);
  open abstract class <multiple-child-composite-pane> (<basic-composite-pane>);
  sealed instantiable class <null-pane> (<leaf-pane>);
  open abstract instantiable class <drawing-pane> (<sheet>);
  open abstract instantiable class <simple-pane> (<sheet>);
  open abstract instantiable class <top-level-sheet> (<sheet>);
  open abstract instantiable class <column-layout> (<layout>);
  open abstract instantiable class <fixed-layout> (<layout>);
  open abstract instantiable class <pinboard-layout> (<layout>);
  open abstract instantiable class <row-layout> (<layout>);
  open abstract instantiable class <stack-layout> (<layout>);
  open abstract instantiable class <table-layout> (<layout>);
  open abstract instantiable class <grid-layout> (<table-layout>);
end module-spec duim-layouts;
           

/// Gadget classes

define module-spec duim-gadgets ()
  open abstract class <gadget> (<object>);
  open abstract class <action-gadget> (<gadget>);
  open abstract class <collection-gadget> (<value-gadget>);
  open abstract class <text-gadget> (<value-gadget>);
  open abstract class <value-gadget> (<gadget>);
  open abstract class <value-range-gadget> (<value-gadget>);
  open abstract instantiable class <label> (<gadget>);
  open abstract instantiable class <password-field> (<text-gadget>);
  open abstract instantiable class <slider> (<value-range-gadget>);
  open abstract instantiable class <text-editor> (<text-gadget>);
  open abstract instantiable class <text-field> (<text-gadget>);
  open abstract instantiable class <button-box> (<collection-gadget>);
  open abstract instantiable class <button> (<value-gadget>);
  open abstract instantiable class <check-box> (<button-box>);
  open abstract instantiable class <check-button> (<button>);
  open abstract instantiable class <check-menu-button> (<button>);
  open abstract instantiable class <check-menu-box> (<menu-box>);
  open abstract instantiable class <list-box> (<collection-gadget>);
  open abstract instantiable class <menu-bar> (<value-gadget>);
  open abstract instantiable class <menu-button> (<button>);
  open abstract instantiable class <menu-box> (<collection-gadget>);
  open abstract instantiable class <menu> (<gadget>);
  open abstract instantiable class <option-box> (<collection-gadget>);
  open abstract instantiable class <combo-box> (<collection-gadget>);
  open abstract instantiable class <push-box> (<button-box>);
  open abstract instantiable class <push-button> (<button>);
  open abstract instantiable class <push-menu-button> (<button>);
  open abstract instantiable class <push-menu-box> (<menu-box>);
  open abstract instantiable class <radio-box> (<button-box>);
  open abstract instantiable class <radio-button> (<button>);
  open abstract instantiable class <radio-menu-button> (<button>);
  open abstract instantiable class <radio-menu-box> (<menu-box>);
  open abstract instantiable class <spin-box> (<collection-gadget>);
  open abstract instantiable class <status-bar> (<value-gadget>);
  open abstract instantiable class <tool-bar> (<gadget>);
  open abstract instantiable class <scroll-bar> (<value-range-gadget>);
  open abstract instantiable class <scroller> (<gadget>);
  open abstract instantiable class <viewport> (<gadget>);
  open abstract instantiable class <border> (<gadget>);
  open abstract instantiable class <group-box> (<gadget>);
  open abstract instantiable class <separator> (<gadget>);
  open abstract instantiable class <spacing> (<gadget>);
  open abstract instantiable class <splitter> (<gadget>);
  open abstract instantiable class <tab-control> (<value-gadget>);
  open abstract instantiable class <page> (<gadget>);
  open abstract instantiable class <tab-control-page> (<page>);
  open abstract instantiable class <list-control> (<collection-gadget>);
  open abstract instantiable class <list-item> (<object>);
  open abstract instantiable class <tree-control> (<collection-gadget>);
  open abstract instantiable class <tree-node> (<object>);
  open abstract instantiable class <table-control> (<collection-gadget>);
  open abstract instantiable class <table-item> (<object>);
  open abstract instantiable class <progress-bar> (<value-range-gadget>);
end module-spec duim-gadgets;
           

/// Frames

define module-spec duim-frames ()
  open abstract instantiable class <simple-frame> (<frame>);
  open abstract instantiable class <dialog-frame> (<simple-frame>);
  open instantiable class <property-frame> (<dialog-frame>);
  open instantiable class <property-page> (<page>);
  open instantiable class <wizard-frame> (<dialog-frame>);
  open instantiable class <wizard-page> (<page>);
  protocol duim-commands;
  protocol duim-frame-events;
end module-spec duim-frames;

define protocol-spec duim-commands ()
  open abstract class <command> (<object>);
  sealed instantiable class <functional-command> (<command>);
  open abstract instantiable class <command-decorator> (<object>);
  open abstract instantiable class <command-table> (<object>);
end protocol-spec duim-commands;

define protocol-spec duim-frame-events ()           
  sealed instantiable class <frame-created-event> (<frame-event>);
  sealed instantiable class <frame-destroyed-event> (<frame-event>);
  sealed instantiable class <frame-exit-event> (<frame-event>);
  sealed instantiable class <frame-exited-event> (<frame-event>);
  sealed instantiable class <frame-mapped-event> (<frame-event>);
  sealed instantiable class <frame-unmapped-event> (<frame-event>);
  sealed instantiable class <application-exited-event> (<frame-exited-event>);
end protocol-spec duim-frame-events;


/// Extended Geometry

//---*** This should be in a separate test suite
define module-spec duim-extended-geometry ()
  open abstract instantiable class <ellipse> (<area>);
  open abstract instantiable class <elliptical-arc> (<path>);
  open abstract instantiable class <line> (<path>);
  open abstract instantiable class <polygon> (<area>);
  open abstract instantiable class <polyline> (<path>);
  open abstract instantiable class <rectangle> (<area>);
end module-spec duim-extended-geometry;
           

/// Gadget panes

/*---*** This should be in a separate test suite
define module-spec duim-gadget-panes ()
  sealed instantiable class <border-pane> (<border>);
  sealed instantiable class <group-box-pane> (<group-box>);
  sealed instantiable class <spacing-pane> (<spacing>);
  sealed instantiable class <spin-box-pane> (<spin-box>);
  sealed instantiable class <tab-control-pane> (<tab-control>);
  sealed instantiable class <list-control-pane> (<list-control>);
  sealed instantiable class <list-item-pane> (<list-item>);
  sealed instantiable class <table-control-pane> (<table-control>);
  sealed instantiable class <table-item-pane> (<table-item>);
  sealed instantiable class <tree-control-pane> (<tree-control>);
  sealed instantiable class <tree-node-pane> (<tree-node>);
end module-spec duim-gadget-panes;
*/
