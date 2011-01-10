Module:       Dylan-User
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-gadget-panes
  // Border and label implementation classes
  create <border-pane>,
         <group-box-pane>,
         <spacing-pane>;

  // Spin box panes
  create <spin-box-pane>;

  // Tab control panes
  create <tab-control-pane>;

  // List and table control panes
  create <list-control-pane>,
         <list-item-pane>,
         <table-control-pane>,
         <table-item-pane>;

  // Tree control panes
  create <tree-control-pane>,
         <tree-node-pane>;

  // Graph control panes
  create <graph-control-pane>,
	 <tree-graph-pane>, <DAG-graph-pane>,
	 <graph-node-pane>,
	 <graph-edge-pane>,
	 <line-graph-edge>, <arrow-graph-edge>;

  // Splitters
  create <column-splitter-pane>,
	 <row-splitter-pane>;

  // Carets
  create <simple-caret>;
end module duim-gadget-panes;

define module duim-gadget-panes-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals;
  use duim-layouts-internals;
  use duim-gadgets-internals;
  use duim-frames-internals;
  use duim-gadget-panes, export: all;

  // Spin box panes
  export <arrow-button-pane>;

  // Border support
  export <border-pane-mixin>,
         border-characteristics;

  // Dialog support
  export make-top-level-drawing-pane,
         dialog-needs-separator?,
         dialog-needs-title-pane?,
         default-dialog-frame-wrapper,
         update-default-dialog-layout,
         default-dialog-border,
         default-dialog-extra-size,
         default-dialog-button-spacing,
         default-dialog-button-x-alignment,
         default-dialog-button-y-alignment,
         default-dialog-spacing,
	 make-exit-box,
	 make-exit-buttons,
	 make-exit-button;

  // Homegrown controls
  export <homegrown-control-mixin>,
	 <homegrown-control-button-mixin>,
	 <homegrown-control-layout-mixin>;

  // Tree control panes
  export $tree-control-black, $tree-control-gray,
	 $tree-expand-icon, $tree-contract-icon,
	 <homegrown-tree-control-mixin>,
	 <tree-control-layout>,
	 <tree-node-control-button>,
	 <tree-node-label-button>,
	 <tree-node-pane-mixin>,
	 tree-control-expand-icon, tree-control-expand-icon-setter,
	 tree-control-contract-icon, tree-control-contract-icon-setter,
	 initialize-tree-control-icons;

  // Graph control panes
  export <graph-control-layout>,
	 <tree-graph-layout>, <DAG-graph-layout>;
end module duim-gadget-panes-internals;
