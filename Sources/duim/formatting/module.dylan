Module:       Dylan-User
Synopsis:     DUIM output formatting
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-formatting
  // Table formatting
  create <cell-record>,
         <column-record>,
         <row-record>,
         <table-record>,
         \formatting-cell, do-formatting-cell,
         \formatting-column, do-formatting-column,
         \formatting-row, do-formatting-row,
         \formatting-table, do-formatting-table;

  // Menu, aka item list, formatting
  create <item-list-record>,
         format-items,
         \formatting-items, do-formatting-items;

  // Graph formatting
  create <arrow-graph-edge>,
         <dag-graph-record>,
         <line-graph-edge>,
         <tree-graph-record>,
         format-graph-from-roots;
end module duim-formatting;

define module duim-formatting-internals
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
  use duim-recording-internals;
  use duim-formatting, export: all;

  export process-spacing-arg;

  export <basic-graph-record>,
         <graph-edge-record>,
	 <graph-node-record>,
         edge-attachment-points,
         generate-graph-nodes,
         graph-edge-from-node, graph-edge-to-node,
         graph-node-table,
         graph-node-x, graph-node-x-setter,
         graph-node-y, graph-node-y-setter,
         graph-properties,
         graph-root-nodes,
         layout-graph-nodes,
         layout-graph-edges;
end module duim-formatting-internals;
