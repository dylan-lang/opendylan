Module:       Dylan-User
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-layouts
  //--- Try to make some DUIM-Sheets forward references less visible
  use duim-sheets,
    import: { relayout-children,
	      relayout-parent },
    export: all;

  // Space requirements
  create $fill,
         <space-requirement>,
         space-requirement-components,
         space-requirement-height,
         space-requirement-max-height,
         space-requirement-max-width,
         space-requirement-min-height,
         space-requirement-min-width,
         space-requirement-width,
         space-requirement?;

  // General panes
  create <basic-composite-pane>,
         <drawing-pane>,
         <layout>,
	 <leaf-pane>,
         <multiple-child-composite-pane>,
         <null-pane>,
         <simple-pane>,
         <single-child-composite-pane>,
         <top-level-sheet>,
         layout?,
	 pane-display-function, pane-display-function-setter,
         sheet-container,
         sheet-container-region;

  // Layout
  create <column-layout>,
         <fixed-layout>,
         <grid-layout>,
         <pinboard-layout>,
         <row-layout>,
         <stack-layout>,
         <table-layout>,
         allocate-space, do-allocate-space,
         compose-space, do-compose-space,
         \horizontally,
         stack-layout-mapped-page, stack-layout-mapped-page-setter,
	 table-contents, table-contents-setter,
         \tabling,
         \vertically;

  // Panes
  create <basic-user-pane>,
         current-pane,
         \pane-definer,
         pane-layout;

  //--- Needed until macro hygiene works...
  create *current-pane*,
         %pane-framem, %pane-framem-setter,
         %pane-layout, %pane-layout-setter,
         \pane-class-definer,
         \pane-generators-definer,
         \pane-layout-definer;
end module duim-layouts;

define module duim-layouts-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals;
  use duim-layouts, export: all;

  // Space requirements
  export space-requirement+, space-requirement+*,
         space-requirement-combine;

  // Layout
  export <box-layout-pane>,
         <cached-space-requirement-mixin>,
         <client-overridability-mixin>,
         <column-layout-pane>,
         <composite-layout-mixin>,
         <fixed-layout-pane>,
         <grid-layout-pane>,
         <horizontal-layout-mixin>,
	 <horizontal-position>,
         <layout-border-mixin>,
         <layout-mixin>,
         <layout-pane>,
         <leaf-layout-mixin>,
         <multiple-child-wrapping-pane>,
         <pane-display-function-mixin>,
         <pinboard-layout-pane>,
         <row-layout-pane>,
         <single-child-wrapping-pane>,
         <space-requirement-mixin>,
         <stack-layout-pane>,
         <table-layout-pane>,
         <vertical-layout-mixin>,
	 <vertical-position>,
         <wrapping-layout-mixin>,
	 <x-alignment>,
	 <y-alignment>,
	 box-pane-allocate-space,
         box-pane-compose-space,
         compose-space-for-items,
         constrain-size,
         default-space-requirement,
         invalidate-space-requirements,
         layout-border, layout-border-setter,
         layout-x-alignment, layout-x-alignment-setter,
         layout-x-ratios, layout-x-ratios-setter,
         layout-x-spacing, layout-x-spacing-setter,
         layout-y-alignment, layout-y-alignment-setter,
         layout-y-ratios, layout-y-ratios-setter,
         layout-y-spacing, layout-y-spacing-setter,
         pane-space-requirement,
	 reset-space-requirement;
end module duim-layouts-internals;
