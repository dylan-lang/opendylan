Module:       DUIM-formatting-internals
Synopsis:     DUIM formatted output
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Table formatting

// Table output record
define sealed class <table-record> (<sequence-record>)
  sealed slot %x-spacing :: <integer>,
    required-init-keyword: x-spacing:;
  sealed slot %y-spacing :: <integer>,
    required-init-keyword: y-spacing:;
  sealed slot %equalize-column-widths? :: <boolean> = #f,
    init-keyword: equalize-column-widths?:;
  sealed slot %row-table? = #"unknown";
end class <table-record>;

define output-record-constructor <table-record>
    (#key parent, sheet, region, transform,
          x-spacing, y-spacing, equalize-column-widths?)
  parent: parent, sheet: sheet,
  region: region, transform: transform,
  x-spacing: x-spacing, y-spacing: y-spacing,
  equalize-column-widths?: equalize-column-widths?
end;

// Skip the 'update-region-for-new-child', because we're going to do
// layout and 'recompute-region' right away
define method note-child-added
    (record :: <table-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;

define method match-output-records
    (record :: <table-record>,
     #key x-spacing, y-spacing, equalize-column-widths?, #all-keys) => (true? :: <boolean>)
  // Reset this to the unknown state in case the table is changing
  // from a row table to a column table (or vice-versa)
  record.%row-table? := #"unknown";
  record.%x-spacing == x-spacing
  & record.%y-spacing == y-spacing
  & record.%equalize-column-widths? == equalize-column-widths?
end method match-output-records;

define method row-table? (table :: <table-record>) => (true? :: <boolean>)
  when (table.%row-table? == #"unknown")
    block (return)
      // Find the first row or column
      local method find-row-or-column (record) => ()
	      table.%row-table? := instance?(record, <row-record>);
	      return()
	    end method;
      do-table-rows-or-columns(find-row-or-column, table)
    end
  end;
  table.%row-table?
end method row-table?;


// Row output record
define sealed class <row-record> (<sequence-record>)
end class <row-record>;

define output-record-constructor <row-record>
    (#key parent, sheet, region, transform)
  parent: parent, sheet: sheet,
  region: region, transform: transform
end;

define method note-child-added
    (record :: <row-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;


// Column output record
define sealed class <column-record> (<sequence-record>)
end class <column-record>;

define output-record-constructor <column-record>
    (#key parent, sheet, region, transform)
  parent: parent, sheet: sheet,
  region: region, transform: transform
end;

define method note-child-added
    (record :: <column-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;


// Cell output record
define sealed class <cell-record> (<sequence-record>)
  sealed slot %align-x = #"left",
    init-keyword: align-x:;
  sealed slot %align-y = #"top",
    init-keyword: align-y:;
  sealed slot %min-width :: <integer> = 0,
    init-keyword: min-width:;
  sealed slot %min-height :: <integer> = 0,
    init-keyword: min-height:;
end class <cell-record>;

define output-record-constructor <cell-record>
    (#key parent, sheet, region, transform,
          align-x, align-y, min-width, min-height)
  parent: parent, sheet: sheet,
  region: region, transform: transform,
  align-x: align-x, align-y: align-y,
  min-width: min-width, min-height: min-height
end;

define method match-output-records
    (record :: <cell-record>,
     #key align-x, align-y, min-width, min-height, #all-keys) => (true? :: <boolean>)
  record.%align-x == align-x
  & record.%align-y == align-y
  & record.%min-width == min-width
  & record.%min-height == min-height
end method match-output-records;


/// Walking table structure

define method do-table-rows-or-columns
    (function :: <function>, table :: <table-record>) => ()
  do-table-elements(function, table, #"row-or-column")
end method do-table-rows-or-columns;

define method do-table-rows
    (function :: <function>, table :: <table-record>) => ()
  do-table-elements(function, table, #"row")
end method do-table-rows;

define method do-table-columns
    (function :: <function>, table :: <table-record>) => ()
  do-table-elements(function, table, #"column")
end method do-table-columns;

define method do-table-cells
    (function :: <function>, table :: <row-record>) => ()
  do-table-elements(function, table, #"cell")
end method do-table-rows;

define method do-table-cells
    (function :: <function>, table :: <column-record>) => ()
  do-table-elements(function, table, #"cell")
end method do-table-rows;


define method do-table-elements
    (function :: <function>, record :: <output-record>, type) => ()
  // FUNCTION is applied to each child of type TYPE.  Error checking is
  // done to verify that the children are, in fact, of type TYPE.
  local method do-elements (child) => ()
	  do-table-elements-1(function, child, type)
	end method;
  do-sheet-children(do-elements, record)
end method do-table-elements;

define method do-table-elements-1
    (function :: <function>, record :: <row-record>, type) => ()
  unless (member?(type, #[#"row", #"row-or-column"]))
    error("Expected a record of type %=, but this is a row", type)
  end;
  function(record)
end method do-table-elements-1;

define method do-table-elements-1
    (function :: <function>, record :: <column-record>, type) => ()
  unless (member?(type, #[#"column", #"row-or-column"]))
    error("Expected a record of type %=, but this is a column", type)
  end;
  function(record)
end method do-table-elements-1;

define method do-table-elements-1
    (function :: <function>, record :: <cell-record>, type) => ()
  unless (type == #"cell")
    error("Expected a record of type %=, but this is a cell", type)
  end;
  function(record)
end method do-table-elements-1;

define method do-table-elements-1
    (function :: <function>, record :: <composite-output-record-mixin>, type) => ()
  // Just recurse into this guy
  do-table-elements(function, record, type)
end method do-table-elements-1;

define method do-table-elements-1
    (function :: <function>, record :: <output-record-element-mixin>, type) => ()
  ignore(function);
  error("Expected a record of type %=, but this is not a table component", type)
end method do-table-elements-1;


/// Table formatting engine

//---*** Reconcile with 'compose-space' and 'allocate-space'
define method layout-table
    (table :: <table-record>, sheet :: <output-recording-mixin>) => ()
  block (return)
    let nrows :: <integer> = 0;
    let ncells = #f;
    let cells :: <integer> = 0;
    let row-table? = row-table?(table);
    let table-mapper = if (row-table?) do-table-rows else do-table-columns end;
    let x-spacing = table.%x-spacing;
    let y-spacing = table.%y-spacing;
    let equalize? = table.%equalize-column-widths?;
    local method count-rows (row)
	    inc!(nrows);
	    cells := 0;
	    do-table-cells(count-cells, row);
	    assert(~zero?(cells),
		   "Row or column does not contain any cells");
	    if (~ncells) ncells := cells else max!(ncells, cells) end
	  end method,
	  method count-cells (cell)
	    assert(instance?(cell, <cell-record>),
		   "Bogus record found where a cell was expected");
	    inc!(cells)
	  end method;
    let ncells :: <integer> = ncells;	// tighten up the type
    // Calculate nrows & ncells (= ncells per row)
    table-mapper(count-rows, table);
    // If there are no rows, COUNT-ROWS won't get invoked and NCELLS
    // will be #f.  If all the rows and columns are empty, NCELLS will
    // be 0.  In either case, that means we're done.
    when (~ncells | zero?(ncells))
      return()
    end;
    let row-heights :: <simple-object-vector>
      = make(<simple-vector>, size: nrows, fill: 0);
    let column-widths :: <simple-object-vector>
      = make(<simple-vector>, size: ncells, fill: 0);
    let x-pos :: <integer> = 0;
    let y-pos :: <integer> = 0;
    let total-width  :: <integer> = 0;
    let total-height :: <integer> = 0;
    let row-count    :: <integer> = 0;
    let column-count :: <integer> = 0;
    // Figure out max height for each row,
    //            max width for each column.
    // Collect row heights and column widths into temporary vectors.
    // We need to remember for each row its total height and the 
    // difference between the smallest top and the largest top.
    // For each row remember the total height and then remember the maximum
    // difference between the row top and the y-position of the row.
    // Rows and columns are pretty symmetric, but we need to arrange for
    // a few things to work out right...
    unless (row-table?)
      swap!(row-heights, column-widths)
    end;
    if (row-table?) row-count := -1 else column-count := -1 end;
    local method row-mapper (row) => ()
	    if (row-table?) inc!(row-count) else inc!(column-count) end;
	    if (row-table?) column-count := -1 else row-count := -1 end;
	    layout-row-or-column(row, sheet);
	    local method cell-mapper (cell) => ()
		    if (row-table?) inc!(column-count) else inc!(row-count) end;
		    let (width, height) = box-size(cell);
		    max!(row-heights[row-count], max(height, cell.%min-height));
		    max!(column-widths[column-count], max(width, cell.%min-width))
		  end method;
	    do-table-cells(cell-mapper, row)
	  end method;
    table-mapper(row-mapper, table);
    when (equalize?)
      let column-width :: <integer> = 0;
      let n-columns :: <integer> = column-count + 1;
      for (i :: <integer> from 0 below n-columns)
	max!(column-width, column-widths[i])
      end;
      for (i :: <integer> from 0 below n-columns)
	column-widths[i] := column-width
      end
    end;
    if (row-table?) row-count := -1 else column-count := -1 end;
    local method row-mapper (row) => ()
	    if (row-table?) inc!(row-count) else inc!(column-count) end;
	    let this-row-height = row-heights[row-count];
	    let this-column-width = column-widths[column-count];
	    if (row-table?) column-count := -1 else row-count := -1 end;
	    total-width := x-pos;
	    total-height := y-pos;
	    local method cell-mapper (cell) => ()
		    if (row-table?) inc!(column-count) else inc!(row-count) end;
		    let column-width = column-widths[column-count];
		    let row-height = row-heights[row-count];
		    let (cell-width, cell-height) = box-size(cell);
		    let x-alignment-adjust = 0;
		    let y-alignment-adjust = 0;
		    select (cell.%align-x)
		      #"left"   => #f;
		      #"right"  => x-alignment-adjust := column-width - cell-width;
		      #"center" => x-alignment-adjust := floor/(column-width - cell-width, 2);
		    end;
		    select (cell.%align-y)
		      #"top"     => #f;
		      #"bottom"  => y-alignment-adjust := row-height - cell-height;
		      #"center"  => y-alignment-adjust := floor/(row-height - cell-height, 2);
		    end;
		    //---*** This assumes that the cell is right inside of the row.
		    //---*** What if you want a presentation around a cell?
		    sheet-transform(cell)
		      := make(<mutable-translation-transform>,
			      tx: total-width + x-alignment-adjust,
			      ty: total-height + y-alignment-adjust);
		    if (row-table?)
		      inc!(total-width, column-width + x-spacing)
		    else
		      inc!(total-height, row-height + y-spacing)
		    end
		  end method;
            do-table-cells(cell-mapper, row);
	    if (row-table?)
	      inc!(y-pos, this-row-height + y-spacing)
	    else
	      inc!(x-pos, this-column-width + x-spacing)
	    end
	  end method;
    table-mapper(row-mapper, table)
  end
end method layout-table;

define method layout-row-or-column
    (row :: <row-record>, sheet :: <output-recording-mixin>) => ()
  #f
end method layout-row-or-column;

define method layout-row-or-column
    (row :: <column-record>, sheet :: <output-recording-mixin>) => ()
  #f
end method layout-row-or-column;


// Table has already been laid out.  Break it into multiple columns.
define method layout-multiple-columns
    (table :: <table-record>, sheet :: <output-recording-mixin>,
     n-columns :: <integer>, x-spacing :: <integer>) => ()
  let row-count = 0;
  when (row-table?(table))
    let row-counter = method (row) ignore(row); inc!(row-count) end;
    do-table-rows(row-counter, table);
    // Break into columns if the table is of any significant size,
    // or the user requested an explicit number of columns
    when (row-count > 5)
      let (tleft, ttop, tright, tbottom) = box-edges(table);
      let (sheet-width, sheet-height) = box-size(sheet);
      ignore(sheet-height);		// for now
      let table-width = tright - tleft;
      let table-height = tbottom - ttop;
      let between-column-margin
	= if (x-spacing)
	    process-spacing-arg(sheet, x-spacing, #"horizontal",
				form: #"formatting-table")
	  else
	    text-size(sheet, ' ')
	  end;
      let column-width = table-width + between-column-margin;
      let possible-columns = n-columns | max(floor/(stream-width, column-width), 1);
      let y-spacing = table.%y-spacing;
      let rows-per-column = max(3, ceiling/(row-count, possible-columns));
      let row-number = 0;
      let row-x :: <integer> = 0;
      let row-y :: <integer> = 0;
      local method layout-columns (row) => ()
	      let (rl, rt) = sheet-position(row);
	      // Position the row so that the X position relative to the
	      // original table is preserved, so that :ALIGN-X :RIGHT works
	      //--- ROW-Y needs the same treatment for :ALIGN-Y
	      %set-sheet-position(row, row-x + rl - tleft, row-y);
	      inc!(row-number);
	      inc!(row-y, box-height(row) + y-spacing);
	      when (zero?(modulo(row-number, rows-per-column)))
		row-x := row-x + column-width;
		row-y := 0
	      end
	    end method;
      do-table-rows(layout-columns, table)
    end
  end
end method layout-multiple-columns;


/// The table formatter

define method do-formatting-table
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #rest initargs,
     #key x, y, x-spacing, y-spacing, equalize-column-widths?,
          multiple-columns?, multiple-columns-x-spacing,
          record-class = <table-record>, move-caret? = #t,
     #all-keys) => (record :: <table-record>)
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[x:, y:, x-spacing:, y-spacing:,
						equalize-column-widths?:,
						multiple-columns?:, multiple-columns-x-spacing:,
						record-class:, move-caret?:])
    let table
      = with-output-recording-options (sheet, draw?: #f, record?: #t)
          with-end-of-line-action (sheet, #"allow")
            with-end-of-page-action (sheet, #"allow")
	      apply(do-with-new-output-record,
		    sheet, continuation,
		    record-class: record-class,
		    constructor: (record-class == <table-record>)
		                 & <table-record>-constructor,
		    x-spacing:
		      process-spacing-arg(sheet, x-spacing, #"horizontal",
					  form: #"formatting-table")
		      | text-size(sheet, ' '),
		    y-spacing:
		      process-spacing-arg(sheet, y-spacing, #"vertical",
					  form: #"formatting-table")
		      | 2,
		    equalize-column-widths?: equalize-column-widths?,
		    initargs)
	    end
	  end
        end;
    layout-table(table, sheet);
    when (multiple-columns?)
      layout-multiple-columns(table, sheet, multiple-columns?, multiple-columns-x-spacing)
    end;
    if (x & y)
      %set-sheet-position(table, x, y)
    else
      let (x, y) = sheet-caret-position(sheet);
      %set-sheet-position(table, x, y)
    end;
    recompute-region(table);
    when (sheet-drawing?(sheet))
      repaint-sheet(table, $everywhere)
    end;
    when (move-caret?)
      move-caret-beyond-output-record(sheet, table)
    end;
    table
  end
end method do-formatting-table;

define method do-formatting-row
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #rest initargs, #key record-class = <row-record>, #all-keys)
 => (record :: <row-record>)
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[record-class:])
    apply(do-with-new-output-record-1,
	  sheet, continuation,
	  record-class,
	  (record-class == <row-record>) & <row-record>-constructor,
	  #f,
	  initargs)
  end
end method do-formatting-row;

define method do-formatting-column
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #rest initargs, #key record-class = <column-record>, #all-keys)
 => (record :: <column-record>)
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[record-class:])
    apply(do-with-new-output-record-1,
	  sheet, continuation,
	  record-class,
	  (record-class == <column-record>) & <column-record>-constructor,
	  #f,
	  initargs)
  end
end method do-formatting-column;

define method do-formatting-cell
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #rest initargs,
     #key align-x = #"left", align-y = #"top", min-width, min-height,
          record-class = <cell-record>,
     #all-keys) => (record :: <cell-record>)
  dynamic-extent(initargs);
  dynamic-bind (stream-text-output-record(sheet) = #f)
    with-keywords-removed (initargs = initargs,
			   #[align-x:, align-y:, min-width:, min-height:, record-class:])
      min-width := (min-width
                    & process-spacing-arg(sheet, min-width, #"horizontal",
					  form: #"formatting-cell")) | 0;
      min-height := (min-height
		     & process-spacing-arg(sheet, min-height, #"vertical",
					   form: #"formatting-cell")) | 0;
      with-caret-position-saved (sheet)
	apply(do-with-new-output-record-1,
	      sheet, continuation,
	      record-class,
	      (record-class == <cell-record>) & <cell-record>-constructor,
	      #f,
	      align-x: align-x, align-y: align-y,
	      min-width: min-width, min-height: min-height,
	      initargs)
      end
    end
  end
end method do-formatting-cell;
