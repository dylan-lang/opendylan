Module:       DUIM-formatting-internals
Synopsis:     DUIM formatted output
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Menu formatting, aka item list formatting

// Item list output record
define sealed class <item-list-record> (<sequence-record>)
  sealed slot %x-spacing :: <integer>,
    required-init-keyword: x-spacing:;
  sealed slot %y-spacing :: <integer>,
    required-init-keyword: y-spacing:;
  sealed slot %initial-spacing = #f,
    init-keyword: initial-spacing:;
  sealed slot %n-columns = #f,
    init-keyword: n-columns:;
  sealed slot %n-rows = #f,
    init-keyword: n-rows:;
  sealed slot %max-width  = #f,
    init-keyword: max-width:;
  sealed slot %max-height = #f,
    init-keyword: max-height:;
  sealed slot %row-wise? = #t,
    init-keyword: row-wise?:;
end class <item-list-record>;

define output-record-constructor <item-list-record>
    (#key parent, sheet, region, transform,
          x-spacing, y-spacing, initial-spacing,
          n-columns, n-rows, max-width, max-height, row-wise? = #t)
  parent: parent, sheet: sheet,
  region: region, transform: transform,
  x-spacing: x-spacing, y-spacing: y-spacing, initial-spacing: initial-spacing,
  n-columns: n-columns, n-rows: n-rows, max-width: max-width, max-height: max-height,
  row-wise?: row-wise?
end;

define method note-child-added
    (record :: <item-list-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;

define method match-output-records
    (record :: <item-list-record>,
     #key x-spacing, y-spacing, initial-spacing,
          n-columns, n-rows, max-width, max-height, row-wise? = #t,
     #all-keys) => (true? :: <boolean>)
  record.%x-spacing == x-spacing
  & record.%y-spacing == y-spacing
  & record.%initial-spacing == initial-spacing
  & record.%n-columns == n-columns
  & record.%n-rows == n-rows
  & record.%max-width == max-width
  & record.%max-height == max-height
  & record.%row-wise? == row-wise?
end method match-output-records;

define method do-item-list-cells
    (function :: <function>, item-list :: <item-list-record>) => ()
  do-table-elements(function, item-list, #"cell")
end method do-item-list-cells;

//---*** Reconcile with 'compose-space' and 'allocate-space'
define method layout-item-list 
    (item-list :: <item-list-record>, sheet :: <output-recording-mixin>) => ()
  // We will set the local variables NROWS and NCOLUMNS below, but we never
  // change the slot values themselves, because that will break redisplay.
  // Ditto, X-SPACING and Y-SPACING.
  let ncolumns = item-list.%n-columns;
  let nrows    = item-list.%n-rows;
  let ncells = 0;
  let max-cell-width  = 0;
  let max-cell-height = 0;
  let x-spacing = item-list.%x-spacing;
  let y-spacing = item-list.%y-spacing;
  let initial-spacing = item-list.%initial-spacing;
  let max-width  = item-list.%max-width;	// don't make the men;
  let max-height = item-list.%max-height;	// exceed these bound;
  let row-wise? = item-list.%row-wise?;
  local method count-cells (cell) => ()
	  let (width, height) = box-size(cell);
	  max!(max-cell-width,  max(width,  cell.%min-width));
	  max!(max-cell-height, max(height, cell.%min-height));
	  inc!(ncells)
	end method;
  do-item-list-cells(count-cells, item-list);
  // Compute geometry
  case
    ncolumns =>
      nrows := max(ceiling/(ncells, ncolumns), 1);
    nrows =>
      ncolumns := max(ceiling/(ncells, nrows), 1);
    max-height =>
      // Could compute nrows/ncols better...
      let total-height = max-cell-height;
      let count = 0;
      block (break)
	while (#t)
	  inc!(total-height, max-cell-height);
	  when (total-height > max-height)
	    break()
	  end;
	  inc!(count);
	  inc!(total-height, y-spacing)
	end
      end;
      nrows := max(count, 1);
      ncolumns := max(ceiling/(ncells, nrows), 1);
    max-width =>
      let spacing = x-spacing | text-size(sheet, ' ');
      ncolumns := block (break)
		    let total-width = spacing;
		    local method sum-width (cell) => ()
			    inc!(total-width, box-width(cell) + spacing);
			    when (total-width > max-width)
			      break()
			    end
			  end method;
		    do-item-list-cells(sum-width, item-list);
		    max(ncells, 1)
		  end
	          // Won't fit in one row, use a more conservative computation
	          // that uses max-cell-width instead of the actual widths
	          // This could still be more accurate than it is.
                  | begin
		      let total-width = spacing;
		      let count = 0;
		      block (break)
			while (#t)
			  inc!(total-width, max-cell-width + spacing);
			  when (total-width > max-width)
			    break()
			  end;
			  inc!(count)
			end
		      end;
		      max(count, 1)
		    end;
	nrows := max(ceiling/(ncells, ncolumns), 1);
    otherwise =>
      // Try to make this a golden-ratio menu
      // Deduce golden ratio from other parameters
      ncolumns := max(floor/(sqrt(ncells * max-cell-width * max-cell-height / 1.6),
                             max(max-cell-width, 1)), 1);
      nrows := max(ceiling/(ncells, ncolumns), 1);
  end;
  let row-heights :: <simple-object-vector>
    = make(<simple-vector>, size: nrows, fill: 0);
  let column-widths :: <simple-object-vector>
    = make(<simple-vector>, size: ncolumns, fill: 0);
  let row-count = 0;
  let column-count = 0;
  // Collect row heights and column widths into temp arrays.
  // We need to remember for each row its total height and
  // the difference between the smallest top and the largest top.
  // For each row remember the total height and then remember the maximum
  // difference between the row top and the y-position of the row.
  local method size-cells (cell) => ()
	  let (width, height) = box-size(cell);
	  max!(column-widths[column-count],
	       max(width, cell.%min-width));
	  max!(row-heights[row-count],
	       max(height, cell.%min-height));
	  if (row-wise?)
	    inc!(column-count);
	    when (column-count = ncolumns)
	      inc!(row-count);
	      column-count := 0
	    end
	  else
	      inc!(row-count);
	    when (nrows = row-count)
	      inc!(column-count);
	      row-count := 0
	    end
	  end
	end method;
  do-item-list-cells(size-cells, item-list);
  // Now default the x-spacing to a spacing that spreads the
  // columns evenly over the entire width of the menu
  unless (x-spacing)
    x-spacing := if (max-width)
		   let total-width = 0;
		   for (column :: <integer> from 0 below n-columns)
		     inc!(total-width, column-widths[column])
		   end;
		   floor/(max-width - total-width, ncolumns + 1)
		 else
		   text-size(sheet, ' ')
		 end
  end;
  row-count := 0;
  column-count := 0;
  let total-height = 0;
  let total-width = if (initial-spacing) x-spacing else 0 end;
  local method adjust-cells (cell) => ()
	  let (cell-width, cell-height) = box-size(cell);
	  let column-width = column-widths[column-count];
	  let row-height = row-heights[row-count];
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
	  if (row-wise?)
	    inc!(total-width, column-width + x-spacing);
	    inc!(column-count);
	    when (ncolumns = column-count)
	      total-width := if (initial-spacing) x-spacing else 0 end;
	      inc!(total-height, row-height + y-spacing);
	      column-count := 0;
	      inc!(row-count)
	    end
	  else
	    inc!(total-height, row-height + y-spacing);
	    inc!(row-count);
	    when (nrows = row-count)
	      total-height := 0;
	      inc!(total-width, column-width + x-spacing);
	      row-count := 0;
	      inc!(column-count)
	    end
	  end
	end method;
  do-item-list-cells(adjust-cells, item-list)
end method layout-item-list;

define method do-formatting-items
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #rest initargs,
     #key x, y, x-spacing, y-spacing, initial-spacing,
          n-columns, n-rows, max-width, max-height, row-wise? = #t,
          record-class = <item-list-record>, move-caret? = #t,
     #all-keys) => (record :: <item-list-record>)
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[x:, y:, x-spacing:, y-spacing:, initial-spacing:,
						n-rows:, n-columns:, max-width:, max-height:,
						row-wise?:, record-class:, move-caret?:])
    let item-list
      = with-output-recording-options (sheet, draw?: #f, record?: #t)
          with-end-of-line-action (sheet, #"allow")
            with-end-of-page-action (sheet, #"allow")
	      apply(do-with-new-output-record,
		    sheet, continuation,
		    record-class: record-class,
		    constructor: (record-class == <item-list-record>)
		                 & <item-list-record>-constructor,
		    x-spacing:
		      process-spacing-arg(sheet, x-spacing, #"horizontal",
					  form: #"formatting-items"),
		    y-spacing:
		      process-spacing-arg(sheet, y-spacing, #"vertical",
					  form: #"formatting-items")
		      | 2,
		    initial-spacing: initial-spacing,
		    n-columns: n-columns, n-rows: n-rows,
		    max-width: max-width, max-height: max-height,
		    row-wise?: row-wise?,
		    initargs)
	    end
	  end
        end;
    layout-item-list(item-list, sheet);
    if (x & y)
      %set-sheet-position(item-list, x, y)
    else
      let (x, y) = sheet-caret-position(sheet);
      %set-sheet-position(item-list, x, y)
    end;
    recompute-region(item-list);
    when (sheet-drawing?(sheet))
      repaint-sheet(item-list, $everywhere)
    end;
    when (move-caret?)
      move-caret-beyond-output-record(sheet, item-list)
    end;
    item-list
  end
end method do-formatting-items;

define method format-items
    (items :: <sequence>, sheet :: <output-recording-mixin>, object-printer :: <function>,
     #key x, y, x-spacing, y-spacing, initial-spacing, row-wise? = #t,
          n-rows, n-columns, max-width, max-height,
          record-class = <item-list-record>,
          cell-align-x = #"left", cell-align-y = #"top") => (record :: <item-list-record>)
  formatting-items (sheet,
		    x: x, y: y, x-spacing: x-spacing, y-spacing: y-spacing,
                    initial-spacing: initial-spacing, row-wise?: row-wise?,
		    n-rows: n-rows, n-columns: n-columns,
                    max-width: max-width, max-height: max-height,
		    record-class: record-class)
    local method format-item (item) => ()
	    formatting-cell (sheet,
			     align-x: cell-align-x,
			     align-y: cell-align-y)
	      object-printer(item, sheet)
	    end
	  end method;
    do(format-item, items)
  end
end method format-items;
