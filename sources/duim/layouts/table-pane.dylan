Module:       duim-layouts-internals
Synopsis:     DUIM layouts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Table panes

define open abstract class <table-layout> 
    (<layout-border-mixin>,
     <horizontal-layout-mixin>,
     <vertical-layout-mixin>,
     <layout-pane>)
  sealed slot table-contents = #f,
    setter: %contents-setter;
  sealed constant slot table-rows :: false-or(<integer>) = #f,
    init-keyword: rows:;
  sealed constant slot table-columns :: false-or(<integer>) = #f,
    init-keyword: columns:;
  sealed slot %row-space-requirements  :: false-or(<sequence>) = #f;
  sealed slot %cell-space-requirements :: false-or(<sequence>) = #f;
end class <table-layout>;

define method initialize 
    (table :: <table-layout>, #key contents, children, rows, columns) => ()
  // Either the contents should be a sequence of sequences of sheets,
  // or supply children with a number of rows and columns
  assert(~(contents & children),
	 "You can't supply both contents and children to a table pane");
  next-method();
  when (contents | (children & ~empty?(children)))
    // Compute size of contents array and allocate it
    if (children)
      case
	rows    => columns := columns | ceiling/(size(children), rows);
	columns => rows    := rows    | ceiling/(size(children), columns);
	otherwise => error("You must supply either rows or columns for table panes");
      end
    elseif (contents)
      select (contents by instance?)
	<sequence> =>
	  rows    := size(contents);
	  columns := if (empty?(contents))
		       0		// he asked for a useless table
		     else
		       reduce(method (v, x) max(v, size(x)) end, 0, contents)
		     end;
	<array> => 
	  rows    := dimension(contents, 0);
	  columns := dimension(contents, 1);
      end
    end;
    let rows    :: <integer> = rows;	// tighten up the types
    let columns :: <integer> = columns;
    table.%contents := make(<array>,
			    dimensions: list(rows, columns));
    if (children)
      // Initialize contents from a sequence of children
      assert(size(children) <= rows * columns,
	     "You are giving too many children to %=", table);
      fill-array!(table-contents(table), children)
    elseif (contents & ~empty?(contents))
      select (contents by instance?)
	<sequence> =>
	  // Initialize children nested sequence of contents
	  for (cells in contents,	// a row of panes, that is...
	       row :: <integer> from 0)
	    for (cell in cells,		// a cell is one pane in a row
		 column :: <integer> from 0)
	      when (cell)
		add-child(table, cell, row: row, column: column)
	      end
	    end
	  end;
	<array> => 
	  // Initialize children from a contents array
	  for (row :: <integer> from 0 below rows)
	    for (column :: <integer> from 0 below columns)
	      let cell = contents[row, column];
	      when (cell)
		add-child(table, cell, row: row, column: column)
	      end
	    end
	  end
      end
    end
  end
end method initialize;

// Don't change the order of the sheets, because that will break the layout
// Note that 'raise-sheet' will arrange to raise mirrors, which is OK
define method do-raise-sheet
    (parent :: <table-layout>, sheet :: <sheet>, #key activate? = #t) => ()
  ignore(activate?);
  #f
end method do-raise-sheet;

define method table-contents-setter 
    (contents :: <array>, table :: <table-layout>)
  assert(size(dimensions(contents)) = 2,
         "You are supplying a non-two dimensional array for %=", table);
  table.%contents := contents
end method table-contents-setter;

define method sheet-children-setter
    (children :: <sequence>, table :: <table-layout>) => (children :: <sequence>)
  let rows    = table-rows(table);
  let columns = table-columns(table);
  case
    rows    => columns := columns | ceiling/(size(children), rows);
    columns => rows    := rows    | ceiling/(size(children), columns);
    otherwise => error("You must supply either rows or columns for table panes");
  end;
  next-method();
  table.%contents := make(<array>, dimensions: list(rows, columns));
  fill-array!(table-contents(table), children);
  children
end method sheet-children-setter;

define method table-end-position
    (table :: <table-layout>) => (index :: <integer>)
  block (return)
    let contents = table-contents(table);
    let nrows  :: <integer> = dimension(contents, 1);
    let ncells :: <integer> = dimension(contents, 0) * nrows;
    // Finds the first empty cell at the _end_ of the table
    for (index :: <integer> from ncells - 1 to 0 by -1)
      let (row, column) = floor/(index, nrows);
      when (contents[row, column])
	return(if (index < ncells - 1)
		 index + 1
	       else
		 error("The table pane %= is full", table)
	       end)
      end
    end;
    // If the table is empty, the last shall be the first
    0
  end
end method table-end-position;

define method do-add-child
    (table :: <table-layout>, child :: <sheet>,
     #key index = #"end", row, column) => ()
  let contents = table-contents(table);
  unless (row & column)
    let nrows = dimension(contents, 0);
    let index = select (index)
		  #"start"  => 0;
		  #"end"    => table-end-position(table);
		  otherwise => index;
		end;
    let (the-column, the-row) = floor/(index, nrows);
    row := the-row;
    column := the-column;
  end;
  assert(~contents[row, column],
	 "Attempting to replace an existing child in %= using ADD-CHILD", table);
  contents[row, column] := child;
  next-method()
end method do-add-child;

define method do-remove-child 
    (table :: <table-layout>, child :: <sheet>) => ()
  let contents = table-contents(table);
  let (row, col) = find-table-child(contents, child);
  when (row)
    contents[row, col] := #f
  end;
  next-method();
end method do-remove-child;

define method do-replace-child
    (table :: <table-layout>, old-child :: <sheet>, new-child :: <sheet>) => ()
  let contents = table-contents(table);
  let (row, col) = find-table-child(contents, old-child);
  contents[row, col] := new-child;
  next-method()
end method do-replace-child;

define method find-table-child
    (contents :: <array>, child) => (row, col)
  let nrows :: <integer> = dimension(contents, 0);
  let ncols :: <integer> = dimension(contents, 1);
  block (return)
    for (row :: <integer> from 0 below nrows)
      for (col :: <integer> from 0 below ncols)
	when (contents[row, col] == child)
	  return(row, col)
	end
      end
    end;
    values(#f, #f)
  end
end method find-table-child;


// Options can be any of the pane sizing options
// Note that this has no syntax 
define macro tabling
  { tabling (#rest ?options:expression)
      ?contents:*
    end }
    => { make(<table-layout>, children: vector(?contents), ?options) }
 contents:
  { } => { }
  { ?pane-spec:*; ... } => { ?pane-spec, ... }
 pane-spec:
  { ?pane:expression } => { ?pane }
end macro tabling;

define method do-compose-space
    (table :: <table-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  let contents = table-contents(table);
  if (~contents | empty?(contents))
    default-space-requirement(table, width: width, height: height)
  else
    let nrows :: <integer> = dimension(contents, 0);
    let ncols :: <integer> = dimension(contents, 1);
    // Overall preferred/min/max width and height
    let ow     :: <integer> = 0;
    let omin-w :: <integer> = 0;
    let omax-w :: <integer> = 0;
    let oh     :: <integer> = 0;
    let omin-h :: <integer> = 0;
    let omax-h :: <integer> = 0;
    let row-srs  :: <stretchy-object-vector> = make(<stretchy-vector>);
    let cell-srs :: <stretchy-object-vector> = make(<stretchy-vector>);
    // Iterate over the rows, determining the height of each
    for (row :: <integer> from 0 below nrows)
      let height     :: <integer> = 0;
      let min-height :: <integer> = 0;
      let max-height :: <integer> = 0;
      for (cell :: <integer> from 0 below ncols)
	let item = contents[row, cell];
	when (item & ~sheet-withdrawn?(item))
	  let space-req = compose-space(item);
	  let (w, w-, w+, h, h-, h+) = space-requirement-components(item, space-req);
	  ignore(w, w-, w+);
	  // Max the heights
	  max!(height,     h);
	  max!(min-height, h-);
	  // That max height of the row is the largest max height
	  // for any cell in the row
	  max!(max-height, h+)
	end
      end;
      add!(row-srs,
	   make(<space-requirement>,
		width: 0,
		height: height, min-height: min-height, max-height: max-height));
      // Add the heights
      inc!(oh,     height);
      inc!(omin-h, min-height);
      inc!(omax-h, max-height)
    end;
    table.%row-space-requirements := row-srs;
    // Iterate over the cells determing the widths of each
    for (cell :: <integer> from 0 below ncols)
      let width     :: <integer> = 0;
      let min-width :: <integer> = 0;
      let max-width :: <integer> = 0;
      for (row :: <integer> from 0 below nrows)
	let item = contents[row, cell];
	when (item & ~sheet-withdrawn?(item))
	  let space-req = compose-space(item);
	  let (w, w-, w+, h, h-, h+) = space-requirement-components(item, space-req);
	  ignore(h, h-, h+);
	  // Max the widths
	  max!(width,     w);
	  max!(min-width, w-);
	  // That max width of the column is the largest max height
	  // for any cell in the column
	  max!(max-width, w+)
	end
      end;
      add!(cell-srs,
	   make(<space-requirement>,
		width: width, min-width: min-width, max-width: max-width,
		height: 0));
      inc!(ow,     width);
      inc!(omin-w, min-width);
      inc!(omax-w, max-width)
    end;
    let border*2 = layout-border(table) * 2;
    let total-x-spacing :: <integer>
      = (layout-x-spacing(table) * (ncols - 1)) + border*2;
    let total-y-spacing :: <integer>
      = (layout-y-spacing(table) * (nrows - 1)) + border*2;
    inc!(ow,     total-x-spacing);
    inc!(omin-w, total-x-spacing);
    inc!(omax-w, total-x-spacing);
    inc!(oh,     total-y-spacing);
    inc!(omin-h, total-y-spacing);
    inc!(omax-h, total-y-spacing);
    when (width)
      ow := max(omin-w, min(width, omax-w))
    end;
    when (height)
      oh := max(omin-h, min(height, omax-h))
    end;
    table.%cell-space-requirements := cell-srs;
    make(<space-requirement>,
	 width:  ow, min-width:  omin-w, max-width:  omax-w,
	 height: oh, min-height: omin-h, max-height: omax-h)
  end
end method do-compose-space;

define method do-allocate-space
    (table :: <table-layout>, width :: <integer>, height :: <integer>) => ()
  let contents = table-contents(table);
  when (contents & ~empty?(contents))
    let space-req = compose-space(table, width: width, height: height);
    let nrows :: <integer> = dimension(contents, 0);
    let ncols :: <integer> = dimension(contents, 1);
    let x-spacing :: <integer> = layout-x-spacing(table);
    let y-spacing :: <integer> = layout-y-spacing(table);
    let border    :: <integer> = layout-border(table);
    let total-x-spacing :: <integer> = x-spacing * (ncols - 1);
    let total-y-spacing :: <integer> = y-spacing * (nrows - 1);
    let row-heights
      = compose-space-for-items
	  (table,
	   height - total-y-spacing, space-req, table.%row-space-requirements,
	   space-requirement-height, space-requirement-min-height, space-requirement-max-height,
	   identity,
	   ratios: layout-y-ratios(table));
    let cell-widths
      = compose-space-for-items
	  (table,
	   width - total-x-spacing, space-req, table.%cell-space-requirements,
	   space-requirement-width, space-requirement-min-width, space-requirement-max-width,
	   identity,
	   ratios: layout-x-ratios(table));
    let y :: <integer> = border;
    for (row :: <integer> from 0 below nrows,
	 row-height :: <integer> in row-heights)
      let cell-widths = cell-widths;
      let x :: <integer> = border;
      for (cell :: <integer> from 0 below ncols,
	   cell-width :: <integer> in cell-widths)
        let item = contents[row, cell];
        when (item & ~sheet-withdrawn?(item))
          let item-space
            = compose-space(item, width: cell-width, height: row-height);
	  let (w, w-, w+, h, h-, h+) = space-requirement-components(item, item-space);
	  ignore(w, h);
	  let item-width  :: <integer> = constrain-size(cell-width, w-, w+);
          let item-height :: <integer> = constrain-size(row-height, h-, h+);
	  let aligned-x :: <integer>
	    = layout-align-sheet-x(table, item,
				   x, x + cell-width - item-width,
				   key: cell);
	  let aligned-y :: <integer>
	    = layout-align-sheet-y(table, item,
				   y, y + row-height - item-height,
				   key: row);
	  set-sheet-edges(item,
			  aligned-x, aligned-y,
			  // Ensure end cells stay within table
			  min(aligned-x + item-width, width),
			  min(aligned-y + item-height, height))
	end;
	inc!(x, cell-width + x-spacing)
      end;
      inc!(y, row-height + y-spacing)
    end
  end
end method do-allocate-space;


/// Default implementation

define sealed class <table-layout-pane> (<table-layout>)
  keyword accepts-focus?: = #f;
end class <table-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <table-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<table-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<table-layout-pane>));
define sealed domain initialize (<table-layout-pane>);


/// Grid panes

define open abstract class <grid-layout> (<table-layout>)
  sealed slot cell-space-requirement :: <space-requirement>,
    required-init-keyword: cell-space-requirement:;
end class <grid-layout>;

define method do-compose-space
    (grid :: <grid-layout>, #key width, height)
 => (space-req :: <space-requirement>)
  let contents = table-contents(grid);
  if (~contents | empty?(contents))
    default-space-requirement(grid, width: width, height: height)
  else
    let nrows :: <integer> = dimension(contents, 0);
    let ncols :: <integer> = dimension(contents, 1);
    let border*2 = layout-border(grid) * 2;
    let total-x-spacing :: <integer>
      = (layout-x-spacing(grid) * (ncols - 1)) + border*2;
    let total-y-spacing :: <integer>
      = (layout-y-spacing(grid) * (nrows - 1)) + border*2;
    let (width :: <integer>, min-width :: <integer>, max-width :: <integer>,
	 height :: <integer>, min-height :: <integer>, max-height :: <integer>)
      = space-requirement-components(grid, cell-space-requirement(grid));
    width      := width      * ncols + total-x-spacing;
    min-width  := min-width  * ncols + total-x-spacing;
    min-width  := max-width  * ncols + total-x-spacing;
    height     := height     * nrows + total-y-spacing;
    min-height := min-height * nrows + total-y-spacing;
    min-height := max-height * nrows + total-y-spacing;
    make(<space-requirement>,
	 width:  width,  min-width:  min-width,  max-width:  max-width,
	 height: height, min-height: min-height, max-height: max-height)
  end
end method do-compose-space;

define method do-allocate-space
    (grid :: <grid-layout>, width :: <integer>, height :: <integer>) => ()
  let contents = table-contents(grid);
  when (contents & ~empty?(contents))
    let nrows  :: <integer> = dimension(contents, 0);
    let ncols  :: <integer> = dimension(contents, 1);
    let border :: <integer> = layout-border(grid);
    let x-spacing :: <integer> = layout-x-spacing(grid);
    let y-spacing :: <integer> = layout-y-spacing(grid);
    let (cell-width  :: <integer>, min-width, max-width,
	 cell-height :: <integer>, min-height, max-height)
      = space-requirement-components(grid, cell-space-requirement(grid));
    ignore(min-width, max-width, min-height, max-height);
    //--- This should give extra space to the cell, obeying min and max sizes
    let y :: <integer> = border;
    for (row :: <integer> from 0 below nrows)
      let x :: <integer> = border;
      for (cell :: <integer> from 0 below ncols)
        let item = contents[row, cell];
        when (item & ~sheet-withdrawn?(item))
	  set-sheet-edges(item, x, y, x + cell-width, y + cell-height)
	end;
	inc!(x, cell-width + x-spacing)
      end;
      inc!(y, cell-height + y-spacing)
    end
  end
end method do-allocate-space;


/// Default implementation

define sealed class <grid-layout-pane> (<grid-layout>)
  keyword accepts-focus?: = #f;
end class <grid-layout-pane>;

define method class-for-make-pane 
    (framem :: <frame-manager>, class == <grid-layout>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<grid-layout-pane>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<grid-layout-pane>));
define sealed domain initialize (<grid-layout-pane>);
