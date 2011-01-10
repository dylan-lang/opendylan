Module:       database-viewer
Author:       Andy Armstrong, Keith Playford
Synopsis:     A simple database viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Parameters.

// define constant $default-database-viewer-database = "Northwind";
define constant $default-database-viewer-database = "testDB";
define constant $default-database-viewer-width  = 700;
define constant $default-database-viewer-height = 500;

//// Select viewer frame.

define frame <database-viewer> (<simple-frame>)
  // I don't seem to able to introspect to find this out from DUIM...
  slot connection :: <odbc-connection>,
    required-init-keyword: connection:;
  slot columns-displayed :: <integer> = 0;
  pane file-menu (frame)
    make(<menu>,
         label: "&File",
         children:
           vector(make(<menu-button>,
                       label: "New Select Viewer",
                       activate-callback: 
                         method (#rest args)
			   let name = choose-string(title: "Database name", owner: frame);
			   name & spawn-database-viewer(name)
                         end),
                  make(<menu-button>,
                       label: "Close",
                       activate-callback:
                         method (sheet)
                           exit-frame(sheet-frame(sheet))
                         end)));
  pane tree-pane (frame)
    make(<tree-control>,
	 roots: vector(frame.connection),
	 depth: 2,
	 value: frame.connection,
	 /*
	 icon-function: method (info)
			  select (info by instance?)
			    <library-category-info> => $examples-bitmap;
			    <library-pack-info>     => $examples-bitmap;
			    <library-info>          => $project-bitmap;
			  end select;
			end method,
	 */
	 children-predicate: method (node)
			       select (node by instance?)
				 <connection> => #t;
				 otherwise    => #f;
			       end
			     end,
	 children-generator: method (node)
			       select (node by instance?)
				 <connection> => list-all-catalogs(node);
				 otherwise    => #[];
			       end
			     end,
	 use-buttons-only?: #f,
	 /*
	 activate-callback: method (gadget)
			      let dialog = sheet-frame(gadget);
			      exit-dialog(dialog)
			    end,
	 value-changed-callback: method (gadget)
				   let dialog = sheet-frame(gadget);
				   let info = gadget-value(gadget);
				   note-example-selected(dialog, info)
				 end method,
	 */
	 label-key: method (node)
		      select (node by instance?)
			<connection>   => datasource-name(database(node));
			<odbc-catalog> => database-object-name(node);
			otherwise      => format-to-string("%=", node);
		      end
		    end);
  pane sql-pane (frame)
    make(<combo-box>, 
         items:             #(),
         activate-callback: sql-entry-callback);
  pane results-pane (frame)
    make(<table-control>, headings: #[], generators: #[], items: #[]);
  pane splitter-pane (frame)
    make(<row-splitter>,
	 ratios: #[1, 2],
	 children: vector(frame.tree-pane, frame.results-pane));
  layout (frame)
    vertically (spacing: 2)
      make(<separator>);
      horizontally (spacing: 2, y-alignment: #"center")
        make(<label>, label: "Select"); 
        frame.sql-pane;
      end;
      make(<separator>);
      frame.splitter-pane;
    end;
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(file-menu(frame)));
  status-bar (frame)
    make(<status-bar>);
  keyword title:  
    = format-to-string("Select Viewer on %s", $default-database-viewer-database);
  keyword width:  = $default-database-viewer-width;
  keyword height: = $default-database-viewer-height;
end frame;

define method choose-string 
    (#key title = "Select a string",
          port = default-port(),
          owner)
 => (string :: false-or(<string>))
  with-frame-manager (frame-manager(owner))
    let text-field 
      = make(<text-field>, activate-callback: exit-dialog);
    let dialog
      = make(<dialog-frame>, 
	     title: title, 
	     owner: owner,
	     layout: text-field,
	     input-focus: text-field);
    start-dialog(dialog)
      & gadget-value(text-field)
  end
end method choose-string;

define method spawn-database-viewer 
    (database :: <string>) => ()
  let connection = open-database(database);
  make-application-thread
    (function: method () start-frame(make(<database-viewer>, connection: connection)) end);
end method spawn-database-viewer;

define method handle-event
    (frame :: <database-viewer>, event :: <frame-exited-event>) => ()
  close-database(frame.connection);
  next-method();
end method handle-event;

define method sql-entry-callback (pane :: <combo-box>) => ()
  let frame = sheet-frame(pane);
  let raw-sql-expr = gadget-value(pane);
  gadget-items(pane) := history-add(gadget-items(pane), raw-sql-expr);
  let sql-expr = concatenate("select ", raw-sql-expr);
  notify-user(format-to-string("SQL: %s", sql-expr),
	      title: "Debug",
	      owner: frame);
  let connection = frame.connection;
  block ()
    with-connection (connection)
      let (headings, results) = query-database(connection, sql-expr);
      display-query-results(frame, results-pane(frame), sql-expr, headings, results)
    end
  exception (error :: <diagnostic>)
    notify-user(format-to-string("SQL Error: %s", error),
		title: "Query error",
		owner: frame)
  end
end method sql-entry-callback;

define method display-query-results
    (frame :: <database-viewer>, pane :: <table-control>, query :: <byte-string>,
       headings :: <sequence>, results :: <sequence>) 
 => ()
  // Remove the existing columns.
  for (i from columns-displayed(frame) - 1 to 0 by -1)
    remove-column(pane, i);
  end;
  columns-displayed(frame) := 0;
  if (~empty?(results))
    // Compute the new columns.
    let cols = compute-query-columns(headings);
    for (col in cols, i from 0)
      add-column(pane, col, i);
    finally
      columns-displayed(frame) := i;
    end;
    // Install the items.
    // This little gyration works around what I think is a DUIM bug.
    gadget-items(pane) := #[]; 
    gadget-items(pane) := results;
    gadget-label(frame-status-bar(frame)) 
      := format-to-string("%d records returned for query: \"%s\"", 
                          size(results), query);
  end;
end method;

define method compute-query-columns 
    (headings :: <sequence>) => (columns :: <sequence>)
  map(method (heading, i) 
        make(<table-column>, 
             heading: heading, 
             generator: rcurry(element, i));
      end,
      headings, range(from: 0))
end method;

define method history-add 
    (seq :: <sequence>, entry :: <string>) => (new-seq :: <sequence>)
  // Ugly!
  let seq = remove(seq, entry, test: \=);
  pair(entry, as(<list>, seq));
end method;
