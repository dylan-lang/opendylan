Module: grid-server
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DYNAMIC (FORWARDING) IMPLEMENTATION

define class <dynamic-grid-implementation> (portableserver/<dynamic-servant>)
  constant slot grid-forward :: false-or(corba/<object>) = #f, init-keyword: forward:;
end class;

define method corba/serverrequest/invoke
    (request :: corba/<serverrequest>, servant :: <dynamic-grid-implementation>)
 => ()
  error(make(portableserver/<forwardrequest>, forward-reference: grid-forward(servant)))
end method;

define method portableserver/servant/primary-interface
    (servant :: <dynamic-grid-implementation>, objectid :: <string>, poa :: portableserver/<poa>)
 => (repository-id :: <string>)
  "IDL:grid:1.0"
end method;


/// IMPLEMENTATION

define class <grid-implementation> (<grid-servant>)
  slot grid-array :: false-or(<array>) = make(<array>, dimensions: #[100, 100]), init-keyword: array:;
end class;

define method grid/width (object :: <grid-implementation>)
  => (result :: corba/<short>)
  dimension(grid-array(object), 0);
end method;

define method grid/width-setter (value :: corba/<short>, object :: <grid-implementation>)
  => (value :: corba/<short>)
  grid-array(object) := make(<array>, dimensions: vector(value, grid/height(object)));
  // ---*** could try and copy old elements that fit
  value;
end method;

define method grid/height (object :: <grid-implementation>)
  => (result :: corba/<short>)
  dimension(grid-array(object), 1);
end method;

define method grid/height-setter (value :: corba/<short>, object :: <grid-implementation>)
  => (value :: corba/<short>)
  grid-array(object) := make(<array>, dimensions: vector(grid/width(object), value));
  value
end method;

define method grid/set (object :: <grid-implementation>, n :: corba/<short>, m :: corba/<short>, value :: corba/<long>)
 => ()
  aref(grid-array(object), n, m) := value;
end method;

define method grid/get (object :: <grid-implementation>, n :: corba/<short>, m :: corba/<short>)
 => (result :: corba/<long>)
  aref(grid-array(object), n, m);
end method;

define method grid/context-get-width (object :: <grid-implementation>, #key context)
 => (result :: corba/<short>)
  let extras = corba/context/get-values(context, "", 0, "Extra");
  let extra = ~empty?(extras) & as(<string>, corba/namedvalue/argument(extras[0]));
  grid/width(object) + select (extra by \=)
			 #f, "No" => 0;
			 "Yes" => 1;
		       end select;
end method;

define constant $grid-ior-file :: <string> = "c:\\temp\\grid.ior";

define constant $wrong-grid-ior-file :: <string> = "c:\\temp\\wrong-grid.ior";

define method start-grid-server ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a grid object 
  let grid = make(<grid-implementation>);

  // implicitly activate them as well as getting reference
  let gridref = portableserver/poa/servant-to-reference(root-poa, grid);

  // and lets make the wrong object to test forwarding as well
  let wrong-grid = make(<dynamic-grid-implementation>, forward: gridref);
  let wrong-gridref = portableserver/poa/servant-to-reference(root-poa, wrong-grid);

  // create an ior string to pass to clients via file
  corba/orb/object-to-file(orb, $grid-ior-file, gridref);
  corba/orb/object-to-file(orb, $wrong-grid-ior-file, wrong-gridref);

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(poa-manager);

  /// block until all ORB process done
  /// corba/orb/run(orb);
end method;

register-server(start-grid-server);
