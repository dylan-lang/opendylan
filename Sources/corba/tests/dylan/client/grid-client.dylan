Module: grid-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite grid-test-suite ()
  test grid-request-tests;
end suite;

define constant $grid-ior-file :: <string> = "c:\\temp\\grid.ior";

define constant $wrong-grid-ior-file :: <string> = "c:\\temp\\wrong-grid.ior";

define test grid-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = corba/orb/get-default-context(orb);
  let reference = as(<grid>, corba/orb/file-to-object(orb, $grid-ior-file));
  let width = 7;
  let height = 11;
  let x = 2;
  let y = 3;
  let value = 5;
  check("width reaches server", \=, (grid/width(reference) := width), width);
  check("height reaches server", \=, (grid/height(reference) := height), height);
  check("width comes back to client", \=, grid/width(reference), width);
  check("height comes back to client", \=, grid/height(reference), height);
  check-false("value reaches server", grid/set(reference, x, y, value));
  check("value comes back to client", \=, grid/get(reference, x, y), value);
  check("height comes back to client asynchronously", \=, grid/asynch-height(reference), height);
  corba/context/set-one-value(context, "Extra", as(corba/<any>, "Yes"));
  check("width comes back to client with context2", \=, grid/context-get-width(reference, context: context), width + 1);

  let wrong-reference = as(<grid>, corba/orb/file-to-object(orb, $wrong-grid-ior-file));
  check("width reaches server", \=, (grid/width(wrong-reference) := width), width);
  check("height reaches server", \=, (grid/height(wrong-reference) := height), height);
  check("width comes back to client", \=, grid/width(wrong-reference), width);
  check("height comes back to client", \=, grid/height(wrong-reference), height);
  check-false("value reaches server", grid/set(wrong-reference, x, y, value));
  check("value comes back to client", \=, grid/get(wrong-reference, x, y), value);
  check("height comes back to client asynchronously", \=, grid/asynch-height(wrong-reference), height);
  corba/context/set-one-value(context, "Extra", as(corba/<any>, "Yes"));
  check("width comes back to client with context2", \=, grid/context-get-width(wrong-reference, context: context), width + 1);
end test;

/// ASYNCHRONOUS CLIENT IMPLEMENTION USING DII

define method grid/asynch-height (object :: <grid>)
 => (result :: corba/<short>)
  let (result, request) = corba/object/create-request(object,
					       corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB")),
					       "_get_height",
					       make(corba/<nvlist>),
					       make(corba/<namedvalue>,
						    name: "result",
						    argument: make(corba/<any>, type: corba/$short-typecode),
						    len: 0,
						    arg-modes: 0),
					       0); // ---*** which request flags?
  corba/request/get-response(request, corba/$inv-no-response); // NB don't block because not even sent!
  corba/request/send(request, 0);
  corba/request/get-response(request, 0); // NB now block until result arrives
  as(corba/<short>, corba/namedvalue/argument(result));
end method;
