Module:    OLE-Automation
Synopsis:  simple way to create a class factory 
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Server-side utilities have been moved to "../com/factory.dylan"


// Automation controller
// Client-side utilities:

define function create-dispatch ( class-id :: <object>,
				 #key context = $CLSCTX-ALL,
				      interface-id = $IID-IDispatch)
 => dispinterface :: <LPDISPATCH>;
  create-COM-instance(class-id, context: context,
		      interface-id: interface-id,
		      class: <LPDISPATCH>)
end create-dispatch;

// Like with-COM-interface but default to $IID-IDispatch, <LPDISPATCH>.
define macro with-dispatch-interface
  { with-dispatch-interface (?:name :: ?type:expression = ?class-id:expression,
			     #key ?context:expression = $CLSCTX-ALL,
			          ?interface-id:expression = $IID-IDispatch)
      ?body:body
    end }
    => { with-COM-interface (?name :: ?type = ?class-id,
			     context: ?context,
			     interface-id: ?interface-id)
	  ?body
         end }

  // Old cruddy syntax
  { with-dispatch-interface ?:name (?class-id:expression, ?more:*) ?:body end }
     => { with-dispatch-interface (?name = ?class-id, ?more) ?body end }

type:
    { <object> } => { <LPDISPATCH> }
    { ?:expression } => { ?expression }
end macro;
