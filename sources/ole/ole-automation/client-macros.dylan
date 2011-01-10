Module:    ole-automation
Synopsis:  Macros for defining clients of dispatch objects.
Author:    Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open /*abstract*/ primary C-subtype <dispatch-client> (<C-interface>)
end C-subtype <dispatch-client>;


define open generic dispatch-client-uuid 
	(this :: type-union(<class>, <dispatch-client>)) 
     => (uuid :: false-or(<LPGUID>));


define method dispatch-client-uuid (this :: <dispatch-client>)
 => (uuid :: false-or(<LPGUID>))
  dispatch-client-uuid(object-class(this))
end;
    
define function make-dispatch-client 
	(#key disp-interface :: false-or(<C-interface>) = #f, 
	      class-id = #f, interface-ID = #f) => (r :: <LPDISPATCH>)
  if (class-id)
    create-dispatch(class-id, interface-ID: interface-ID | $IID-IDispatch)
  elseif (disp-interface)
    if (interface-ID)
      let (status :: <SCODE>, new-disp :: <LPDISPATCH>)
	      = QueryInterface(disp-interface, interface-ID);
      if (FAILED?(status))
	ole-error(status, QueryInterface, disp-interface, interface-ID);
      end if;
      new-disp
    else
      AddRef(disp-interface);
      disp-interface
    end if
  else
    error("make(<dispatch-client>, ...) not provided with a "
	  "disp-interface or class-id.");
  end if
end function make-dispatch-client;


define macro dispatch-client-definer
  {
    define ?modifiers:* dispatch-client ?:name (?supers:*)
      ?body:*
    end
  } => {
    define ?modifiers dispatch-client-help ?name (?supers)
      { ?body } { ?body } { ?body } { ?body } { ?body }
    end;
  }

  {
    define ?modifiers:* dispatch-client ?:name
      ?body:*
    end
  } => {
    define ?modifiers dispatch-client-help ?name ()
      { ?body } { ?body } { ?body } { ?body } { ?body }
    end;
  }
end macro dispatch-client-definer;


define macro dispatch-client-help-definer

  {
    define ?modifiers:* dispatch-client-help ?:name (?supers:*)
      { ?collection } { ?slots } { ?uuid } { ?getters } { ?body:* }
    end
  } => {
    define ?modifiers dispatch-client-class ?name (?supers, <dispatch-client>) 
				       (?supers, ?collection)
      ?slots
    end dispatch-client-class ?name;

    define constant "$" ## ?name ## "-uuid" :: false-or(<LPGUID>) = ?uuid;
    define method dispatch-client-uuid (class == ?name) 
				    => (uuid :: false-or(<LPGUID>));
      "$" ## ?name ## "-uuid"
    end method dispatch-client-uuid;

    distribute-over-semicolons(\define-dispid-getter, (?name), (?getters));

    define method make (class == ?name, #next next-method,
    			#rest keys,
			#key address = #f,
			     interface-ID :: false-or(<LPGUID>) = 
			                 "$" ## ?name ## "-uuid",
			#all-keys)
		    => (r :: ?name)
      if (address)
	next-method()
      else
	next-method(class, 
		    address: pointer-address(apply(make-dispatch-client, 
						   interface-ID: interface-ID, keys)))
      end if;
    end method make;

    distribute-over-semicolons(\define-stub, (?name), (?body))
  }

  // Note that duplicate superclasses will be filtered out later:
  collection:
    { } => { }
    { ?collection-line; ... } => { ?collection-line ... }
  collection-line:
    { size ?rest:* } => { <collection>, }
    { element ?rest:* } => { <collection>, }
    { constant ... } => { ... }
    { write-only ... } => { ... }
    { ?rest:* } => { }

  slots:
    { } => { }
    { ?property-mods? property ?:name ?rest:*, ?cancel; ... }
      => { slot ?name ## "-disp-id-internal", ?cancel; ... }
    { ?size-or-element? function ?:name ?rest:*, ?cancel; ... }
      => { slot ?name ## "-disp-id-internal", ?cancel; ... }
    { ?size-or-element? member-function ?:name ?rest:*, ?cancel; ... }
      => { slot ?name ## "-disp-id-internal", ?cancel; ... }
    { ?krap:*; ... } => { ... }
  uuid:
    { } => { #f }
    { uuid ?id:expression; ?krap:* }
      => { as(<LPGUID>, ?id) }
    { ?krap:*; ... } => { ... }
  cancel:
    { } => { }
    { disp-id: ?dispid:expression, ... } => { cancel-slot }
    { ?rest:*, ... } => { ... }
  getters:
    { } => { }
    { ?property-mods? property ?:name ?krap:*, ?rest:*; ... }
      => { getter ?name ## "-disp-id", ?name, ?rest; ... }
    { ?size-or-element? function ?:name ?krap:*, ?rest:*; ... }
      => { getter ?name ## "-disp-id", ?name, ?rest; ... }
    { ?size-or-element? member-function ?:name ?krap:*, ?rest:*; ... }
      => { getter ?name ## "-disp-id", ?name, ?rest; ... }
    { uuid ?krap:*; ... } => { ... }
  property-mods?:
    { } => { }
    { size ... } => { ... }
    { element ... } => { ... }
    { constant ... } => { ... }
    { write-only ... } => { ... }
  size-or-element?:
    { } => { }
    { size ... } => { ... }
    { element ... } => { ... }

end macro dispatch-client-help-definer;


define macro dispatch-client-class-definer
  {
    define ?modifiers:* dispatch-client-class ?:name (?supers) (?collection-supers)
      ?slots
    end
  } => {
    define ?modifiers C-subtype ?name (?supers, ?collection-supers)
      ?slots
    end C-subtype ?name;
  }

  supers:
    { } => { }
    { , ... } => { ... }	// Awful hack...
    { <collection>, ... } => { ... }
    { ?:name, ... } => { ?name, ... }
  collection-supers:
    { } => { }
    { , ... } => { ... }	// Awful hack...
    { <collection>, ?rest:* } => { <collection> }
    { ?:name, ... } => { ... }
  slots:
    { } => { }
    { ?rest:*, cancel-slot; ... } => { ... }
    { ?rest:*; ... } => { ?rest; ... }
end macro dispatch-client-class-definer;


define macro define-dispid-getter
  {
    define-dispid-getter((?class-name:name), (getter ?:name, ?pn:name ?rest:*))
  } => {
    define sealed inline method ?name (this :: ?class-name) => (r)
      get-dispid( ( if (~ slot-initialized?(this, ?name ## "-internal"))
		      ?name ## "-internal"(this) := 
			      get-id-of-name(this, get-c-name(?pn, ?rest))
		    else
		      ?name ## "-internal"(this)
		    end if 
		  ), ?rest
		)
    end method ?name;
  }
end macro define-dispid-getter;


define macro get-c-name
  { get-c-name(?name:expression) } => { stringify(?name) }
  { get-c-name(?name:expression, name: ?cname:expression, ?rest:*) }
    => { get-c-name(?cname, ?rest) }
  { get-c-name(?name:expression, c-name: ?cname:expression, ?rest:*) }
    => { get-c-name(?cname, ?rest) }
  { get-c-name(?name:expression, ?stuff:*, ?rest:*) }
    => { get-c-name(?name, ?rest) }
end macro get-c-name;


define macro get-dispid
  { get-dispid(?default:expression) } => { ?default }
  { get-dispid(?default:expression, disp-id: ?dispid:expression, ?rest:*) }
    => { get-dispid(as(<machine-word>, ?dispid), ?rest) }
  { get-dispid(?default:expression, ?stuff:*, ?rest:*) }
    => { get-dispid(?default, ?rest) }
end macro get-dispid;


define macro distribute-over-semicolons
  { distribute-over-semicolons(?macro:name, (?distributee:*), ()) } => { }

  { 
    distribute-over-semicolons(?macro:name, (?distributee:*), 
			       (?item:*; ?rest:*))
  } => {
    ?macro ## ""((?distributee), (?item));
    distribute-over-semicolons(?macro, (?distributee), (?rest))
  }
end macro distribute-over-semicolons;


define macro distribute-over-commas
  { distribute-over-commas(?macro:name, (?distributee:*), ()) } => { }

  { 
    distribute-over-commas(?macro:name, (?distributee:*), 
			       (?item:*, ?rest:*))
  } => {
    ?macro ## ""((?distributee), (?item));
    distribute-over-commas(?macro, (?distributee), (?rest))
  }
end macro distribute-over-commas;


define macro distribute-over-words
  { distribute-over-words(?macro:name, (?distributee:*), ()) } => { }

  { 
    distribute-over-words(?macro:name, (?distributee:*), 
			       (?item:name ?rest:*))
  } => {
    ?macro ## ""((?distributee), (?item));
    distribute-over-words(?macro, (?distributee), (?rest))
  }
end macro distribute-over-words;


define macro define-stub

  { 
    define-stub((?class-name:name),
		(?pmods:* property ?:name, ?attributes:*))
  } => {
    define property-stubs ?class-name (?pmods) (?pmods) (?pmods) 
			  ?name () :: <object>;
  }
  // Note: This must not use the binding pattern (?:name :: ?type) because
  // that causes the type to be prematurely parsed as an expression and
  // then dylan-automation-type-macro can't deconstruct it.
  { 
    define-stub((?class-name:name),
		(?pmods:* property ?:name \:: ?type:*, ?attributes:*))
  } => {
    define property-stubs ?class-name (?pmods) (?pmods) (?pmods) 
			  ?name () :: dylan-automation-type-macro(?type);
  }

  { 
    define-stub((?class-name:name),
		(?pmods:* property ?:name (?arg-list), ?attributes:*))
  } => {
    define property-stubs ?class-name (?pmods) (?pmods) (?pmods)
			  ?name (?arg-list) :: <object>;
  }

  { 
    define-stub((?class-name:name),
     (?pmods:* property ?:name (?arg-list) \:: ?type:*, ?attributes:*))
  } => {
    define property-stubs ?class-name (?pmods) (?pmods) (?pmods)
			  ?name (?arg-list) 
			  :: dylan-automation-type-macro(?type);
  }

  { 
    define-stub((?class-name:name),
      (?modifiers function ?:name ?args => ?results, ?attributes:*)
	       )
  } => {
    define stub {?modifiers} $DISPATCH-METHOD ?name ?class-name 
    		(?args) => (?results);
  }

  { 
    define-stub((?class-name:name),
      (?modifiers member-function ?:name ?args => ?results, ?attributes:*)
	       )
  } => {
    define stub {?modifiers} $DISPATCH-METHOD ?name ?class-name 
    		(?args) => (?results);
  }

  { 
    define-stub((?class-name:name),
      (?modifiers function ?:name ?args, ?attributes:*)
	       )
  } => {
    define stub {?modifiers} $DISPATCH-METHOD ?name ?class-name 
    		(?args) => ();
  }

  { 
    define-stub((?class-name:name),
      (?modifiers member-function ?:name ?args, ?attributes:*)
	       )
  } => {
    define stub {?modifiers} $DISPATCH-METHOD ?name ?class-name 
    		(?args) => ();
  }

  { define-stub((?class-name:name), (?krap:*)) } => { }

  modifiers:
    { } => { }
    { size ... } => { size ... }
    { element ... } => { element ... }

  args:
    { } => { }
    { ( ?arg-list ) } => { ?arg-list }

  results:
    { ( ?arg-list ) } => { ?arg-list }
    { ?arg-list } => { ?arg-list }
  
  arg-list:
    { } => { }
    { ?:name, ... } => 
      { ?name :: <object>, ... }
    // Note: This must not use the binding pattern (?:name :: ?type) because
    // that causes the type to be prematurely parsed as an expression and
    // then dylan-automation-type-macro can't deconstruct it.
    { ?:name \:: ?type:*, ... } => 
      { ?name :: dylan-automation-type-macro(?type), ... }

end macro define-stub;


define macro property-stubs-definer
  {
    define property-stubs ?class-name:name (?mods) (?constant?) (?write-only?)
			  ?:name (?args:*) \:: ?type:expression
  } => {
    define property-stubs-getter ?class-name (?mods) (?write-only?)
				 ?name (?args) :: ?type;
    define property-stubs-setter ?class-name (?mods) (?constant?)
				 ?name (?args) :: ?type;
  }

  mods:
    { } => { }
    { constant ... } => { ... }
    { write-only ... } => { ... }
    { ?other:name ... } => { ?other ... }
  constant?:
    { } => { }
    { constant ... } => { constant ... }
    { ?other:name ... } => { ... }
  write-only?:
    { } => { }
    { write-only ... } => { write-only ... }
    { ?other:name ... } => { ... }
end macro property-stubs-definer;


define macro property-stubs-getter-definer
  { define property-stubs-getter ?x:name (?mods:*) (write-only) ?rest:* } => { }

  {
    define property-stubs-getter ?class-name:name (?modifiers:*) ()
				 ?:name (?args:*) \:: ?type:expression
  } => {
    define inline method ?name (this :: ?class-name, ?args, 
    				#key default = $unsupplied) => (r)
      let r = apply-over-args(simple-invoke, 
			      (this, ?name ## "-disp-id"(this), 
			       $DISPATCH-PROPERTYGET, $LOCALE-USER-DEFAULT, 
			       default), (?args), ());
      if (unsupplied?(r))
	error("No value received and no default specified getting property %s",
	      ?"name");
      end if;
      if (subtype?(?type, <dispatch-client>) & instance?(r, <LPDISPATCH>))
	make(?type, disp-interface: r)
      else
	r
      end if
    end method ?name;
    define stub-sealing ?name (this :: ?class-name, ?args);
    distribute-over-words(\define-stub-modifier, 
			  (?name, (this :: ?class-name, ?args) => (r)),
			  (?modifiers));
  }
end macro property-stubs-getter-definer;


define macro property-stubs-setter-definer
  { define property-stubs-setter ?x:name (?mods:*) (constant) ?rest:* } => { }

  {
    define property-stubs-setter ?class-name:name (?mods:*) ()
				 ?:name (?args:*) \:: ?type:expression
  } => {
    define setter-stub {?mods} ?name ?class-name (?args) :: ?type;
  }
end macro property-stubs-setter-definer;


define macro setter-stub-definer
  { 
    define setter-stub {?modifiers} ?name:* ?class-name:name 
		       (?args:*) \:: ?type:expression
  } => {
    define method ?name ## "-setter" (v :: ?type, this :: ?class-name, ?args) 
				  => (v :: ?type)
      // Note: empty ?args results in extra ',' which apply-over-args ignores.
      apply-over-args(get-indexed-property-setter, 
		      (v, this, ?name ## "-disp-id"(this)), (?args), ())
    end method ?name ## "-setter";
    define stub-sealing ?name ## "-setter" 
	(v :: ?type, this :: ?class-name, ?args);
    distribute-over-words(\define-stub-modifier, 
			  (?name ## "-setter", (v :: ?type, 
			  			this :: ?class-name, ?args) 
					    => (v :: ?type)), (?modifiers));
  }

  modifiers:
    { } => { }
    { ?mod:name ... } => { ?mod ## "-setter" }
end macro setter-stub-definer;


/* // not used
define macro get-arg-name
  { get-arg-name(?:name :: ?type:expression) } => { ?name }
end macro get-arg-name;
*/

define macro get-arg-type
  { get-arg-type(?:name :: ?type:expression) } => { ?type }
end macro get-arg-type;


define macro stub-definer
  { 
    define stub {?modifiers:*} ?type:expression ?:name ?class-name:name 
    		(?args:*) => ()
  } => {
    define inline method ?name (this :: ?class-name, ?args) => ()
      apply-over-args(simple-invoke, 
		      (this, ?name ## "-disp-id"(this), ?type, 
		       $LOCALE-USER-DEFAULT, #f), (?args), ());
    end method ?name;
    define stub-sealing ?name (this :: ?class-name, ?args);
    distribute-over-words(\define-stub-modifier, 
			  (?name, (this :: ?class-name, ?args) => ()),
			  (?modifiers));
  }

  { 
    define stub {?modifiers:*} ?type:expression ?:name ?class-name:name 
    		(?args:*) => (?results:*)
  } => {
    define inline method ?name (this :: ?class-name, ?args, #key default = #f) 
			    => (?results)
      let r = apply-over-args(simple-invoke, 
			      (this, ?name ## "-disp-id"(this), ?type, 
			       $LOCALE-USER-DEFAULT, default), (?args), ());
      if (subtype?(get-arg-type(?results), <dispatch-client>))
	make(get-arg-type(?results), disp-interface: r)
      else
	r
      end if
    end method ?name;
    define stub-sealing ?name (this :: ?class-name, ?args);
    distribute-over-words(\define-stub-modifier, 
			  (?name, (this :: ?class-name, ?args) => (?results)),
			  (?modifiers));
  }
end macro stub-definer;


define macro apply-over-args
  { 
    apply-over-args(?func:expression, (), (?args), (?post-args:*))
  } => {
    ?func(?args, ?post-args)
  }

  { 
    apply-over-args(?func:expression, (?pre-args:*), (?args), (?post-args:*))
  } => {
    ?func(?pre-args, ?args, ?post-args)
  }

  args:
    { } => { }
    { ?:name :: ?type:expression, ... } => { ?name, ... }
    { , ... } => { ... }	// Awful hack to allow extra commas!
end macro apply-over-args;


define macro stub-sealing-definer
  { 
    define stub-sealing ?:name (?args) 
  } => { 
    //define sealed domain ?name (?args)
  }

  args:
    { } => { }
    { ?:name :: ?type:expression, ... } => { ?type, ... }
end macro stub-sealing-definer;


define macro define-stub-modifier
  { define-stub-modifier((?stub-name:name,
			  (?args:*) => (?results:*)),
			 (element))
  } => {
    define method element (?args, #key default = $unsupplied) => (?results)
      apply-over-args(?stub-name, (), (?args), (default: default))
    end method element;
    define stub-sealing element (?args);
  }

  { define-stub-modifier((?stub-name:name,
			  (?args:*) => (?results:*)),
			 (?:name))
  } => {
    define method ?name (?args) => (?results)
      apply-over-args(?stub-name, (), (?args), ())
    end method ?name;
    define stub-sealing ?name (?args);
  }
end macro define-stub-modifier;


define macro dylan-automation-type-macro
  { dylan-automation-type-macro(?d-type) } => { ?d-type }
  d-type:
    { out-ref(?rest:*) } => { <ole-arg-spec> }
    { inout-ref(?rest:*) } => { <ole-arg-spec> }
    { ?rest:* } => { dylan-type-macro(?rest) }
end macro dylan-automation-type-macro;
