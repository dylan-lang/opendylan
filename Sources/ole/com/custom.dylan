Module:    COM
Synopsis:  Macro to define a custom COM v-table interface.
Author:    David N. Gray and Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define /* exported */ macro custom-interface-definer
  { 
    define ?modifiers:* custom-interface ?class-name:name (?superclass:name)
      ?slots-and-stuff:* 
    end 
  } => { 
    define ?modifiers custom-interface-class ?class-name (?superclass) 
      uuid { ?slots-and-stuff };
      vtable-slots { ?slots-and-stuff };
      vtable-inits { ?slots-and-stuff };
    end;

    define ?modifiers custom-interface-methods ?class-name
      class-options { ?slots-and-stuff };
      methods { ?slots-and-stuff };
    end;
  }
end macro custom-interface-definer;

define macro custom-interface-class-definer
  { 
    define ?modifiers:* custom-interface-class 
	    ?class-name:name (?superclass:name)
      uuid { ?typelib-clauses };
      vtable-slots { ?vtable-slots };
      vtable-inits { ?vtable-inits };
    end 
  } => {
    define ?modifiers COM-interface ?class-name ( ?superclass )
    end ?class-name;

    define C-struct ?class-name ## "-vstruct"
      sealed inline-only slot "vtbl-" ## ?superclass :: 
	      ?superclass ## "-vstruct", getter: #f, setter: #f;
      ?vtable-slots;     //  slot vtbl-Frob :: <C-function-pointer>;
      pointer-type-name: ?class-name ## "-vptr";
    end C-struct ?class-name ## "-vstruct";

    define constant "$IID-" ## ?class-name :: <REFIID> = 
	    as(<REFIID>, ?typelib-clauses);

    define variable ?class-name ## "-v-table" :: <C-COM-vtbl> = $null-vtable;

    define method initialize ( This :: ?class-name, #next next-method,
			       #rest ignore, #key );
      next-method();
      if ( ?class-name ## "-v-table" == $null-vtable )
	// Don't create the table until the first time it is needed.
	let new-vtbl :: ?class-name ## "-vptr" =
          make(?class-name ## "-vptr");
	copy-into!(new-vtbl, This.vtbl, size-of(?superclass ## "-vstruct"));
	?vtable-inits;   // new-vtbl.vtbl-Frob := CCW_IFoo_Frob;
	?class-name ## "-v-table" := pointer-cast(<C-COM-vtbl>, new-vtbl);
      end if;
      This.vtbl := ?class-name ## "-v-table";
      add-interface(This, "$IID-" ## ?class-name);
    end method initialize;
  }

     // Note, the only thing we extract from this is the interface-id, but
     // the variable is called `typelib-clauses' for the sake of clearer
     // error messages if an undefined clause is encountered.
  typelib-clauses:
    { } => { } 
    { uuid ?uuid:expression ; ... } => { ?uuid }
    // ignore clauses processed elsewhere
    { client-class ?ignore:* ; ... } => { ... }
    { member-function ?ignore:* ; ... } => { ... }
    { function ?ignore:* ; ... } => { ... }
    { vtable-member ?ignore:* ; ... } => { ... }
    { ?maybe-virtual:* property ?ignore:* ; ... } => { ... }
    // ignore clauses that are only used for type info
    { name ?stuff-to-ignore:* ; ... } => { ... }
    { documentation ?stuff-to-ignore:* ; ... } => { ... }
    { help-file ?stuff-to-ignore:* ; ... } => { ... }
    { help-context ?stuff-to-ignore:* ; ... } => { ... }
    { major-version ?stuff-to-ignore:* ; ... } => { ... }
    { minor-version ?stuff-to-ignore:* ; ... } => { ... }
    { locale ?stuff-to-ignore:* ; ... } => { ... }
    // anything else is an error.
  vtable-slots:
    { } => { } 
    { 
      member-function ?function-name:name ?ignore:* ;
      ...
    } => { 
      sealed inline-only slot "vtbl-" ## ?function-name
	    :: <C-function-pointer>;
      ...
    }
    { 
      function ?function-name:name ?ignore:* ;
      ...
    } => { 
      sealed inline-only slot "vtbl-" ## ?function-name
	    :: <C-function-pointer>;
      ...
    }
    { 
      vtable-member ?function-name:name ?ignore:* ;
      ...
    } => { 
      sealed inline-only slot "vtbl-" ## ?function-name
	    :: <C-function-pointer>;
      ...
    }
    { 
      constant property ?property-name:name ?ignore:* ; ...
    } => { 
      sealed inline-only slot "vtbl-" ## ?property-name
	    :: <C-function-pointer>;
      ...
    }
    { 
      ?maybe-virtual:* property ?property-name:name ?ignore:* ; ...
    } => { 
      sealed inline-only slot "vtbl-" ## ?property-name
	    :: <C-function-pointer>;
      sealed inline-only slot "vtbl-set_" ## ?property-name
	    :: <C-function-pointer>;
      ...
    }
    { ?stuff-to-ignore:* ; ... } => { ... }

  vtable-inits:
    { } => { } 
    { 
      member-function ?function-name:name ?stuff-to-ignore:* ;
      ...
    } => { 
      let fn :: <C-function-pointer> = "CCW_" ## ?function-name;
      new-vtbl."vtbl-" ## ?function-name := fn;
      ...
    }
    { 
      function ?function-name:name ?stuff-to-ignore:* ;
      ...
    } => { 
      let fn :: <C-function-pointer> = "CCW_" ## ?function-name;
      new-vtbl."vtbl-" ## ?function-name := fn;
      ...
    }
    { 
      vtable-member ?function-name:name ?stuff-to-ignore:* ;
      ...
    } => { 
      let fn :: <C-function-pointer> = "CCW_" ## ?function-name;
      new-vtbl."vtbl-" ## ?function-name := fn;
      ...
    }
    { 
      constant property ?property-name:name ?ignore:* ; ...
    } => { 
      let fn :: <C-function-pointer> = "CCW_" ## ?property-name; 
      new-vtbl."vtbl-" ## ?property-name := fn;
      ...
    }
    { 
      ?maybe-virtual:* property ?property-name:name ?ignore:* ; ...
    } => { 
      let fn :: <C-function-pointer> = "CCW_" ## ?property-name;
      new-vtbl."vtbl-" ## ?property-name := fn;
      let fn :: <C-function-pointer> = "CCW_set_" ## ?property-name;
      new-vtbl."vtbl-set_" ## ?property-name := fn;
      ...
    }
    { ?stuff-to-ignore:* ; ... } => { ... }
end macro custom-interface-class-definer;


define macro custom-interface-methods-definer
  { 
    define ?modifiers:* custom-interface-methods ?class-name:name
      class-options { ?c-client-class:* };
      methods { ?methods:* };
    end 
  } => { 
    emit-client-class(?c-client-class, ?modifiers);
    distribute-over-semicolons(\member-clauses, (?c-client-class, ?class-name),
			       (?methods));
  }
c-client-class:
  { } => { <C-interface>, <C-interface> } // default
  { client-class ?c-class-name:name (?client-superclass:name); 
    ?stuff-to-ignore:* } => { ?c-class-name, ?client-superclass }
  { client-class ?c-class-name:name; 
    ?stuff-to-ignore:* } => { ?c-class-name, <C-interface> }
  { ?stuff-to-ignore:* ; ... } => { ... }
end macro custom-interface-methods-definer;

define macro emit-client-class
  { emit-client-class ( <C-interface>, ?ignore:* ) }
    => { }
  { emit-client-class ( ?c-client-class:name, ?superclass:name, ?modifiers:* ) }
    => { define ?modifiers C-subtype ?c-client-class ( ?superclass ) end }
end;

define macro member-clauses

    // with explicit result declaration:
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( vtable-member ?function-name:name (?argument-list:*) => (?results:*),
	    ?function-options ))
  } => {

    emit-c-functions( ?function-name [ ?c-interface-class ]
	(?argument-list) => (?results) );

    emit-dylan-method ?function-name [ ?c-interface-class, ?class-name ] 
		(?argument-list) (?argument-list) => (?results) 
	end;
  }

    // with implicit status result:
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( member-function ?function-name:name (?argument-list:*) => (?result:*),
	     ?function-options ))
  } => {

    emit-c-functions( ?function-name [ ?c-interface-class ] (?argument-list)
		       => (status :: <HRESULT>, ?result) );

    emit-dylan-method  ?function-name [ ?c-interface-class, ?class-name ]
        (?argument-list) (?argument-list)
	 => (status :: <HRESULT>, ?result)
      end;
  }

    // with implicit status result:
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( function ?function-name:name (?argument-list:*) => (?result:*),
	     ?function-options ))
  } => {

    emit-c-functions( ?function-name [ ?c-interface-class ] (?argument-list)
		       => (status :: <HRESULT>, ?result) );

    emit-dylan-method  ?function-name [ ?c-interface-class, ?class-name ]
        (?argument-list) (?argument-list)
	 => (status :: <HRESULT>, ?result)
      end;
  }

  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( constant property ?property-name:name :: ?property-type:expression,
		?function-options ))
  } => {
    member-clauses((?c-interface-class, ?super, ?class-name), 
    ( vtable-member ?property-name () => (value :: ?property-type) ));
  }
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( constant property ?property-name:name :: ?property-type:expression
       = ?value:expression, ?function-options ))
  } => {
    member-clauses((?c-interface-class, ?super, ?class-name), 
    ( vtable-member ?property-name () => (value :: ?property-type) ));

    define inline method ?property-name ( this :: ?class-name )
				=> ( value :: ?property-type )
      ?value
    end;
  }
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( constant property ?property-name:name = ?value:expression,
	?function-options ))
  } => {
    member-clauses((?c-interface-class, ?super, ?class-name), 
    ( vtable-member ?property-name () => (value :: object-class(?value)) ));

    define inline method ?property-name ( this :: ?class-name ) => ( value )
      ?value
    end;
  }
  { 
    member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    ( ?maybe-virtual:* property ?property-name:name :: ?property-type:*,
		?function-options ))
  } => {
    member-clauses((?c-interface-class, ?super, ?class-name), 
    ( vtable-member ?property-name () => (value :: ?property-type) ));
    member-clauses((?c-interface-class, ?super, ?class-name), 
    ( vtable-member "set_" ## ?property-name (value :: ?property-type)
       => ()));

    define dynamic generic ?property-name ## "-setter" (value, this) => (value);
    define inline method ?property-name ## "-setter"
	(value :: dylan-type-macro(?property-type), this :: ?c-interface-class)
     => ( value :: dylan-type-macro(?property-type) )
      "set_" ## ?property-name(this, value);
      value
    end;

    // internal function for server side:
    define inline method "set_" ## ?property-name
	(this :: ?class-name, value :: dylan-type-macro(?property-type) ) => ()
      ?property-name(this) := value;
    end;

  }
  { member-clauses((?c-interface-class:name, ?super:name, ?class-name:name), 
    (?rest:*)) } => { }
function-options:
    // Ignore options that only apply to a dispatch interface or that
    // only affect the type info; they are not documented as being supported
    // here, but may be received when passed on by `define dual-interface'. 
    { } => { }
    { name: ?function-name:expression, ... } => { ... }
    { disp-id: ?value:expression, ... } => { ... }
    { documentation: ?value:expression, ... } => { ... }
    { help-context: ?value:expression, ... } => { ... }
    { flags: ?value:expression, ... } => { ... }
    { scodes: ?value:expression, ... } => { ... }
end macro member-clauses;

define macro emit-c-functions
  { 
    emit-c-functions( ?function-name:name [ ?c-interface-class:name ]
		       (?c-in-args)
                       => ( ?value:name :: ?result-type:*, ?c-out-args) )
  } => {
    define C-callable-wrapper "CCW_" ## ?function-name of ?function-name
      input parameter This :: <mapped-interface>;
      ?c-in-args ;
      ?c-out-args ;
      result ?value :: c-type-macro(?result-type);
      c-modifiers: "__stdcall";
    end;

    define inline-only C-function "CV_" ## ?function-name
      input parameter This :: ?c-interface-class;
      ?c-in-args ;
      ?c-out-args ;
      result ?value :: c-type-macro(?result-type);
      indirect: #t; 
      c-modifiers: "__stdcall";
    end;
  }
  { 
    emit-c-functions( ?function-name:name [ ?c-interface-class:name ]
		       (?c-in-args) => () )
  } => {
    define C-callable-wrapper "CCW_" ## ?function-name of ?function-name
      input parameter This :: <mapped-interface>;
      ?c-in-args ;
      c-modifiers: "__stdcall";
    end;

    define inline-only C-function "CV_" ## ?function-name
      input parameter This :: ?c-interface-class;
      ?c-in-args ;
      indirect: #t; 
      c-modifiers: "__stdcall";
    end;
  }
c-in-args:
    { } => { }
    // Note: This must not use the binding pattern (?:name :: ?type) because
    // that causes the type to be prematurely parsed as an expression and
    // then c-type-macro can't deconstruct it.
    { ?:name, ... } => { input parameter ?name :: <object>; ... }
    { ?:name \:: ?type:*, ... } =>
      { input parameter ?name :: c-type-macro(?type); ... }
c-out-args:
    { } => { }
    // Note: sticking '&' in front of the name to make sure it doesn't have
    // the same name as one of the original input parameters. (see Bug 3613)
    // Note: This must not use the binding pattern (?:name :: ?type) because
    // that causes the type to be prematurely parsed as an expression and
    // then c-pointer-type-macro can't deconstruct it.
    { ?:name, ... } =>
      { output parameter "&" ## ?name :: <object>; ... }
    { ?:name \:: ?type:*, ... } =>
      { output parameter "&" ## ?name :: c-pointer-type-macro(?type); ... }
end macro;

// This should be done as an inlined generic function instead of a macro,
// but compiler Bug 3612 needs to be fixed first.
define macro c-type-macro 
  { c-type-macro( ?c-type ) } => { ?c-type }
c-type:
  { <integer> }		=> { <C-long> }
  { <single-float> }	=> { <C-float> }
  { <double-float> }	=> { <C-double> }
  { <string> }		=> { <C-string> }
  { <byte-string> }	=> { <C-string> }
  { <machine-word> }	=> { <C-raw-unsigned-long> }
  { <SCODE> }		=> { <C-HRESULT> }
  { <HRESULT> }		=> { <C-HRESULT> }
  { <character> }	=> { <C-character> }
  { <byte-character> }	=> { <C-character> }
  { <byte> }		=> { <C-byte> }
  { <boolean> }		=> { <C-boolean> }
  { out-ref(?type:expression) } => { c-pointer-type-macro(?type) }
  { inout-ref(?type:expression) } => { c-pointer-type-macro(?type) }
  { ?type:expression }	=> { ?type } // hope it's already a C type
end;

define macro c-pointer-type-macro 
  { c-pointer-type-macro( ?c-type ) } => { ?c-type }
c-type:
  { <integer> }		=> { <C-long*> }
  { <single-float> }	=> { <C-float*> }
  { <double-float> }	=> { <C-double*> }
  { <string> }		=> { <C-string*> }
  { <byte-string> }	=> { <C-string*> }
  { <machine-word> }	=> { <C-raw-unsigned-long*> }
  { <SCODE> }		=> { <C-HRESULT*> }
  { <HRESULT> }		=> { <C-HRESULT*> }
  { <character> }	=> { <C-character*> }
  { <byte-character> }	=> { <C-character*> }
  { <byte> }		=> { <C-byte*> }
  { <boolean> }		=> { <PBOOL> }
  { <C-boolean> }	=> { <PBOOL> }
  { <Interface> }	=> { <Interface*> }
  { <LPUNKNOWN> }	=> { <Interface*> }
  { <BSTR> }		=> { ?=<LPBSTR> }
  { <ULONG> }		=> { <LPULONG> }
  { <USHORT> }		=> { <PUSHORT> }
  { <DWORD> }		=> { <LPDWORD> }
  { <WORD> }		=> { <LPWORD> }
  { "<C-" ## ?name-without-brackets:name ## ">" } =>
     { "<C-" ## ?name-without-brackets ## "*>" }
  { "<LP" ## ?name-without-brackets:name ## ">" } =>
     { "<LPLP" ## ?name-without-brackets ## ">" }
  { ?type:* } => { pointer-type(c-type-macro(?type)) } // won't work, but ...   ???
end;



define macro emit-dylan-method
  { emit-dylan-method ?function-name:name [ ?c-interface-class:expression,
					    ?class-name:name ]
     (?argument-names:*) (?argument-list:*)
     => (?results:*) end
   } => {
    // This will be sealed on <C-interface>, but need to allow server
    // to write methods on subclasses of <Dylan-interface>.
    // TODO: need to allow user to specify sealed, in case server
    // implementation is in same library...
    define dynamic generic ?function-name (This, ?argument-list)
           => (?results);

    define sealed method ?function-name(this :: ?c-interface-class,
					?argument-list)
			=> (?results)
        let ptr :: ?class-name ## "-vptr"
          = pointer-cast(?class-name ## "-vptr", this.vtbl);
        "CV_" ## ?function-name ("vtbl-" ## ?function-name(ptr),
				this, ?argument-names)
    end
  }
argument-names:
    { } => { }
    { ?:name :: ?type:expression, ... } => { ?name, ... }
    { ?:name, ... } => { ?name, ... }
argument-list:
    { ?name-type-list } => { ?name-type-list }
results:
    { ?name-type-list } => { ?name-type-list }
name-type-list:
    { } => { }
    // Note: This must not use the binding pattern (?:name :: ?type) because
    // that causes the type to be prematurely parsed as an expression and
    // then dylan-type-macro can't deconstruct it.
    { ?:name, ... } => { ?name :: <object>, ... }
    { ?:name \:: ?type:*, ... } => { ?name :: dylan-type-macro(?type), ... }
end macro emit-dylan-method;

define macro dylan-type-macro
  { dylan-type-macro( ?d-type ) } => { ?d-type }
d-type:
  { <C-long> }		=> { <integer> }
  { <C-signed-long> }	=> { <integer> }
  { <C-int> }		=> { <integer> }
  { <C-signed-int> }	=> { <integer> }
  { <C-short> }		=> { <integer> }
  { <C-signed-short> }	=> { <integer> }
  { <C-unsigned-int> }	=> { <integer> }
  { <C-unsigned-short> } => { <integer> }
  { <C-both-unsigned-long> } => { <unsigned-long> }
  { <C-both-signed-long> } => { <signed-long> }
  { <C-float> }		=> { <single-float> }
  { <C-double> }	=> { <double-float> }
  { <C-raw-unsigned-long> } => { <machine-word> }
  { <C-HRESULT> }	=> { <HRESULT> }
  { <C-character> }	=> { <character> }
  { <C-byte> }  	=> { <byte> }
  { <C-char> }  	=> { <integer> }
  { <C-unsigned-char> }	=> { <byte> }
  { <C-signed-char> }  	=> { <integer> }
  { <C-boolean> }	=> { <boolean> }
  { <VARIANT-BOOL> }	=> { <boolean> }
  { <VARIANT> }		=> { ?=<LPVARIANT> }
  { <ULONG> }		=> { <unsigned-long> }
  { <LONG> }            => { <signed-long> }
  { <USHORT> }		=> { <integer> }
  { <DWORD> }		=> { <unsigned-long> }
  { <WORD> }		=> { <integer> }
  { <C-string> }        => { <string> }
  { <C-unicode-string> } => { <string> }
  { <mapped-interface> } => { <Interface> }
  { out-ref(?type:expression) } => { c-pointer-type-macro(?type) }
  { inout-ref(?type:expression) } => { c-pointer-type-macro(?type) }
  { ?type:expression }	=> { ?type } // hope it's already a Dylan type
end;



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


// The following function is used when we need to return some value that
// satisfies the type constraint even though the value will not actually
// be used.

define open generic dummy-value-for-type ( type :: <type> )
				=> ( value :: <object> );

define inline sealed method dummy-value-for-type ( type :: subclass(<number>) )
  => ( value :: <number> );
  as(type, 0)
end;

define method dummy-value-for-type (type :: subclass(<sequence>))
  => ( value :: <sequence> );
  make(type, size: 0)
end;

define inline sealed method dummy-value-for-type ( type == <machine-word> )
  => ( value :: <machine-word> );
  as(<machine-word>, 0)
end;

define method dummy-value-for-type(type :: subclass(<C-pointer>))
  => ( value :: <C-pointer> );
  null-pointer(type)
end;

define inline sealed method dummy-value-for-type (type == <Interface>)
  => ( value :: <Interface> );
  $NULL-interface
end;

define inline sealed method dummy-value-for-type ( type == <boolean> )
  => ( value :: <boolean> );
  #f
end;

define inline sealed method dummy-value-for-type(type :: subclass(<character>))
  => ( value :: <character> );
  '\0'
end;
