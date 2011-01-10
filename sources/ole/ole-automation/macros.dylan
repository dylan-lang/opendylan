Module:    ole-automation
Synopsis:  Macros for defining IDispatch interfaces and coclasses.
Author:    Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define /* exported */ open generic type-information 
    (interface :: type-union(<Interface>, <class>))
 => (result :: false-or(<Dylan-Type-Info>));

// Default method returns false so that type-information can be safely called 
// anywhere.
define method type-information (interface :: <class>)
 => (result :: singleton(#f))
  #f
end;

define method type-information (interface :: <Interface>)
 => (result :: false-or(<dylan-type-info>))
  type-information(object-class(interface))
end;

define macro stringify
  { stringify(?a-name:name) } => { ?"a-name" }
  { stringify(?a-string:expression) } => { ?a-string }
end macro stringify;

define macro use-default
  { use-default(use-default:, ?default:expression) }
    => { ?default }
  { use-default(?given:expression, ?default:*) }
    => { ?given }
end macro use-default;

define /* exported */ macro dispatch-interface-definer
  { define ?modifiers:* dispatch-interface ?class-name:name (?superclasses:*)
      ?slots-and-stuff:* end }
    => { define ?modifiers dispatch-interface-class ?class-name (?superclasses) 
	     ?slots-and-stuff
         end;

         define dispatch-type-info "$" ## ?class-name ## "-type-information"
			 (maybe-inherit(?superclasses))
	   default-interface-name { ?class-name };
           ?slots-and-stuff
	 end;

        define type-information-method ?class-name (?modifiers) }
end macro dispatch-interface-definer;

define macro type-information-method-definer
  { define type-information-method ?:name (?mods) }
    => { define ?mods inline method type-information (cls :: subclass(?name))
	  => (result :: <disp-type-info>)
	   "$" ## ?name ## "-type-information"
	 end }
mods:
  { } => { sealed }
  { open ... } => { open }
  { ?mod:name ... } => { ... }
end;

define macro dispatch-interface-class-definer
  { define ?modifiers:* dispatch-interface-class ?class-name:name (?superclasses:*)
      ?slots-and-stuff:* end }
    => { define ?modifiers COM-interface ?class-name (?superclasses) 
	     ?slots-and-stuff
         end; }
slots-and-stuff:
    { }
      => { }
    { property ?property-as-slot ; ... }
      => { ?property-as-slot ; ... }
    { virtual property ?stuff-to-ignore:* ; ... } // no corresponding slot
      => { ... }
    { constant property ?stuff-to-ignore:* ; ... } // no corresponding slot
      => { ... }
    { function-descriptor ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { function ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { member-function ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { name ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { uuid ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { documentation ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { help-file ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { help-context ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { major-version ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { minor-version ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { locale ?stuff-to-ignore:* ; ... } // ignore
      => { ... }
    { ?modifiers:* slot ?slot-definition:* ; ... } // pass through unchanged
      => { ?modifiers slot ?slot-definition ; ... }
    { client-class ?ignore:* ; ... } // ignore option for custom interface
      => { ... }
property-as-slot:
    { ?property-spec:*, ?property-keywords }
    => { slot ?property-spec, ?property-keywords }
property-keywords:
  { } 
    => { }
  { name: ?value:expression, ... }
    => { ... }
  { disp-id: ?property-disp-id:expression, ... }
    => { ... }
  { type: ?value:expression, ... }
    => { ... }
  { documentation: ?val:expression, ... } 
    => { ... }
  { help-context: ?val:expression, ... } 
    => { ... }
  { property-setter: ?val:expression, ... } 
    => { ... }
  { ?slot-keyword:*, ... } 
    => { ?slot-keyword, ... }
slot-keyword:
  { setter: ?val:expression } 
    => { setter: ?val }
  { init-keyword: ?val:expression } 
    => { init-keyword: ?val }
  { required-init-keyword: ?val:expression } 
    => { required-init-keyword: ?val }
  { init-value: ?value:expression }
    => { init-value: ?value }
  { init-function: ?value:expression } 
    => { init-function: ?value }
/*  { ?key:token ?val:expression } 
    => { ?key ?val } */
end macro dispatch-interface-class-definer;




define /* exported */ macro dispatch-type-info-definer
  { define ?modifiers:* dispatch-type-info ?constant-name:name
			  (?supertype:expression)
      default-interface-name { ?interface-name };
      ?slots-and-stuff:*
    end }
    => {
         define internal-dispatch-type-info ?constant-name (?supertype) 
	     options { ?slots-and-stuff; name ?interface-name; };
	     properties { ?slots-and-stuff };
             methods { ?slots-and-stuff }; 
         end; }
interface-name:
    { "<" ## ?name-without-brackets:name ## ">" } => { ?name-without-brackets }
    { ?other-name:name } => { ?other-name }
end macro dispatch-type-info-definer;

define macro internal-dispatch-type-info-definer
  { define  internal-dispatch-type-info ?constant-name:name 
		( ?supertype:expression )
      options { ?options };
      properties { ?properties };
      methods { ?methods };
    end }
    => { define constant ?constant-name :: <disp-type-info> =
             make( <disp-type-info>,
		   ?options,
		   inherit: ?supertype,
		   ?properties,
		   ?methods );
       }
options:
    { } 
      => { } 
    { ; ... } => { ... }
    { property ?stuff-to-ignore:* ; ... }
      => { ... }
    { virtual property ?stuff-to-ignore:* ; ... }
      => { ... }
    { constant property ?stuff-to-ignore:* ; ... }
      => { ... }
    { function-descriptor ?stuff-to-ignore:* ; ... } 
      => { ... }
    { function ?stuff-to-ignore:* ; ... } 
      => { ... }
    { member-function ?stuff-to-ignore:* ; ... } 
      => { ... }
    { name ?disp-class-name:* ; ... } // This option will shadow the one in 
      => { name: stringify(?disp-class-name) , ... } // the template above.
    { uuid ?uuid-stuff:expression ; ... }
      => { uuid: ?uuid-stuff , ... }
    { documentation ?documentation-stuff:expression ; ... }
      => { documentation: ?documentation-stuff , ... }
    { help-file ?help-file-stuff:expression ; ... }
      => { help-file: ?help-file-stuff, ... }
    { help-context ? help-context-stuff:expression ; ... }
      => { help-context: ? help-context-stuff , ... }
    { major-version ?major-version-stuff:expression ; ... }
      => { major-version: ?major-version-stuff , ... }
    { minor-version ?minor-version-stuff:expression ; ... }
      => { minor-version: ?minor-version-stuff , ... }
    { locale ?locale-stuff:expression ; ... } 
      => { locale: ?locale-stuff , ... }
    { ?modifiers:* slot ?slot-definition:* ; ... } 
      => { ... }
    { client-class ?ignore:* ; ... } // ignore option for custom interface
      => { ... }
properties:
    { ?inner-properties } => { properties: vector(?inner-properties) }
inner-properties:
    { } => { }
    { property ?variable-property-desc:* ; ... } 
      => { ?variable-property-desc, ... }
    { virtual property ?variable-property-desc:* ; ... } 
      => { ?variable-property-desc, ... }
    { constant property ?constant-property-desc:* ; ... } 
      => { ?constant-property-desc , ... }
    { ?anything-else:name ?stuff-to-ignore:* ; ... }
      => { ... }
variable-property-desc:
    { ?property-name:name :: ?property-type:expression ?property-init,
        #key ?name:expression = use-default:,
             ?disp-id:expression = $unknown-disp-id,
             ?type:expression = use-default:,
             ?setter:expression = use-default:,
             ?property-setter:expression = use-default:,
             ?documentation:expression = $NULL-OLESTR,
             ?help-context:expression = 0,
        #all-keys }
      => { make(<variable-description>,
		getter: ?property-name,
		setter: use-default(?property-setter,
				    use-default(?setter,
						?property-name ## "-setter")),
		name: use-default(?name, ?"property-name"),
		type: use-default(?type, ?property-type),
		disp-id: ?disp-id,
		documentation: ?documentation,
		help-context: ?help-context) }
constant-property-desc:
    { ?property-name:name :: ?property-type:expression ?property-init,
        #key ?name:expression = use-default:,
             ?type:expression = use-default:,
             ?disp-id:expression = $unknown-disp-id,
             ?documentation:expression = $NULL-OLESTR,
             ?help-context:expression = 0,
             ?init-value:expression = use-default:,
             ?value:expression = use-default:,
        #all-keys }
    => { make(<constant-description>,
	      value: use-default(?value,
				 use-default(?init-value, ?property-init)),
	      name: use-default(?name, ?"property-name"),
	      type: use-default(?type, ?property-type),
	      disp-id: ?disp-id,
	      documentation: ?documentation,
	      help-context: ?help-context) }
property-init:
    // TODO: really want a compile-time error if got neither a
    // property-init nor a value: paramater
    { } => { error("Missing initial value for constant property") }
    { = ?:expression } => { ?expression }

methods:
    { ?inner-methods } => { methods: vector(?inner-methods) }
inner-methods:
    { } => { }
    { function-descriptor ?function-stuff ; ... } 
      => { ?function-stuff , ... }
    { function ?function-stuff ; ... } 
      => { ?function-stuff , ... }
    { member-function ?function-stuff ; ... } 
      => { ?function-stuff , ... }
    { ?anything-else:name ?stuff-to-ignore:* ; ... }
      => { ... }
function-stuff:
    { ?function-name:name, ?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ) }
    { ?function-name:name => ( ?result ), ?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ,
		result-type: ?result ) }
    { ?function-name:name (), ?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ) }
    { ?function-name:name () => ( ?result:* ), ?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ,
		result-type: ?result ) }
    { ?function-name:name ( ?argument-list:* ), ?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ,
		argument-names: 
		  dispatch-extract-argument-names  ?argument-list end,
	        argument-types: 
	          dispatch-extract-argument-types  ?argument-list end)
	  }
    { ?function-name:name ( ?argument-list:* ) => ( ?result:* ),
    						?function-options:* }
      => { make(<function-description>, ?function-options ,
		name: ?"function-name" , function: ?function-name ,
		result-type: ?result  ,
		argument-names: 
		  dispatch-extract-argument-names  ?argument-list end,
	        argument-types: 
	          dispatch-extract-argument-types  ?argument-list end )
          }
function-options:
    { } => { }
    { name: ?function-name:expression, ... }
      => { name: stringify(?function-name) , ... }
    { disp-id: ?property-disp-id:expression, ... }
      => { disp-id: ?property-disp-id , ... }
    { ?key:token ?value:expression, ... }
      => { ?key ?value , ... }
result:
    { } => { #f } // was () so no value
    {  ?result-name:name :: ?result-type:expression } => { ?result-type }
    // Apparently multiple return values aren't supported.
end macro internal-dispatch-type-info-definer;

define macro dispatch-extract-argument-names
  { dispatch-extract-argument-names  ?argument-list:* end }
    => { vector( ?argument-list ) }
argument-list:
    { } => { }
    { ?argument-name:name :: ?argument-type:expression, ... }
        => { ?"argument-name", ... }
    { , ... } => { ... }
end macro;

define macro dispatch-extract-argument-types
  { dispatch-extract-argument-types  ?argument-list:* end }
    => { vector( ?argument-list ) }
argument-list:
    { } => { }
    { ?argument-name:name :: ?argument-type:expression, ... }
        => { ?argument-type, ... }
    { , ... } => { ... }
end macro;

define function maybe-inherit (#rest superclasses)
 => (inherited-typeinfo :: false-or(<Disp-Type-Info>));
  // If one of the superclasses is also a dispatch interface, then
  // returns its type info, else false.
  // (We can assume there is not more than one, because that
  // would get an error elsewhere for mixing primary classes.)
  any?(method (superclass)
	 subtype?(superclass, <simple-dispatch>)
	   & dispatch-type-information(superclass)
       end,
       superclasses);
end maybe-inherit;


define /* exported */ macro coclass-definer
  { define ?modifiers:* coclass ?coclass-name:name ?clauses:* end }
    => { define ?modifiers coclass-helper ?coclass-name
           the-clauses { ?clauses }, the-interfaces { ?clauses } end; }
end macro coclass-definer;

define macro coclass-helper-definer
  { define ?modifiers:* coclass-helper ?coclass-name:name 
           the-clauses { ?clauses:* }, the-interfaces { ?interfaces:* } end }
    => { define ?modifiers constant ?coclass-name :: <coclass-type-info> 
           = make(<coclass-type-info>, ?clauses, ?interfaces ); }
clauses:
    { } 
      => { } 
    { interface ?stuff-to-ignore:* ; ... }
      => { ... }
    { default interface ?stuff-to-ignore:* ; ... }
      => { ... }
    { source interface ?stuff-to-ignore:* ; ... }
      => { ... }
    { restricted interface ?stuff-to-ignore:* ; ... } 
      => { ... }
    { name ?disp-class-name:name ; ... } // This option will shadow
      => { name: ?"disp-class-name" , ... } // the one in the template above.
    { name ?disp-class-name:expression ; ... } // This option will shadow
      => { name: ?disp-class-name , ... } // the one in the template above.
    { uuid ?uuid-stuff:expression ; ... }
      => { uuid: ?uuid-stuff , ... }
    { component-class ?component-class-name:expression ; ... }
      => { class: ?component-class-name , ... } 
    { args { } ; ... }
      => { ... }
    { args { ?non-empty-argument-list:* } ; ... }
      => { args: vector( ?non-empty-argument-list ) , ... }
    { documentation ?documentation-stuff:expression ; ... }
      => { documentation: ?documentation-stuff , ... }
    { help-file ?help-file-stuff:expression ; ... }
      => { help-file: ?help-file-stuff, ... }
    { help-context ? help-context-stuff:expression ; ... }
      => { help-context: ? help-context-stuff , ... }
    { major-version ?major-version-stuff:expression ; ... }
      => { major-version: ?major-version-stuff , ... }
    { minor-version ?minor-version-stuff:expression ; ... }
      => { minor-version: ?minor-version-stuff , ... }
    { locale ?locale-stuff:expression ; ... }
      => { locale ?locale-stuff , ... }
interfaces:
    { ?inner-interfaces } => { interfaces: vector(?inner-interfaces) }
inner-interfaces:
    { } => { }
    { interface ?component-interface-stuff:* ; ... } 
      => {  make(<component-interface-description>,
		 ?component-interface-stuff ),  ... }
    { ?flag:name interface ?component-interface-stuff:* ; ... } 
      => {  make(<component-interface-description>, flags: ?flag,
		 ?component-interface-stuff ),  ... }
    { ?anything-else:name ?stuff-to-ignore:* ; ... }
      => { ... }
flag:
    { default } => { $IMPLTYPEFLAG-FDEFAULT }
    { source } => { $IMPLTYPEFLAG-FSOURCE }
    { restricted } => { $IMPLTYPEFLAG-FRESTRICTED }
component-interface-stuff:
    { ?dispatch-class:expression, ?component-interface-options:* }
	=> { class: ?dispatch-class, 
	     ?component-interface-options, 
	     typeinfo: ?dispatch-class.type-information }
component-interface-options:
    { } => { }
    { name: ?property-name:name, ... }
      => { name: ?"property-name" , ... }
    { name: ?property-name:expression, ... }
      => { name: ?property-name , ... }
    { typeinfo: ?type-stuff:expression, ... } // Explicit type information will shadow
      => { typeinfo: ?type-stuff , ... } // the type-information derived from the class
    { args: { } , ... }
      => { ... }
    { args: { ?non-empty-argument-list:* } , ... }
      => { args: vector( ?non-empty-argument-list ) , ... }
    { args: ?argument-sequence:expression , ... }
      => { args: ?argument-sequence , ... }
non-empty-argument-list:
    { } => { }
    { ?keyword:token ?value:expression, ... }
      => { ?keyword, ?value, ... }
end macro coclass-helper-definer;
