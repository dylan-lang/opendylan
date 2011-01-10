Module:    OLE-Automation
Synopsis:  Type info macro for custom v-table interfaces.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic vtable-type-information 
    (interface :: type-union(<Interface>, <class>))
 => (result :: false-or(<vtable-type-info>));

define method vtable-type-information (interface :: <Interface>)
 => (result :: false-or(<vtable-type-info>))
  vtable-type-information(object-class(interface))
end;

define method vtable-type-information (class :: <class>)
 => (result :: singleton(#f))
  #f
end;

define macro vtable-interface-definer
  { 
    define ?modifiers:* vtable-interface ?class-name:name
				 (?superclasses:*)
      ?slots-and-stuff:* 
    end 
  } => { 

    define ?modifiers custom-interface ?class-name (?superclasses)
      ?slots-and-stuff 
    end;

    define vtable-type-info "$" ## ?class-name ## "-type-information" 
	(vtable-maybe-inherit(?superclasses))
      ?slots-and-stuff;
      default-interface-name ?class-name ;
    end;

    define inline method vtable-type-information (x :: subclass(?class-name))
	   => ( result :: <vtable-type-info> )
      "$" ## ?class-name ## "-type-information"
    end;
  }
end macro vtable-interface-definer;


define macro vtable-type-info-definer
  { 
    define ?modifiers:* vtable-type-info ?variable-name:name
				 (?supertype:expression)
      ?slots-and-stuff:* 
    end 
  } => { 
    define internal-vtable-type-info ?variable-name (?supertype) 
      typeinfo-class { <vtable-type-info> };
      class-options { };
      typeinfo { ?slots-and-stuff };
      members { ?slots-and-stuff }; 
    end;
   }
end macro;

define macro internal-vtable-type-info-definer
  { define  internal-vtable-type-info ?variable-name:name
				( ?supertype:expression )
      typeinfo-class { ?info-class:expression };
      class-options { ?class-options:* };
      typeinfo { ?typeinfo-options }; 
      members { ?members };
    end }
    => { define constant ?variable-name :: ?info-class =
             make( ?info-class, ?class-options,
		   ?typeinfo-options,
		   inherit: validate-supertype(?supertype),
		   ?members );
       }
typeinfo-options:
    { } 
      => { } 
    { client-class ?stuff-to-ignore:* ; ... }
      => { ... }
    { ?adjectives:* property ?stuff-to-ignore:* ; ... }
      => { ... }
    { function-descriptor ?stuff-to-ignore:* ; ... } 
      => { ... }
    { function ?stuff-to-ignore:* ; ... } 
      => { ... }
    { member-function ?stuff-to-ignore:* ; ... } 
      => { ... }
    { vtable-member ?stuff-to-ignore:* ; ... } 
      => { ... }
    { default-interface-name ?interface-name:* ; ... }
       => { name: stringify(?interface-name) , ... }
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
    { ; ... } => { ... } // don't freak out on redundant semicolon
    // anything else is an error
interface-name:
    { "<" ## ?name-without-brackets:name ## ">" } => { ?name-without-brackets }
    { ?other-name:name } => { ?other-name }
type-expression: 
    { ?the-type:expression } => { ?the-type }
variable-property-options:
    { } => { }
    { ?variable-property-option:*, } => { , ?variable-property-option }
    { ?variable-property-option:*, ... } => { , ?variable-property-option ... }
variable-property-option:
    { name: ?property-name:* }
      => { name: stringify(?property-name)  }
    { disp-id: ?property-disp-id:expression }
      => { }
    { type: ?property-type:expression }
      => { result-type: ?property-type  }
    { setter: ?setter-function:expression  } 
      => { }      
    { documentation: ?documentation-string:expression } 
      => { documentation: ?documentation-string  }
    { help-context: ?help-context:expression } 
      => { help-context: ?help-context  }
    { init-keyword: ?val:expression } 
      => { }
    { required-init-keyword: ?val:expression } 
      => { }
    { init-value: ?value:expression }
      => { }
    { init-function: ?value:expression } 
      => { }
constant-property-options:
    { } => { }
    { ?constant-property-option:*, } => { , ?constant-property-option }
    { ?constant-property-option:*, ... } => { , ?constant-property-option ... }
constant-property-option:
    { name: ?property-name:* }
      => { name: stringify(?property-name)  }
    { disp-id: ?property-disp-id:expression }
      => { }
    { value: ?property-value:expression }
      => { }
    { type: ?property-type:expression }
      => { result-type: ?property-type  }
    { documentation: ?documentation-string:expression } 
      => { documentation: ?documentation-string  }
    { help-context: ?help-context:expression } 
      => { help-context: ?help-context  }
members:
    { } => { }
    { function-descriptor ?function-stuff:* ; ?remaining-members:* } 
      => { members: vector(?function-stuff, ?remaining-members ) }
    { function ?function-stuff:* ; ?remaining-members:* } 
      => { members: vector(?function-stuff, ?remaining-members ) }
    { member-function ?function-stuff:* ; ?remaining-members:* } 
      => { members: vector(?function-stuff, ?remaining-members ) }
    { vtable-member ?vtable-member-stuff ; ?remaining-members:* } 
      => { members: vector(?vtable-member-stuff, ?remaining-members ) }
    { property ?variable-property-stuff:* ; ?remaining-members:* } 
      => { members: vector(?variable-property-stuff, ?remaining-members ) }
    { virtual property ?variable-property-stuff:* ; ?remaining-members:* } 
      => { members: vector(?variable-property-stuff, ?remaining-members ) }
    { constant property ?constant-property-stuff:* ; ?remaining-members:* } 
      => { members: vector(?constant-property-stuff, ?remaining-members ) }
    { ?anything-else:name ?stuff-to-ignore:* ; ... }
      => { ... }
    { ; ... } => { ... } // don't freak out on redundant semicolon
remaining-members:
    { } => { }
    { function-descriptor ?function-stuff:* ; ... } 
      => { ?function-stuff , ... }
    { function ?function-stuff:* ; ... } 
      => { ?function-stuff , ... }
    { member-function ?function-stuff:* ; ... } 
      => { ?function-stuff , ... }
    { vtable-member ?vtable-member-stuff ; ... } 
      => { ?vtable-member-stuff , ... }
    { property ?variable-property-stuff:* ; ... } 
      => { ?variable-property-stuff , ... }
    { virtual property ?variable-property-stuff:* ; ... } 
      => { ?variable-property-stuff , ... }
    { constant property ?constant-property-stuff:* ; ... } 
      => { ?constant-property-stuff , ... }
    { ; ... } => { ... } // don't freak out on redundant semicolon
    { ?anything-else:name ?stuff-to-ignore:* ; ... }
      => { ... }

variable-property-stuff:
    { ?property-name:name \:: ?type:expression,
		?variable-property-options:* } 
      => { make(<vtable-member-description>
		?variable-property-options, name: ?"property-name",
		result-type: ?type,
		invoke: $INVOKE-PROPERTYGET),
	   make(<vtable-member-description>
		?variable-property-options, name: ?"property-name",
		argument-names: vector("value"),
		argument-types: vector(?type),
		invoke: $INVOKE-PROPERTYPUT) }
constant-property-stuff:
    // use a separate rule for the type expression so it won't include the `='.
    { ?property-name:name \:: ?type-expression = ?value:expression,
	?constant-property-options:* }
      => { make(<vtable-member-description>
		?constant-property-options, name: ?"property-name",
		result-type: ?type-expression,
		invoke: $INVOKE-PROPERTYGET) }
    { ?property-name:name \:: ?type:expression, ?constant-property-options:* } 
      => { make(<vtable-member-description>
		?constant-property-options, name: ?"property-name",
		result-type: ?type,
		invoke: $INVOKE-PROPERTYGET) }
    { ?property-name:name = ?property-value:expression, 
	?constant-property-options:* } 
      => { make(<vtable-member-description>
		?constant-property-options, name: ?"property-name",
		result-type: object-class(?property-value),
		invoke: $INVOKE-PROPERTYGET) }
function-stuff:
    { ?function-name:name, ?function-options:* }
      => { make(<vtable-member-description>, ?function-options ,
		name: ?"function-name", result-type: <HRESULT> ) }
    { ?function-name:name (), ?function-options:* }
      => { make(<vtable-member-description>, ?function-options ,
		name: ?"function-name", result-type: <HRESULT> ) }
    { ?function-name:name ( ?argument-list:* ), ?function-options:* }
      => { make(<vtable-member-description>, ?function-options ,
		name: ?"function-name",
		result-type: <HRESULT>,
		argument-names: 
		  dispatch-extract-argument-names  ?argument-list end,
	        argument-types: 
	          dispatch-extract-argument-types  ?argument-list end)
	  }
    { ?function-name:name ( ?argument-list:* ) => ( ?out-args ),
    						?function-options:* }
      => { make(<vtable-member-description>, ?function-options ,
		name: ?"function-name",
		result-type: <HRESULT>,
		argument-names: 
		  dispatch-extract-argument-names ?argument-list, ?out-args end,
	        argument-types: 
		  dispatch-extract-argument-types ?argument-list, ?out-args end
		)
          }
vtable-member-stuff:
    { ?function-name:name ( ?argument-list:* ) => ( ?result, ?out-args ),
    						?function-options:* }
      => { make(<vtable-member-description>, ?function-options ,
		name: ?"function-name",
		result-type: ?result,
		argument-names: 
		  dispatch-extract-argument-names ?argument-list, ?out-args end,
	        argument-types: 
		  dispatch-extract-argument-types ?argument-list, ?out-args end
		)
          }

function-options:
    { } => { }
    { name: ?function-name:expression, ... }
      => { name: stringify(?function-name) , ... }
    { disp-id: ?property-disp-id:expression, ... }
      => { ... }
    { scodes: ?codes:expression, ... }
      => { ... }
    { ?key:token ?value:expression, ... }
      => { ?key ?value , ... }
result:
    { } => { #f } // was () so no value
    {  ?result-name:name :: ?result-type:expression } => { ?result-type }
out-args:
    { } => { }
    { ?value-name:name :: ?value-type:expression, ... }
    => { ?value-name :: out-ref(?value-type), ...}
end macro internal-vtable-type-info-definer;


define sealed inline method validate-supertype ( super :: <class> )
 => (super :: <vtable-type-info>)
  vtable-type-information(super)
end;

define sealed inline method validate-supertype ( super :: <vtable-type-info> )
 => (super :: <vtable-type-info>)
  super
end;

define sealed inline method validate-supertype ( super == #f )
 => (super == #f)
  super
end;


define function vtable-maybe-inherit (#rest superclasses)
 => (inherited-typeinfo :: false-or(<vtable-type-info>));
  // If one of the superclasses is also an interface, then
  // returns its type info, else false.
  // (We can assume there is not more than one, because that
  // would get an error elsewhere for mixing primary classes.)
  any?(method (class)
	 subtype?(class, <dylan-interface>) & vtable-type-information(class)
       end,
       superclasses);
end vtable-maybe-inherit;



define vtable-type-info $IUnknown-type-info (#f)
  uuid $IID-IUnknown;
  name "IUnknown";
  vtable-member QueryInterface (rrid :: <REFIID>)
		=> (status :: <C-HRESULT>, obj :: <Interface>);
  vtable-member AddRef () => (count :: <ULONG>);
  vtable-member Release () => (count :: <ULONG>);
end;

// Note: deliberately using singleton(<IUnknown>) instead of
// subclass(<IUnknown>) because we don't want this to match for interface
// subclasses that don't have their own vtable-type-information methods.
define method vtable-type-information ( x :: singleton(<IUnknown>) )
	   => ( result :: <vtable-type-info> )
  $IUnknown-type-info
end;

define vtable-type-info $IDispatch-type-info ($IUnknown-type-info)
  uuid $IID-IDispatch;
  name "IDispatch";
  function GetTypeInfoCount () => (count :: <C-unsigned-int>);
  function GetTypeInfo (iTInfo :: <C-unsigned-int>, lcid :: <LCID>)
	=> (info :: <Interface>);
  function GetIDsOfNames (riid :: <REFIID>, rgszNames :: <LPLPOLESTR>,
			  cNames :: <C-unsigned-int>, lcid :: <LCID>,
			  rgDispId :: <LPDISPID>) => ();
  function Invoke (dispIdMember :: <DISPID>, riid :: <REFIID>,
		   lcid :: <LCID>, wFlags :: <WORD>,
		   pDispParams :: <LPDISPPARAMS>,
		   pVarResult :: <LPVARIANT>,
		   pExcepInfo :: <LPEXCEPINFO>)
	=> (uArgErr :: <C-unsigned-int>);
end;

define method vtable-type-information ( x :: subclass(<IDispatch>) )
	   => ( result :: <vtable-type-info> )
  $IDispatch-type-info
end;

define method type-information ( x :: subclass(<Dylan-interface>) )
 => (typeinfo :: false-or(<vtable-type-info>))
  vtable-type-information(x)
end;

// We don't want this to ever be terminated:
AddRef($IDispatch-type-info);
