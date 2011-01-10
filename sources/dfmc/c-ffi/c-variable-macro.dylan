Module:    dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// C-ADDRESS-DEFINER
//

define sealed class <c-address-options-descriptor> (<object>)
  constant slot c-address-c-name = #f, init-keyword: c-name:;
  constant slot c-address-import = #{ #f }, init-keyword: import:;
end class;

define option <c-address-c-name-option>
  => c-name: :: expression
end option;

define option <c-address-import-option>
  => import: :: expression
end option;

define constant $c-address-options =
  list(<c-address-c-name-option>,
       <c-address-import-option>);

define &macro c-address-definer
  { define c-address ?var-name:name :: ?pointer-designator:expression
      ?options:*;
    end }
    =>
  begin
    let initargs = parse-options($c-address-options, options, var-name);
    let options = apply(make, <c-address-options-descriptor>, initargs);
    let c-name = c-address-c-name(options);
    if (c-name)
      let import = c-address-import(options);
      #{ define constant ?var-name
           = make(check-c-address-designator(?var-name, ?pointer-designator),
	          address: primitive-wrap-machine-word
		    (primitive-cast-pointer-as-raw
		       (%c-variable-pointer(?c-name, ?import)))) };
    else
      note(<missing-c-name>,
	   source-location: fragment-source-location(form),
	   definition-name: var-name);
      #{ };
    end if;
  end;
end &macro;

define &macro check-c-address-designator
  { check-c-address-designator (?name:name, ?designator:expression) }
  =>
    begin
      let designator-model = ^eval-designator(designator);
      if (designator-class?(designator-model))
	unless (^referenced-type(designator-model))
	  note(<designator-not-a-pointer>,
	       source-location: fragment-source-location(designator),
	       definition-name: name,
	       designator-name: designator);
	  designator := #{ <C-void*> };
	end unless;
      else
	generate-unresolved-designator-error
	  (designator, name, #{ C-address }, #());
	designator := #{ <C-void*> };
      end if;
      #{ ?designator }
    end;
end &macro;


//
// C-VARIABLE-DEFINER
//

define class <c-variable-options-descriptor> (<object>)
  constant slot c-variable-setter-name = #{ #t }, init-keyword: setter:;
  constant slot c-variable-c-name = #f, init-keyword: c-name:;
  constant slot c-variable-import = #{ #f }, init-keyword: import:;
end class;

define option <c-variable-setter-option>
  => setter: :: expression
end option;

define option <c-variable-c-name-option>
  => c-name: :: expression
end option;

define option <c-variable-import-option>
  => import: :: expression
end option;

define constant $c-variable-options =
  list(<c-variable-setter-option>,
       <c-variable-c-name-option>,
       <c-variable-import-option>);

// Make sure designator-type is not a C aggregate type (struct or union)
//   What if user adds pointer-value and pointer-value-setter methods for
//   such an aggregate type????
//
define &macro c-variable-definer
  { define c-variable ?accessor:name :: ?designator-type:expression
      ?options:*;
    end
     }
  =>
  begin
    let initargs = parse-options($c-variable-options, options, accessor);
    let options = apply(make, <c-variable-options-descriptor>, initargs);
    let c-name = c-variable-c-name(options);
    if (c-name = #f)
      note(<missing-c-name>,
	   source-location: fragment-source-location(form),
	   definition-name: accessor);
      #{ };
    else
      let pointer-type-name = #{ "pointer-type-defined-for-" ## ?accessor };
      let pointer-name = #{ "$pointer-to-" ## ?accessor };
      let import = c-variable-import(options);
      let setter-name
        = macro-case (c-variable-setter-name(options))
            { #f } => #f;
            { #t } => #{ ?accessor ## "-setter" };
            { ?:name } => name;
            { ?anything-else:* }
              => begin
		   let setter = c-variable-setter-name(options);
		   note(<invalid-c-variable-setter>,
			source-location: fragment-source-location(setter),
			form-name: accessor,
			setter-expr: setter);
                   #{ ?accessor ## "-setter" };
                 end;
          end;

      let setter-definition
        = if (setter-name)
            #{ define inline method ?setter-name
	           (new :: export-type-for(?designator-type))
	        => (new :: export-type-for(?designator-type));
	         pointer-value(?pointer-name) := new;
                 new
	       end method };
	  else
	    #{ }
	  end if;

      #{ define c-pointer-type ?pointer-type-name
           => check-c-variable-designator(?accessor, ?designator-type);

         define c-address ?pointer-name :: ?pointer-type-name
           import: ?import, c-name: ?c-name;
         end;

         define inline method ?accessor ()
          => (value :: import-type-for(?designator-type))
	   pointer-value(?pointer-name)
         end;

         ?setter-definition };
    end if;
  end;
end &macro;

define &macro check-c-variable-designator
  { check-c-variable-designator (?name:name, ?designator:expression) }
  =>
    begin
      let designator-model = ^eval-designator(designator);
      if (designator-class?(designator-model))
	if (instance?(designator-model, <&c-struct/union-designator-class>))
	  note(<aggregate-designator-type>,
	       source-location: fragment-source-location(designator),
	       definition-name: name,
	       designator-name: designator);
	end if;
      else
	// Don't need to generate an undefined designator error here
	// while C-variable expands into C-address.
	designator := #{ <C-void*> };
      end if;
      #{ ?designator };
    end;
end &macro;
