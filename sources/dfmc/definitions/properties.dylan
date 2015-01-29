Module:   dfmc-definitions
Synopsis: A framework for property adjective specification.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Properties.

// A number of Dylan language components have properties whose values are
// indicated by adjectives. The following code provides a convenient way
// of defining these adjectives and a generic adjective parser that
// attempts to generate informative error conditions.

// Perhaps this is overkill now, but when we come to support a wider
// range of adjectives, such as for inlining control, I think we'll be
// glad of this.

// Each property has a set of possible-value specifications, a default
// value, and an init-keyword used to specify the value of the
// property in a call to make (or elsewhere).

define dood-class <property> (<object>)
  lazy constant slot property-values :: <sequence>,
    required-init-keyword: values:;
  lazy constant slot property-default :: <object>,
    required-init-keyword: default:;
  lazy constant slot property-keyword :: <symbol>,
    required-init-keyword: keyword:;
end dood-class;

// Each property value specification is an association between an
// adjective in the syntax and a value.

define dood-class <property-value> (<object>)
  lazy constant slot property-value-syntax :: <symbol>,
    required-init-keyword: syntax:;
  lazy constant slot property-value-value :: <object>,
    required-init-keyword: value:;
end dood-class;

// The define property macro provides a convenient interface for defining
// a property with a set of adjectives and their associated values, and
// a default value.

/*

  define property <sealed-property> => sealed?: = #t
    value sealed = #t;
    value open   = #f;
  end property;

*/

define macro property-definer
  { define property ?property:name => ?keyword:token = ?default:expression
      ?values:*
    end }
    => { define constant ?property
           = make(<property>,
                  values:  list(?values),
                  default: ?default,
                  keyword: ?keyword); }
values:
  { }
    => { }
  { value ?syntax:name = ?value:expression; ... }
    => { make(<property-value>, syntax: ?#"syntax", value: ?value), ... }
end macro;

//// Properties parser.

// This code parses a set of adjectives looking for the given properties.
// It returns a corresponding keyword/value property list suitable,
// typically, for use in a call to make.

// TODO: Turn these into program errors.

define program-warning <unrecognized-properties>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-properties,
    required-init-keyword: properties:;
  format-string
    "Unrecognized properties %= specified in the definition of %= "
    "- ignoring.";
  format-arguments
    properties, variable-name;
end program-warning;

define program-warning <contradictory-properties>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-properties,
    required-init-keyword: properties:;
  format-string
    "Contradictory properties %= specified in the definition of %= "
    "- using default setting.";
  format-arguments
    properties, variable-name;
end program-warning;

define program-warning <duplicated-property>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-property,
    required-init-keyword: property:;
  format-string
    "The property %= is specified more than once in the definition of %=.";
  format-arguments
    property, variable-name;
end program-warning;

define function find-property
    (properties :: <list>, name :: <symbol>)
 => (prop :: false-or(<property>), prop-val :: false-or(<property-value>))
  block (return)
    for (prop :: <property> in properties)
      for (prop-val :: <property-value> in property-values(prop))
        if (property-value-syntax(prop-val) == name)
          return(prop, prop-val)
        end;
      end;
    finally
      values(#f, #f);
    end;
  end;
end function;

define method parse-property-adjectives
    (properties :: <list>, adjectives-form, name)
      => (initargs :: <list>, adjective-symbols :: <simple-object-vector>)
  // Zero, one, and two modifiers are the most common, so we special
  // case them.
  macro-case (adjectives-form)
    { }
      => values(#(), #[]);
    { ?name1:name }
      => begin
           let (prop, prop-val)
             = find-property(properties, fragment-identifier(name1));
           if (prop)
             values
               (list(property-keyword(prop), property-value-value(prop-val)),
                vector(fragment-identifier(name1)))
           else
             note(<unrecognized-properties>,
                  source-location: fragment-source-location(adjectives-form),
                  variable-name:   name,
                  properties:      list(name1));
             values(#(), #[])
           end
         end;
    { ?name1:name ?name2:name }
      => begin
           let initargs = #();
           let symbols = #();
           let unknown = #();
           let (prop1, prop-val1)
             = find-property(properties, fragment-identifier(name1));
           if (prop1)
             initargs
               := pair(property-keyword(prop1),
                       pair(property-value-value(prop-val1), initargs));
             symbols
               := pair(fragment-identifier(name1), symbols);
           else
             unknown := pair(name1, unknown);
           end;
           let (prop2, prop-val2)
             = find-property(properties, fragment-identifier(name2));
           if (prop2)
             initargs
               := pair(property-keyword(prop2),
                       pair(property-value-value(prop-val2), initargs));
             symbols
               := pair(fragment-identifier(name2), symbols);
           else
             unknown := pair(name2, unknown);
           end;
           if (prop1 & prop2 & prop1 == prop2)
             if (prop-val1 ~== prop-val2)
               note(<contradictory-properties>,
                    source-location:
                      fragment-source-location(adjectives-form),
                    variable-name:   name,
                    properties:
                      list(property-value-syntax(prop-val1),
                           property-value-syntax(prop-val2)));
               initargs := #();
               symbols := #();
             else
               note(<duplicated-property>,
                    source-location:
                      fragment-source-location(adjectives-form),
                    variable-name:   name,
                    property:
                      list(property-value-syntax(prop-val1),
                           property-value-syntax(prop-val2)));
             end;
           elseif (~empty?(unknown))
             note(<unrecognized-properties>,
                  source-location: fragment-source-location(adjectives-form),
                  variable-name:   name,
                  properties: unknown);
           end;
           values(initargs, as(<vector>, symbols));
         end;
    { ?other:* }
      => parse-many-property-adjectives(properties, adjectives-form, name);
  end macro-case;
end method;

define method parse-many-property-adjectives
    (properties :: <list>, adjectives-form, name)
      => (initargs :: <list>, adjective-symbols :: <simple-object-vector>)
  // Set up a table indexed by syntax for recognising declared properties
  // and a property table for recording all declared values.
  let syntax-table = make(<table>);
  let prop-table = make(<table>);
  for (prop in properties)
    let val-collector = make(<deque>);
    prop-table[prop] := val-collector;
    for (prop-val in prop.property-values)
      syntax-table[prop-val.property-value-syntax]
        := pair(val-collector, prop-val.property-value-value);
    end;
  end;
  // We collect any unrecognised adjectives as we go along.
  collecting (unrecognised, symbols :: <simple-object-vector>, initargs)
    macro-case (adjectives-form)
      { ?adjectives:* }
        => #f;
    adjectives:
      { }
        => #f;
      { ?adjective:name ... }
        => begin
             collect-into(symbols, adjective.fragment-identifier);
             let collector+value
               = element(syntax-table, adjective.fragment-identifier,
                         default: #f);
             if (collector+value)
               push-last(collector+value.head, collector+value.tail);
             else
               collect-into(unrecognised, adjective.fragment-identifier);
             end;
           end;
    end macro-case;
    // Was there anything we weren't expecting?
    if (~empty?(collected(unrecognised)))
      note(<unrecognized-properties>,
           source-location: fragment-source-location(adjectives-form),
           variable-name:   name,
           properties: collected(unrecognised));
    end;
    // Generate the initarg list, doing consistency checking along the way.
    for (prop-vals keyed-by prop in prop-table)
      if (~empty?(prop-vals))
        collect-into(initargs, prop.property-keyword);
        if (properties-contradictory?(prop-vals))
          note(<contradictory-properties>,
               source-location: fragment-source-location(adjectives-form),
               variable-name:   name,
               properties: properties-syntax(prop, prop-vals));
          collect-into(initargs, prop.property-default);
        elseif (properties-duplicated?(prop-vals))
          note(<duplicated-property>,
               source-location: fragment-source-location(adjectives-form),
               variable-name:   name,
               property: properties-syntax(prop, prop-vals));
          collect-into(initargs, prop-vals.first);
        else
          collect-into(initargs, prop-vals.first);
        end;
      end;
    end;
    values(collected(initargs), collected(symbols))
  end
end method;

define method properties-syntax (prop :: <property>, vals)
  map-as(<list>, curry(find-property-syntax-from-value, prop),
         remove-duplicates(vals))
end method;

define method find-property-syntax-from-value (prop, val)
  block (return)
    for (prop-val in prop.property-values)
      if (prop-val.property-value-value == val)
        return(prop-val.property-value-syntax);
      end;
    end;
  end;
end method;

//// Utilities.

define method properties-contradictory?
    (properties :: <sequence>) => (well? :: <boolean>)
  if (properties.size < 2)
    #f
  else
    ~all-identical?(properties)
  end
end method;

define method properties-duplicated?
    (properties :: <sequence>) => (well? :: <boolean>)
  properties.size > 1
end method;

define method all-identical? (l :: <sequence>)
  every?(curry(\==, l.first), l)
end method;
