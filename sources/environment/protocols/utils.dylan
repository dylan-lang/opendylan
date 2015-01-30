module: environment-protocols


define constant $n/a                        = "n/a";
define constant $type-n/a                   = "<?>";
define constant $no-information-available   = "No information available";
define constant $unknown-name               = "{unknown-name}";
define constant $project-not-built          = "[project not built]";
define constant $interactive-definition     = "interactive definition";
define constant $not-applicable = "not applicable";
define constant $not-available  = "not available";



// Describer-specific methods

define generic environment-object-description
    (project :: <project-object>,
     object :: <environment-object>, module :: <module-object>)
 => (description :: <string>);

define generic environment-object-contents
    (project :: <project-object>, environment-object :: <environment-object>)
 => (contents :: <string>);

define method environment-object-description
    (project :: <project-object>,
     object :: <environment-object>, module :: <module-object>)
 => (description :: <string>)
  environment-object-unique-name(project, object, module, qualify-names?: #t)
end method environment-object-description;

define method environment-object-description
    (project :: <project-object>,
     object :: <definition-object>, module :: <module-object>)
 => (description :: <string>)
  let home-name
    = environment-object-home-name(project, object);
  if (instance?(home-name, <binding-name-object>))
    let name         = environment-object-primitive-name(project, home-name);
    let home-module  = name-namespace(project, home-name);
    let home-library = environment-object-library(project, home-module);
    let module-name  = environment-object-primitive-name(project, home-module);
    let library-name = environment-object-primitive-name(project, home-library);
    let location     = print-environment-object-location(project, object);
    let type-name    = environment-object-type-name(object);
    concatenate
      (// The "Name:" is in the title bar.
       "In module ",   module-name,
       ", library ",   library-name,
       ", from file ", location, ":\n",
       type-name, " ", name)
  else
    environment-object-unique-name
      (project, object, module, qualify-names?: #t)
  end
end method environment-object-description;

define method environment-object-description
    (project :: <project-object>,
     object :: <user-object>, module :: <module-object>)
 => (description :: <string>)
  if (instance?(object, <definition-object>))
    next-method()
  else
    let class = application-object-class(project, object);
    concatenate
      (format-to-string
         ("Object %s:\n",
          environment-object-unique-name
            (project, object, module, qualify-names?: #t)),
       if (class)
         environment-object-description(project, class, module)
       else
         #[]
       end)
  end
end method environment-object-description;

define method environment-object-description
    (project :: <project-object>,
     object :: <collection-object>, module :: <module-object>)
 => (description :: <string>)
  let size = collection-size(project, object);
  concatenate
    (format-to-string
       ("Collection %s:\n",
        environment-object-unique-name
          (project, object, module, qualify-names?: #t)),
     format-to-string("Size: %s\n", size | ""))
end method environment-object-description;

define macro environment-object-description-method-definer
  { define environment-object-description-method ( ?class:name )
      ?:body
    end }
 => { define method environment-object-description
          (?=project :: <project-object>,
           ?=object :: ?class, ?=module :: <module-object>,
           #next next-method)
       => (description :: <string>)
        concatenate(next-method(), begin ?body end | "")
      end method environment-object-description }
end macro environment-object-description-method-definer;

define function names->name-list
    (names :: <sequence> /* of: <string> */)
 => (name-list :: <string>)
  concatenate
    ("(",
     select (names.size)
       0 => "";
       1 => names[0];
       otherwise =>
         reduce1
           (method (names-so-far, new-name)
              concatenate(names-so-far, ", ", new-name)
            end,
            names);
     end,
     ")")
end function names->name-list;

define environment-object-description-method (<class-object>)
  // ---*** Can we show init-keywords, including those accepted by make
  // and initialize methods?
  let superclass-names :: <sequence>
    = map(method (class-object)
            environment-object-display-name(project, class-object, module)
          end,
          class-direct-superclasses(project, object));
  names->name-list(superclass-names)
end environment-object-description-method;

define environment-object-description-method (<domain-object>)
  let specializers = domain-specializers(project, object);
  names->name-list(specializers)
end environment-object-description-method;

// Covers "define {generic,method,function}".
define environment-object-description-method (<dylan-function-object>)
  // ---*** Can we, for <generic-function-object>s, show all keywords
  // accepted by all methods (of those defined at compile-time)?
  // ---*** For make and initialize, can we show keyword parameters from
  // slots descriptions for the relevant class?
  concatenate
    ("\n    ",
     print-function-parameters(project, object, module),
     "\n => ",
     print-function-values(project, object, module))
end environment-object-description-method;

define environment-object-description-method (<library-object>)
  concatenate
    ("\nContents:\t",
     environment-object-contents(project, object));
end environment-object-description-method;

define environment-object-description-method (<macro-object>)
  // ---*** Would like the lhs of the first main rule.
end environment-object-description-method;

define environment-object-description-method (<module-object>)
  concatenate
    ("\nContents:\t",
     environment-object-contents(project, object));
end environment-object-description-method;

define environment-object-description-method (<module-variable-object>)
  let type = variable-type(project, object);
  concatenate
    (" :: ",
     if (type)
       environment-object-display-name(project, type, module)
     else
       $unknown
     end)
end environment-object-description-method;

define environment-object-description-method (<constant-object>)
  let type = variable-type(project, object);
  concatenate
    (" :: ",
     if (type)
       environment-object-display-name(project, type, module)
     else
       $unknown
     end)
end environment-object-description-method;


/// Counting contents

define generic environment-object-content-counts
    (project :: <project-object>, environment-object :: <environment-object>)
 => (count :: false-or(<integer>), #rest more-counts :: <integer>);

define method environment-object-content-counts
    (project :: <project-object>, project-object :: <project-object>)
 => (count :: false-or(<integer>), #rest more-counts :: <integer>)
  let library = project-library(project-object);
  if (library)
    let library-count = 1;
    let module-count :: <integer> = 0;
    let name-count :: <integer> = 0;
    let (library-module-count, library-name-count)
      = environment-object-content-counts(project-object, library);
    module-count := module-count + library-module-count;
    name-count := name-count + library-name-count;
    values(library-count, module-count, name-count)
  end
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>, library-object :: <library-object>)
 => (count :: <integer>, #rest more-counts :: <integer>)
  let module-count :: <integer> = 0;
  let name-count :: <integer> = 0;
  local method do-module (m :: <module-object>) => ()
          module-count := module-count + 1;
          let module-name-count
            = environment-object-content-counts(project, m);
          name-count := name-count + module-name-count;
        end method do-module;
  do-library-modules(do-module, project, library-object, imported?: #f);
  values(module-count, name-count)
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>, module-object :: <module-object>)
 => (count :: <integer>, #rest more-counts :: <integer>)
  let macro-count :: <integer> = 0;
  let class-count :: <integer> = 0;
  let constant-count :: <integer> = 0;
  let variable-count :: <integer> = 0;
  let gf-count :: <integer> = 0;
  let method-count :: <integer> = 0;
  let domain-count :: <integer> = 0;
  local
    method do-definition (definition :: <definition-object>) => ()
      select (definition by instance?)
        <macro-object>            => macro-count    := macro-count + 1;
        <class-object>            => class-count    := class-count + 1;
        <constant-object>         => constant-count := constant-count + 1;
        <variable-object>         => variable-count := variable-count + 1;
        <generic-function-object> => gf-count       := gf-count + 1;
        <method-object>           => method-count   := method-count + 1;
        <domain-object>           => domain-count   := domain-count + 1;
        <environment-object>      =>
          debug-out(#"environment-protocols",
                    "Unexpected definition %= when counting -- ignored",
                    definition);
      end
    end method do-definition;

  do-module-definitions(do-definition, project, module-object);
  values(macro-count,
         class-count,
         constant-count,
         variable-count,
         gf-count,
         method-count,
         domain-count)
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>,
     generic-function-object :: <generic-function-object>)
 => (count :: <integer>, #rest more-counts :: <integer>)
  let method-count :: <integer> = 0;
  local method do-method (m :: <method-object>) => ()
          ignore(m);
          method-count := method-count + 1;
        end method do-method;
  do-generic-function-methods(do-method, project, generic-function-object);
  method-count
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>, class-object :: <class-object>)
 => (count :: <integer>, #rest more-counts :: <integer>)
  let slot-count :: <integer> = 0;
  local method do-slot (s :: <slot-object>) => ()
          ignore(s);
          slot-count := slot-count + 1;
        end method do-slot;
  do-direct-slots(do-slot, project, class-object);
  slot-count
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>, composite-object :: <composite-object>)
 => (count :: false-or(<integer>), #rest more-counts :: <integer>)
  composite-object-size(project, composite-object)
end method environment-object-content-counts;

define method environment-object-content-counts
    (project :: <project-object>, collection-object :: <collection-object>)
 => (count :: false-or(<integer>), #rest more-counts :: <integer>)
  collection-size(project, collection-object)
end method environment-object-content-counts;


/// Generate contents strings

// Return a string describing a content type.

define function content-type-name
    (content-type :: <symbol>) => (string :: <string>)
  select (content-type)
    #"library"             => "library";
     #"module"             => "module";
      #"name"              => "name";
      #"definition"        => "definition";
       #"unbound"          => "unbound name";
       #"element"          => "element";
       #"macro"            => "macro";
       #"class"            => "class";
        #"slot"            => "slot";
       #"variable"         => "variable";
       #"constant"         => "constant";
       #"generic-function" => "generic function";
        #"method"          => "method";
       #"domain"           => "domain";
  end
end function content-type-name;


// Return a string describing a number of content items.

define function print-content-count
    (stream :: <stream>, count :: <integer>, content-type :: <symbol>)
 => ()
  format(stream, "%d %s",
         count,
         string-pluralize(content-type-name(content-type), count: count))
end function print-content-count;


/// print-environment-object-contents
//
// Return a string describing the contents of an environment object.

define method environment-object-contents
    (project :: <project-object>, object :: <environment-object>)
 => (contents :: <string>)
  let contents
    = with-output-to-string (stream)
        print-environment-object-contents(stream, project, object)
      end;
  if (empty?(contents))
    $not-available
  else
    contents
  end
end method environment-object-contents;

define generic print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     object :: <environment-object>)
 => ();

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     environment-object :: <environment-object>)
 => ()
  write(stream, $not-applicable)
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     project-object :: <project-object>)
 => ()
  let (library-count, module-count, name-count)
    = environment-object-content-counts(project, project-object);
  ignore(library-count);
  if (library-count)
    print-content-count(stream, module-count, #"module");
    write(stream, $list-separator);
    print-content-count(stream, name-count, #"name")
  end
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     library-object :: <library-object>)
 => ()
  let (module-count, name-count)
    = environment-object-content-counts(project, library-object);
  if (module-count)
    print-content-count(stream, module-count,  #"module");
    write(stream, $list-separator);
    print-content-count(stream, name-count, #"name")
  end
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     module-object :: <module-object>)
 => ()
  let need-separator? :: <boolean> = #f;
  local method maybe-print-content-count
            (stream :: <stream>, count :: <integer>, content-type :: <symbol>,
             #key prefix, suffix) => ()
          if (count > 0)
            if (need-separator?)
              write(stream, $list-separator)
            end;
            if (prefix) write(stream, prefix) end;
            print-content-count(stream, count, content-type);
            if (suffix) write(stream, suffix) end;
            need-separator? := #t
          end
        end method maybe-print-content-count;
  let (macro-count,
       class-count,
       constant-count,
       variable-count,
       gf-count,
       method-count,
       domain-count)
    = environment-object-content-counts(project, module-object);
  if (macro-count)
    let total-count :: <integer>
      = macro-count + class-count + constant-count + variable-count
          + gf-count + method-count + domain-count;
    if (total-count == 0)
      print-content-count(stream, total-count, #"definition")
    else
      maybe-print-content-count(stream, macro-count, #"macro");
      maybe-print-content-count(stream, class-count, #"class");
      maybe-print-content-count(stream, constant-count, #"constant");
      maybe-print-content-count(stream, variable-count, #"variable");
      maybe-print-content-count(stream, gf-count, #"generic-function");
      maybe-print-content-count(stream, method-count, #"method");
      maybe-print-content-count(stream, domain-count, #"domain");
      write(stream, " (");
      print-content-count(stream, total-count, #"definition");
      write(stream, " total)")
    end
  end
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     generic-function-object :: <generic-function-object>)
 => ()
  let method-count
    = environment-object-content-counts(project, generic-function-object);
  method-count & print-content-count(stream, method-count, #"method")
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     class-object :: <class-object>)
 => ()
  let slot-count = environment-object-content-counts(project, class-object);
  slot-count & print-content-count(stream, slot-count, #"slot")
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     collection-object :: <collection-object>)
 => ()
  let element-count
    = environment-object-content-counts(project, collection-object);
  element-count & print-content-count(stream, element-count, #"element")
end method print-environment-object-contents;

define method print-environment-object-contents
    (stream :: <stream>, project :: <project-object>,
     user-object :: <user-object>)
 => ()
  let slot-count
    = environment-object-content-counts(project, user-object);
  slot-count & print-content-count(stream, slot-count, #"slot")
end method print-environment-object-contents;

/// Function names

define constant $show-keyword-function-names = #f;

// Create a user-visible string to describe function parameters
define function print-function-parameters
    (server :: <server>, function-object :: <function-object>,
     namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  with-output-to-string (stream)
    let <object>-class = find-environment-object(server, $<object>-id);
    let (required, rest, key, all-keys?, next) // ... values, rest-value)
      = function-parameters(server, function-object);
    format(stream, "(");
    local method do-parameter (parameter :: <parameter>) => ()
            let keyword
              = instance?(parameter, <optional-parameter>)
                  & parameter.parameter-keyword;
            let type = parameter.parameter-type;
            if ($show-keyword-function-names)
              if (keyword)
                format(stream, "%s: ", keyword)
              end;
              format(stream, "%s", parameter.parameter-name);
            else
              format(stream, "%s", keyword | parameter.parameter-name)
            end;
            unless (type == <object>-class)
              format(stream, " :: %s",
                     environment-object-display-name(server, type, namespace)
                       | $type-n/a)
            end
          end method do-parameter;
    local method do-parameters (parameters :: <parameters>) => ()
            for (parameter :: <parameter> in parameters,
                 separator = "" then ", ")
              format(stream, separator);
              do-parameter(parameter)
            end for;
          end method do-parameters;
    do-parameters(required);
    let printed-something = size(required) > 0;
    local method print-separator () => ()
            if (printed-something)
              format(stream, ", ");
            else
              printed-something := #t;
            end;
          end method print-separator;
    if (next)
      print-separator();
      format(stream, "#next ");
      do-parameter(next);
    end;
    if (rest)
      print-separator();
      format(stream, "#rest ");
      do-parameter(rest);
    end;
    case
      key & size(key) > 0 =>
        print-separator();
        format(stream, "#key ");
        do-parameters(key);
        if (all-keys?)
          format(stream, ", #all-keys")
        end;
      all-keys? =>
        print-separator();
        format(stream, "#key, #all-keys");
      otherwise =>
        #f;
    end;
    format(stream, ")");
  end
end function print-function-parameters;

// Create a user-visible string to describe function values
define function print-function-values
    (server :: <server>, function-object :: <function-object>,
     namespace :: false-or(<namespace-object>))
 => (name :: <string>)
  with-output-to-string (stream)
    let <object>-class = find-environment-object(server, $<object>-id);
    let (required-params, rest-param, key-params, all-keys?, next-param,
         required-values, rest-value)
      = function-parameters(server, function-object);
    ignore(required-params, rest-param, key-params, all-keys?, next-param);
    format(stream, "(");
    local method do-value (parameter :: <parameter>) => ()
            let type = parameter.parameter-type;
            format(stream, "%s", parameter.parameter-name);
            unless (type == <object>-class)
              format(stream, " :: %s",
                     environment-object-display-name(server, type, namespace)
                       | $type-n/a)
            end
          end method do-value;
    local method do-values (_values :: <parameters>) => ()
            for (value in _values,
              count from size(_values) - 1 by -1)
              do-value(value);
              if (count > 0)
                format(stream, ", ")
              end;
            end for;
          end method do-values;
    do-values(required-values);
    if (rest-value)
      if (size(required-values) > 0)
        format(stream, ", ");
      end;
      format(stream, "#rest ");
      do-value(rest-value);
    end;
    format(stream, ")")
  end
end function print-function-values;


/// Source location display

define open generic print-environment-object-location
    (server :: <server>, object :: <environment-object>,
     #key absolute-path? :: <boolean>)
 => (location :: <string>);

define method print-environment-object-location
    (project :: <project-object>, object :: <environment-object>,
     #key absolute-path? :: <boolean>)
 => (location :: <string>)
  let source-location = environment-object-source-location(project, object);
  if (source-location)
    let source-record = source-location.source-location-source-record;
    select (source-record by instance?)
      <interactive-source-record> =>
        $interactive-definition;
      <file-source-record> =>
        let location = source-record.source-record-location;
        if (file-exists?(location))
          if (absolute-path?)
            locator-as-string(<byte-string>, location);
          else
            location.locator-name;
          end;
        end;
      otherwise =>
        source-record.source-record-name;
    end
  end
    | $n/a
end method print-environment-object-location;

define method print-environment-object-location
    (project :: <project-object>,
     project-object :: <project-object>,
     #key absolute-path? :: <boolean>)
 => (location :: <string>)
  ignore(project);
  block (ret)
    local method printit (locator :: false-or(<locator>))
            if (locator)
              if (absolute-path?)
                ret(locator-as-string(<byte-string>, locator))
              else
                ret(as(<string>, locator))
              end;
            end;
          end;
    printit(project-object.project-filename);
    printit(project-object.project-debug-filename);
    $n/a;
  end;
end method print-environment-object-location;

