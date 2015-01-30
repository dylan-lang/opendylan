Module:    environment-reports
Author:    Andy Armstrong, Jason Trenouth
Synopsis:  ReStructuredText Library report generator (use with Sphinx)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <rst-report-stream> (<report-stream>)
end class <rst-report-stream>;

define method stream-class-for-report
    (_format == #"rst") => (class :: subclass(<report-stream>))
  <rst-report-stream>
end method stream-class-for-report;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <namespace-report>,
     namespace :: <namespace-object>)
 => ()
  format(stream, "%s %s\n\n",
         definition-kind(namespace),
         environment-object-primitive-name(report.report-project, namespace));
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <namespace-report>,
     library :: <library-object>)
 => ()
  let library-name = definition-name(report, library);
  let title = concatenate("The ",
                          as-uppercase(library-name),
                          " library");
  let decorator = make(<byte-string>, fill: '*', size: title.size);
  format(stream, "%s\n%s\n%s\n\n.. current-library:: %s\n\n",
         decorator,
         title,
         decorator,
         library-name);
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     module :: <module-object>)
 => ()
  let module-name = definition-name(report, module);
  let title = concatenate("The ",
                          as-uppercase(module-name),
                          " module");
  format(stream, "\n%s\n%s\n\n.. current-module:: %s\n\n",
         title,
         make(<byte-string>, fill: '*', size: title.size),
         module-name)
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <definition-object>)
 => ()
  let project = report.report-project;
  let title = definition-name(report, definition);
  let type = definition-kind(definition);
  let rst-directive
    = select (type by \=)
        "Generic" => "generic-function";
        otherwise => as-lowercase(type);
      end;
  format(stream, "\n.. %s:: %s\n", rst-directive, title);
  write-definition-adjectives(stream, report, definition);
end method write-definition-name;

define method write-definition-adjectives
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <class-object>)
 => ()
  do(method (mod) format(stream, "   :%s:\n", as-lowercase(mod)) end,
     split(class-modifiers(report.report-project, definition), " ",
           remove-if-empty?: #t));
  next-method();
end method write-definition-adjectives;

define method write-definition-adjectives
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <generic-function-object>)
 => ()
  do(method (mod) format(stream, "   :%s:\n", as-lowercase(mod)) end,
     split(generic-function-modifiers(report.report-project, definition), " ",
           remove-if-empty?: #t));
  next-method();
end method write-definition-adjectives;

define method write-definition-adjectives
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <thread-variable-object>)
 => ()
  format(stream, "   :thread:\n");
  next-method();
end method write-definition-adjectives;

define method write-definition-adjectives
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <definition-object>)
 => ()
end method write-definition-adjectives;

define method write-superclasses-header
    (stream :: <rst-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "\n   :superclasses: ");
end method write-superclasses-header;

define method write-superclass
    (stream :: <rst-report-stream>, report :: <module-report>,
     superclass :: <definition-object>,
     #key last? :: <boolean> = #f, first? :: <boolean> = #f)
 => ()
  format(stream, "%s%s",
         if (~first?) ", " else "" end,
         rst-xref-definition-name(report, superclass))
end method write-superclass;

define method write-superclasses-footer
    (stream :: <rst-report-stream>, report :: <module-report>,
     superclass :: <class-object>)
 => ()
  new-line(stream)
end method write-superclasses-footer;

define method write-init-keywords-header
    (stream :: <rst-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  new-line(stream)
end method write-init-keywords-header;

define method write-init-keyword
    (stream :: <rst-report-stream>, report :: <module-report>,
     keyword :: <symbol>, type :: false-or(<environment-object>),
     required? :: <boolean>)
 => ()
  format(stream, "   :keyword %s%s: An instance of %s.\n",
         if (required?) "required " else "" end if,
         as(<string>, keyword),
         if (type)
           rst-xref-definition-name(report, type)
         else
           ":drm:`<object>`"
         end if)
end method write-init-keyword;

define method write-function-signature-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  format(stream, "\n   :signature: %s",
         definition-name(report, function));
end method write-function-signature-name;

define method write-function-arguments
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let (required, rest, key, all-keys?, next, required-values, rest-value)
    = function-parameters(project, function);
  local method do-parameter
            (parameter :: <parameter>, kind :: <argument-kind>) => ()
          write-function-parameter(stream, report, parameter, kind: kind)
        end method do-parameter;
  local method do-parameters
            (parameters :: <parameters>, kind :: <argument-kind>) => ()
          do(rcurry(do-parameter, kind), parameters)
        end method do-parameters;
  write-function-parameters-header(stream, report, function);
  do-parameters(required, #"input");
  rest & do-parameter(rest, #"input-rest");
  if (key & size(key) > 0)
    do-parameters(key, #"input-keyword")
  end;
  write-function-parameters-footer(stream, report, function);
end method write-function-arguments;

define method write-function-values
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let (required, rest, key, all-keys?, next, required-values, rest-value)
    = function-parameters(project, function);
  local method do-parameter
            (parameter :: <parameter>, kind :: <argument-kind>) => ();
          write-function-parameter(stream, report, parameter, kind: kind);
        end method do-parameter;
  local method do-parameters
            (parameters :: <parameters>, kind :: <argument-kind>) => ()
          do(rcurry(do-parameter, kind), parameters)
        end method do-parameters;
  write-function-parameters-header(stream, report, function, kind: #"output");
  do-parameters(required-values, #"output");
  rest-value & do-parameter(rest-value, #"output-rest");
  write-function-parameters-footer(stream, report, function, kind: #"output");
end method write-function-values;

define method write-function-parameters-header
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
  when (kind = #"input")
    new-line(stream)
  end
end method write-function-parameters-header;

define method write-function-parameter
    (stream :: <rst-report-stream>, report :: <module-report>,
     parameter :: <parameter>, #key kind :: <argument-kind> = #"input")
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let type = parameter.parameter-type;
  local method kind-to-rst-field(kind :: <argument-kind>) => (field)
          select (kind)
            #"input" => "parameter";
            #"input-rest" => "parameter #rest";
            #"input-keyword" => "parameter #key";
            #"output" => "value";
            #"output-rest" => "value #rest";
          end
        end method kind-to-rst-field;
  format(stream, "   :%s %s: An instance of %s.",
             kind-to-rst-field(kind),
             if (instance?(parameter, <optional-parameter>))
               parameter.parameter-keyword
             end
               | parameter.parameter-name,
         rst-xref-definition-name(report, type));
  new-line(stream);
end method write-function-parameter;

define method write-definition-footer
    (stream :: <rst-report-stream>, report :: <namespace-report>,
     definition :: <generic-function-object>)
 => ()
  next-method();
  write-generic-function-methods(stream, report, definition);
end method write-definition-footer;

define method write-generic-function-method
    (stream :: <rst-report-stream>, report :: <module-report>,
     gf :: <generic-function-object>, m :: <method-object>)
 => ()
  let project = report.report-project;
  format(stream, "\n.. method:: %s\n", definition-name(report, gf));
  let (required, _) = function-parameters(project, m);
  for (parameter :: <parameter> in required,
       separator = "   :specializer: " then ", ")
    write(stream, separator);
    write(stream, definition-name(report, parameter.parameter-type));
  end for;
  new-line(stream);
end method write-generic-function-method;

define function rst-xref-definition-name
    (report :: <module-report>, definition :: <environment-object>)
 => (xref :: <string>)
  let project = report.report-project;
  let namespace = report.report-namespace;
  let name = environment-object-name(project, definition, namespace);
  if (name)
    // This is defined in the same module in the same library and is
    // a named Dylan type.
    let role = definition-rst-role(definition);
    let display-name = environment-object-primitive-name(project, name);
    format-to-string(":%s:`%s`", role, display-name)
  else
    let lib = environment-object-library(project, definition);
    let display-name
      = environment-object-display-name(project, definition,
                                        namespace, qualify-names?: #f);
    if (lib & environment-object-primitive-name(project, lib) = "dylan")
      // This is something from the dylan library, so let's assume it
      // can be a DRM link. This should probably change for checking
      // that the module is also "dylan".
      format-to-string(":drm:`%s`", display-name)
    elseif (lib & lib == report.report-parent.report-namespace)
      // This is something from the same library, so it should be in
      // the same documentation set, so let's link it.
      let role = definition-rst-role(definition);
      format-to-string(":%s:`%s`", role, display-name)
    else
      // This is from a different library or is something like a
      // one-of() or false-or() value.
      format-to-string("``%s``", display-name)
    end if
  end if
end function rst-xref-definition-name;

define method definition-rst-role (object :: <constant-object>)
 => (rst-role :: <string>)
  "const"
end method definition-rst-role;

define method definition-rst-role (object :: <variable-object>)
 => (rst-role :: <string>)
  "var"
end method definition-rst-role;

define method definition-rst-role (object :: <macro-object>)
 => (rst-role :: <string>)
  "macro"
end method definition-rst-role;

define method definition-rst-role (object :: <function-object>)
 => (rst-role :: <string>)
  "func"
end method definition-rst-role;

define method definition-rst-role (object :: <generic-function-object>)
 => (rst-role :: <string>)
  "gf"
end method definition-rst-role;

define method definition-rst-role (object :: <class-object>)
 => (rst-role :: <string>)
  "class"
end method definition-rst-role;

define method definition-rst-role (object :: <library-object>)
 => (rst-role :: <string>)
  "lib"
end method definition-rst-role;

define method definition-rst-role (object :: <module-object>)
 => (rst-role :: <string>)
  "mod"
end method definition-rst-role;

