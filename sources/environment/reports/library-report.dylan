Module:    environment-reports
Author:    Andy Armstrong, Jason Trenouth
Synopsis:  Library report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** consider merging report-streams and reports
/// ---*** maybe headers and footers and bodies could be done as specializers
/// ---*** could use indentation support

/// Namespace, Library, and Module reports

define constant $default-dtd
  = "./refman.dtd";
define constant $default-organization
  = "Dylan Hackers";
define constant $default-copyright
  = "Copyright (c) 2012 Dylan Hackers. All rights reserved.";
define constant $default-version
  = "1.0";

define class <namespace-report> (<project-report>, <multi-file-report>)
  slot report-multi-file? :: <boolean> = #f, init-keyword: multi-file?:;
end class <namespace-report>;

define class <library-report> (<namespace-report>)
  constant slot report-dtd :: <string> = $default-dtd,
    init-keyword: dtd:;
  constant slot report-organization :: <string> = $default-organization,
    init-keyword: organization:;
  constant slot report-copyright :: <string> = $default-copyright,
    init-keyword: copyright:;
  constant slot report-version :: <string> = $default-version,
    init-keyword: version:;
  slot report-children :: <stretchy-vector> = make(<stretchy-vector>);
  slot report-contents-file :: false-or(<locator>) = #f;
  constant slot report-object-filenames :: <object-table> = make(<object-table>);
  slot report-anonymous-count :: <integer> = 0;
end class <library-report>;

install-report(#"interface-reference", "Library interface reference",
               <library-report>,
               formats: #[#"text", #"html", #"rst", #"xml"],
               multi-file?: #t);

define class <module-report> (<namespace-report>)
  sealed constant slot report-namespace :: <namespace-object>,
    required-init-keyword: namespace:;
  sealed constant slot report-parent :: <library-report>,
    required-init-keyword: parent:;
end class <module-report>;

define method report-namespace (report :: <library-report>)
  report.report-project.project-library
end method report-namespace;

define method write-report-as
    (stream :: <stream>, report :: <library-report>, format :: <report-format>)
  => ()
  write-library-report(stream, report)
end method write-report-as;

define method create-multi-file-report-as
    (report :: <library-report>, directory :: <directory-locator>, format :: <report-format>)
 => (filename :: <file-locator>)
  report-directory(report) := directory;
  report-multi-file?(report) := #t;
  write-library-report(directory, report);
  report-contents-file(report);
end method create-multi-file-report-as;

define method write-library-report
    (stream :: <stream>, report :: <library-report>)
 => ()
  if (report.report-multi-file? & (report.report-format ~= #"html"))
    error(make(<report-error>)) // ---*** we only support multi-file reports for HTML at the moment
  end if;
  let library = report.report-project.project-library;
  let stream
    = make(stream-class-for-report(report.report-format),
           inner-stream: stream);
  write-definition-report(stream, report, library);
end method write-library-report;

define method write-library-report
    (file :: <locator>, report :: <library-report>)
 => ()
  report-contents-file(report) := compute-contents-file(report, file);
  with-open-file (stream = report-contents-file(report), direction: #"output")
    write-library-report(stream, report)
  end with-open-file;
end method write-library-report;

define method compute-contents-file
    (report :: <library-report>, file :: <locator>)
 => (contents :: <locator>)
  if (report-multi-file?(report))
    let project = report.report-project;
    let library = project.project-library;
    let library-name = environment-object-primitive-name(project, library);
    let name = format-to-string("%s-library-reference.htm", library-name);
    merge-locators(as(<file-locator>, name), as(<directory-locator>, file));
  else
    file
  end if;
end method compute-contents-file;


/// <REPORT-STREAM> Protocols

define class <report-stream> (<wrapper-stream>)
end class <report-stream>;

define constant <argument-kind>
  = one-of(#"input", #"input-rest", #"input-keyword",
           #"output", #"output-rest");

define generic write-definition-report (stream :: <report-stream>, report :: <namespace-report>, object :: false-or(<definition-object>)) => ();
define generic write-definition-header (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>)  => ();
define generic write-definition-body (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>) => ();
define generic write-definition-footer (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>) => ();
define generic write-definition-separator (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>) => ();
define generic write-definition-name (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>) => ();
define generic write-variable-type (stream :: <report-stream>, report :: <namespace-report>, variable :: <variable-object>) => ();
define generic write-variable-value (stream :: <report-stream>, report :: <namespace-report>, variable :: <variable-object>) => ();
define generic write-class-superclasses (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-superclasses-header (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-superclass (stream :: <report-stream>, report :: <namespace-report>, superclass :: <definition-object>, #key last? :: <boolean> = #f, first? :: <boolean> = #f) => ();
define generic write-superclasses-footer (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-class-init-keywords (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-init-keywords-header (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-init-keyword (stream :: <report-stream>, report :: <namespace-report>, keyword :: <symbol>, type :: false-or(<environment-object>)) => ();
define generic write-init-keywords-footer (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-operations (stream :: <report-stream>, report :: <namespace-report>, class :: <class-object>) => ();
define generic write-function-signature (stream :: <report-stream>, report :: <namespace-report>, function :: <function-object>) => ();
define generic write-function-arguments (stream :: <report-stream>, report :: <namespace-report>, function :: <function-object>) => ();
define generic write-function-values (stream :: <report-stream>, report :: <namespace-report>, function :: <function-object>) => ();
define generic write-function-parameter (stream :: <report-stream>, report :: <namespace-report>, function :: <parameter>, #key kind :: <argument-kind> = #"input") => ();
define generic write-function-parameters-header (stream :: <report-stream>, report :: <namespace-report>, function :: <function-object>, #key kind :: <argument-kind> = #"input") => ();
define generic write-function-parameters-footer (stream :: <report-stream>, report :: <namespace-report>, function :: <function-object>, #key kind :: <argument-kind> = #"input") => ();
define generic write-see-also (stream :: <report-stream>, report :: <namespace-report>, definition :: <definition-object>) => ();

/// <REPORT-STREAM> Default Methods

define method write-definition-report
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  write-definition-separator(stream, report, definition);
  write-definition-header(stream, report, definition);
  write-definition-body(stream, report, definition);
  write-definition-footer(stream, report, definition);
end method write-definition-report;

define method write-definition-report
    (stream :: <report-stream>, report :: <namespace-report>,
     object == #f)
 => ()
  // NB ignore non <definition-object>s
end method write-definition-report;

define method write-definition-header
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  write-definition-name(stream, report, definition);
  write-definition-contents(stream, report, definition);
end method write-definition-header;

define method write-definition-separator
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-definition-separator;

define method write-definition-name
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-definition-name;

define method write-definition-contents
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-definition-contents;

define method write-definition-body
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-definition-body;

define method write-definition-footer
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  write-description(stream, report, definition);
  write-see-also(stream, report, definition)
end method write-definition-footer;


define method write-description
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-description;

define method write-see-also
    (stream :: <report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
end method write-see-also;

define method write-definition-contents
    (stream :: <report-stream>, report :: <library-report>,
     library :: <library-object>)
 => ()
  let project = report.report-project;
  let names = namespace-sorted-names(project, library);
  for (name :: <module-name-object> in names)
    let module = name-value(project, name);
    report.report-children :=
      add!(report.report-children,
           make(<module-report>,
                format: report.report-format,
                multi-file?: report.report-multi-file?,
                project: project,
                parent:  report,
                namespace:  module));
  end for;
end method write-definition-contents;

define method write-definition-body
    (stream :: <report-stream>, report :: <library-report>,
     library :: <library-object>)
  => ()
  for (subreport in report.report-children)
    let module = subreport.report-namespace;
    if (subreport.report-multi-file?)
      let filename = report-object-filename(subreport, module);
      with-open-file (stream = filename, direction: #"output")
        let stream
          = make(stream-class-for-report(report.report-format),
                 inner-stream: stream);
        with-html-rubric (istream = stream.inner-stream, // ---*** push this down into HTML method
                          format-to-string("%s %s",
                                           definition-name(subreport, module),
                                           definition-kind(module)))
          write-definition-report(stream, subreport, module)
        end with-html-rubric;
      end with-open-file;
    else
      write-definition-report(stream, subreport, module)
    end if;
  end for;
end method write-definition-body;

define method write-definition-body
    (stream :: <report-stream>, report :: <module-report>,
     module :: <module-object>)
 => ()
  let project = report.report-project;
  let names = namespace-sorted-names(project, module);
  for (name :: <binding-name-object> in names)
    let definition = name-value(project, name);
    if (instance?(definition, <definition-object>))
      if (report.report-multi-file?)
        let filename = report-object-filename(report, definition);
        with-open-file (stream = filename, direction: #"output")
          let stream
          = make(stream-class-for-report(report.report-format),
                 inner-stream: stream);
          with-html-rubric (istream = stream.inner-stream, // ---*** push this down into HTML method
                            format-to-string("%s %s",
                                             definition-name(report, definition),
                                             definition-kind(definition)))
            write-definition-report(stream, report, definition)
          end with-html-rubric;
        end with-open-file;
      else
        write-definition-report(stream, report, definition);
      end if;
    end if;
  end for;
end method write-definition-body;

define method write-definition-body
    (stream :: <report-stream>, report :: <module-report>,
     variable :: type-union(<module-variable-object>, <constant-object>))
 => ()
  write-variable-type(stream, report, variable);
  write-variable-value(stream, report, variable);
end method write-definition-body;

define method write-definition-body
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  write-class-superclasses(stream, report, class);
  write-class-init-keywords(stream, report, class);
  write-operations(stream, report, class);
end method write-definition-body;

define method write-definition-body
    (stream :: <report-stream>, report :: <module-report>,
     function :: <dylan-function-object>)
 => ()
  write-function-signature(stream, report, function);
  write-function-arguments(stream, report, function);
  write-function-values(stream, report, function);
end method write-definition-body;

define method write-definition-body
    (stream :: <report-stream>, report :: <module-report>,
     _macro :: <macro-object>)
 => ()
  // ---*** what to do?
end method write-definition-body;

define method write-variable-type
    (stream :: <report-stream>, report :: <module-report>,
     variable :: <variable-object>)
 => ()
end method write-variable-type;

define method write-variable-value
    (stream :: <report-stream>, report :: <module-report>,
     variable :: <variable-object>)
 => ()
end method write-variable-value;

define method write-class-superclasses
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  write-superclasses-header(stream, report, class);
  let superclasses = class-direct-superclasses(project, class);
  let length = size(superclasses);
  for (superclass in superclasses,
       i from 0 below length,
       first? = #t then #f)
    let last? = (i = (length - 1));
    write-superclass(stream, report, superclass, first?: first?, last?: last?);
  end;
  write-superclasses-footer(stream, report, class);
end method write-class-superclasses;

define method write-superclasses-header
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
end method write-superclasses-header;

define method write-superclass
    (stream :: <report-stream>, report :: <module-report>,
     superclass :: <definition-object>,
     #key last? :: <boolean> = #f, first? :: <boolean> = #f)
 => ()
end method write-superclass;

define method write-superclasses-footer
    (stream :: <report-stream>, report :: <module-report>,
     superclass :: <class-object>)
 => ()
end method write-superclasses-footer;

define method write-class-init-keywords
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  write-init-keywords-header(stream, report, class);
  let types = make(<table>);
  do-init-keywords
    (method
         (definition :: <definition-object>,
          keyword :: <symbol>,
          type :: false-or(<environment-object>),
          required? :: <boolean>,
          inherited? :: <boolean>)
       unless (element(types, keyword, default: #f))
         types[keyword] := type
       end unless;
     end,
     project,
     class,
     inherited?: #f);
  for (keyword in sort!(key-sequence(types)))
    write-init-keyword(stream, report, keyword, types[keyword])
  end;
  write-init-keywords-footer(stream, report, class);
end method write-class-init-keywords;

define method write-init-keywords-header
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
end method write-init-keywords-header;

define method write-init-keyword
    (stream :: <report-stream>, report :: <module-report>,
     keyword :: <symbol>, type :: false-or(<environment-object>))
 => ()
end method write-init-keyword;

define method write-init-keywords-footer
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
end method write-init-keywords-footer;

define method write-operations
    (stream :: <report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
end method write-operations;

define method write-function-signature
    (stream :: <report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let (required, rest, key, all-keys?, next, required-values, rest-value)
    = function-parameters(project, function);
  write-function-name(stream, report, function);
  format(stream, " (");
  local method do-parameter (parameter :: <parameter>) => ()
          format(stream, "%s",
                 if (instance?(parameter, <optional-parameter>))
                   parameter.parameter-keyword
                 end
                   | parameter.parameter-name)
        end method do-parameter;
  local method do-parameters (parameters :: <parameters>) => ()
          for (parameter :: <parameter> in parameters,
               separator = "" then " ")
            format(stream, separator);
            do-parameter(parameter)
          end for;
        end method do-parameters;
  do-parameters(required);
  let printed-something = size(required) > 0;
  local method print-separator () => ()
          if (printed-something)
            format(stream, " ");
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
        format(stream, " #all-keys")
      end;
    all-keys? =>
      print-separator();
      format(stream, "#key #all-keys");
    otherwise =>
      #f;
  end;
  format(stream, ") => (");
  do-parameters(required-values);
  if (rest-value)
    if (size(required-values) > 0)
      format(stream, ", ");
    end;
    format(stream, "#rest ");
    do-parameter(rest-value)
  end;
  format(stream, ")");
  new-line(stream);
end method write-function-signature;

define method write-function-arguments
    (stream :: <report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let (required, rest, key, all-keys?, next, required-values, rest-value)
    = function-parameters(project, function);
  local method do-parameter (parameter :: <parameter>) => ()
          write-function-parameter(stream, report, parameter)
        end method do-parameter;
  local method do-parameters (parameters :: <parameters>) => ()
          do(do-parameter, parameters)
        end method do-parameters;
  write-function-parameters-header(stream, report, function);
  do-parameters(required);
  rest & do-parameter(rest);
  if (key & size(key) > 0)
    do-parameters(key)
  end;
  write-function-parameters-footer(stream, report, function);
end method write-function-arguments;

define method write-function-values
    (stream :: <report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let (required, rest, key, all-keys?, next, required-values, rest-value)
    = function-parameters(project, function);
  local method do-parameter (parameter :: <parameter>) => ()
          write-function-parameter(stream, report, parameter, kind: #"output");
        end method do-parameter;
  local method do-parameters (parameters :: <parameters>) => ()
          do(do-parameter, parameters)
        end method do-parameters;
  write-function-parameters-header(stream, report, function, kind: #"output");
  do-parameters(required-values);
  rest-value & do-parameter(rest-value);
  write-function-parameters-footer(stream, report, function, kind: #"output");
end method write-function-values;

define method write-function-parameter
    (stream :: <report-stream>, report :: <module-report>,
     function :: <parameter>, #key kind :: <argument-kind> = #"input")
 => ()
end method write-function-parameter;

define method write-function-parameters-header
    (stream :: <report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
end method write-function-parameters-header;

define method write-function-parameters-footer
    (stream :: <report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
end method write-function-parameters-footer;


/// <TEXT-REPORT-STREAM> methods

define class <text-report-stream> (<report-stream>)
end class <text-report-stream>;

define method stream-class-for-report
    (_format == #"text") => (class :: subclass(<report-stream>))
  <text-report-stream>
end method stream-class-for-report;

define method write-definition-separator
    (stream :: <text-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  format(stream, "%s\n", $report-separator);
end method write-definition-separator;

define method write-definition-name
    (stream :: <text-report-stream>, report :: <namespace-report>, namespace :: <namespace-object>)
 => ()
  format(stream, "%s %s\n\n",
         definition-kind(namespace),
         environment-object-primitive-name(report.report-project, namespace));
end method write-definition-name;

define method write-definition-name
    (stream :: <text-report-stream>, report :: <module-report>, definition :: <definition-object>)
 => ()
  let project = report.report-project;
  let title = definition-name(report, definition);
  let type = definition-type-description(project, definition);
  let padding = max(6, $report-width - title.size - type.size);
  format(stream, "%s%s%s\n",
         title,
         make(<byte-string>, fill: ' ', size: padding),
         type)
end method write-definition-name;

define method write-superclasses-header
    (stream :: <text-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "\nSuperclasses:\n\n");
end method write-superclasses-header;

define method write-superclass
    (stream :: <text-report-stream>, report :: <module-report>,
     superclass :: <definition-object>,
     #key last? :: <boolean> = #f, first? :: <boolean> = #f)
 => ()
  format(stream, "%s\n", definition-name(report, superclass))
end method write-superclass;

define method write-init-keywords-header
    (stream :: <text-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "\nInit-keywords:\n\n");
end method write-init-keywords-header;

define method write-init-keyword
    (stream :: <text-report-stream>, report :: <module-report>,
     keyword :: <symbol>, type :: false-or(<environment-object>))
 => ()
  format(stream, "  %s:\n", as(<string>, keyword))
end method write-init-keyword;

define method write-function-name
    (stream :: <text-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  format(stream, "\n%s ",
         definition-name(report, function));
end method write-function-name;

define method write-function-parameter
    (stream :: <text-report-stream>, report :: <module-report>,
     parameter :: <parameter>, #key kind :: <argument-kind> = #"input")
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let type = parameter.parameter-type;
  format(stream, "%s :: %s",
             if (instance?(parameter, <optional-parameter>))
               parameter.parameter-keyword
             end
               | parameter.parameter-name,
         definition-name(report, type));
  new-line(stream);
end method write-function-parameter;


/// <RST-REPORT-STREAM> methods

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
  let title = concatenate("The ",
                          as-uppercase(definition-name(report, library)),
                          " library");
  let decorator = make(<byte-string>, fill: '*', size: title.size);
  format(stream, "%s\n%s\n%s\n",
         decorator,
         title,
         decorator)
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     module :: <module-object>)
 => ()
  let title = concatenate("The ",
                          as-uppercase(definition-name(report, module)),
                          " module");
  format(stream, "\n%s\n%s\n",
         title,
         make(<byte-string>, fill: '-', size: title.size))
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  next-method();
  do(method (mod) format(stream, "   :%s:\n", as-lowercase(mod)) end,
     split(class-modifiers(report.report-project, class), " ",
           remove-if-empty?: #t))
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <generic-function-object>)
 => ()
  next-method();
  do(method (mod) format(stream, "   :%s:\n", as-lowercase(mod)) end,
     split(generic-function-modifiers(report.report-project, function), " ",
           remove-if-empty?: #t))
end method write-definition-name;

define method write-definition-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     definition :: <definition-object>)
 => ()
  let project = report.report-project;
  let title = definition-name(report, definition);
  let type = definition-kind(definition);
  let rst-directive = select(type)
    "Generic" => "generic-function";
    otherwise => as-lowercase(type);
  end;
  format(stream, "\n.. %s:: %s\n",
         rst-directive, title)
end method write-definition-name;

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
         definition-name(report, superclass))
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
     keyword :: <symbol>, type :: false-or(<environment-object>))
 => ()
  format(stream, "   :keyword %s:\n", as(<string>, keyword))
end method write-init-keyword;

define method write-function-name
    (stream :: <rst-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  format(stream, "\n   :signature: %s",
         definition-name(report, function));
end method write-function-name;

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
  format(stream, "   :%s %s: An instance of ``%s``.",
             kind-to-rst-field(kind),
             if (instance?(parameter, <optional-parameter>))
               parameter.parameter-keyword
             end
               | parameter.parameter-name,
         definition-name(report, type));
  new-line(stream);
end method write-function-parameter;


/// HTML implementation

/// ----------------------------------------------------------------------
/// HTML

define class <html-report-stream> (<report-stream>)
end class <html-report-stream>;

define method initialize (stream :: <html-report-stream>, #key inner-stream)
  next-method();
  stream.inner-stream := ensure-html-stream(inner-stream);

end method initialize;

define method ensure-html-stream (stream :: <stream>)
 => (html-stream :: <html-wrapper-stream>)
  make(<html-wrapper-stream>, inner-stream: stream)
end method ensure-html-stream;

define method ensure-html-stream (stream :: <html-wrapper-stream>)
 => (html-stream :: <html-wrapper-stream>)
  stream
end method ensure-html-stream;

define method write-html (stream :: <html-report-stream>, #rest sequence)
 => ()
  apply(write-html, stream.inner-stream, sequence)
end method write-html;

define method stream-class-for-report
    (_format == #"html") => (class :: subclass(<report-stream>))
  <html-report-stream>
end method stream-class-for-report;

define method write-definition-separator
    (stream :: <html-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  write-html(stream, #"hr", '\n');
end method write-definition-separator;

define method write-definition-name
    (stream :: <html-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  let title = definition-name(report, definition);
  write-html(stream,
             #"h3", make(<html-anchor>, name: title),
             title, #"/h3", '\n');
end method write-definition-name;


/// HTML section generation

define method write-superclasses-header
    (stream :: <html-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  write-html(stream, #"p", "Superclasses: ");
end method write-superclasses-header;

define method write-superclass
    (stream :: <html-report-stream>, report :: <module-report>,
     superclass :: <definition-object>,
     #key last? :: <boolean> = #f, first? :: <boolean> = #f)
 => ()
  write-object-reference(stream, report, superclass)
end method write-superclass;

define method write-superclasses-footer
    (stream :: <html-report-stream>, report :: <module-report>,
     superclass :: <class-object>)
 => ()
  new-line(stream)
end method write-superclasses-footer;

define method write-init-keywords-header
    (stream :: <html-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  write-html(stream, #"p", "Init-keywords:", '\n', '\n');
  write-html(stream, #"ul", '\n');
end method write-init-keywords-header;

define method write-init-keyword
    (stream :: <html-report-stream>, report :: <module-report>,
     keyword :: <symbol>, type :: false-or(<environment-object>))
 => ()
  write-html(stream, #"li", as(<string>, keyword), ':');
  new-line(stream);
end method write-init-keyword;

define method write-init-keywords-footer
    (stream :: <html-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  write-html(stream, #"/ul", '\n');
  new-line(stream)
end method write-init-keywords-footer;

define method write-function-name
    (stream :: <html-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
  format(stream, "%s ",
         definition-name(report, function));
end method write-function-name;

define method write-function-parameter
    (stream :: <html-report-stream>, report :: <module-report>,
     parameter :: <parameter>, #key kind :: <argument-kind> = #"input")
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let type = parameter.parameter-type;
  write-html(stream, #"li",
             if (instance?(parameter, <optional-parameter>))
               parameter.parameter-keyword
             end
               | parameter.parameter-name,
             " :: ");
  print-environment-object-name
    (stream, project, type, namespace: module);
  write-html(stream, #"/li", '\n')
end method write-function-parameter;

define method write-function-parameters-header
    (stream :: <html-report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
  write-html(stream, #"ul", '\n');
end method write-function-parameters-header;

define method write-function-parameters-footer
    (stream :: <html-report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
  write-html(stream, #"/ul", '\n')
end method write-function-parameters-footer;

define method write-definition-contents
    (stream :: <html-report-stream>, report :: <library-report>,
     library :: <library-object>)
 => ()
  next-method();
  if (report-multi-file?(report))
    let project = report.report-project;
    let library = project.project-library;
    let library-name = environment-object-primitive-name(project, library);
    with-html-output (stream = stream.inner-stream, format-to-string("%s Library", library-name))
      write-html(stream, #"ul", '\n');
      for (report in report.report-children)
        let module = report.report-namespace;
        let filename = report-object-filename(report, module);
        write-html(stream,
                   #"li", make(<html-reference>, name: filename),
                   environment-object-primitive-name(project, module),
                   " Module", #"/a");
      end for;
    end with-html-output;
  end if;
end method write-definition-contents;

define method write-definition-contents
    (stream :: <html-report-stream>, report :: <module-report>,
     module :: <module-object>)
 => ()
  if (report-multi-file?(report))
    let project = report.report-project;
    let module-name = environment-object-primitive-name(project, module);
    let filename = report-object-filename(report, module);
    let names = namespace-sorted-names(project, module);
    write-html(stream, #"ul", '\n');
    for (name :: <binding-name-object> in names)
      let definition = name-value(project, name);
      if (instance?(definition, <definition-object>))
        let title = definition-name(report, definition);
        let filename = report-object-filename(report, definition);
        write-html(stream,
                   #"li",
                   make(<html-reference>, name: format-to-string("%s#%s", filename, title)),
                   title,
                   #"/a",
                   '\n');
      end if;
    end for;
  end if;
end method write-definition-contents;


/// <XML-REPORT-STREAM> methods

define class <xml-report-stream> (<report-stream>)
end class <xml-report-stream>;

define method stream-class-for-report
    (_format == #"xml") => (class :: subclass(<report-stream>))
  <xml-report-stream>
end method stream-class-for-report;

define method write-definition-header
    (stream :: <xml-report-stream>, report :: <library-report>,
     definition :: <library-object>)
 => ()
  format(stream,
         "<?xml version=\"1.0\"?>\n"
           "<!DOCTYPE refman SYSTEM \"%s\">\n"
           "<refman>\n"
           "\n"
           "<head>\n"
           "<title>Open Dylan %s Reference Manual</title>\n"
           "<organization>%s</organization>\n"
           "<copyright>%s</copyright>\n"
           "<version>%s</version>\n"
           "</head>\n"
           "\n",
         report.report-dtd,
         definition-name(report, report.report-namespace),
         report.report-organization,
         report.report-copyright,
         report.report-version);
  next-method();
end method write-definition-header;

define method write-definition-header
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     definition :: <namespace-object>)
 => ()
  format(stream, "<%s>\n", as-lowercase(definition-kind(definition)));
  write-definition-name(stream, report, definition);
  write-definition-contents(stream, report, definition);
end method write-definition-header;

define method write-definition-footer
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     definition :: <namespace-object>)
  => ()
  format(stream, "</%s>\n", as-lowercase(definition-kind(definition)));
end method write-definition-footer;

define method write-definition-footer
    (stream :: <xml-report-stream>, report :: <library-report>,
     definition :: <library-object>)
  => ()
  next-method();
  format(stream, "</refman>\n");
end method write-definition-footer;

define method write-definition-header
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  let project = report.report-project;
  format(stream, "<entry>\n");
  next-method();
  format(stream, "  <%sdef>\n", as-lowercase(definition-kind(definition)));
end method write-definition-header;

define method write-definition-header
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     class :: <class-object>)
 => ()
  next-method();
  format(stream, "    <modifiers>%s</modifiers>\n", class-modifiers(report.report-project, class));
end method write-definition-header;

define method write-definition-header
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     function :: <generic-function-object>)
 => ()
  next-method();
  format(stream, "    <modifiers>%s</modifiers>\n",
         generic-function-modifiers(report.report-project, function));
end method write-definition-header;

define method write-definition-footer
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
  => ()
  let project = report.report-project;
  format(stream, "  </%sdef>\n", as-lowercase(definition-kind(definition)));
  next-method();
  format(stream, "</entry>\n");
end method write-definition-footer;

define method write-definition-separator
    (stream :: <xml-report-stream>, report :: <namespace-report>,
     definition :: <definition-object>)
 => ()
  new-line(stream)
end method write-definition-separator;

define method write-definition-separator
    (stream :: <xml-report-stream>, report :: <library-report>,
     definition :: <library-object>)
 => ()
end method write-definition-separator;

define method write-definition-name
    (stream :: <xml-report-stream>, report :: <library-report>, library :: <namespace-object>)
 => ()
  format(stream, "  <name>%s</name>\n\n",
         environment-object-primitive-name(report.report-project, library));
end method write-definition-name;

define method write-definition-name
    (stream :: <xml-report-stream>, report :: <module-report>, definition :: <definition-object>)
 => ()
  let name = definition-name(report, definition);
  format(stream, "  <name><![CDATA[%s]]></name>\n", name);
end method write-definition-name;

define method write-variable-type
    (stream :: <xml-report-stream>, report :: <module-report>,
     variable :: <variable-object>)
 => ()
  let type = variable-type(report.report-project, variable);
  let name = type & definition-name(report, type);
  format(stream, "    <type><![CDATA[%s]]></type>\n", name | "<object>");
end method write-variable-type;

define method write-variable-value
    (stream :: <xml-report-stream>, report :: <module-report>,
     variable :: <variable-object>)
 => ()
  let value = variable-value(report.report-project, variable);
  let name = value & definition-name(report, value);
  format(stream, "    <value><![CDATA[%s]]></value>\n", name | "#f");
end method write-variable-value;

define method write-superclasses-header
    (stream :: <xml-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "    <superclasses>\n");
end method write-superclasses-header;

define method write-superclass
    (stream :: <xml-report-stream>, report :: <module-report>,
     superclass :: <definition-object>,
     #key last? :: <boolean> = #f, first? :: <boolean> = #f)
 => ()
  format(stream, "      <![CDATA[%s]]>\n", definition-name(report, superclass))
end method write-superclass;

define method write-superclasses-footer
    (stream :: <xml-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "    </superclasses>\n");
end method write-superclasses-footer;

define method write-init-keywords-header
    (stream :: <xml-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "    <keywords>\n");
end method write-init-keywords-header;

define method write-init-keyword
    (stream :: <xml-report-stream>, report :: <module-report>,
     keyword :: <symbol>, type :: false-or(<environment-object>))
 => ()
  format(stream,
         "      <keyword>\n"
           "        <name>%s:</name>\n"
           "        <type><![CDATA[%s]]></type>\n"
           "        <description></description>\n"
           "      </keyword>\n",
         as(<string>, keyword),
         if (type)
           definition-name(report, type)
         else
           "<object>"
         end if);
end method write-init-keyword;

define method write-init-keywords-footer
    (stream :: <xml-report-stream>, report :: <module-report>,
     class :: <class-object>)
 => ()
  format(stream, "    </keywords>\n");
end method write-init-keywords-footer;

define method write-function-signature
    (stream :: <xml-report-stream>, report :: <module-report>,
     function :: <function-object>)
 => ()
end method write-function-signature;

define method write-function-arguments
    (stream :: <xml-report-stream>, report :: <module-report>,
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
  if(all-keys?) format(stream, "      <all-keys/>\n") end;
  write-function-parameters-footer(stream, report, function);
end method write-function-arguments;

define method write-function-values
    (stream :: <xml-report-stream>, report :: <module-report>,
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
    (stream :: <xml-report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
  select (kind)
    #"input" => format(stream, "    <ins>\n");
    #"output" => format(stream, "    <outs>\n");
  end select;
end method write-function-parameters-header;

define method write-function-parameters-footer
    (stream :: <xml-report-stream>, report :: <module-report>,
     function :: <function-object>, #key kind :: <argument-kind> = #"input")
 => ()
  select (kind)
    #"input" => format(stream, "    </ins>\n");
    #"output" => format(stream, "    </outs>\n");
  end select;
end method write-function-parameters-footer;

define method write-function-parameter
    (stream :: <xml-report-stream>, report :: <module-report>,
     parameter :: <parameter>, #key kind :: <argument-kind> = #"input")
 => ()
  let project = report.report-project;
  let module = report.report-namespace;
  let type = parameter.parameter-type;
  let tag
    = select(kind)
        #"input" => "in";
        #"input-rest" => "rest-in";
        #"input-keyword" => "keyword-in";
        #"output" => "out";
        #"output-rest" => "rest-out"
      end select;
  format(stream,
         "      <%s>\n"
         "        <name>%s</name>\n"
           "        <type><![CDATA[%s]]></type>\n"
           "        <description></description>\n"
           "      </%s>\n",
         tag,
         (if (instance?(parameter, <optional-parameter>))
            parameter.parameter-keyword
          end
          | parameter.parameter-name),
         definition-name(report, type),
         tag);
end method write-function-parameter;

define method write-description
    (stream :: <xml-report-stream>, report :: <module-report>,
     definition :: <definition-object>)
  => ()
  format(stream, "  <description></description>\n");
end method write-description;

define method write-see-also
    (stream :: <xml-report-stream>, report :: <module-report>,
     definition :: <definition-object>)
  => ()
  format(stream, "  <seealso></seealso>\n");
end method write-see-also;


/// Utilities

define method report-object-filename
    (report :: <module-report>, object :: <environment-object>)
 => (filename :: <string>)
  let parent = report.report-parent;
  let table = parent.report-object-filenames;
  element(table, object, default: #f)
    | begin
        let directory = parent.report-directory;
        let file
          = select (object by instance?)
              <definition-object> =>
                let name = mangle-for-filename(definition-name(report, object));
                format-to-string("%s-reference.htm", name);
              otherwise =>
                let next-count = parent.report-anonymous-count + 1;
                parent.report-anonymous-count := next-count;
                format-to-string("anonymous-%d.htm", next-count);
            end;
        as(<string>, merge-locators(as(<file-locator>, file),
                                    as(<directory-locator>, directory)))
      end
end method report-object-filename;

define method mangle-for-filename
    (filename :: <string>)
 => (mangled :: <string>)
  let chars = make(<stretchy-vector>);
  for (c in filename)
    let next = select (c)
                 '<' => "lt";
                 '>' => "gt";
                 '?' => "qm";
                 '$' => "dl";
                 otherwise => c;
               end select;
    select (next by instance?)
      <string> =>
        for (n in next)
          chars := add!(chars, n)
        end for;
      <character> =>
        chars := add!(chars, next);
    end
  end;
  as(<string>, chars)
end method mangle-for-filename;

define method write-object-reference
    (stream :: <html-report-stream>, report :: <module-report>,
     object :: <environment-object>)
 => ()
  let reference = html-object-reference(report, object);
  write-html(stream,
             make(<html-reference>, name: reference),
             definition-name(report, object),
             #"/a")
end method write-object-reference;

define method html-object-reference
    (report :: <module-report>, object :: <environment-object>)
 => ()
  let parent = report.report-parent;
  let title = definition-name(report, object);
  if (parent.report-directory)
    let filename = report-object-filename(report, object);
    format-to-string("%s#%s", filename, title)
  else
    format-to-string("#%s", title)
  end
end method html-object-reference;

define function namespace-sorted-names
    (project :: <project-object>, namespace :: <namespace-object>,
     #key internal?)
 => (sorted-names :: <stretchy-object-vector>)
  let names :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-namespace-names
    (method (name :: <name-object>)
       if (internal? | name-exported?(project, name))
         add!(names, name)
       end
     end,
     project, namespace);
  sort(names,
       test: method
                 (name1 :: <name-object>, name2 :: <name-object>)
               let p1 = environment-object-primitive-name(project, name1);
               let p2 = environment-object-primitive-name(project, name2);
               p1 < p2
             end)
end function namespace-sorted-names;

define method definition-name
    (report :: <namespace-report>, definition :: <environment-object>)
 => (name :: <string>)
  let project = report.report-project;
  let namespace = report.report-namespace;
  let name = environment-object-name(project, definition, namespace);
  if (name)
    environment-object-primitive-name(project, name)
  else
    environment-object-display-name(project, definition, namespace)
  end
end method definition-name;

define method definition-type-description
    (project :: <project-object>, class :: <definition-object>)
 => (description :: <string>)
  definition-kind(class)
end method definition-type-description;

define method definition-kind (object :: <constant-object>)
 => (kind :: <string>)
  "Constant"
end method definition-kind;

define method definition-kind (object :: <variable-object>)
 => (description :: <string>)
  "Variable"
end method definition-kind;

define method definition-kind (object :: <macro-object>)
 => (description :: <string>)
  "Macro"
end method definition-kind;

define method definition-kind (object :: <function-object>)
 => (description :: <string>)
  "Function"
end method definition-kind;

define method definition-kind (object :: <generic-function-object>)
 => (description :: <string>)
  "Generic"
end method definition-kind;

define method definition-kind (object :: <class-object>)
 => (description :: <string>)
  "Class"
end method definition-kind;

define method definition-kind (object :: <library-object>)
 => (description :: <string>)
  "Library"
end method definition-kind;

define method definition-kind (object :: <module-object>)
 => (description :: <string>)
  "Module"
end method definition-kind;

define method definition-type-description
    (project :: <project-object>, function :: <generic-function-object>)
 => (description :: <string>)
    concatenate(generic-function-modifiers(project, function),
                definition-kind(function))
end method definition-type-description;

define method definition-type-description
    (project :: <project-object>, class :: <class-object>)
 => (description :: <string>)
    concatenate(class-modifiers(project, class),
                definition-kind(class))
end method definition-type-description;

define method class-modifiers
    (project :: <project-object>, class :: <class-object>)
  => (modifiers :: <string>)
  let modifiers = definition-modifiers(project, class);
  let open?     = member?(#"open", modifiers);
  let abstract? = member?(#"abstract", modifiers);
  let primary?  = member?(#"primary", modifiers);
  let instantiable? = #f;        // How can we get this?
  with-output-to-string (stream)
    open?         & write(stream, "Open ");
    abstract?     & write(stream, "Abstract ");
    primary?      & write(stream, "Primary ");
    instantiable? & write(stream, "Instantiable ");
  end
end method class-modifiers;

define method generic-function-modifiers
    (project :: <project-object>, function :: <generic-function-object>)
  => (modifiers :: <string>)
  let modifiers = definition-modifiers(project, function);
  let open?     = member?(#"open", modifiers);
  let dynamic?  = member?(#"dynamic", modifiers);
  with-output-to-string (stream)
    open?         & write(stream, "Open ");
    dynamic?      & write(stream, "Dynamic ");
  end
end method generic-function-modifiers;
