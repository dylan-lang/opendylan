Module:    environment-reports
Author:    Andy Armstrong, Jason Trenouth
Synopsis:  XML Library report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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
     keyword :: <symbol>, type :: false-or(<environment-object>),
     required? :: <boolean>)
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

define method write-generic-function-method
    (stream :: <xml-report-stream>, report :: <module-report>,
     gf :: <generic-function-object>, m :: <method-object>)
 => ()
end method write-generic-function-method;

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

