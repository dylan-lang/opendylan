Module:    environment-reports
Author:    Peter S. Housel
Synopsis:  Emacs TAGS file report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TAGS file generation

define class <tags-report> (<multi-file-report>, <project-report>)
end class;

install-report(#"tags", "Project TAGS file", <tags-report>);


/// Report protocols

define constant $newline-code = as(<integer>, '\n');

define method write-report-as
    (stream :: <stream>, report :: <tags-report>, _format == #"text")
 => ()
  let project = report.report-project;

  let record-definitions = make(<object-table>);

  local
    method collect-library-definitions
        (library :: <library-object>) => ()
      do-namespace-names(collect-module-definitions, project, library,
                         imported?: #f);
    end method,
    method collect-module-definitions
        (module-name :: <module-name-object>) => ()
      let module = name-value(project, module-name);
      do-module-definitions(collect-definition, project, module);
    end method,
    method collect-definition
        (definition :: <definition-object>) => ()
      let location = environment-object-source-location(project, definition);
      let record = location.source-location-source-record;
      let definitions
        = element(record-definitions, record, default: #f)
        | (record-definitions[record] := make(<stretchy-object-vector>));
      add!(definitions, definition);
    end,
    method test-definitions
        (a :: <environment-object>, b :: <environment-object>)
     => (well? :: <boolean>)
      let a-location = environment-object-source-location(project, a);
      let b-location = environment-object-source-location(project, b);
      a-location.source-location-start-line < b-location.source-location-start-line
    end method;
  if (project.project-library)
    collect-library-definitions(project.project-library);
    do-project-used-libraries(collect-library-definitions, project, project);
  end if;

  for (definitions keyed-by record in record-definitions)
    let sr-location = record.source-record-location;
    let sr-start-line = source-record-start-line(record);
    let sr-char-offset = source-char-offset(record);
    block ()
      let text = source-record-contents(record);
      let text-line :: <integer> = 1;
      let text-index :: <integer> = 0;
      let regularsec
        = with-output-to-string (s)
            for (definition in sort(definitions, test: test-definitions))
              let location
                = environment-object-source-location(project, definition);
              let start-offset = source-location-start-offset(location);
              let location-line = start-offset.source-offset-line;

              let (extracted, line-start-index)
                = extract-line(text, text-index, text-line, location-line);
              text-line := location-line;
              text-index := line-start-index;

              let binding-name
                = environment-object-home-name(project, definition);
              let name
                = environment-object-primitive-name(project, binding-name);

              format(s, "%s\<7F>%s\<01>%d,%d\n", extracted, name,
                     location-line + sr-start-line,
                     line-start-index + sr-char-offset);
            end for;
          end;
      unless (empty?(regularsec))
        format(stream, "\f\n%s,%d\n",
               locator-as-string(<string>, sr-location), regularsec.size);
        write(stream, regularsec);
      end;
    exception (<source-record-missing>)
      // Ignore
    end;
  end for;
end method write-report-as;

define function extract-line
    (text :: <sequence>, text-index :: <integer>, text-line :: <integer>,
     location-line :: <integer>)
 => (extracted :: <string>, line-start-index :: <integer>);
  iterate loop (line-start-index :: <integer> = text-index,
                current-index :: <integer> = text-index,
                current-line :: <integer> = text-line)
    if (current-index = text.size)
      error("Tried to locate line %d but last line was %d",
            location-line, current-line)
    elseif (text[current-index] = $newline-code)
      if (current-line = location-line)
        let extracted
          = copy-sequence(text, start: line-start-index, end: current-index);
        values(as(<string>, extracted), line-start-index)
      else
        loop(current-index + 1, current-index + 1, current-line + 1)
      end if
    else
      loop(line-start-index, current-index + 1, current-line)
    end if
  end iterate
end function;
