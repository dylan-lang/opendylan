Module:    tool-scepter
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

tool-register(#"OMG-IDL", ide-scepter-invoke);

define inline-only function origin-keyword? (arg)
  arg == #"Origin"
end function;

define sealed class <tool-scepter> (<scepter>)
  constant slot scepter-specification, required-init-keyword: specification:;
end class;

define sealed domain make (singleton(<tool-scepter>));
define sealed domain initialize (<tool-scepter>);

define method scepter-default-back-end-class (scepter :: <tool-scepter>)
 => (back-end-class :: subclass(<scepter-back-end>));
  scepter-back-end-class("dylan");
end method;

define method scepter-default-front-end-class (scepter :: <tool-scepter>)
 => (front-end-class :: subclass(<scepter-front-end>));
  scepter-front-end-class("file");
end method;

define method scepter-parse-options (scepter :: <tool-scepter>)
 => ()
  let specification = scepter.scepter-specification;
  for (value-list keyed-by symbol in specification)
    unless (origin-keyword?(symbol))
      let arg = as(<string>, symbol);
      let value = keyword-file-element-value(first(value-list));
      debug-message("Tool Scepter: Option (%s, %s)", arg, value);
      let (option, rest) = scepter-find-option(scepter, arg);
      if (option)
        if (scepter-option-value?(option))
          scepter-option-add-value(value, option);
	  debug-message("Tool Scepter: Option value set (%s, %s)", arg, value);
        elseif (case-insensitive-equal(value, "yes"))
          scepter-option-supplied(option);
	  debug-message("Tool Scepter: Option supplied (%s, %s)", arg, value);
        end if;
      else
        error(make(<illegal-option>, source: #f, option-name: arg));
      end if;  
    end unless;
  end for;
end method;

define method scepter-report-condition (scepter :: <tool-scepter>, condition :: <condition>)
 => ()
  tool-error("%s: Unrecoverable internal error %=", format-arguments: list(scepter.scepter-program-name, condition));
end method;

define method scepter-report-condition (scepter :: <tool-scepter>, condition :: <idl-condition>)
 => ()
  let message = with-output-to-string (stream)
                  idl-condition-title(stream, condition);
                  idl-condition-body(stream, condition);
                end;
  let source = idl-condition-source(condition);
  if (source)
    let filename = file-source-file(source);
    let line-number = file-source-line(source);
    tool-warning(message,
		 serious?: idl-condition-serious?(condition),
		 recoverable?: idl-condition-recoverable?(condition),
		 file: filename,
		 line: line-number);
  else
    tool-warning(message,
		 serious?: idl-condition-serious?(condition),
		 recoverable?: idl-condition-recoverable?(condition));
  end if;
end method;

define method ide-scepter-invoke (spec-file :: <file-locator>, project-file :: false-or(<file-locator>), last-run :: false-or(<date>),
				  #key clean-build? :: <boolean> = #f)
 => (success? :: <boolean>, modified-projects :: <sequence> /* of: <file-locator> */)
  debug-message("Tool Scepter invoked. Specification: %=, project: %=, last: %=, clean: %=",
  		as(<string>, spec-file),
		project-file & as(<string>, project-file),
		last-run & date-as-string(last-run),
		clean-build?);

  let specification = read-spec-file(spec-file);
  let spec-file-modification-date = file-property(spec-file, #"modification-date");
  if (last-run & spec-file-modification-date > last-run)
    debug-message("Tool Scepter: Spec file has changed");
    clean-build? := #t;
  end if;

  let scepter = make(<tool-scepter>,
		     directory: locator-directory(spec-file),
		     last-run: last-run,
		     specification: specification);
  scepter.scepter-force-build? := clean-build?;
  let scepter-results = scepter-invoke(scepter);
  debug-message("Tool Scepter: Scepter invoked %s, %s",
                scepter.scepter-back-ends,
                scepter.scepter-sources);

  let success? = #t;
  let modified-projects = #();
  if (instance?(scepter-results, <sequence>))
    for (source-result in scepter-results)
      if (source-result)
	for (back-end-result in source-result)
	  if (scepter-back-end-success?(back-end-result))
	    let projects = scepter-back-end-dylan-subprojects(back-end-result);
	    for (project in projects)
	      add-subproject(project-file, project);
	    end for;
	    modified-projects := concatenate!(modified-projects, scepter-back-end-modified-dylan-projects(back-end-result));
	  else
	    success? := #f;
	  end if;
	end for;
      else
	success? := #f;
      end if;
    end for;
  else
    success? := #f;
  end if;
  if (success?)
    debug-message("Tool Scepter: Scepter succeeded");
  else
    debug-message("Tool Scepter: Scepter failed");
  end if;
  values(success?, modified-projects);
end method;

define inline-only function read-spec-file (file :: <file-locator>)
 => (specification)
  read-keyword-pair-file(file);
end function;

define function date-as-string (date :: false-or(<date>)) => (r :: <string>)
  if (date)
    date := date + make(<day/time-duration>, days: 0);	// Stupid way to copy
    date.date-time-zone-offset := local-time-zone-offset();
    let (year, month, day, hours, minutes) = decode-date(date);
    format-to-string("%d:%02d %d-%02d-%02d %s", 
		     hours, minutes, year, month, day, local-time-zone-name())
  else
    "<no date>"
  end if
end function date-as-string;

define function add-subproject (project-file :: <file-locator>, subproject :: <file-locator>)
 => ()
  let project = read-project-file(project-file);
  project.project-information-subprojects
    := add-new!(project.project-information-subprojects,
                relative-locator(subproject, project-file),
                test: \=);
  write-project-file(project-file, project);
end function;

