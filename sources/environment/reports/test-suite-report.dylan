Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  Test suite report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test suite skeleton generation

define class <test-suite-report> (<multi-file-report>, <project-report>)
end class <test-suite-report>;

//---*** Ultimately this should be available in all editions...
install-report(#"test-suite", "Project test suite skeleton", 
	       <test-suite-report>);


/// Report protocols

define method write-report-as
    (stream :: <stream>, report :: <test-suite-report>, _format == #"text")
 => ()
  let project = report.report-project;
  let library = project.project-library;
  let library-name = environment-object-primitive-name(project, library);
  let module-names = namespace-exported-names(project, library);
  format(stream, "Module: %s-test-suite\n", library-name);
  format(stream, "\n");
  for (name :: <module-name-object> in module-names)
    let module = name-value(project, name);
    let module-name = environment-object-primitive-name(project, name);
    let names = module-names-to-document(project, module);
    format(stream, "\n");
    format(stream, "define module-spec %s ()\n", module-name);
    for (name :: <binding-name-object> in names)
      let value = name-value(project, name);
      write-binding-spec(stream, report, name, value)
    end;
    format(stream, "end module-spec %s;\n", module-name)
  end;
  for (name :: <module-name-object> in module-names)
    let module = name-value(project, name);
    let module-name = environment-object-primitive-name(project, name);
    let names = module-names-to-document(project, module);
    format(stream, "\n");
    format(stream, "// Module: %s\n", module-name);
    for (name :: <binding-name-object> in names)
      let value = name-value(project, name);
      let class-name
	= select (value by instance?)
	    <class-object>    => "class";
	    <constant-object> => "constant";
	    <function-object> => "function";
	    <macro-object>    => "macro";
	    <variable-object> => "variable";
            otherwise         => "constant"
	  end;
      let print-name = environment-object-primitive-name(project, name);
      let test-suffix
	= if (instance?(value, <macro-object>)) "-test" else "" end;
      format(stream, "\n");
      format(stream, "define %s %s-test %s%s ()\n",
	     module-name, class-name, print-name, test-suffix);
      format(stream, "  //---*** Fill this in...\n");
      format(stream, "end %s-test %s%s;\n",
	     class-name, print-name, test-suffix)
    end;
  end;
  format(stream, "\n");
  format(stream, "define library-spec %s ()\n", library-name);
  for (name :: <module-name-object> in module-names)
    format(stream, "  module %s;\n",
	   as-lowercase(environment-object-primitive-name(project, name)))
  end;
  format(stream, "end library-spec %s;\n", library-name);
end method write-report-as;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, variable :: <variable-object>)
 => ()
  let project = report.report-project;
  let name = environment-object-primitive-name(project, name);
  let type = variable-type(project, variable);
  format(stream, "  %s %s :: ", 
	 select (variable by instance?)
	   <constant-object> => "constant";
	   otherwise         => "variable";
	 end,
	 name);
  //---*** What to do with unexported types?
  print-environment-object-name
    (stream, project, type, qualify-names?: #f);
  format(stream, ";\n")
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, class :: <class-object>)
 => ()
  let project = report.report-project;
  let name = environment-object-primitive-name(project, name);
  let separator = "";

  format(stream, "  ");
  let modifiers = definition-modifiers(project, class);
  for (modifier in #[#"open", #"abstract", #"primary"])
    if (member?(modifier, modifiers))
      format(stream, "%s ", modifier);
    end if;
  end for;
  format(stream, "class %s (", name);
  for (superclass in class-direct-superclasses(project, class))
    format(stream, "%s", separator);
    //---*** What to do with unexported types?
    print-environment-object-name
      (stream, project, superclass, qualify-names?: #f);
    separator := ", "
  end;
  format(stream, ");\n")
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, function :: <function-object>)
 => ()
  let project = report.report-project;
  format(stream, "  function %s ",
	 environment-object-primitive-name(project, name));
  write-binding-spec-parameters(stream, project, function);
  format(stream, ";\n");
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, function :: <generic-function-object>)
 => ()
  let project = report.report-project;
  let modifiers = definition-modifiers(project, function);
  let open?     = member?(#"open", modifiers);
  if (open?)
    format(stream, "  open generic-function %s ",
           environment-object-primitive-name(project, name));
    write-binding-spec-parameters(stream, project, function);
    format(stream, ";\n");
  else
    let methods = generic-function-object-methods(project, function);
    if (methods.size = 1)
      next-method(stream, report, name, methods[0]);
    else
      next-method();
    end if;
  end if;
end method write-binding-spec;

define method write-binding-spec-parameters
    (stream :: <stream>, project :: <project-object>,
     function :: <function-object>)
 => ();
  let (required :: <parameters>,
       rest :: false-or(<parameter>),
       keys :: <optional-parameters>,
       all-keys? :: <boolean>,
       next :: false-or(<parameter>),
       values :: <parameters>,
       rest-value :: false-or(<parameter>))
    = function-parameters(project, function);
  format(stream, "(");
  for (parameter in required, first? = #t then #f)
    unless (first?)
      format(stream, ", ");
    end;
    print-environment-object-name
      (stream, project, parameter-type(parameter), qualify-names?: #f);
  finally
    if (rest)
      unless (first?)
        format(stream, ", ");
      end;
      format(stream, "#\"rest\"");
    end if;
  end for;

  if (~keys.empty? | all-keys?)
    if (~required.empty? | rest)
      format(stream, ", ");
    end if;

    format(stream, "#\"key\"");

    for (key in keys)
      if (key.parameter-keyword)
        format(stream, ", %=", key.parameter-keyword);
      else
        format(stream, ", #\"%s\"", key.parameter-name);
      end if;
    end for;

    if (all-keys?)
      format(stream, ", #\"all-keys\"");
    end if;
  end if;

  format(stream, ") => (");

  for (value in values, first? = #t then #f)
    unless (first?)
      format(stream, ", ");
    end;
    print-environment-object-name
      (stream, project, parameter-type(value), qualify-names?: #f);
  finally
    if (rest-value)
      unless (first?)
        format(stream, ", ");
      end;
      format(stream, "#\"rest\"");
    end if;
  end for;

  format(stream, ")");
end method;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, macro-object :: <macro-object>)
 => ()
  let project = report.report-project;
  format(stream, "  macro-test %s-test;\n",
	 environment-object-primitive-name(project, name))
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, macro-object :: <source-form-object>)
 => ()
  let project = report.report-project;
  format(stream, "  constant %s;\n",
	 environment-object-primitive-name(project, name))
end method write-binding-spec;

define method write-report-as
    (stream :: <stream>, report :: <test-suite-report>, _format == #"html")
 => ()
  error("Not yet implemented!")
end method write-report-as;

define method create-multi-file-report-as
    (report :: <test-suite-report>, directory :: <directory-locator>,
     _format == #"text")
 => (filename :: <file-locator>)
  report.report-directory := directory;
  let project = report.report-project;
  let library = project.project-library;
  let library-name = environment-object-primitive-name(project, library);
  error("Not yet implemented!")
end method create-multi-file-report-as;


/// Utilities

define method namespace-exported-names
    (project :: <project-object>, namespace :: <namespace-object>)
 => (names :: <stretchy-object-vector>)
  let names :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-namespace-names
    (method (name :: <name-object>)
       if (name-exported?(project, name))
	 add!(names, name)
       end
     end,
     project, namespace);
  names
end method namespace-exported-names;

define method module-names-to-document
    (project :: <project-object>, module :: <module-object>)
 => (names :: <stretchy-object-vector>)
  //---*** We should group these by class, if possible
  namespace-exported-names(project, module)
end method module-names-to-document;
