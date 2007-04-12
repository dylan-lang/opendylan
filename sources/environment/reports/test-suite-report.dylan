Module:    environment-reports
Author:    Andy Armstrong
Synopsis:  Test suite report generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test suite skeleton generation

define class <test-suite-report> (<multi-file-report>, <project-report>)
end class <test-suite-report>;

//---*** Ultimately this should be available in all editions...
install-report(#"test-suite", "Project test suite skeleton", 
	       <test-suite-report>,
	       edition: #"internal");


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
  format(stream, "define library-spec %s ()\n", library-name);
  for (name :: <module-name-object> in module-names)
    format(stream, "  module %s;\n",
	   as-lowercase(environment-object-primitive-name(project, name)))
  end;
  format(stream, "end library-spec %s;\n", library-name);
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
    for (name :: <binding-name-object> in names)
      let value = name-value(project, name);
      let class-name
	= select (value by instance?)
	    <class-object>    => "class";
	    <constant-object> => "constant";
	    <function-object> => "function";
	    <macro-object>    => "macro";
	    <variable-object> => "variable";
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
    end
  end
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
  format(stream, ");\n")
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, class :: <class-object>)
 => ()
  let project = report.report-project;
  let name = environment-object-primitive-name(project, name);
  let separator = "";
  format(stream, "  class %s (", name);
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
  //---*** Flesh this out, handle generics etc...
  format(stream, "  function %s;\n",
	 environment-object-primitive-name(project, name))
end method write-binding-spec;

define method write-binding-spec
    (stream :: <stream>, report :: <test-suite-report>, 
     name :: <binding-name-object>, macro-object :: <macro-object>)
 => ()
  let project = report.report-project;
  format(stream, "  macro-test %s-test;\n",
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
