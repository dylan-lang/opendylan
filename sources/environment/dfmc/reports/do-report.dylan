Module:    dfmc-environment-reports
Synopsis:  DFMC report generation
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Report information about a DFMC context/library's contents

// Report context information
define method do-report (report :: <report>) => ()
  format-to-report(report, "Report: %s\n", report.report-name);
  report-indent-one (report)
    format-to-report(report,
                     "Flags: inherited? = %=, internal? = %=\n",
                     report.report-inherited?,
                     report.report-internal?);
  end;
  format-to-report(report, "\n");
//format-to-report(report, "// Context: %=\n\n", report.report-context);
  do-report-library(report);
  format-to-report(report, "--- End of Report ---\n");
end method do-report;

// Report library information
define method do-report-library (report :: <report>) => ()
  let library = project-library-definition(report.report-context);
  if (~library)
    format-to-report(report, "--library definition unavailable--\n");
  else
    format-to-report(report,
                     "Library: %s\n",
                     as-string(library.library-definition-name));
    report-indent-one (report)
//    format-to-report(report, "// Definition: %=\n", library);
      do-report-source-form(report, library);
    end report-indent-one;
  end if;
end method do-report-library;

// Report module information
define method do-report-module
    (report :: <report>, module-name :: <symbol>, export-kind :: <symbol>) => ()
  format-to-report(report, "Module: %s\n", as-string(module-name));
  report-indent-one (report)
    format-to-report(report, "Export Kind: %s\n", as-string(export-kind));
    let context = report.report-context;
    let library = project-library-definition(context);
    let (module, module-kind)
      = find-module-definition(context, library, module-name);
    if (~module)
      format-to-report(report, "--module definition unavailable--\n", module-name);
    else
//    format-to-report(report, "// Definition: %=\n", module);
      format-to-report(report, "Definition Kind: %s\n", as-string(module-kind));
      do-report-source-form(report, module);
    end if;
  end report-indent-one;
end method do-report-module;


// Report type expression information (without line breaks)
define generic do-report-type (report :: <report>, type :: <type-expression>) => ();

define method do-report-type (report :: <report>, type == #f) => ()
  format-to-report(report, "--type unspecified--");
end do-report-type;

define method do-report-type (report :: <report>, type == #t) => ()
  format-to-report(report, "--complex type expression--");
end do-report-type;

define method do-report-type (report :: <report>, type :: <type-expression>) => ()
  //---*** cpage: Apparently, some type-expressions are not <variable> as the
  //              documentation says it is. Find out what to do with them.
  let type-name
    = block ()
        variable-name(type)
      exception (err :: <error>)
        debug-message("Exception \"%=\" in variable-name(%=)", err, type);
        #"#error#"
      end block;
  format-to-report(report, "%s", as-string(type-name));
end do-report-type;


// Report variable information
define method do-report-variable
    (report :: <report>, variable :: <variable>, export-kind :: <symbol>) => ()
//format-to-report(report, "// Variable: %=\n", variable);
  let (var-name, var-module) = variable-name(variable);
  format-to-report(report, "Variable: %s\n", as-string(var-name));
  report-indent-one (report)
    format-to-report(report, "Export Kind: %s\n", as-string(export-kind));
    let home = variable-home(report.report-context, variable);
    let (home-name, home-module) = variable-name(home);
    //---*** cpage: Testing.
/**/ assert(var-module ~= #"functional-extensions-internals", "module functional-extensions-internals");
    unless (same-variable-name?(home, variable))
//    format-to-report(report, "// Home variable: %=\n", home);
      format-to-report(report, "Home variable: %s\n", as-string(home-name));
      report-indent-one (report)
        format-to-report(report, "Module: %s\n", as-string(home-module));
      end;
    end unless;
    let context = report.report-context;
    let all-definitions = variable-all-definitions(context, variable);
//  format-to-report(report, "// All definitions: %=\n", all-definitions);
    let definition-count = size(all-definitions);
    if (definition-count > 1)
      format-to-report(report, "Definition Count: %d\n", definition-count);
    end;
    let definition = variable-active-definition(context, variable);
    if (definition)
      //---*** cpage: source-form-variable-type() sometimes generates a bus error.
      let variable-type
        = block ()
            source-form-variable-type(definition, variable)
          exception (err :: <error>)
            debug-message("Exception \"%=\" in source-form-variable-type(%=, %=)",
                          err, definition, variable);
            #f
          end block;
      format-to-report(report, "Type: ");
      do-report-type(report, variable-type);
      format-to-report(report, "\n");
    end if;
    do-report-variable-definition(report, definition);
  end report-indent-one;
end method do-report-variable;

// Report variable definition information
define method do-report-variable-definition
    (report :: <report>,
     definition :: type-union(<source-form>, <boolean>))
 => ()
    format-to-report(report, "Definition: ");
//    format-to-report(report, "// Definition %=\n", definition);
  if (definition = #t)
    format-to-report(report, "--implicit generic function--\n");
  elseif (~definition)
//---*** cpage: When do we need to call source-form-parent-form()?
/*  let parent-form = source-form-parent-form(definition);
    if (~parent-form)
*/
      format-to-report(report, "--variable definition unavailable--\n");
/*  else
      format-to-report(report, "--child definition--\n");
      format-to-report(report, "Parent Definition:\n");
      report-indent-one (report)
        do-report-definition(report, parent-form);
     end report-indent-one;
    end if;
*/
  else
    let define-word = source-form-define-word(definition);
    let adjectives = source-form-adjectives(definition);
    for (adjective in adjectives)
      format-to-report(report, "%s ", as-string(adjective));
    end;
    if (~define-word)
      format-to-report(report, "--no define word--\n");
    else
      format-to-report(report, "%s\n", as-string(define-word));
    end if;
    unless (report.report-depth < $depth-definitions)
      report-indent-one (report)
        do-report-source-form(report, definition);
      end report-indent-one;
    end unless;
  end if;
end method do-report-variable-definition;


/// Report source code

define method do-report-source-form-source
    (report :: <report>, source-form :: <source-form>) => ()
  format-to-report(report, "Source Location: ");
  let source-location = source-form-location(source-form);
  if (~source-location)
    format-to-report(report, "--no source code available--\n");
  else
    let source-record = source-location.source-location-source-record;
    let start-line = source-location.source-location-start-line;
    let end-line = source-location.source-location-end-line;
    format-to-report(report,
                     "\"%s\" lines %d-%d\n",
                     source-record.source-record-name,
                     start-line,
                     end-line);
    if (report.report-source?)
      format-to-report(report, "Source Code:\n");
      report-indent-one (report)
        let source = as(<string>, copy-source-location-contents(source-location));
        let source-size = size(source);
        //---*** cpage: I bet we could change the following while loops to some
        //              more handy form of for loop.
        let line-start = 0;
        let line-number = 1;
        while (line-start < source-size)
          let line-end = line-start;
          block (return)
            while (line-end < source-size)
              if (source[line-end] = '\n')
                return();
              end;
              line-end := line-end + 1;
            end;
          end block;
          format-to-report(report, "|");
          if (line-start < line-end - 1)
            let line = copy-sequence(source, start: line-start, end: line-end);
            format-to-report(report, "%s\n", line);
          else
            format-to-report(report, "\n");
          end;
          line-start := line-end + 1;
          line-number := line-number + 1;
        end while;
      end report-indent-one;
    end if;
  end if;
end method do-report-source-form-source;


/// Report source form information

define generic do-report-source-form
    (report :: <report>, source-form :: <source-form>) => ();

// Default handler for source-forms
//---*** cpage: I'm not sure whether this should ever happen.
define method do-report-source-form
    (report :: <report>, source-form :: <source-form>) => ()
  debug-message("Error: Unexpected source form class encountered %=", source-form);
  do-report-source-form-source(report, source-form);
end method do-report-source-form;

// Report library definition information
define method do-report-source-form /**/
    (report :: <report>, definition :: <library-definition>) => ()
  format-to-report(report, "Library definition:\n");
  report-indent-one (report)
    format-to-report(report, "Used Libraries:\n");
    let used-libraries = library-definition-used-libraries(definition);
    report-indent-one (report)
      for (used-library in used-libraries)
        format-to-report(report, "%s\n", as-string(used-library));
      end;
    end report-indent-one;
    do-report-source-form-source(report, definition);
  end report-indent-one;
  let context = report.report-context;
  if (report.report-depth < $depth-modules)
    let module-count = 0;
    local method do-module (module-name :: <symbol>, export-kind :: <symbol>) => ()
            module-count := module-count + 1;
          end method;
    do-library-modules(context,
                       definition,
                       do-module,
                       inherited?: report.report-inherited?,
                       internal?: report.report-internal?);
    report-indent-one (report)
      format-to-report(report, "Modules: %d\n", module-count);
    end;
    format-to-report(report, "\n");
  else
    format-to-report(report, "\n");
    local method do-module (module-name :: <symbol>, export-kind :: <symbol>) => ()
            do-report-module(report, module-name, export-kind);
            format-to-report(report, "\n");
          end method;
    do-library-modules(context,
                       definition,
                       do-module,
                       inherited?: report.report-inherited?,
                       internal?: report.report-internal?);
  end if;
end method do-report-source-form;

// Report module definition information
define method do-report-source-form
    (report :: <report>, definition :: <module-definition>) => ()
  format-to-report(report, "Module definition:\n");
  report-indent-one (report)
    let name = module-definition-name(definition);
    format-to-report(report, "Name: %s\n", as-string(name));
    format-to-report(report, "Used Modules:\n");
    let used-modules = module-definition-used-modules(definition);
    report-indent-one (report)
      for (used-module in used-modules)
        format-to-report(report, "%s\n", as-string(used-module));
      end;
    end report-indent-one;
    do-report-source-form-source(report, definition);
    if (report.report-depth < $depth-variables)
      let variable-count = 0;
      local method do-variable
                (variable :: <variable>, export-kind :: <symbol>) => ()
              variable-count := variable-count + 1;
            end method;
      do-module-variables(report.report-context,
                          definition,
                          do-variable,
                          inherited?: report.report-inherited?,
                          internal?: report.report-internal?);
      format-to-report(report, "Variables: %d\n", variable-count);
    else
      local method do-variable
                (variable :: <variable>, export-kind :: <symbol>) => ()
              do-report-variable(report, variable, export-kind);
            end method;
      do-module-variables(report.report-context,
                          definition,
                          do-variable,
                          inherited?: report.report-inherited?,
                          internal?: report.report-internal?);
    end if;
  end report-indent-one;
end method do-report-source-form;

// Report macro definition information
define method do-report-source-form
    (report :: <report>, definition :: <macro-definition>) => ()
  format-to-report(report, "Macro definition:\n");
  report-indent-one (report)
    let (word, kind) = macro-definition-word(definition);
    format-to-report(report, "Word: ");
    if (~word)
      format-to-report(report, "--definition word unavailable--\n");
    else
      format-to-report(report, "%s\n", as-string(word));
    end if;
    format-to-report(report, "Kind: %s\n", as-string(kind));
    do-report-source-form-source(report, definition);
  end report-indent-one;
end method do-report-source-form;

// Report functional definition information
define method do-report-source-form
    (report :: <report>, definition :: <functional-definition>) => ()
  let (req-vars, rest-var, next-var, key-vars, value-vars, rest-value-var)
    = functional-parameters(definition);
  let (req-types, rest-type, next-type, key-types, value-types, rest-value-type)
    = functional-parameter-types(definition);
  let (keys, all-keys?) = functional-keys(definition);
  format-to-report(report, "Parameters: ");
  let indent-param? = #f;
  local method do-report-param
            (variable :: <variable>, type :: <type-expression>,
             #key key: = #f) //---*** cpage: Change to false-or(<symbol>) when
                             //              bug fixed; see below.
         => ()
          if (indent-param?)
            format-to-report(report, "          ");
            //                       '#all-keys '
          else
            indent-param? := #t;
          end if;
          let var-name = variable-name(variable);
          if (key)
            //---*** cpage: Keywords are currently not instances of <symbol>.
            //              Remove this when the bug is fixed in dfmc-browser-support.
            let key-name = keyword-name(key);
            if (key-name ~= var-name)
              format-to-report(report, "%s: ", as-string(key-name));
            end if;
          end if;
          format-to-report(report, "%s", as-string(var-name));
          if (type)
            format-to-report(report, " :: ");
            do-report-type(report, type);
          end if;
          format-to-report(report, "\n");
        end method;
  report-indent-one (report)
    if (req-vars & size(req-vars) > 0)
      for (var in req-vars,
           type in req-types)
        do-report-param(var, type);
      end;
    else
      format-to-report(report, "--\n");
    end if;
    if (next-var)
      format-to-report(report, "   #next  ");
      indent-param? := #f;
      do-report-param(next-var, next-type);
    end if;
    if (rest-var)
      format-to-report(report, "   #rest  ");
      indent-param? := #f;
      do-report-param(rest-var, rest-type);
    end if;
    if (key-vars & size(key-vars) > 0)
      format-to-report(report, "    #key  ");
      indent-param? := #f;
      for (var in key-vars,
           type in key-types,
           key in keys)
        do-report-param(var, type, key: key);
      end;
    end if;
    if (all-keys?)
      format-to-report(report, "#all-keys\n");
    end;
  end report-indent-one;
  format-to-report(report, "Values:     ");
  indent-param? := #f;
  report-indent-one (report)
    if (value-vars & size(value-vars) > 0)
      for (var in value-vars,
           type in value-types)
        do-report-param(var, type);
      end;
    else
      format-to-report(report, "--\n");
    end if;
    if (rest-value-var)
      format-to-report(report, "   #rest  ");
      indent-param? := #f;
      do-report-param(rest-value-var, rest-value-type);
    end if;
  end report-indent-one;
  do-report-source-form-source(report, definition);
end method do-report-source-form;

// Report method definition information
define method do-report-source-form
    (report :: <report>, definition :: <method-definition>) => ()
  format-to-report(report, "Method definition:\n");
  report-indent-one (report)
    next-method();
  end;
end method do-report-source-form;

// Report generic function definition information
define method do-report-source-form
    (report :: <report>, definition :: <generic-definition>) => ()
  format-to-report(report, "Generic function definition:\n");
  report-indent-one (report)
    let options = generic-definition-options(definition);
    if (size(options) > 0)
      report-indent-one (report)
        format-to-report(report, "Options:");
        for (option in options)
          format-to-report(report, " %s", as-string(option));
        end;
        format-to-report(report, "\n");
      end report-indent-one;
    end if;
    next-method();
    let variables = source-form-defined-variables(definition);
    if (size(variables) > 1)
      format-to-report(report,
                       "// *** More than one variable defined: %= ***\n",
                       variables);
    end;
    let variable = variables[0];
    let method-definitions
      = variable-active-method-definitions(report.report-context, variable);
    if (size(method-definitions) = 0)
      format-to-report(report, "// *** No method definitions ***\n");
    else
      for (method-definition in method-definitions)
        do-report-source-form(report, method-definition);
      end;
    end if;
  end report-indent-one;
end method do-report-source-form;

// Report function definition information
define method do-report-source-form
    (report :: <report>, definition :: <function-definition>) => ()
  format-to-report(report, "Function definition:\n");
  report-indent-one (report)
    next-method();
  end;
end method do-report-source-form;

// Report constant method definition information
define method do-report-source-form
    (report :: <report>, definition :: <constant-method-definition>) => ()
  format-to-report(report, "Constant method definition:\n");
  report-indent-one (report)
    next-method();
  end;
end method do-report-source-form;

// Report constant definition information
define method do-report-source-form
    (report :: <report>, definition :: <constant-definition>) => ()
  format-to-report(report, "Constant definition:\n");
  report-indent-one (report)
    do-report-source-form-source(report, definition);
  end;
end method do-report-source-form;

// Report variable definition information
define method do-report-source-form
    (report :: <report>, definition :: <variable-definition>) => ()
  format-to-report(report, "Variable definition:\n");
  report-indent-one (report)
    do-report-source-form-source(report, definition);
  end;
end method do-report-source-form;

// Report sealed domain definition information
define method do-report-source-form
    (report :: <report>, definition :: <domain-definition>) => ()
  format-to-report(report, "Domain definition:\n");
  report-indent-one (report)
    let types = domain-definition-domain-types(definition);
    format-to-report(report, "Types: ");
    format-to-report(report, "%=", types);
    format-to-report(report, "\n");
    do-report-source-form-source(report, definition);
  end report-indent-one;
end method do-report-source-form;

// Report init-keyword information
define function do-report-init-keyword
    (report :: <report>,
     init-keyword /* :: <symbol> */, //---*** cpage: Enable this when bug fixed.
     required? :: <boolean>)
 => ()
  //---*** cpage: Keywords are currently not instances of <symbol>.
  //              Remove this when the bug is fixed in dfmc-browser-support.
  let key-name = keyword-name(init-keyword);
  format-to-report(report,
                   "%s: %s\n",
                   if (required?)
                     "Required-init-keyword"
                   else
                     "Init-keyword"
                   end,
                   as-string(key-name));
end function do-report-init-keyword;

// Report class definition information
define method do-report-source-form
    (report :: <report>, definition :: <class-definition>) => ()
  format-to-report(report, "Class definition:\n");
  report-indent-one (report)
    let superclasses = class-definition-superclass-types(definition);
    format-to-report(report, "Superclasses:\n");
    report-indent-one (report)
      for (superclass in superclasses)
        do-report-type(report, superclass);
        format-to-report(report, "\n");
      end;
    end report-indent-one;
    let subclass-definitions
      = class-direct-subclass-definitions(report.report-context, definition);
    if (size(subclass-definitions) > 0)
      format-to-report(report, "Subclasses:\n");
      report-indent-one (report)
        for (subclass-definition in subclass-definitions)
          let variables = source-form-defined-variables(subclass-definition);
          if (size(variables) > 1)
            format-to-report(report,
                             "// *** More than one variable defined: %= ***\n",
                             variables);
          end;
          let subclass-name = variable-name(variables[0]);
          format-to-report(report, "%s\n", as-string(subclass-name));
        end for;
      end report-indent-one;
    end if;
    let init-keywords = class-definition-init-keywords(definition);
    report-indent-one (report)
      for (init-keyword in init-keywords)
        let required?
          = class-definition-init-keyword-required?(definition, init-keyword);
        do-report-init-keyword(report, init-keyword, required?);
        report-indent-one (report)
          let init-kind
            = class-definition-init-keyword-init-kind(definition, init-keyword);
          format-to-report(report, "Init Kind: %=\n", as-string(init-kind));
          let type = class-definition-init-keyword-type(definition, init-keyword);
          format-to-report(report, "Type: ");
          do-report-type(report, type);
          format-to-report(report, "\n");
        end report-indent-one;
      end for;
    end report-indent-one;
    let class-slots = class-definition-slot-definitions(definition);
    if (class-slots)
      for (slot-definition in class-slots)
        do-report-source-form(report, slot-definition);
      end;
    end if;
    do-report-source-form-source(report, definition);
  end report-indent-one;
end method do-report-source-form;

// Report macro invocation information
define method do-report-source-form
    (report :: <report>, macro-form :: <macro-form>) => ()
  format-to-report(report, "Macro form:\n");
  report-indent-one (report)
    let expanded-forms = macro-form-expanded-forms(macro-form);
    for (source-form in expanded-forms)
      do-report-source-form(report, source-form);
    end;
    do-report-source-form-source(report, macro-form);
  end report-indent-one;
end method do-report-source-form;

// Report initialization form information
define method do-report-source-form
    (report :: <report>, init-form :: <init-form>) => ()
  format-to-report(report, "Init form:\n");
  report-indent-one (report)
    do-report-source-form-source(report, init-form);
  end;
end method do-report-source-form;

// Report slot definition information
define method do-report-source-form
    (report :: <report>, definition :: <slot-definition>) => ()
  format-to-report(report, "Slot definition:\n");
  report-indent-one (report)
    let slot-class = slot-definition-class-definition(definition);
    format-to-report(report, "Class: ");
    if (~slot-class)
      format-to-report(report, "--class definition unavailable--\n");
    else
      format-to-report(report, "%s\n", as-string(slot-class.definition-name));
    end;
    let allocation = slot-definition-allocation(definition);
    allocation
      & format-to-report(report, "Allocation: %s\n", as-string(allocation));
    let init-kind = slot-definition-init-kind(definition);
    init-kind
      & format-to-report(report, "Init Kind: %s\n", as-string(init-kind));
    let (init-keyword, required?) = slot-definition-keyword(definition);
    init-keyword
      & do-report-init-keyword(report, init-keyword, required?);
    let getter-method = block () //---*** cpage: Virtual slots raise an exception.
                          slot-definition-getter(definition)
                        exception (err :: <error>)
                          debug-message("Exception \"%=\" in slot-definition-getter(%=)",
                                        err,
                                        definition);
                          #f
                        end block;
    format-to-report(report, "Getter: ");
    if (~getter-method)
      format-to-report(report, "--getter method unavailable--\n");
    else
      format-to-report(report, "%s\n", as-string(getter-method.definition-name));
    end;
    let setter-method = slot-definition-setter(definition);
    format-to-report(report, "Setter: ");
    if (~setter-method)
      format-to-report(report, "--setter method unavailable--\n");
    else
      format-to-report(report, "%s\n", as-string(setter-method.definition-name));
    end;
    let slot-type = slot-definition-type(definition);
    format-to-report(report, "Type: ");
    do-report-type(report, slot-type);
    format-to-report(report, "\n");
//  do-report-source-form-source(report, definition);
  end report-indent-one;
end method do-report-source-form;
