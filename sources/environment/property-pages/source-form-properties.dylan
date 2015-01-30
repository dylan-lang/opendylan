Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source form object properties

define sideways method frame-property-types
    (frame :: <environment-frame>,
     class :: subclass(<source-form-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"source", #"warnings", #"usage"))
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>,
     class :: subclass(<source-form-object>))
 => (type :: false-or(<symbol>))
  #"source"
end method frame-default-property-type;


/// Source form object General property types

define method general-property-types
    (object :: subclass(<source-form-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"defined-in", #"exported"))
end method general-property-types;


/// Source form properties

//---*** cpage: 1997.11.20 This function returns the name of a source record
//              including the file extension (e.g. "foo.dylan"), if it is a
//              file source record. We should probably put this somewhere it
//              can be shared.
define method source-record-file-name
    (record :: <source-record>) => (name :: <string>)
  source-record-name(record)
    | "[interactive]"
end method source-record-file-name;

define method source-record-file-name
    (record :: <file-source-record>) => (name :: <string>)
  locator-name(record.source-record-location)
end method source-record-file-name;


/// Warnings page

define settings <property-pages-settings> (<open-dylan-user-settings>)
  key-name "Property Pages";
end settings <property-pages-settings>;

define settings <warnings-page-settings> (<property-pages-settings>)
  key-name "Warnings Page";
  slot warnings-pane-ratio :: <integer>  = 3;
  slot message-pane-ratio :: <integer>   = 1;
end settings <warnings-page-settings>;

define constant $warnings-page-settings = make(<warnings-page-settings>);

define constant $warning-filters
  = #[#["All warnings",         #"warnings"],
      #["Serious warnings",     #"serious"],
      #["Non-serious warnings", #"non-serious"],
      #["Interactive warnings", #"interactive"]];

define constant $default-warning-filter = #"warnings";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>,
     class :: subclass(<environment-object>),
     type == #"warnings")
 => (label :: <string>, displayer :: <filtering-table-control-displayer>)
  let project = frame.ensure-frame-project;
  let message-pane
    = make(<text-editor>,
           read-only?: #t, tab-stop?: #t,
           text-style:     make(<text-style>, family: #"fix"),
           documentation: "Shows the full compiler warning message",
           scroll-bars:   #"vertical");
  let browsing-project? = class == <project-object>;
  let (headings, sort-orders, generators, widths)
    = case
        browsing-project? =>
          values(#["Source", "Library", "Owner", "Warning"],
                 #[#"source", #"library", #"owner", #"message"],
                 vector(curry(environment-object-source-location, project),
                        curry(environment-object-library, project),
                        curry(warning-owner, project),
                        identity),
                 #[150, 70, 100, 1000]);
        otherwise =>
          values(#["Source", "Owner", "Warning"],
                 #[#"source", #"owner", #"message"],
                 vector(curry(environment-object-source-location, project),
                        curry(warning-owner, project),
                        identity),
                 #[150, 130, 1000]);
      end;
  let displayer
    = make(<filtering-table-control-displayer>,
           extra-pane: message-pane,
           ratios: warnings-page-ratios(),
           ratios-changed-callback: note-warnings-displayer-ratios-changed,
           information-available?-function: browsing-project? & always(#t),
           headings: headings,
           widths: widths,
           sort-orders: sort-orders,
           sort-function: curry(frame-sort-source-form-warnings, frame),
           children-generator: curry(frame-source-form-warnings, frame),
           generators: generators,
           filter-types: $warning-filters,
           filter-type: $default-warning-filter,
           filter-function: curry(filter-source-form-warnings, frame),
           label-key:  curry(source-form-warnings-label-key, frame),
           value-changed-callback: method
                                       (displayer :: <filtering-table-control-displayer>,
                                        warnings :: <sequence>)
                                     let warning
                                       = ~empty?(warnings) & warnings[0];
                                     update-warning-message-pane
                                       (displayer, message-pane, project, warning)
                                   end,
           items-changed-callback: method (displayer :: <displayer-mixin>)
                                     let gadget
                                       = displayer.displayer-collection-gadget;
                                     let warnings = gadget-value(gadget);
                                     let warning
                                       = ~empty?(warnings) & warnings[0];
                                     update-warning-message-pane
                                       (displayer, message-pane, project, warning);
                                     update-status-bar-warning-count(displayer)
                                   end);
  values("Warnings", displayer)
end method make-frame-property-page-displayer;

define method frame-sort-source-form-warnings
    (frame :: <environment-frame>, warnings :: <sequence>,
     order :: <symbol>)
 => (warnings :: <sequence>)
  let project = frame.ensure-frame-project;
  let original-warnings = copy-sequence(warnings);
  local method warning-location<
            (warning1 :: <warning-object>,
             warning2 :: <warning-object>)
         => (less-than? :: <boolean>)
          let location1
            = environment-object-source-location(project, warning1);
          let location2
            = environment-object-source-location(project, warning2);
          if (location1 & location2)
            let record1 = location1.source-location-source-record;
            let record2 = location2.source-location-source-record;
            let name1   = source-record-file-name(record1);
            let name2   = source-record-file-name(record2);
            if (name1 = name2)
              let offset1 = location1.source-location-start-offset;
              let offset2 = location2.source-location-start-offset;
              let line1   = offset1.source-offset-line;
              let line2   = offset2.source-offset-line;
              if (line1 = line2)
                let column1 = offset1.source-offset-column;
                let column2 = offset2.source-offset-column;
                column1 < column2
              else
                line1 < line2
              end if
            else
              name1 < name2
            end if;
          else
            location1 ~= #f
          end
        end method warning-location<;
  let sorted-warnings
    = select (order)
        #"message" =>
          keyed-sort(warnings,
                     key: curry(compiler-warning-short-message, project));
        #"library" =>
          frame-sort-items(frame, warnings,
                           key: curry(environment-object-library, project));
        #"source" =>
          sort(warnings, test: warning-location<);
        #"owner" =>
          frame-sort-items
            (frame, warnings,
             key:       curry(warning-owner, project),
             label-key: curry(source-form-warnings-label-key, frame));
      end;
  assert(warnings = original-warnings,
         "Whoops, destructively modified compiler warnings!");
  sorted-warnings
end method frame-sort-source-form-warnings;

define method frame-source-form-warnings
    (frame :: <environment-frame>,
     object :: <source-form-object>)
 => (warnings :: <sequence>)
  let project = frame.ensure-frame-project;
  source-form-compiler-warnings(project, object)
end method frame-source-form-warnings;

define method frame-source-form-warnings
    (frame :: <environment-frame>,
     project :: <project-object>)
 => (warnings :: <sequence>)
  project-warnings(project)
end method frame-source-form-warnings;

define method filter-source-form-warnings
    (frame :: <environment-frame>, warnings :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let project = frame.ensure-frame-project;
  let no-filter? = empty?(substring-filter);
  local method object-matches-type-filter?
            (warning :: <warning-object>) => (matches? :: <boolean>)
          let location = environment-object-source-location(project, warning);
          let record = location & source-location-source-record(location);
          let interactive? = record & (source-record-name(record) == #f);
          let serious? = instance?(warning, <serious-compiler-warning-object>);
          select (type-filter)
            #"warnings"    => #t;
            #"serious"     => serious?;
            #"non-serious" => ~serious?;
            #"interactive" => interactive?;
            otherwise      => #f;
          end
        end method object-matches-type-filter?;
  local method object-matches-substring-filter?
            (warning :: <warning-object>) => (matches? :: <boolean>)
          no-filter?
            | begin
                //---*** This really needs to match against all the columns
                let message = compiler-warning-short-message(project, warning);
                subsequence-position(message, substring-filter) ~= #f
              end
        end method object-matches-substring-filter?;
  local method show-object?
            (warning :: <warning-object>) => (show? :: <boolean>)
          object-matches-type-filter?(warning)
            & object-matches-substring-filter?(warning)
        end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in warnings)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-source-form-warnings;

//--- Some warnings don't have definitions at all
define method source-form-warnings-label-key
    (frame :: <environment-frame>, object == #f)
 => (label :: <string>)
  ""
end method source-form-warnings-label-key;

define method source-form-warnings-label-key
    (frame :: <environment-frame>,
     object :: <environment-object>)
 => (label :: <string>)
  frame-object-unique-name(frame, object)
end method source-form-warnings-label-key;

define method source-form-warnings-label-key
    (frame :: <environment-frame>, object :: <method-object>)
 => (label :: <string>)
  frame-default-object-name(frame, object)
end method source-form-warnings-label-key;

define method source-form-warnings-label-key
    (frame :: <environment-frame>, library :: <library-object>)
 => (label :: <string>)
  frame-default-object-name(frame, library)
end method source-form-warnings-label-key;

define method source-form-warnings-label-key
    (frame :: <environment-frame>, location :: <source-location>)
 => (label :: <string>)
  ignore(frame);
  let stream = make(<string-stream>, direction: #"output");
  print-source-location(stream, location);
  as(<string>, stream-contents(stream))
end method source-form-warnings-label-key;

define method note-warnings-displayer-ratios-changed
    (displayer :: <table-control-displayer>) => ()
  let ratios = displayer.displayer-ratios;
  $warnings-page-settings.warnings-pane-ratio := ratios[0];
  $warnings-page-settings.message-pane-ratio := ratios[1];
end method note-warnings-displayer-ratios-changed;

define method warnings-page-ratios
    () => (ratios :: <sequence>)
  vector($warnings-page-settings.warnings-pane-ratio,
         $warnings-page-settings.message-pane-ratio)
end method warnings-page-ratios;

define method update-warning-message-pane
    (displayer :: <table-control-displayer>, pane :: <text-editor>,
     project :: <project-object>,
     warning :: false-or(<warning-object>))
 => ()
  let frame = sheet-frame(pane);
  let message
    = if (warning)
        environment-object-display-name
          (project, warning, #f,
           qualify-names?:   frame-qualify-names?(frame),
           full-message?:    #t)
      else
        ""
      end;
  gadget-text(pane) := message
end method update-warning-message-pane;

define function update-status-bar-warning-count
    (displayer :: <table-control-displayer>) => ()
  let frame = sheet-frame(displayer);
  let project = frame.ensure-frame-project;
  let gadget = displayer.displayer-collection-gadget;
  let warnings = gadget.gadget-items;
  let message
    = if (project.project-compiler-database)
        compilation-warning-count-message(project, warnings: warnings)
          | "No warnings"
      else
        project-not-built-message(project)
      end;
  frame-status-message(frame) := message
end function update-status-bar-warning-count;


/// Sources page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>,
     class :: subclass(<environment-object>),
     type == #"source")
 => (label :: <string>, displayer :: <displayer-mixin>)
  values("Source", make-code-viewer(project: frame.ensure-frame-project))
end method make-frame-property-page-displayer;


/// Usage page

define constant $usage-definition-filters
  = #[#["All definitions", #"definitions"],
      #["Classes",         #"classes"],
      #["Constants",       #"constants"],
      #["Domains",         #"domains"],
      #["Functions",       #"functions"],
      #["Libraries",       #"libraries"],
      #["Macros",          #"macros"],
      #["Methods",         #"methods"],
      #["Modules",         #"modules"],
      #["Variables",       #"variables"]];

define constant $default-usage-definition-filter = #"definitions";

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>,
     class :: subclass(<source-form-object>),
     type == #"usage")
 => (label :: <string>, displayer :: <tree-control-displayer>)
  let displayer
    = make(<filtering-tree-control-displayer>,
           element-label: "source form",
           children-predicate: curry(source-form-usage-children-predicate, frame),
           children-generator: curry(source-form-usage-children-generator, frame),
           filter-types: $usage-definition-filters,
           filter-type: $default-usage-definition-filter,
           filter-function: curry(filter-source-form-usage, frame),
           label-key: curry(source-form-usage-label-key, frame),
           icon-function: curry(source-form-usage-icon-function, frame));
  values("Usage", displayer)
end method make-frame-property-page-displayer;

define method source-form-usage-children-predicate
    (frame :: <environment-frame>,
     source-form :: <source-form-object>)
 => (has-children? :: <boolean>)
  let project = frame.ensure-frame-project;
  source-form-uses-definitions?(project, source-form)
    | source-form-has-clients?(project, source-form)
end method source-form-usage-children-predicate;

define method source-form-usage-children-generator
    (frame :: <environment-frame>,
     source-form :: <source-form-object>)
 => (users :: <sequence>)
  let project = frame.ensure-frame-project;
  let uses-definitions? = source-form-uses-definitions?(project, source-form);
  let has-clients? = source-form-has-clients?(project, source-form);
  concatenate(if (uses-definitions?)
                vector(vector(#"used-definitions", source-form))
              else
                #[]
              end,
              if (has-clients?)
                vector(vector(#"clients", source-form))
              else
                #[]
              end)
end method source-form-usage-children-generator;

define method filter-source-form-usage
    (frame :: <environment-frame>, contents :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (names :: <sequence>)
  let no-filter? = empty?(substring-filter);
  local method object-matches-type-filter?
            (object :: <environment-object>) => (matches? :: <boolean>)
          select (type-filter)
            #"definitions" => #t;
            #"classes"     => instance?(object, <class-object>);
            #"constants"   => instance?(object, <constant-object>);
            #"domains"     => instance?(object, <domain-object>);
            #"functions"   => instance?(object, <function-object>);
            #"libraries"   => instance?(object, <library-object>);
            #"macros"      => instance?(object, <macro-object>);
            #"methods"     => instance?(object, <method-object>);
            #"modules"     => instance?(object, <module-object>);
            #"variables"   => instance?(object, <variable-object>);
            otherwise      => #f;
          end
        end method object-matches-type-filter?;
  local method object-matches-substring-filter?
            (object :: <environment-object>) => (matches? :: <boolean>)
          no-filter?
            | begin
                let label = definition-label-key(frame, object);
                subsequence-position(label, substring-filter) ~= #f
              end
        end method object-matches-substring-filter?;
  local method show-object?
            (object :: <object>) => (show? :: <boolean>)
          ~instance?(object, <definition-object>)
            | (object-matches-type-filter?(object)
                 & object-matches-substring-filter?(object))
        end method show-object?;
  let results = make(<stretchy-vector>);
  for (object in contents)
    if (show-object?(object))
      add!(results, object)
    end
  end;
  results
end method filter-source-form-usage;

define method source-form-usage-label-key
    (frame :: <environment-frame>, object :: <environment-object>)
 => (label :: <string>)
  frame-default-object-name(frame, object)
end method source-form-usage-label-key;

define method source-form-usage-icon-function
    (frame :: <environment-frame>, object :: <environment-object>)
 => (icon)
  environment-object-icon(ensure-frame-project(frame), object)
end method source-form-usage-icon-function;

define method source-form-usage-children-predicate
    (frame :: <environment-frame>, usage-vector :: <vector>)
 => (has-children? :: <boolean>)
  #t
end method source-form-usage-children-predicate;

define method source-form-usage-children-generator
    (frame :: <environment-frame>, usage-vector :: <vector>)
 => (users :: <sequence>)
  let project = frame.ensure-frame-project;
  let type = usage-vector[0];
  let source-form = usage-vector[1];
  let source-forms
    = select (type)
        #"used-definitions" =>
          source-form-used-definitions(project, source-form);
        #"clients" =>
          source-form-clients(project, source-form);
      end;
  frame-order-sequence(frame, source-forms,
                       label-key: curry(frame-default-object-name, frame))
end method source-form-usage-children-generator;

define method source-form-usage-label-key
    (frame :: <environment-frame>, usage-vector :: <vector>)
 => (label :: <string>)
  let type = usage-vector[0];
  select (type)
    #"used-definitions" => "Used Definitions";
    #"clients"          => "Clients";
  end
end method source-form-usage-label-key;

define method source-form-usage-icon-function
    (frame :: <environment-frame>, usage-vector :: <vector>)
 => (icon)
  let type = usage-vector[0];
  select (type)
    #"used-definitions" => $uses-folder-bitmap;
    #"clients"          => $clients-folder-bitmap;
  end
end method source-form-usage-icon-function;
