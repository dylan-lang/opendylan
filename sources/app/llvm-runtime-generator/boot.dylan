Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// DFMC expects to be able to call these functions
define sideways method library-progress-text (context, #rest args)
  apply(format, *standard-output*, args);
  new-line(*standard-output*);
  force-output(*standard-output*);
end method;

define sideways method library-progress-report
    (context, sofar :: <float>, #key abort-safe? :: <boolean>)
  // Do nothing
end method;

define sideways method library-stage-text(context, #rest args)
  //apply(format, *standard-output*, args);
  //new-line(*standard-output*);
  //force-output(*standard-output*);
end method;

define sideways method internal-progress-text(context, #rest args)
  //apply(format, *standard-output*, args);
  //new-line(*standard-output*);
  //force-output(*standard-output*);
end method;

define sideways method library-condition-report
    (context, condition :: <condition>)
  format(*standard-error*, "%s\n", condition);
  force-output(*standard-error*);
end method;

define sideways method note-definitions-updated (context)
  // Nothing to do here
end method;

define function do-with-booted-dylan-context
    (body :: <function>,
     #key lid-locator,
          operating-system = unsupplied(),
          processor = unsupplied(),
          back-end = unsupplied())
  // Dummy object representing the dylan library
  let dylan-project
    = #"dummy-dylan-project";

  // Create source records for all of the files in the dylan library
  let lid-directory = lid-locator.locator-directory;
  let properties :: <table> = read-file-header(lid-locator);
  if (element(properties, #"library", default: #f).first ~= "dylan")
    error("LID file does not define the dylan library (%s)",
          properties[#"library"]);
  end if;
  let source-records
    = map-as(limited(<vector>, of: <source-record>),
             method (name)
               let locator
                 = make(<file-locator>, base: name, extension: "dylan");
               let merged
                 = merge-locators(locator, lid-directory);
               let ids
                 = file-source-record-ids(<file-source-record>,
                                          lid-directory, merged);
               id-as-source-record(<file-source-record>, dylan-project,
                                   merged, ids.first)
             end,
             properties[#"files"]);

  // Need not exist; just needs to be a locator with a base name of
  // "dylan" for make-library-description.
  let dylan-database
    = make(<file-locator>, base: "dylan", extension: "ddb");

  // Make a library description for the dylan library.
  let dylan-ld
    = make-library-description(dylan-project,
                               database-location: dylan-database,
                               build-settings: #());
  if (supplied?(operating-system))
    dylan-ld.library-description-os-name := operating-system;
  end;
  if (supplied?(processor))
    dylan-ld.library-description-processor-name := processor;
  end;
  if (supplied?(back-end))
    dylan-ld.library-description-compiler-back-end-name := back-end;
  end;

  with-library-progress (dylan-ld, 1.0)
    install-project-sources(dylan-ld, source-records, 0, 0);
    parse-project-sources(dylan-ld);
    with-library-context (dylan-ld)
      ensure-library-models-computed(dylan-ld);
      ensure-library-models-finished(dylan-ld);
    end;
  end;

  // Now that the dylan library is properly booted, execute the
  // supplied body within its context.
  with-library-context (dylan-ld)
    body();
  end;
end function;

define macro with-booted-dylan-context
  { with-booted-dylan-context (?options:*) ?:body end }
    => { do-with-booted-dylan-context(method() ?body end, ?options) }
end macro;
