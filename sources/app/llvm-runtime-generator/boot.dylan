Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// DFMC expects to be able to call these functions
define sideways method library-progress-text (context, #rest args)
  //apply(format, *standard-output*, args);
  //new-line(*standard-output*);
  //force-output(*standard-output*);
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

define function do-with-booted-dylan-context (body :: <function>)
  // Dummy object representing the dylan library
  let dylan-project
    = #"dummy-dylan-project";

  // Need not exist; just needs to be a locator with a base name of
  // "dylan" for make-library-description.
  let dylan-database
    = make(<file-locator>, base: "dylan", extension: "ddb");

  // Make a library description for the dylan library.
  let dylan-ld
    = make-library-description(dylan-project,
                               database-location: dylan-database,
                               build-settings: #());

  // Parse and compile the magic form which causes booted definitions
  // to take effect (as with dylan/dfmc-boot.dylan).
  let record
    = make(<interactive-source-record>,
           project: dylan-project,
           module: #"dylan-user",
           source: as(<byte-vector>, "boot-dylan-definitions();"));
  with-library-progress (dylan-ld, 1.0)
    install-project-sources(dylan-ld,
                            as(limited(<vector>, of: <source-record>),
                               vector(record)),
                            0, 0);
  end;

  // Now that the dylan library is properly booted, execute the
  // supplied body within its context.
  with-library-context (dylan-ld)
    body();
  end;
end function;

define macro with-booted-dylan-context
  { with-booted-dylan-context ?:body end }
    => { do-with-booted-dylan-context(method() ?body end) }
end macro;
