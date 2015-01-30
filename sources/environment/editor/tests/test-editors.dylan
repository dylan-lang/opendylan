Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Two test kinds of editor.

// First, a specialised kind of <exe-editor>.
define function test-exe-editor-commands-init
    ()
 => (commands :: <table>)
  // Make the table.
  let test-exe-editor-commands = make(<table>);

  editor-add-command-factory!
    (test-exe-editor-commands, *test-string-based-command-factory*);

  test-exe-editor-commands
end function;

define variable *test-exe-editor-factory* = #f;

// Second, a custom editor class
define function test-custom-editor-commands-init
    ()
 => (commands :: <table>)
  // Make the table.
  let test-custom-editor-commands = make(<table>);

  editor-add-command-factory!
    (test-custom-editor-commands, *test-table-based-command-factory*);

  test-custom-editor-commands
end function;

define class <test-custom-editor> (<editor>)
  inherited slot editor-name = #"test-custom-editor";
  inherited slot editor-title = "A test editor direct subclass of <editor>";
  inherited slot editor-commands,
    init-function: test-custom-editor-commands-init;
  // The following slot is for checking that init-keywords supplied to
  // factories are passed on to the editors they create.
  slot test-extra-slot :: <integer>,
    required-init-keyword: test-extra-slot:;
end class;

define method editor-call-main
    (editor :: <test-custom-editor>,
     command :: <editor-table-based-command>,
     #rest args, #key fail-main?, #all-keys)
 => (#rest objects)
  if ( fail-main? )
    signal(make(<editor-warning-command-failed>,
                other-values: "Oops!"))
  else
    command.editor-command-variable-table
  end if;
end method;

define method editor-call-finalize
    (editor :: <test-custom-editor>,
     command :: false-or(<editor-command>),
     #key fail-finalize?, #all-keys)
 => ()
  if ( fail-finalize? )
    error(make(<editor-error-finalize-failed>,
               other-values: "Oops again!"));
  else
    editor-uncache-instances(#"test-custom-editor");
    next-method()
  end if
end method;


define variable *test-custom-editor-factory* = #f;



/// Test editors.

define variable *previous-editor-default-factory-name* = #f;

define test test-editors
    (description: "Test editors.")

  check-true
    ("editors: making a specialised <exe-editor> factory",
     *test-exe-editor-factory*
       := make(<editor-factory>,
               editor-class: <exe-editor>,
               editor-name: #"test-exe-editor",
               editor-title: "A test editor specialised from <exe-editor>",
               editor-commands: test-exe-editor-commands-init(),
               editor-image: "echo \"You shouldn't USE this editor!\""));
  check-equal
    ("editors: editor-supported-command-names on test <exe-editor>",
      editor-supported-command-names
        (*test-exe-editor-factory*.make-instance()),
     #[#"test-string-based-command"]);
  check-true
    ("editors: initialisation of factory for test custom editor class",
     *test-custom-editor-factory*
       := make(<editor-factory>,
            editor-class: <test-custom-editor>,
            test-extra-slot: 1046));
  check-equal
    ("editors: factory of origin for test custom editor class",
     *test-custom-editor-factory*.make-instance().editor-factory-of-origin,
     *test-custom-editor-factory*);

  // Register our factory, then make it the default, remembering the
  // previous default so we can reset it later.
  editor-register-factory(*test-custom-editor-factory*);
  *previous-editor-default-factory-name* := editor-default-factory-name();
  editor-default-factory-name() := #"test-custom-editor";

  check-equal
    ("editors: getting default factory name",
     editor-default-factory-name(), #"test-custom-editor");
  check-equal
    ("editors: getting default factory",
     editor-default-factory(), *test-custom-editor-factory*);

  // Ensure that the editor instance cache does NOT contain an instance
  // of the test editor (which might happen if, say, the test suite is
  // re-run after a crash part-way through).
  editor-uncache-instances(#"test-custom-editor");

  // The first call will have to create a new instance but the second
  // should return the same instance.
  check-equal
    ("editors: editor-get-instance cacheing",
     editor-default-instance(), editor-default-instance());
end test;
