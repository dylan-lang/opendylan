Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test each part of the editor call-out process.

// Test default method for initialisation of call-context information.
define test test-editor-call-initialize
    (description: "Test the call-initialization mechanism")
  let other-editor = editor-default-instance();

  check-equal
    ("editor-call-initialize: supply editor but no factory",
     editor-call-initialize(other-editor, #f),
     other-editor);
  check-equal
    ("editor-call-initialize: supply factory but no editor",
     editor-call-initialize(#f, *test-custom-editor-factory*),
     editor-default-instance());
  check-equal
    ("editor-call-initialize: supply both editor and factory",
     editor-call-initialize(other-editor, *test-custom-editor-factory*),
     other-editor);
  check-false
    ("editor-call-initialize: supply neither editor and no factory",
     editor-call-initialize(#f, #f));
end test;

// Test command translation.
define test test-editor-translate-command
    (description: "Test the command translation mechanism")
  let translated-command
    = editor-translate-command
        (editor-default-instance(),
         make(<editor-frontend-command>,
              name: #"test-table-based-command"));

  check-equal
    ("editor-translate-command: translation of known command",
     translated-command,
     *test-table-based-command*);
  check-false
    ("editor-translate-command: translation of unknown command",
     editor-translate-command
       (editor-default-instance(),
        make(<editor-frontend-command>,
             name: #"test-unknown-command")));
end test;

// No tests here for variable substitution, as the mechanism is
// tested in a preceding test (see test-substitution.dylan).

// No tests here for the main call-out, since its results are specific
// to each back-end and the tests in this file are part of a suite
// testing the front-end.

// No tests here for finalization, as it is tested in the
// "overall call-out test" below.



/// Test the overall call-out mechanism on the test editor backend.
define test test-editor-call
    (description: "Test the overall call-out mechanism")
  let default-factory = editor-default-factory();
  let default-factory-name = editor-default-factory-name();

  // This checks that a normal call proceeds correctly and variable
  // substitution works as expected.
  check-true
    ("editor-call: call returning normally",
     begin
       let variable-table
         = *editor-call*(#"test-table-based-command",
                         file-name: *test-file-name*, size: 1046);
       element(variable-table, #"$file-name") = *test-file-name*
         & element(variable-table, #"size", default: #f) = #f
     end);

  // In the following two checks I'd like to look at the contents of the
  // <condition> objects, but I can't yet in TestWorks :-(
  check-condition
    ("editor-call: call signalling <editor-warning-command-failed>",
     <editor-warning-command-failed>,
     *editor-call*(#"test-table-based-command", fail-main?: #t));
  check-condition
    ("editor-call: call signalling <editor-error-finalize-failed>",
     <editor-error-finalize-failed>,
     *editor-call*(#"test-table-based-command", fail-finalize?: #t));

  // Unless the next test crashes badly, it should also remove any
  // instances of the test editor from $editor-instances, which I'll
  // check, to make sure editor-call-finalize was called.
  check-condition
    ("editor-call: call signalling <editor-error-unsupported-command>",
     <editor-error-unsupported-command>,
     *editor-call*(#"test-unknown-command"));
  check-false
    ("editor-call: check finalize was performed despite error being signalled",
     any?(method (ed) ed.editor-name = default-factory-name end,
          $editor-instances));

  // Check what happens if there's a broken factory.  First make sure
  // there are no cached editors, so that the factory's make-instance
  // function will be used.  Then set that function to not return an
  // editor.
  editor-uncache-instances(default-factory-name);
  default-factory.make-instance := method () end;
  check-condition
    ("editor-call: check broken editor factory",
     <error>,
     block ()
       *editor-call*(#"test-unknown-command")
     exception (<editor-condition>)
       // We want to ignore <editor-condition>s, as we're checking that
       // something more serious goes wrong.
     end);

  // Check what happens if there's no default factory.
  editor-deregister-factory(default-factory-name);
  check-condition
    ("editor-call: call returning <editor-error-factory-unavailable>",
     <editor-error-factory-unavailable>,
     *editor-call*(#"test-unknown-command"));
end test;
