Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Testing all operations for the current default editor backend.

define test test-open-editor
    (description: "Test the 'open-editor' command.")
  check-true
    ("Call open-editor.  This should open an editor window\n"
     "for the current backend.",
     *editor-call*(#"open-editor"));
end test;


define test test-new-file
    (description: "Test the 'new-file' command.")
  check-true
    ("Call new-file.  This should reuse or open an editor window and "
     "create a new file in that window.\n"
     "For some backends, this is equivalent to 'open-editor'.",
     *editor-call*(#"new-file"));
end test;


define test test-show-location
    (description:
     "Test the 'show-location' command.  Positions are given as '(line,col)'.")

  check-true
    ("show-location: no file-name, no position.\n"
     "This should reuse or open an editor window, without opening a "
     "file or moving the editor cursor.",
     *editor-call*(#"show-location"));

  check-true
    ("show-location: file-name is directory, no position.\n"
     "This should reuse or open an editor window, without opening a "
     "file or moving the editor cursor.  It may cause some other "
     "backend-specific behaviour as well.",
     *editor-call*(#"show-location",
       file-name: *test-file-path*));

  check-true
    ("show-location: file-name is file-without-directory, no position.\n"
     "This should reuse or open an editor window, opening the test file "
     "file from the current directory (if it can be found there).",
     *editor-call*(#"show-location",
       file-name: *test-file-name*));

  let test-full-file-name :: <string>
    = concatenate(*test-file-path*, *test-file-name*);

  check-true
    ("show-location: full file-name, no position; i.e., (0,0) = start.\n"
     "This should reuse or open an editor window, opening the test file "
     "file and positioning the cursor at the start of the file.\n"
     "(If the editor's cursor co-ordinates start from 1, this may "
     "cause an editor error.)",
     *editor-call*(#"show-location",
       file-name: test-full-file-name));

  check-true
    ("show-location: full file-name, position (1,1) = near start.\n"
     "This should reuse or open an editor window, opening the test file "
     "file (or re-using a buffer which shows it) and positioning the "
     "cursor _near_ the start of the file.",
     *editor-call*(#"show-location",
       file-name: test-full-file-name,
       from-line: 1, from-col: 1));

  check-true
    ("show-location: full file-name, position (10,5) = middle.\n"
     "This should reuse or open an editor window, opening the test file "
     "file (or re-using a buffer which shows it) and positioning the "
     "cursor some way into the file.",
     *editor-call*(#"show-location",
       file-name: test-full-file-name,
       from-line: 10, from-col: 5));

  check-true
    ("show-location: full file-name, position (1,1000) = past EOL.\n"
     "This should reuse or open an editor window, opening the test file "
     "file (or re-using a buffer which shows it) and positioning the "
     "cursor past the end of a line.  This may cause an editor error.",
     *editor-call*(#"show-location",
       file-name: test-full-file-name,
       from-line: 0, from-col: 1000));

  check-true
    ("show-location: full file-name, position (1000,1) = past EOF.\n"
     "This should reuse or open an editor window, opening the test file "
     "file (or re-using a buffer which shows it) and positioning the "
     "cursor past the end of the file.  This may cause an editor error.",
     *editor-call*(#"show-location",
       file-name: test-full-file-name,
       from-line: 1000, from-col: 0));
end test;


define function backend-setup-function ()
  let name = editor-default-factory-name();
  gui-progress-display-message(#"check", "");
  gui-progress-display-message
    (#"information",
     format-to-string
       ("Testing the %= editor backend.\n"
        "Some checks may succeed but cause an editor error; please "
        "record any such errors.\n"
	"Please close any editor windows opened by previous checks,\n"
	"then click 'Continue' to proceed.", name));
  gui-progress-pause();
  gui-progress-display-message
    (#"information",
     format-to-string("Testing the %= editor backend.\n", name));
end function backend-setup-function;

define suite suite-test-default-backend
    (description:
       "Test all operations for the current default editor back-end.",
     setup-function: backend-setup-function)
  test test-open-editor;
  test test-new-file;
  test test-show-location;
end suite;



/// Testing all available editor back-ends.

define test test-all-backends
    (description: "Test all available editor back-ends.")
  editor-register-all-factories();
  for (factory in $editor-factories)
    editor-default-factory-name() := factory.make-instance().editor-name;
    perform-suite(suite-test-default-backend);
  end for;
end test;


define function all-backends-cleanup-function () => ()
  gui-progress-display-message
    (#"information",
     "All editor backend tests complete.\n"
     "Please close any editor windows opened by previous checks,\n"
     "then click 'Continue' to close this window.");
  gui-progress-pause();
end function all-backends-cleanup-function;


/// NOTE
// Use gui-perform-suite for this suite.
/// NOTE

define suite suite-test-all-backends
    (description: "Test all available editor back-ends.",
     cleanup-function: all-backends-cleanup-function)
  test test-all-backends;
end suite;
