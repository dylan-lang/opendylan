Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Test <editor-variable> substitution.

define constant $editor-variable-test-variable :: <editor-variable>
  = make(<editor-variable>, name: #"$test-variable");

define method editor-variable-value
    (variable-name == #"$test-variable",
     variable :: <editor-variable>,
     #key test-variable, #all-keys)
 => (value :: <object>)
  (test-variable & shallow-copy(test-variable)) | "default-value"
end method;

define test test-editor-substitution
    (description: "Test editor variable substitution.")

  $editor-variables[#"$editor-variable-test-variable"]
    := $editor-variable-test-variable;

  check-equal
    ("editor-substitution: replace-matched-subsequence-all!",
     replace-matched-subsequence-all!
       ("Hello world, hello, HELLO!","ello","i"),
     "Hi world, hi, HELLO!");
  check-equal
    ("editor-substitution: replace-matched-substring-all-nocase!",
     replace-matched-substring-all-nocase!
       ("HeLLo world, hello, hEllO!","ELlo","i"),
     "Hi world, hi, hi!");
  check-equal
    ("editor-substitution: substitution on string-based commands",
     editor-command-string
       (editor-substitute-variables!
          (*test-string-based-command-factory*.make-instance(),
           test-variable: "test-value")),
     "test-variable = test-value");
  check-equal
    ("editor-substitution: defaulted substitution on string-based commands",
     editor-command-string
       (editor-substitute-variables!
          (*test-string-based-command-factory*.make-instance())),
     "test-variable = default-value");
  check-equal
    ("editor-substitution: substitution on table-based commands",
     element
       (editor-command-variable-table
          (editor-substitute-variables!
             (*test-table-based-command-factory*.make-instance(),
              test-variable: "test-value")),
        #"$test-variable"),
     "test-value");
  check-equal
    ("editor-substitution: defaulted substitution on table-based commands",
     element
       (editor-command-variable-table
          (editor-substitute-variables!
             (*test-table-based-command-factory*.make-instance())),
        #"$test-variable"),
     "default-value");
end test;
