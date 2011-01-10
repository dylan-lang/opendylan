Module:    test-editor-manager-common
Synopsis:  Environment-Editor Interface Test -- platform-independent part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Places to store two test commands and factories for them.
define variable *test-string-based-command* = #f;
define variable *test-table-based-command* = #f;
define variable *test-string-based-command-factory* = #f;
define variable *test-table-based-command-factory* = #f;



/// Equality for <editor-command>s.
// Two instances of <editor-commands> are \= if their object-class()es
// are equatable and the instances are equal, as defined in a
// class-pair-specific way.  By default, different subclasses of
// <editor-command> are *not* equatable.

// Are two subclasses of <editor-command> equatable?
define generic equatable-editor-command-classes?
    (class1 :: subclass(<editor-command>),
     class2 :: subclass(<editor-command>))
 => (equal? :: <boolean>);

define method equatable-editor-command-classes?
    (class1 :: subclass(<editor-command>),
     class2 :: subclass(<editor-command>))
 => (equatable? :: <boolean>)
  // Subclasses of <editor-command> are not equatable, by default.
  #f
end method;

define method equatable-editor-command-classes?
    (class1 == <editor-string-based-command>,
     class2 == <editor-string-based-command>)
 => (equatable? :: <boolean>)
  #t
end method;

define method equatable-editor-command-classes?
    (class1 == <editor-table-based-command>,
     class2 == <editor-table-based-command>)
 => (equatable? :: <boolean>)
  #t
end method;


// Are two instances of <editor-command> equal (given that their
// classes are equatable)?
define generic editor-command-equal?
    (cmd1 :: <editor-command>, cmd2 :: <editor-command>)
 => (equal? :: <boolean>);

define method editor-command-equal?
    (cmd1 :: <editor-command>, cmd2 :: <editor-command>)
 => (equal? :: <boolean>)
  cmd1.editor-command-name = cmd2.editor-command-name
end method;

define method editor-command-equal?
    (cmd1 :: <editor-string-based-command>,
     cmd2 :: <editor-string-based-command>)
 => (equal? :: <boolean>)
  next-method()
    & (cmd1.editor-command-string = cmd2.editor-command-string);
end method;

define method editor-command-equal?
    (cmd1 :: <editor-table-based-command>,
     cmd2 :: <editor-table-based-command>)
 => (equal? :: <boolean>)
  next-method()
    & (cmd1.editor-command-variable-table
         = cmd2.editor-command-variable-table);
end method;


// At last, the method on \= for <editor-command> pairs.
define method \=
    (cmd1 :: <editor-command>, cmd2 :: <editor-command>)
 => (equal? :: <boolean>)
  equatable-editor-command-classes?(cmd1.object-class, cmd2.object-class)
    & editor-command-equal?(cmd1, cmd2)
end method;



/// Test <editor-command-factory>s.

define test test-editor-command-factories
    (description: "Test editor command factories.")
  let command-factory-table = make(<table>);

  check-true
    ("editor-command-factories: making string-based command",
     *test-string-based-command*
       := make(<editor-string-based-command>,
               name: #"test-string-based-command",
               string: "test-variable = $test-variable"));
  check-true
    ("editor-command-factories: making table-based command",
     *test-table-based-command*
       := make(<editor-table-based-command>,
               name: #"test-table-based-command"));
  check-true
    ("editor-command-factories: making factory for "
     "string-based commands",
     *test-string-based-command-factory*
       := make(<editor-command-factory>,
               editor-command: *test-string-based-command*));
  check-true
    ("editor-command-factories: making factory for "
     "table-based commands",
     *test-table-based-command-factory*
       := make(<editor-command-factory>,
               editor-command: *test-table-based-command*));
  check-equal
    ("editor-command-factories: output from string-based factory "
     "equals original",
     *test-string-based-command*,
     *test-string-based-command-factory*.make-instance());
  check-equal
    ("editor-command-factories: output from table-based factory "
     "equals original",
     *test-table-based-command*,
     *test-table-based-command-factory*.make-instance());
  check-equal
    ("editor-command-factories: editor-add-command-factory!",
     *test-table-based-command-factory*,
     (editor-add-command-factory!
        (command-factory-table, *test-table-based-command-factory*)
        & element
            (command-factory-table,
             *test-table-based-command*.editor-command-name,
             default: #f)));
end test;
