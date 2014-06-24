Name Mangling
=============

In Dylan, unlike C, identifier names are case insensitive. Dylan also
permits additional characters to appear in names. As a further
complication, Dylan provides multiple namespaces, and the namespaces are
controlled within a two-tier hierarchy of modules and libraries.

In order to make it possible to link Dylan code with tools designed to
support more traditional languages, the Dylan compiler transforms the
names which appear in Dylan programs to C compatible names, via a
process called *mangling*.

The library, module and identifier names are each processed, according
to the following rules:

#. All uppercase characters are converted to lowercase.
#. Any character which appears on the left-hand side of the table is mapped
   to the new character sequence accordingly.

+------+----------+---------------+
| Old  | New      | Comment       |
+======+==========+===============+
| *-*  | *\_*     | dash          |
+------+----------+---------------+
| *!*  | *X*      | exclamation   |
+------+----------+---------------+
| *$*  | *D*      | dollar        |
+------+----------+---------------+
| *%*  | *P*      | percent       |
+------+----------+---------------+
| *\** | *T*      | times         |
+------+----------+---------------+
| */*  | *S*      | slash         |
+------+----------+---------------+
| *<*  | *L*      | less          |
+------+----------+---------------+
| *>*  | *G*      | greater       |
+------+----------+---------------+
| *?*  | *Q*      | question mark |
+------+----------+---------------+
| *+*  | *A*      | plus          |
+------+----------+---------------+
| *&*  | *B*      |               |
+------+----------+---------------+
| *^*  | *C*      | caret         |
+------+----------+---------------+
| *\_* | *U*      | underscore    |
+------+----------+---------------+
| *@*  | *O*      |               |
+------+----------+---------------+
| *=*  | *E*      |               |
+------+----------+---------------+
| *~*  | *N*      |               |
+------+----------+---------------+

Finally, the fully mangled name is created by concatenating the
mangled names together, along with other special markers.

Example 1::

  Kexecute_componentQYPtestworksVtestworks

  K                  = constant (methods are constants)
  execute_componentQ = method name (Q = ?)
  Y                  = module separator
  Ptestworks         = module name (P = %)
  V                  = library separator
  testworks          = library name

Example 2::

  Krun_test_applicationVtestworksMM0I

  K                    = constant
  run_test_application = method name
  V                    = library separator
  testworks            = library name
  M                    = method separator
  (module name omitted when same as library name)
  M                    = method separator
  0                    = 0th method in run_test_application generic
  I                    = Internal Entry Point (IEP)

Some internal modules are given special single letter names.  For
example the dylan module's name is just "d".

See `the source
<https://github.com/dylan-lang/opendylan/blob/master/sources/dfmc/mangling/mangling.dylan>`_
for more detail.
