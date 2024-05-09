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

  Kstream_sizeYstreams_protocolVcommon_dylanMioM0I

  K                    = constant
  stream_size          = method name
  Y                    = module separator
  streams_protocol     = module name
  V                    = library separator
  common_dylan         = library name
  M                    = method separator
  io                   = library which defines this method
  M                    = method separator
  0                    = 0th method in stream_size generic
  I                    = Internal Entry Point (IEP)

The Dylan mangling scheme takes care to omit some elements of
the name when possible to help abbreviate the names. The usual
things that are omitted are:

* The module marker and name when the module name is the same
  as the library name.
* The name of the library where the method is defined (between
  the method separators). This is omitted when it is the same
  as the library which defines the generic function.

We'll see this in our next example::

  Krun_test_applicationVtestworksMM0I

  K                    = constant
  run_test_application = method name
  (module marker and name omitted when same as library name)
  V                    = library separator
  testworks            = library name
  M                    = method separator
  (library name omitted as this is defined in the same library as the generic)
  M                    = method separator
  0                    = 0th method in run_test_application generic
  I                    = Internal Entry Point (IEP)

Bindings within the Dylan library are given special shortened forms.
In this case, instead of the library separator ``V``, the library name
and then the ``Y`` module separator, we would see the library separator
``V`` and then then special ``dylan`` module separator ``K``. Some
modules within the ``dylan`` library are given special single letter
abbreviations.  For example the ``dylan`` module's name is just ``d``
and the ``internal`` module is ``i``.

Examples:

* ``KLempty_listGVKd``
* ``Kcondition_format_arguments_vectorVKiI``

See `the source
<https://github.com/dylan-lang/opendylan/blob/master/sources/dfmc/mangling/mangling.dylan>`_
for more detail.
