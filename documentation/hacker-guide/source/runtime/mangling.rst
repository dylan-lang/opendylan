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
| *!*  | *\_E\_*  | exclamation   |
+------+----------+---------------+
| *$*  | *\_D\_*  | dollar        |
+------+----------+---------------+
| *\** | *\_T\_*  | times         |
+------+----------+---------------+
| */*  | *\_S\_*  | slash         |
+------+----------+---------------+
| *<*  | *\_L\_*  | less          |
+------+----------+---------------+
| *>*  | *\_G\_*  | greater       |
+------+----------+---------------+
| *?*  | *\_Q\_*  | question mark |
+------+----------+---------------+
| *+*  | *\_PL\_* | plus          |
+------+----------+---------------+
| *&*  | *\_AP\_* | ampersand     |
+------+----------+---------------+
| *^*  | *\_CR\_* | caret         |
+------+----------+---------------+
| *\_* | *\_UB\_* | underbar      |
+------+----------+---------------+
| *~*  | *\_SG\_* | squiggle      |
+------+----------+---------------+
|      | *\_SP\_* | space         |
+------+----------+---------------+

Finally, the fully mangled name is created by concatenating the
processed library, module, and identifier names respectively, separated
by ``X``.

For example, the Dylan identifier ``add-new!`` in module ``internal`` of
library ``dylan`` would be mangled as ``dylanXinternalXadd_new_E_``.
