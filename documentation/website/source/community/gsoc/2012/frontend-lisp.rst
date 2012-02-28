=============
Frontend/LISP
=============

The purpose of this project would be to write another syntax for Dylan based on the original S-Expression syntax.

The project would include to write (or revive) a S-Expression lexer and parser for Dylan and translating this into the ``dfmc-definitions`` structure by using a macro-expander for Dylan macros.

The change towards the infix syntax was done before the programming language was finalized, thus not all features are present in the prefix syntax. This has to be taken into consideration and adjusted appropriately (by redefining the prefix syntax).

Using this approach both surface syntaxes (apart from macros) can be completely exchanged. Also, the semantics (control/data flow, optimization, type inference, backends) does not have to be touched at all. Every dylan file already has rfc822 headers, one of these is ``Language:``, which values can be ``prefix-dylan`` and ``infix-dylan`` (where the latter is the default).

Some references:

- `Dylan Interim Reference Manual <http://jim.studt.net/dirm/interim-contents.html>`_
- `Dylan Macros with S-Expressions <https://www.cs.cmu.edu/afs/cs/project/clisp/OldFiles/hackers/wlott/dylan/moon/Dylan-Macros.RTF>`_
- `S-Expression Syntax <https://github.com/dylan-lang/temporary_complete_dylan_repo/tree/393e3fa2089bce8f02d950b561dee7bb3de7fe02/old/Sources/emulator/lib>`_
