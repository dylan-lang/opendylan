*******
Preface
*******

Product
=======

Open Dylan  supports the Common Object Request Broker Architecture (CORBA)
defined by Object Management Group, Inc. (OMG).

The Open Dylan Object Request Broker (ORB) and supporting tools
provide CORBA architecture functionality to Dylan programmers,
combining standardized distributed system development with a
state-of-the-art dynamic object-functional language.

Parts
=====

The CORBA components included in Open Dylan are:

#. An IDL (Interface Definition Language) compiler

   This comes in two forms:

   #. some DLLs linked into the Open Dylan development environment,
      and console Dylan compiler, to allow IDL files to be included in
      Dylan projects. This integrates CORBA into the Open Dylan
      project manager and build system.

   #. a standalone IDL compiler called ``console-scepter``

#. An ORB runtime library

   This consists of the ``dylan-orb`` library and several of the
   libraries on which it depends.

#. Some example CORBA projects including:

   - ``corba-hello-world``
   - ``bank``
   - ``chat``

   These are found in the ``Examples`` subfolder to the top-level Open
   Dylan installation folder on Windows, or in ``sources/corba/demos``
   in the source repository.

#. This manual

Audience
========

This book is intended for use by application programmers who wish to
build CORBA applications using Dylan. The book assumes that the reader
is familiar with both the Dylan programming language and with building
distributed applications using CORBA.

Standards compliance
====================

The Open Dylan Dylan ORB currently conforms to the CORBA 2.0
specification with some elements of CORBA 2.2, most notably the
Portable Object Adapter (POA).


Further reading
=================

Many resources exist for those who want to learn about CORBA and
distributed software development.

`Ciaran McHale's CORBA Explained Simply
<http://www.ciaranmchale.com/corba-explained-simply/>`_ is an
excellent overview of CORBA concepts and the CORBA ecosystem. The
`Other CORBA Resources
<http://www.ciaranmchale.com/corba-explained-simply/other-corba-resources.html>`_
chapter of that book contains helpful links to CORBA books and other
related resources.

Michi Henning's `The Rise and Fall of CORBA
<https://queue.acm.org/detail.cfm?id=1142044>`_ outlines the history,
including the successes and failures, of the CORBA standards.
