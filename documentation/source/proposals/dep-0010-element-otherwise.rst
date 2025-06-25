**********************************
Defaulted Element Reference Syntax
**********************************

===============  =============================================
DEP #:           10
Type:            Standards Track
Author:          Peter S. Housel
Status:          Withdrawn
Created:         13-Oct-2018
Last-Modified:   24-Feb-2019
Post-History:    `04-Mar-2019 <https://groups.google.com/forum/#!topic/dylan-lang/AmXDa5iNJOU/discussion>`_
Target-Version:  2019.1
===============  =============================================


Revision History
================

The revision history of this document is available here:
https://github.com/dylan-lang/opendylan/commits/master/documentation/source/proposals/dep-0010-element-otherwise.rst

Abstract
========

The Dylan language currently provides bracketed element reference
syntax as a shorthand for calls to the :drm:`element` and :drm:`aref`
functions. This DEP proposes an element reference syntax for providing
default values for :drm:`element` calls.

Motivation
==========

The element reference syntax ``collection[i]`` is a compact way of
writing ``element(collection, i)``. However, whenever supplying a
default becomes necessary, element reference syntax cannot be
used. Sometimes this results in a mixing of the two styles in the same
area of code. Adding defaulted element reference syntax can improve
the conciseness and readability of code.

Specification
=============

To the `operand` nonterminal identifier we add the following phrase
grammar rule:

   | `operand`: `operand` ``[`` `arguments` :subscript:`opt` ``]`` ``otherwise`` `otherwise-operand`

In addition we add the following nonterminal and phrase grammar rules:

   | `otherwise-operand`:
   |     `leaf`
   |     `SYMBOL`

Just as ``collection[i]`` is syntactic sugar for ``element(collection, i)``
under the interpretation of ``element`` in force for the module
where it appears, the ``collection[i] otherwise y`` syntax is
equivalent to ``element(collection, i, default: y)``.

Rationale
=========

Note that to prevent parsing ambiguities the operand of the
``otherwise`` clause uses the `leaf` grammar nonterminal rather than
the `expression` nonterminal used for function call arguments. This
requires that complex default value expressions be parenthesized, but
since in practice most supplied defaults are either literals or
(constant) variable names this should not provide much of an
inconvenience.

Backwards Compatibility
=======================

The addition of this new legal syntax does not affect the
interpretation of existing programs.

Reference Implementation
========================

A reference implementation for Open Dylan can be found at
https://github.com/dylan-lang/opendylan/tree/element-otherwise
