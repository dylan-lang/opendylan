Module:   DFMC-Management
Synopsis: How to compile & test a source fragment as though it were a library.
Author:   Steve Rowley, Keith Playford, Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic classes for source records that represent a template.
/// (This whole exercise could easily be repeated, using strings instead
/// of templates for the source.)
///

define class <template-source-record> (<source-record>)
  // A source record whos source comes from a template closure.
  constant slot source-record-template :: <function>, 
    required-init-keyword: template:;
  constant slot source-record-module-name :: <symbol>,
    required-init-keyword: module:;
  // Name for this source record (more conventionally a file name base).
  // Used for name of init function in c-back-end link phase, as well as
  // giving the source records names for printing purposes.
  constant slot source-record-name :: <string>,
    required-init-keyword: name:;
end;

define method print-object (tsr :: <template-source-record>, stream :: <stream>) => ()
  // How to print a <template-source-record>, reasonably informatively.
  format(stream, "{Template SrcRec: %s}", source-record-name(tsr))
end;

///
/// How to get the reader to read input from a template source record.
///

define class <template-input-stream> (<object>)
  // Use this to suborn the reader into thinking it read the template.
  constant slot stream-source-record :: <template-source-record>,
    required-init-keyword: source-record:;
  slot stream-done? :: <boolean> = #f;
end class;

define method call-with-source-record-input-stream 
    (fn :: <function>, tsr :: <template-source-record>, #key)
  fn(make(<template-input-stream>, source-record: tsr));
end method;

define method read-top-level-fragment
    (stream :: <template-input-stream>, cr, offset, #key on-end-of-stream)
 => (fragment, lexer)
  // Here's where the actual reading over the fake stream occurs.  We either
  // return the template (via matching), or take eof action.
  if (stream-done?(stream))
    on-end-of-stream
  else
    stream-done?(stream) := #t;
    dynamic-bind (*fragment-context* = cr.compilation-record-module)
      macro-case (source-record-template(stream-source-record(stream))())
        { ?stuff:body } => stuff
      end
    end
  end
end;

