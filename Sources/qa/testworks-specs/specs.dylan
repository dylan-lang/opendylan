Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Specification modeling

define abstract class <spec> (<object>)
  constant slot spec-name :: <symbol>,
    required-init-keyword: name:;
end class <spec>;

define abstract class <definition-spec> (<spec>)
end class <definition-spec>;

define method spec-title
    (spec :: <spec>) => (title :: <byte-string>)
  as-lowercase(as(<byte-string>, spec-name(spec)))
end method spec-title;


/// Protocols

define open generic make-test-instance
    (class :: <class>) => (object);

define open generic destroy-test-instance
    (class :: <class>, object :: <object>) => ();

define open generic class-test-function
    (class :: <class>) => (test-function :: false-or(<function>));

define method class-test-function
    (class :: <class>) => (test-function :: false-or(<function>))
  #f
end method class-test-function;
