Module:       common-dylan-internals
Author:       Gary Palter
Synopsis:     The three Locator classes used by File System and Streams
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Define <closable-object> so that <socket>s, which are streams, can
// be closed using the same mechanism as <server-socket>s which aren't
// streams.

define open abstract class <closable-object> (<object>)
end class <closable-object>;

define open abstract class <stream> (<closable-object>)
end class <stream>;

define open abstract class <locator> (<object>)
end class <locator>;

define open abstract class <server-locator> (<locator>)
end class <server-locator>;

define open abstract class <physical-locator> (<locator>) 
end class <physical-locator>;


/// Stream protocols

define open generic close
    (object :: <closable-object>, #rest keys, #key, #all-keys) => ();

// default method so that subclasses can call next method with impunity
define method close (object :: <closable-object>, #key) => ()
  #f
end method close;



/// Base locator protocols

// Open protocols

define open generic supports-open-locator?
    (locator :: <locator>) => (openable? :: <boolean>);

define method supports-open-locator?
    (locator :: <locator>) => (openable? :: <boolean>)
  #f
end method supports-open-locator?;

define open generic open-locator
    (locator :: <locator>, #key, #all-keys) => (stream :: <stream>);


// List protocols

define open generic supports-list-locator?
    (locator :: <locator>) => (listable? :: <boolean>);

define method supports-list-locator?
    (locator :: <locator>) => (listable? :: <boolean>)
  #f
end method supports-list-locator?;

define open generic list-locator
    (locator :: <locator>) => (locators :: <sequence>);
