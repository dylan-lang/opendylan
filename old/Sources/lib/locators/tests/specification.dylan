Module:    locators-test-suite
Synopsis:  Locators library test suite
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The module specifications

define library-spec locators ()
  module locators;
  //---*** Temporarily include this until it is subsumed
  suite old-locators-test-suite;
end library-spec locators;

define module-spec locators ()
  // Variables
  variable *abstract-host-table* :: <object>;

  // Locator classes
  open abstract instantiable class <locator> (<object>);
  abstract instantiable class <file-locator> (<locator>);
  abstract instantiable class <directory-locator> (<locator>);
  instantiable class <abstract-locator> (<locator>);
  abstract class <physical-locator> (<locator>);
  abstract instantiable class <native-locator> (<physical-locator>);
  abstract instantiable class <url-locator> (<physical-locator>);
  //---*** Why don't we have this?
  // abstract instantiable class <macos-locator> (<physical-locator>);
  abstract instantiable class <posix-locator> (<physical-locator>);
  abstract instantiable class <microsoft-locator> (<physical-locator>);

  // Path classes
  abstract class <path> (<object>);
  instantiable class <host-path> (<path>);
  instantiable class <type-path> (<path>);

  // Locator functions
  open generic-function wild-locator? (<locator>) => (<boolean>);
  open generic-function locators-match? (<locator>, <locator>) => (<boolean>);
  open generic-function absolute-locator? (<locator>) => (<boolean>);
  open generic-function relative-locator? (<locator>) => (<boolean>);
  open generic-function translate-locator
      (<locator>, <locator>, <locator>) => (<boolean>);
  open generic-function merge-locators (<locator>, <locator>) => (<locator>);
  open generic-function abbreviate-locator
      (<locator>, <locator>) => (<locator>);
  open generic-function simplify-locator (<locator>) => (<locator>);
  open generic-function override-locator
      (<locator>, #"key", #"directory", #"base", #"type", #"prefix", #"name",
		  #"extension")
   => (<locator>);
  open generic-function default-locator
      (<locator>, #"key", #"directory", #"base", #"type", #"prefix", #"name",
		  #"extension")
   => (<locator>);

  // Host table functions
  function make-abstract-host-table () => (<object>);
  function clear-abstract-host-table (#"key", #"host-table") => ();
  function add-abstract-host (<object>, <object>) => ();
  function remove-abstract-host (<object>, #"key", #"host-table") => ();

  // Locator accessors
  open generic-function locator-scheme (<locator>) => (<object>);
  open generic-function locator-host (<locator>) => (<object>);
  open generic-function locator-port (<locator>) => (<object>);
  open generic-function locator-user-id (<locator>) => (<object>);
  open generic-function locator-password (<locator>) => (<object>);
  open generic-function locator-volume (<locator>) => (<object>);
  open generic-function locator-directory (<locator>) => (<object>);
  open generic-function locator-base (<locator>) => (<object>);
  open generic-function locator-type (<locator>) => (<object>);
  open generic-function locator-version (<locator>) => (<object>);
  open generic-function locator-search-keys (<locator>) => (<object>);
  open generic-function locator-transfer-type (<locator>) => (<object>);
  open generic-function locator-prefix (<locator>) => (<object>);
  open generic-function locator-name (<locator>) => (<object>);
  open generic-function locator-extension (<locator>) => (<object>);
  open generic-function locator-suffix (<locator>) => (<object>);

  // Path accessors
  open generic-function path-elements (<path>) => (<sequence>);
end module-spec locators;
