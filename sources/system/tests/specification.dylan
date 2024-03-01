Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface-specification-suite date-specification-suite ()
  // Classes
  sealed class <date> (<number>);

  // Constants
  constant <day-of-week> :: <type>;

  // Functions

  //---*** Note that these leave out the keyword arguments!
  function encode-date (<integer>, <integer>, <integer>, <integer>,
                        <integer>, <integer>)
           => (<date>);
  function decode-date (<date>)
           => (<integer>, <integer>, <integer>, <integer>,
               <integer>, <integer>, <day-of-week>, <integer>);
  function parse-iso8601-string (<string>) => (<date>);
/* Commented out until https://github.com/dylan-lang/testworks/issues/97 is fixed.
  open generic function \= (<date>, <date>) => (<boolean>);
  open generic function \< (<date>, <date>) => (<boolean>);
 */
  sealed generic function date-year (<date>) => (<integer>);
  sealed generic function date-month (<date>) => (<integer>);
  sealed generic function date-day (<date>) => (<integer>);
  sealed generic function date-hours (<date>) => (<integer>);
  sealed generic function date-minutes (<date>) => (<integer>);
  sealed generic function date-seconds (<date>) => (<integer>);
  sealed generic function date-microseconds (<date>) => (<integer>);
  sealed generic function date-time-zone-offset (<date>) => (<integer>);
  sealed generic function date-time-zone-offset-setter (<integer>, <date>)
     => (<integer>);
  function date-day-of-week (<date>) => (<day-of-week>);
  //---*** Note that this leaves out the keyword arguments!
  function as-iso8601-string (<date>) => (<string>);
  function current-date () => (<date>);
  function local-time-zone-offset () => (<integer>);
  function local-time-zone-name () => (<string>);
  //---*** Other time zone tools?  (I.e., conversions from/to zone name to/from offset)
  function local-daylight-savings-time? () => (<boolean>);
end date-specification-suite;


define interface-specification-suite file-system-locators-specification-suite ()
  // Locators
  open abstract class <server-locator> (<locator>);
  open abstract class <physical-locator> (<locator>);
  open abstract instantiable class <directory-locator> (<physical-locator>);
  open abstract instantiable class <file-locator> (<physical-locator>);
  instantiable class <native-directory-locator> (<directory-locator>);
  instantiable class <native-file-locator> (<file-locator>);
  open generic function locator-host (<locator>) => (false-or(<string>));
  open generic function locator-server (<locator>) => (false-or(<server-locator>));
  open generic function locator-volume (<locator>) => (false-or(<string>));
  open generic function locator-directory (<locator>) => (false-or(<directory-locator>));
  open generic function locator-relative? (<locator>) => (<boolean>);
  open generic function locator-path (<locator>) => (<sequence>);
  open generic function locator-base (<locator>) => (false-or(<string>));
  open generic function locator-extension (<locator>) => (false-or(<string>));
  open generic function locator-name (<locator>) => (false-or(<string>));

  // Locator coercion
  open generic function locator-as-string
    (subclass(<string>), <locator>) => (<string>);
  open generic function string-as-locator
    (subclass(<locator>), <string>) => (<locator>);

  // Locator conditions
  instantiable class <locator-error> (<format-string-condition>, <error>);
  function locator-error (<string>, #"rest") => (#"rest");

  // Utilities
  function relative-locator
    (<physical-locator>, <physical-locator>) => (<physical-locator>);
  function subdirectory-locator
    (<directory-locator>, #"rest") => (<directory-locator>);
  function simplify-locator
    (<physical-locator>) => (<physical-locator>);
  function merge-locators
    (<physical-locator>, <physical-locator>) => (<physical-locator>);

  // Web locators
  abstract class <web-locator> (<locator>);
  abstract class <url> (<web-locator>);
  abstract class <server-url> (<server-locator>, <url>);
  instantiable class <http-server> (<server-url>);
  instantiable class <https-server> (<server-url>);
  instantiable class <ftp-server> (<server-url>);
  instantiable class <file-server> (<server-url>);
  instantiable class <directory-url> (<directory-locator>, <url>);
  instantiable class <file-url> (<file-locator>, <url>);
  abstract class <file-index-url> (<url>);
  abstract class <cgi-url> (<url>);
  abstract class <mail-to-locator> (<web-locator>);
end file-system-locators-specification-suite;


define interface-specification-suite file-system-specification-suite ()
  // Constants
  constant <pathname> :: <type>;
  constant <file-type> :: <type>;
  constant <copy/rename-disposition> :: <type>;

  // File system locators
  open abstract class <file-system-locator> (<locator>);
  sealed instantiable class <file-system-directory-locator> (<file-system-locator>);
  sealed instantiable class <file-system-file-locator> (<file-system-locator>);
  function file-system-separator () => (<character>);

  // Microsoft locators
  abstract class <microsoft-server-locator> (<server-locator>);
  instantiable class <microsoft-unc-locator> (<microsoft-server-locator>);
  instantiable class <microsoft-volume-locator> (<microsoft-server-locator>);
  abstract class <microsoft-file-system-locator> (<file-system-locator>);
  instantiable class <microsoft-directory-locator> (<directory-locator>, <microsoft-file-system-locator>);
  instantiable class <microsoft-file-locator> (<file-locator>, <microsoft-file-system-locator>);

  // Posix locators
  abstract class <posix-file-system-locator> (<file-system-locator>);
  instantiable class <posix-directory-locator> (<directory-locator>);
  instantiable class <posix-file-locator> (<file-locator>);

  // Native locators
  abstract class <native-file-system-locator> (<file-system-locator>);

  // Classes

  open abstract instantiable class <file-stream>
      (<buffered-stream>, <positionable-stream>),
    // Running test test-<file-stream>-specification:
    //   make <file-stream> with required arguments: [Error evaluating assertion
    //   expressions: Assertion failed: can't make test instance of unregistered
    //   stream class {<class>: <file-stream>}] FAILED in 0.000128s and 7KiB
    expected-to-fail-reason: "https://github.com/dylan-lang/opendylan/issues/1295";

  class <file-error> (<error>);
  class <file-exists-error> (<file-error>);
  class <file-does-not-exist-error> (<file-error>);
  class <invalid-file-permissions-error> (<file-error>);

  // Functions

  // +++ g.f. method on make (<file-stream> ...)
  // +++ g.f. method on close (<file-stream>, #"key", #"abort", #"wait?") => ();

  function file-exists? (<pathname>) => (<boolean>);
  function file-type (<pathname>) => (<file-type>);
  function delete-file (<pathname>) => ();
  function copy-file (<pathname>, <pathname>) => ();
  function rename-file (<pathname>, <pathname>) => ();
  function file-properties (<pathname>) => (<explicit-key-collection>);
  // Using <symbol> is suspect for these two?  <object> all we can assume?
  generic function file-property (<pathname>, <symbol>)
    => (<object>);
  generic function file-property-setter (<object>, <pathname>, <symbol>)
    => (<object>);
  function do-directory (<function>, <pathname>) => ();
  // directory-contents is NYI currently.  Change <collection> to something
  // more specific when it is done.  -- carlg 06 May 97
  function directory-contents (<pathname>) => (<collection>);
  function create-directory (<pathname>, <string>) => (<pathname>);
  function delete-directory (<pathname>) => ();
  function ensure-directories-exist (<pathname>) => (<boolean>);
  function home-directory () => (false-or(<pathname>));
  function temp-directory () => (false-or(<pathname>));
  function root-directories () => (<sequence>);

  open generic function type-for-file-stream
    (<object>, false-or(<type>), <object>, #"key", #"all-keys")
    => (subclass(<file-stream>));
end file-system-specification-suite;

define interface-specification-suite operating-system-specification-suite ()
  // Operating System constants
  constant $architecture-little-endian? :: <boolean>;
  constant $os-name :: <symbol>;
  constant $os-variant :: <symbol>;
  constant $os-version :: <string>;
  constant $platform-name :: <symbol>;
  constant $machine-architecture :: <symbol>;
  constant $machine-name :: <symbol>; // Deprecated, use $machine-architecture

  // Operating System functions
  function login-name () => (false-or(<string>));
  function login-group () => (false-or(<string>));
  function owner-name () => (false-or(<string>));
  function owner-organization () => (false-or(<string>));
  class <application-process> (<object>);
  function run-application
    (<sequence>,
     #"key", #"under-shell?", #"inherit-console?", #"activate?",
     #"minimize?", #"hide?", #"outputter", #"asynchronous?",
     #"environment", #"working-directory",
     #"input", #"if-input-does-not-exist",
     #"output", #"if-output-exists", #"error", #"if-error-exists")
    => (<integer>, false-or(<integer>),
        false-or(<application-process>), #"rest");
  function wait-for-application-process
    (<application-process>) => (<integer>, false-or(<integer>));
  function load-library (<string>) => (<object>);
  function current-process-id () => (<integer>);
  function parent-process-id () => (<integer>);
  function machine-concurrent-thread-count () => (<integer>);

  function command-line-option-prefix () => (<character>);

  // Environment variables
  function environment-variable (<string>) => (false-or(<string>));
  function environment-variable-setter
    (false-or(<string>), <string>) => (false-or(<string>));
  function tokenize-environment-variable (<string>) => (<sequence>);

end operating-system-specification-suite;

define interface-specification-suite settings-specification-suite ()
  open abstract primary class <settings> (<object>);
  sealed instantiable class <system-settings> (<settings>);
  sealed instantiable class <site-settings> (<settings>);
  sealed instantiable class <site-software-settings> (<settings>);
  sealed instantiable class <local-settings> (<settings>);
  sealed instantiable class <local-software-settings> (<settings>);
  sealed instantiable class <local-hardware-settings> (<settings>);
  sealed instantiable class <default-user-settings> (<settings>);
  sealed instantiable class <default-user-software-settings> (<settings>);
  sealed instantiable class <current-user-settings> (<settings>);
  sealed instantiable class <current-user-software-settings> (<settings>);
end settings-specification-suite;

define interface-specification-suite simple-xml-specification-suite ()
  // Classes
  sealed instantiable class <xml-error> (<simple-error>);
  sealed instantiable class <xml-document> (<object>);
  sealed instantiable class <xml-node> (<object>);
  sealed instantiable class <xml-element> (<xml-node>);

  // Functions
  function document-location (<xml-document>) =>  (<locator>);
  function document-location-setter (<locator>, <xml-document>) =>  (<locator>);
  function document-element (<xml-document>) => (false-or(<xml-element>));
  function document-element-setter (false-or(<xml-element>), <xml-document>) => (false-or(<xml-element>));
  function read-xml-document (<locator>) =>  (<xml-document>);
  function node-attribute (<xml-element>, <string>) => (false-or(<string>));
  function node-attribute-setter (<string>, <xml-element>, <string>) => (<string>);
  function node-attributes (<xml-element>) => (<string-table>);
  function node-children (<xml-element>) => (<vector>);
  function node-name (<xml-element>) => (<string>);
  function node-text (<xml-element>) => (<string>);
  function node-text-setter (<string>, <xml-element>) => (<string>);
  function select-node-text (<xml-element>, <string>, #"key", #"default") => (<string>);
  function select-nodes (<xml-element>, <string>) => (<vector>);
  function select-single-node (<xml-element>, <string>) => (false-or(<xml-element>));
end simple-xml-specification-suite;

define suite system-test-suite ()
  suite date-specification-suite;
  suite date-test-suite;
  suite file-system-locators-specification-suite;
  suite file-system-locators-test-suite;
  suite file-system-specification-suite;
  suite more-locators-test-suite;
  suite operating-system-specification-suite;
  suite operating-system-test-suite;
  suite settings-specification-suite;
  suite settings-test-suite;
  suite simple-xml-specification-suite;
  suite simple-xml-test-suite;
  suite system-regressions-test-suite;
  suite system-universal-streams-suite;
  suite system-additional-streams-suite;
end suite;
