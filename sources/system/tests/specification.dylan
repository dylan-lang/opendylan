Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec system ()
  module operating-system;
  module date;
  module locators;
  module file-system;
  module settings;
  module simple-xml;
  suite universal-streams-suite;
  suite additional-streams-suite;
  suite system-regressions;
end library-spec system;

define module-spec date ()
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
  open generic-function \= (<date>, <date>) => (<boolean>);
  open generic-function \< (<date>, <date>) => (<boolean>);
  sealed generic-function date-year (<date>) => (<integer>);
  sealed generic-function date-month (<date>) => (<integer>);
  sealed generic-function date-day (<date>) => (<integer>);
  sealed generic-function date-hours (<date>) => (<integer>);
  sealed generic-function date-minutes (<date>) => (<integer>);
  sealed generic-function date-seconds (<date>) => (<integer>);
  sealed generic-function date-microseconds (<date>) => (<integer>);
  sealed generic-function date-time-zone-offset (<date>) => (<integer>);
  sealed generic-function date-time-zone-offset-setter (<integer>, <date>)
     => (<integer>);
  function date-day-of-week (<date>) => (<day-of-week>);
  //---*** Note that this leaves out the keyword arguments!
  function as-iso8601-string (<date>) => (<string>);
  function current-date () => (<date>);
  function local-time-zone-offset () => (<integer>);
  function local-time-zone-name () => (<string>);
  //---*** Other time zone tools?  (I.e., conversions from/to zone name to/from offset)
  function local-daylight-savings-time? () => (<boolean>);
end module-spec date;


define module-spec locators ()
  // Locators
  open abstract class <locator> (<object>);
  open abstract class <server-locator> (<locator>);
  open abstract class <physical-locator> (<locator>);
  open abstract instantiable class <directory-locator> (<physical-locator>);
  open abstract instantiable class <file-locator> (<physical-locator>);
  sealed instantiable class <native-directory-locator> (<directory-locator>);
  sealed instantiable class <native-file-locator> (<file-locator>);
  open generic-function supports-open-locator? (<locator>) => (<boolean>);
  open generic-function open-locator (<locator>) => (<stream>);
  open generic-function supports-list-locator? (<locator>) => (<boolean>);
  open generic-function list-locator (<locator>) => (<sequence>);
  open generic-function locator-host (<locator>) => (false-or(<string>));
  open generic-function locator-server (<locator>) => (false-or(<server-locator>));
  open generic-function locator-volume (<locator>) => (false-or(<string>));
  open generic-function locator-directory (<locator>) => (false-or(<directory-locator>));
  open generic-function locator-relative? (<locator>) => (<boolean>);
  open generic-function locator-path (<locator>) => (<sequence>);
  open generic-function locator-base (<locator>) => (false-or(<string>));
  open generic-function locator-extension (<locator>) => (false-or(<string>));
  open generic-function locator-name (<locator>) => (false-or(<string>));

  // Locator coercion
  open generic-function locator-as-string
    (subclass(<string>), <locator>) => (<string>);
  open generic-function string-as-locator
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
end module-spec locators;


define module-spec file-system ()
  // Constants
  constant <pathname> :: <type>;
  constant <file-type> :: <type>;
  constant <copy/rename-disposition> :: <type>;

  // File system locators
  open abstract class <file-system-locator> (<locator>);
  open sealed instantiable class <file-system-directory-locator> (<file-system-locator>);
  open sealed instantiable class <file-system-file-locator> (<file-system-locator>);
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
  instantiable class <native-directory-locator> (<directory-locator>);
  instantiable class <native-file-locator> (<file-locator>);

  // Classes
  open abstract instantiable class <file-stream>
    (<buffered-stream>, <positionable-stream>);
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
  generic-function file-property (<pathname>, <symbol>)
    => (<object>);
  generic-function file-property-setter (<object>, <pathname>, <symbol>)
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

  open generic-function type-for-file-stream
    (<object>, false-or(<type>), <object>, #"key", #"all-keys")
    => (subclass(<file-stream>));

  // Macros
  macro-test with-open-file-test;
end module-spec file-system;

define module-spec operating-system ()
  // Operating System constants
  constant $architecture-little-endian? :: <boolean>;
  constant $os-name :: <symbol>;
  constant $os-variant :: <symbol>;
  constant $os-version :: <string>;
  constant $platform-name :: <symbol>;
  constant $machine-name :: <symbol>;

  // Operating System functions
  function login-name () => (false-or(<string>));
  function login-group () => (false-or(<string>));
  function owner-name () => (false-or(<string>));
  function owner-organization () => (false-or(<string>));
  class <application-process> (<object>);
  function run-application 
    (type-union(<string>, limited(<sequence>, of: <string>)),
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

  // Application startup handling
  function application-name () => (false-or(<string>));
  //---*** application-filename should return <file-locator>...
  function application-filename () => (false-or(<string>));
  function application-arguments () => (<sequence>);
  function tokenize-command-string (<string>) => (<sequence>);
  function command-line-option-prefix () => (<character>);
  function exit-application (<integer>) => ();
  function register-application-exit-function (<function>) => ();

  // Environment variables
  function environment-variable (<string>) => (false-or(<string>));
  function environment-variable-setter
    (false-or(<string>), <string>) => (false-or(<string>));
  function tokenize-environment-variable (<string>) => (<sequence>);

  // Macros
  macro-test with-application-output-test;
end module-spec operating-system;

define module-spec settings ()
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
end module-spec settings;

define module-spec simple-xml ()
  // Classes
  sealed instantiable class <xml-error> (<simple-error>);
  sealed instantiable class <xml-document> (<object>);
  sealed instantiable class <xml-node> (<object>);
  sealed instantiable class <xml-element> (<xml-node>);

  // Functions
  function document-location (<xml-document>) =>  (<locator>);
  function document-location-setter (<locator>, <xml-document>) =>  (<locator>);
  function document-element (<xml-document>) =>  (<xml-element>);
  function document-element-setter (<xml-element>, <xml-document>) =>  (<xml-element>);
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
end module-spec simple-xml;
