Module: unicode-data-generator
Synopsis: UCD properties
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

define constant $ucd-properties-by-native-name = make(<string-table>);

define class <ucd-property> (<object>)
  constant slot ucd-property-name :: <string>,
    required-init-keyword: name:;
  constant slot ucd-property-type :: <type>,
    required-init-keyword: type:;
  constant slot ucd-property-native-name :: <string>,
    required-init-keyword: native-name:;
  constant slot ucd-property-known? :: <boolean> = #t,
    init-keyword: known?:;
end class;

define class <ucd-derived-property> (<ucd-property>)
end class;

define function ucd-property-by-native-name (native-name :: <string>)
 => ();
  let property = element($ucd-properties-by-native-name, native-name, default: #f);
  unless (property)
    property := make(<ucd-property>,
                     name: native-name,
                     native-name: native-name,
                     type: <string>,
                     known?: #f);
    format-out("\nUnknown property %s\n", native-name);
    element($ucd-properties-by-native-name, native-name) := property;
  end;
  property;
end function;

define macro ucd-property-definer
  { define ucd-property ?name:name ( ?type:expression, ?native-name:expression )
  } => {
    begin
      let %name = ?"name";
      let %type = ?type;
      let %native-name = ?native-name;
      let property = make(<ucd-property>,
                          name: %name,
                          type: %type,
                          native-name: %native-name);
      element($ucd-properties-by-native-name, %native-name) := property;
    end }
end macro;


define ucd-property is-other-alphabetic? (<boolean>, "Other_Alphabetic");
define ucd-property is-other-uppercase?  (<boolean>, "Other_Uppercase");
define ucd-property is-other-lowercase?  (<boolean>, "Other_Lowercase");
define ucd-property is-other-math?       (<boolean>, "Other_Math");

define ucd-property is-whitespace?       (<boolean>, "White_Space");

define ucd-property is-hyphen?  (<boolean>, "Hyphen");
define ucd-property is-dash?    (<boolean>, "Dash");

define ucd-property is-deprecated?  (<boolean>, "Deprecated");

define ucd-property is-hex-digit?  (<boolean>, "Hex_Digit");
define ucd-property is-ascii-hex-digit?  (<boolean>, "ASCII_Hex_Digit");

define ucd-property simple-uppercase-mapping (<ucd-reference>, "Simple_Uppercase_Mapping");
define ucd-property simple-lowercase-mapping (<ucd-reference>, "Simple_Lowercase_Mapping");
define ucd-property simple-titlecase-mapping (<ucd-reference>, "Simple_Titlecase_Mapping");

/*

define ucd-derived-property is-uppercase? :: <boolean> ("Uppercase")
  = conjoin(uc-is-uppercase-letter?,
            uc-is-other-uppercase?);

define ucd-derived-property is-lowercase? :: <boolean> ("Lowercase")
  = conjoin(uc-is-lowercase-letter?,
            uc-is-other-lowercase?);

// this is synthetic since there is no "Other_Titlecase"
define ucd-derived-property is-titlecase? :: <boolean> ("Titlecase")
  = uc-is-titlecase-letter?;

define ucd-derived-property is-cased? :: <boolean> ("Cased")
  = conjoin(uc-is-uppercase?, uc-is-lowercase?, uc-is-titlecase?);

define ucd-derived-property is-alphabetic? :: <boolean> ("Alphabetic")
  = conjoin(uc-is-uppercase-letter?,
            uc-is-lowercase-letter?,
            uc-is-titlecase-letter?,
            uc-is-modifier-letter?,
            uc-is-other-letter?,
            uc-is-other-alphabetic?);
*/
