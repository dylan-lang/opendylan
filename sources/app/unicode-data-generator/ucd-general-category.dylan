Module: unicode-data-generator
Synopsis: UCD general categories
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

/* Every Unicode character can be in a single general category
 *
 * Many important character properties are defined solely based on GC membership.
 */

/* All defined general categories */
define constant $ucd-general-categories = make(<string-table>);

/* Object representing a single general category */
define class <ucd-general-category> (<object>)
  constant slot ucd-gc-abbreviation :: <string>,
    required-init-keyword: abbreviation:;
  constant slot ucd-gc-name :: <string>,
    required-init-keyword: name:;
end class;

define method parse-general-category(abbreviation :: <string>)
  => (gc :: <ucd-general-category>);
  let gc = element($ucd-general-categories, abbreviation);
  unless(gc)                                                                                                                                                                        
    error("Unknown general category %s", abbreviation);
  end;
  gc;
end method;

/* Definition macro for general categories */
define macro ucd-general-category-definer
  { define ucd-general-category
      ?propname:name ( ?abbreviation:expression, ?name:expression )
  } => {
    define constant "$ucd-category-" ## ?propname =
      begin
        let %abbreviation = ?abbreviation;
        let %name = ?name;
        let gc = make(<ucd-general-category>,
                      abbreviation: %abbreviation,
                      name: %name);
        element($ucd-general-categories, %abbreviation) := gc;
      end;
    define method "uc-is-" ## ?propname ## "?" (character :: <ucd-character>)
      => (is? :: <boolean>);
      character.uc-general-category == "$ucd-category-" ## ?propname;
    end method
  }
end macro;

/* Letters */
define ucd-general-category uppercase-letter ("Lu", "Uppercase_Letter");
define ucd-general-category lowercase-letter ("Ll", "Lowercase_Letter");
define ucd-general-category titlecase-letter ("Lt", "Titlecase_Letter");
define ucd-general-category modifier-letter  ("Lm", "Modifier_Letter");
define ucd-general-category other-letter     ("Lo", "Other_Letter");
/* Marks */
define ucd-general-category nonspacing-mark ("Mn", "Nonspacing_Mark");
define ucd-general-category spacing-mark    ("Mc", "Spacing_Mark");
define ucd-general-category enclosing-mark  ("Me", "Enclosing_Mark");

/* Numbers */
define ucd-general-category decimal-number ("Nd", "Decimal_Number");
define ucd-general-category letter-number  ("Nl", "Letter_Number");
define ucd-general-category other-number   ("No", "Other_Number");

/* Punctuation */
define ucd-general-category connector-punctuation ("Pc", "Connector_Punctuation");
define ucd-general-category dash-punctuation      ("Pd", "Dash_Punctuation");
define ucd-general-category open-punctuation      ("Ps", "Open_Punctuation");
define ucd-general-category close-punctuation     ("Pe", "Close_Punctuation");
define ucd-general-category initial-punctuation   ("Pi", "Initial_Punctuation");
define ucd-general-category final-punctuation     ("Pf", "Final_Punctuation");
define ucd-general-category other-punctuation     ("Po", "Other_Punctuation");

/* Symbols */
define ucd-general-category math-symbol     ("Sm", "Math_Symbol");
define ucd-general-category currency-symbol ("Sc", "Currency_Symbol");
define ucd-general-category modifier-symbol ("Sk", "Modifier_Symbol");
define ucd-general-category other-symbol    ("So", "Other_Symbol");

/* Separators */
define ucd-general-category space-separator     ("Zs", "Space_Separator");
define ucd-general-category line-separator      ("Zl", "Line_Separator");
define ucd-general-category paragraph-separator ("Zp", "Paragraph_Separator");

/* Other */
define ucd-general-category control     ("Cc", "Control");
define ucd-general-category format      ("Cf", "Format");
define ucd-general-category surrogate   ("Cs", "Surrogate");
define ucd-general-category private-use ("Co", "Private_Use");
define ucd-general-category unassigned  ("Cn", "Unassigned");

define method uc-is-letter? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-uppercase-letter?
    | c.uc-is-lowercase-letter?
    | c.uc-is-titlecase-letter?
    | c.uc-is-modifier-letter?
    | c.uc-is-other-letter?
end;

define method uc-is-cased-letter? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-uppercase-letter?
    | c.uc-is-lowercase-letter?
    | c.uc-is-titlecase-letter?
end;

define method uc-is-mark? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-nonspacing-mark?
    | c.uc-is-spacing-mark?
    | c.uc-is-enclosing-mark?
end;

define method uc-is-number? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-decimal-number?
    | c.uc-is-letter-number?
    | c.uc-is-other-number?
end;

define method uc-is-punctuation? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-dash-punctuation?
    | c.uc-is-open-punctuation?
    | c.uc-is-close-punctuation?
    | c.uc-is-initial-punctuation?
    | c.uc-is-final-punctuation?
    | c.uc-is-other-punctuation?
end;

define method uc-is-symbol? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-math-symbol?
    | c.uc-is-currency-symbol?
    | c.uc-is-modifier-symbol?
    | c.uc-is-other-symbol?
end;

define method uc-is-separator? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-space-separator?
    | c.uc-is-line-separator?
    | c.uc-is-paragraph-separator?
end;

define method uc-is-other? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-control?
    | c.uc-is-format?
    | c.uc-is-surrogate?
    | c.uc-is-private-use?
    | c.uc-is-unassigned?
end method;
