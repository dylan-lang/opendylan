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

/* All defined general categories (indexed by their abbreviation) */
define constant $ucd-general-categories = make(<string-table>);

/* Object representing a single general category */
define class <ucd-general-category> (<object>)
  constant slot ucd-gc-abbreviation :: <string>,
    required-init-keyword: abbreviation:;
  constant slot ucd-gc-name :: <string>,
    required-init-keyword: name:;
  constant slot ucd-gc-is-letter? :: <boolean> = #f,
    init-keyword: letter?:;
  constant slot ucd-gc-is-mark? :: <boolean> = #f,
    init-keyword: mark?:;
  constant slot ucd-gc-is-number? :: <boolean> = #f,
    init-keyword: number?:;
  constant slot ucd-gc-is-punctuation? :: <boolean> = #f,
    init-keyword: punctuation?:;
  constant slot ucd-gc-is-symbol? :: <boolean> = #f,
    init-keyword: symbol?:;
  constant slot ucd-gc-is-separator? :: <boolean> = #f,
    init-keyword: separator?:;
  constant slot ucd-gc-is-other? :: <boolean> = #f,
    init-keyword: other?:;
end class;

/* Parses a general category */
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
      ?propname:name ( ?abbreviation:expression, ?name:expression, #rest ?keywords:*)
  } => {
    define constant "$ucd-category-" ## ?propname =
      begin
        let %abbreviation = ?abbreviation;
        let %name = ?name;
        let gc = make(<ucd-general-category>,
                      abbreviation: %abbreviation,
                      name: %name,
                      ?keywords);
        element($ucd-general-categories, %abbreviation) := gc;
      end;
    define method "uc-is-" ## ?propname ## "?" (character :: <ucd-character>)
      => (is? :: <boolean>);
      character.uc-general-category == "$ucd-category-" ## ?propname;
    end method
  }
end macro;

/* Letters */
define ucd-general-category uppercase-letter ("Lu", "Uppercase_Letter", letter?: #t);
define ucd-general-category lowercase-letter ("Ll", "Lowercase_Letter", letter?: #t);
define ucd-general-category titlecase-letter ("Lt", "Titlecase_Letter", letter?: #t);
define ucd-general-category modifier-letter  ("Lm", "Modifier_Letter", letter?: #t);
define ucd-general-category other-letter     ("Lo", "Other_Letter", letter?: #t);

/* Marks */
define ucd-general-category nonspacing-mark ("Mn", "Nonspacing_Mark", mark?: #t);
define ucd-general-category spacing-mark    ("Mc", "Spacing_Mark", mark?: #t);
define ucd-general-category enclosing-mark  ("Me", "Enclosing_Mark", mark?: #t);

/* Numbers */
define ucd-general-category decimal-number ("Nd", "Decimal_Number", number?: #t);
define ucd-general-category letter-number  ("Nl", "Letter_Number", number?: #t);
define ucd-general-category other-number   ("No", "Other_Number", number?: #t);

/* Punctuation */
define ucd-general-category connector-punctuation ("Pc", "Connector_Punctuation", punctuation?: #t);
define ucd-general-category dash-punctuation      ("Pd", "Dash_Punctuation", punctuation?: #t);
define ucd-general-category open-punctuation      ("Ps", "Open_Punctuation", punctuation?: #t);
define ucd-general-category close-punctuation     ("Pe", "Close_Punctuation", punctuation?: #t);
define ucd-general-category initial-punctuation   ("Pi", "Initial_Punctuation", punctuation?: #t);
define ucd-general-category final-punctuation     ("Pf", "Final_Punctuation", punctuation?: #t);
define ucd-general-category other-punctuation     ("Po", "Other_Punctuation", punctuation?: #t);

/* Symbols */
define ucd-general-category math-symbol     ("Sm", "Math_Symbol", symbol?: #t);
define ucd-general-category currency-symbol ("Sc", "Currency_Symbol", symbol?: #t);
define ucd-general-category modifier-symbol ("Sk", "Modifier_Symbol", symbol?: #t);
define ucd-general-category other-symbol    ("So", "Other_Symbol", symbol?: #t);

/* Separators */
define ucd-general-category space-separator     ("Zs", "Space_Separator", separator?: #t);
define ucd-general-category line-separator      ("Zl", "Line_Separator", separator?: #t);
define ucd-general-category paragraph-separator ("Zp", "Paragraph_Separator", separator?: #t);

/* Other */
define ucd-general-category control     ("Cc", "Control", other?: #t);
define ucd-general-category format      ("Cf", "Format", other?: #t);
define ucd-general-category surrogate   ("Cs", "Surrogate", other?: #t);
define ucd-general-category private-use ("Co", "Private_Use", other?: #t);
define ucd-general-category unassigned  ("Cn", "Unassigned", other?: #t);


/* Category group predicates */

define method uc-is-letter? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-letter?
end;
define method uc-is-mark? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-mark?
end;
define method uc-is-number? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-number?
end;
define method uc-is-punctuation? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-punctuation?
end;
define method uc-is-symbol? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-symbol?
end;
define method uc-is-separator? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-separator?
end;
define method uc-is-other? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-general-category.ucd-gc-is-other?
end method;


/* Predicates derived from categories */

define method uc-is-cased-letter? (c :: <ucd-character>)
  => (is? :: <boolean>);
  c.uc-is-uppercase-letter?
    | c.uc-is-lowercase-letter?
    | c.uc-is-titlecase-letter?
end;
