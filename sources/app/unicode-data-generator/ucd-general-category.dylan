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

/* Definition macro for general categories */
define macro ucd-general-category-definer
  { define ucd-general-category
      ?propname:name ( ?abbreviation:expression, ?name:expression )
  } => {
    begin
      let %abbreviation = ?abbreviation;
      let %name = ?name;
      let gc = make(<ucd-general-category>,
                    abbreviation: %abbreviation,
                    name: %name);
      element($ucd-general-categories, %name) := gc;
    end
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


define method uc-set-general-category (uc :: <ucd-character>, gc :: <string>)
 => ();
  select (gc by \=)
    "Lm", "Lo", "Lu", "Ll", "Lt" =>
      uc.uc-is-letter? := #t;
      select (gc by \=)
        "Lu", "Ll", "Lt" => uc.uc-is-cased-letter? := #t;
        "Lm" =>  uc.uc-is-modifier-letter? := #t;
        "Lo" => uc.uc-is-other-letter? := #t;
      end;
    "Mn", "Mc", "Me" =>
      uc.uc-is-mark? := #t;
      select (gc by \=)
        "Mn" => uc.uc-is-nonspacing-mark? := #t;
        "Mc" => uc.uc-is-spacing-mark? := #t;
        "Me" => uc.uc-is-enclosing-mark? := #t;
      end;
    "Nd", "Nl", "No" =>
      uc.uc-is-number? := #t;
      select (gc by \=)
        "Nd" => uc.uc-is-decimal-number? := #t;
        "Nl" => uc.uc-is-letter-number? := #t;
        "No" => uc.uc-is-other-number? := #t;
      end;
    "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po" =>
      uc.uc-is-punctuation? := #t;
      select (gc by \=)
        "Pc" => uc.uc-is-connector-punctuation? := #t;
        "Pd" => uc.uc-is-dash-punctuation? := #t;
        "Ps" => uc.uc-is-open-punctuation? := #t;
        "Pe" => uc.uc-is-close-punctuation? := #t;
        "Pi" => uc.uc-is-initial-punctuation? := #t;
        "Pf" => uc.uc-is-final-punctuation? := #t;
        "Po" => uc.uc-is-other-punctuation? := #t;
      end;
    "Sm", "Sc", "Sk", "So" =>
      uc.uc-is-symbol? := #t;
      select (gc by \=)
        "Sm" => uc.uc-is-math-symbol? := #t;
        "Sc" => uc.uc-is-currency-symbol? := #t;
        "Sk" => uc.uc-is-modifier-symbol? := #t;
        "So" => uc.uc-is-other-symbol? := #t;
      end;
    "Zs", "Zl", "Zp" =>
      uc.uc-is-separator? := #t;
      select (gc by \=)
        "Zs" => uc.uc-is-space-separator? := #t;
        "Zl" => uc.uc-is-line-separator? := #t;
        "Zp" => uc.uc-is-paragraph-separator? := #t;
      end;
    "Cc", "Cf", "Cs", "Co", "Cn" =>
      uc.uc-is-other? := #t;
      select (gc by \=)
        "Cc" => uc.uc-is-control? := #t;
        "Cf" => uc.uc-is-format? := #t;
        "Cs" => uc.uc-is-surrogate? := #t;
        "Co" => uc.uc-is-private-use? := #t;
        "Cn" => uc.uc-is-unassigned? := #t;
      end;
    otherwise =>
      error("Unknown general category %s for codepoint %d",
            gc,
            uc.uc-codepoint);
  end;
end method;
