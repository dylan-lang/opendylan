Module: unicode-data-generator
Synopsis: UCD character representation
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

define class <ucd-character> (<object>)
  slot uc-codepoint :: <integer>,
    required-init-keyword: codepoint:;

  slot uc-name :: <string>;
  slot uc-name-indices :: <sequence>;
  slot uc-general-category :: <string>;

  slot uc-simple-uppercase-mapping :: false-or(<ucd-reference>) = #f;
  slot uc-simple-lowercase-mapping :: false-or(<ucd-reference>) = #f;
  slot uc-simple-titlecase-mapping :: false-or(<ucd-reference>) = #f;


  slot uc-is-letter? :: <boolean> = #f;
  slot uc-is-cased-letter? :: <boolean> = #f;
  slot uc-is-modifier-letter? :: <boolean> = #f;
  slot uc-is-other-letter? :: <boolean> = #f;

  slot uc-is-mark? :: <boolean> = #f;
  slot uc-is-nonspacing-mark? :: <boolean> = #f;
  slot uc-is-spacing-mark? :: <boolean> = #f;
  slot uc-is-enclosing-mark? :: <boolean> = #f;

  slot uc-is-number? :: <boolean> = #f;
  slot uc-is-decimal-number? :: <boolean> = #f;
  slot uc-is-letter-number? :: <boolean> = #f;
  slot uc-is-other-number? :: <boolean> = #f;

  slot uc-is-punctuation? :: <boolean> = #f;
  slot uc-is-connector-punctuation? :: <boolean> = #f;
  slot uc-is-dash-punctuation? :: <boolean> = #f;
  slot uc-is-open-punctuation? :: <boolean> = #f;
  slot uc-is-close-punctuation? :: <boolean> = #f;
  slot uc-is-initial-punctuation? :: <boolean> = #f;
  slot uc-is-final-punctuation? :: <boolean> = #f;
  slot uc-is-other-punctuation? :: <boolean> = #f;

  slot uc-is-symbol? :: <boolean> = #f;
  slot uc-is-math-symbol? :: <boolean> = #f;
  slot uc-is-currency-symbol? :: <boolean> = #f;
  slot uc-is-modifier-symbol? :: <boolean> = #f;
  slot uc-is-other-symbol? :: <boolean> = #f;

  slot uc-is-separator? :: <boolean> = #f;
  slot uc-is-space-separator? :: <boolean> = #f;
  slot uc-is-line-separator? :: <boolean> = #f;
  slot uc-is-paragraph-separator? :: <boolean> = #f;

  slot uc-is-other? :: <boolean> = #f;
  slot uc-is-control? :: <boolean> = #f;
  slot uc-is-format? :: <boolean> = #f;
  slot uc-is-surrogate? :: <boolean> = #f;
  slot uc-is-private-use? :: <boolean> = #f;
  slot uc-is-unassigned? :: <boolean> = #f;

end class;
