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

  slot uc-general-category :: <ucd-general-category>;

  slot uc-simple-uppercase-mapping :: false-or(<ucd-reference>) = #f;
  slot uc-simple-lowercase-mapping :: false-or(<ucd-reference>) = #f;
  slot uc-simple-titlecase-mapping :: false-or(<ucd-reference>) = #f;

end class;
