Module: unicode-data-generator
Synopsis: UCD database representation
Author: Ingo Albrecht <prom@berlin.ccc.de>
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.

/* Root object of a parsed UCD
 */
define class <ucd-database> (<object>)

  /* all defined characters indexed by codepoint */
  constant slot ucd-characters :: <simple-object-vector>
    = make(<simple-object-vector>,
           size: $u-code-count,
           fill: #f);

  /* blocks are named ranges of codepoints */
  slot ucd-blocks :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);

  slot ucd-name-tokens :: <vector>;

end class;


/* Type for to-be-resolved references in the character database
 *
 * NOTE Intended for lazy resolution of codepoint references.
 */
define constant <ucd-reference> = type-union(<integer>, <ucd-character>);


/* Resolves a <ucd-reference>
 */
define method ucd-resolve-reference (ucd :: <ucd-database>, ref :: false-or(<ucd-reference>))
 => (char :: false-or(<ucd-character>));
  let db = ucd-characters(ucd);
  select (ref)
    #f, -1 => #f;
    otherwise =>
      if(instance?(ref, <integer>))
        if(db[ref])
          db[ref];
        else
          error("Could not resolve reference %d\n", ref);
        end;
      else
        ref;
      end;
  end;
end method;

/* Resolves simple case mappings
 */
define method ucd-resolve-simple-case-mappings (ucd :: <ucd-database>)
 => ();
  message("Resolving simple case mappings...");
  let db = ucd-characters(ucd);
  let resolve = curry(ucd-resolve-reference, ucd);
  let cu = 0;
  let cl = 0;
  let ct = 0;
  for (uc in db)
    if (uc)
      uc.uc-simple-uppercase-mapping := resolve(uc.uc-simple-uppercase-mapping);
      if(uc.uc-simple-uppercase-mapping)
        cu := cu + 1;
      end;
      uc.uc-simple-lowercase-mapping := resolve(uc.uc-simple-lowercase-mapping);
      if(uc.uc-simple-lowercase-mapping)
        cl := cl + 1;
      end;
      uc.uc-simple-titlecase-mapping := resolve(uc.uc-simple-titlecase-mapping);
      if(uc.uc-simple-titlecase-mapping)
        ct := ct + 1;
      end;
    end;
  end;
  message(" %d upper, %d lower, %d title\n", cu, cl, ct);
end method;


/* Optimize character names to token references
 */
define method ucd-optimize-names (ucd :: <ucd-database>)
 => ();
  message("Optimizing character names...");
  let db = ucd-characters(ucd);
  let dictindex = 0;
  let dictionary = make(<string-table>);
  local
    method dict(string :: <string>)
     => (index :: <integer>);
      let idx = element(dictionary, string, default: #f);
      if (false?(idx))
        idx := element(dictionary, string) := dictindex;
        dictindex := dictindex + 1;
      end;
      idx;
    end method;
  for (uc in db)
    if (uc)
      let split-name = split(uc.uc-name, " ");
      let indices = map(dict, split-name);
      uc.uc-name-indices := indices;
    end;
  end;
  message(" %d strings\n", dictindex);
  let vector = make(<simple-object-vector>, size: size(dictionary));
  for (string in key-sequence(dictionary))
    let idx = element(dictionary, string);
    vector[idx] := string;
  end;
  ucd-name-tokens(ucd) := vector;
end method;


/* Parsing Blocks.txt
 */

define constant $block-regex = compile-regex("^([0-9A-Fa-f]*)\\.\\.([0-9A-Fa-f]*); (.*)$");

define method ucd-load-blocks (ucd :: <ucd-database>, filename :: <object>)
 => ();
  message("Loading blocks from %s...", filename);
  let blocks = ucd-blocks(ucd);
  let is = make(<file-stream>, locator: filename, direction: #"input");
  iterate again()
    let line = read-line(is, on-end-of-stream: #f);
    if (line)
      let matches = regex-search($block-regex, line, anchored: #t);
      if (matches)
        let s = string-to-integer(match-group(matches, 1), base: 16);
        let e = string-to-integer(match-group(matches, 2), base: 16);
        let name = match-group(matches, 3);
        let blk = make(<ucd-block>, start: s, end: e, name: name);
        add!(blocks, blk);
      end;
      again();
    end;
  end;
  message(" %d blocks\n", size(blocks));
end method;


/* Parsing UnicodeData.txt
 */

define method ucd-load-unicodedata (ucd :: <ucd-database>, filename :: <object>)
 => ();
  message("Loading unicodedata from %s...", filename);
  let db = ucd-characters(ucd);
  let is = make(<file-stream>, locator: filename, direction: #"input");
  let count = 0;
  iterate again()
    let line = read-line(is, on-end-of-stream: #f);
    if (line)
      let fields = split(line, ";");
      if (size(fields) >= 1)
        let codepoint = string-to-integer(fields[0], base: 16);
        if (db[codepoint])
          error("Repeated codepoint %d in unicodedata\n", codepoint);
        end;
        let uc = make(<ucd-character>, codepoint: codepoint);
        count := count + 1;
        db[codepoint] := uc;
        uc.uc-name := fields[1];
        uc.uc-general-category := fields[2];
        uc-set-general-category(uc, fields[2]);
        uc.uc-simple-uppercase-mapping := string-to-integer(fields[12], base: 16, default: -1);
        uc.uc-simple-lowercase-mapping := string-to-integer(fields[13], base: 16, default: -1);
        uc.uc-simple-titlecase-mapping := string-to-integer(fields[14], base: 16, default: -1);
      end;
      again();
    end;
  end;
  message(" %d codepoints\n", count);
end method;

/* Parsing property list files such as PropList.txt
 */

define constant $proplist-value-match = ";\\s*(\\w+)\\s*#";
define constant $proplist-point-regex
  = compile-regex(concatenate("^\\s*([0-9A-Fa-f]+)\\s*", $proplist-value-match));
define constant $proplist-range-regex
  = compile-regex(concatenate("^\\s*([0-9A-Fa-f]+)\\.\\.([0-9A-Fa-f]+)\\s*", $proplist-value-match));

define method ucd-load-proplist (ucd :: <ucd-database>, filename :: <object>)
 => ();
  message("Loading proplist from %s...", filename);
  let is = make(<file-stream>, locator: filename, direction: #"input");
  let pcount = 0;
  let rcount = 0;
  iterate again()
    let line = read-line(is, on-end-of-stream: #f);
    if (line)
      let rmatches = regex-search($proplist-range-regex, line, anchored: #t);
      if (rmatches)
        let rstart = string-to-integer(match-group(rmatches, 1), base: 16);
        let rend = string-to-integer(match-group(rmatches, 2), base: 16);
        let prop = match-group(rmatches, 3);
        rcount := rcount + 1;
        ucd-property-by-native-name(prop);
        //message("\n range %d..%d has %s", rstart, rend, prop);
      else
        let pmatches = regex-search($proplist-point-regex, line, anchored: #t);
        if (pmatches)
          let point = string-to-integer(match-group(pmatches, 1), base: 16);
          let prop = match-group(pmatches, 2);
          pcount := pcount + 1;
          ucd-property-by-native-name(prop);
          //message("\n point %d has %s", point, prop);
        end;
      end;
      again();
    end;
  end;
  message(" %d points, %d ranges\n", pcount, rcount);
end method;
