Module: dfmc-reader-test-suite
License: See License.txt in this distribution for details.

define function make-compilation-record
    (source :: <byte-string>)
 => (cr :: <compilation-record>)
  let sr = make(<interactive-source-record>,
                project: #f,
                module: #"scratch",
                source: as(<byte-vector>, source));
  let cr = make(<interactive-compilation-record>,
                library: #f,
                source-record: sr);
  cr
end function make-compilation-record;

define function read-fragment
    (source :: <byte-string>)
 => (fragment :: <fragment>)
  let cr = make-compilation-record(source);
  let (fragment, new-state) = read-top-level-fragment(cr, #f);
  fragment
end function read-fragment;
