Module: dylan-user
License: See License.txt in this distribution for details.


define library dfmc-reader-test-suite
  use common-dylan;
  use dfmc-reader;
  use dfmc-common;
  use dfmc-conditions;
  use io;
  use source-records;
  use testworks;

  export dfmc-reader-test-suite;
end library dfmc-reader-test-suite;

define module dfmc-reader-test-suite
  use common-dylan;
  use dfmc-reader;
  use dfmc-common,
    import: { <compilation-record>,
              <interactive-compilation-record> };
  use dfmc-conditions,
    import: { condition-source-location };
  use simple-format;
  use source-records,
    import: { <interactive-source-record>,
              source-location-string,
              source-location-start-line,
              source-location-end-line };
  use streams,
    import: { <string-stream>,
              stream-contents };
  use testworks;

  export dfmc-reader-test-suite;
end module dfmc-reader-test-suite;
