Module: dylan-user
License: See License.txt in this distribution for details.


define library dfmc-reader-test-suite
  use common-dylan;
  use dfmc-reader;
  use dfmc-common;
  use source-records;
  use testworks;

  export dfmc-reader-test-suite;
end library dfmc-reader-test-suite;

define module dfmc-reader-test-suite
  use common-dylan;
  use dfmc-reader;
  use dfmc-common, import { <interactive-compilation-record> };
  use source-records, import { <interactive-source-record> };
  use testworks;

  export dfmc-reader-test-suite;
end module dfmc-reader-test-suite;
