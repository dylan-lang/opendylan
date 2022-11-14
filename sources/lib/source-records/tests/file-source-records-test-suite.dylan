Module: file-source-records-test-suite

define suite file-source-records-test-suite ()
  // header-reader.dylan
  test test-read-header-from-stream/empty-header;
  test test-read-header-from-stream/no-trailing-newline;
  test test-read-header-from-stream/no-continuation-lines;
  test test-read-header-from-stream/continuation-lines;
end suite;
