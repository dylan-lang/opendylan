module: dylan-user

define library cmu-test-suite
  use dylan;
  use common-dylan;
  export dylan-test;
end;

define module dylan-test
  use common-dylan;
  use simple-format;
  use simple-profiling;

  use dispatch-engine;
  use dylan-incremental;
  use dylan-extensions;
//  use debug-dispatch;
end;

// eof
