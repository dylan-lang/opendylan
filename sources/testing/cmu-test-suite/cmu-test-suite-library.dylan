module: dylan-user

define library cmu-test-suite
  use functional-dylan;
//  use debug-dispatch;
  export dylan-test;
end;

define module dylan-test
  use functional-dylan;
  use simple-format;

//gts:  use dispatch-engine;
  use dylan-incremental;
  use dylan-extensions;
//  use debug-dispatch;
end;

// eof
