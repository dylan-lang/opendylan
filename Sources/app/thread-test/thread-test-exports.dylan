module: dylan-user

define library thread-test
  use common-dylan;
  use io;
end library;

define module thread-test
  use common-dylan;
  use threads;
  use streams;
  use standard-io;
  use format-out;
end module;
