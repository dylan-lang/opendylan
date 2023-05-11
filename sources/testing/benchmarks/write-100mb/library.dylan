Module: dylan-user

define library write-100mb
  use common-dylan;
  use io;
  use system;
end library write-100mb;

define module write-100mb
  use common-dylan;
  use file-system;
  use format-out;
  use streams;
end module write-100mb;
