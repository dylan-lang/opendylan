module: dylan-user

define library remake-dylan-app
  use common-dylan;
  use io;
  use system;
  use command-line-parser;
end library remake-dylan-app;

define module remake-dylan-app
  use common-dylan, exclude: { format-to-string };
  use format; 
  use file-system;
  use streams;
  use standard-io;
  use command-line-parser;
  use locators;
end module remake-dylan-app;
