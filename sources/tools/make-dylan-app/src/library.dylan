module: dylan-user

define library make-dylan-app
  use common-dylan;
  use io;
  use system;
  //use command-line-parser;
end library make-dylan-app;

define module make-dylan-app
  use common-dylan, exclude: { format-to-string };
  use format; 
  use file-system;
  use streams;
  use standard-io;
  // use command-line-parser;
  use locators;
end module make-dylan-app;
