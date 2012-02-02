module: dylan-user

define library make-dylan-app
  use common-dylan;
  use io;
  use system;
  use string-extensions;
end library make-dylan-app;

define module make-dylan-app
  use common-dylan, exclude: { format-to-string };
  use format; 
  use file-system;
  use streams;
  use standard-io;
  use locators;
  use character-type;
end module make-dylan-app;
