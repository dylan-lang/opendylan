module: dylan-user

define library make-dylan-app
  use common-dylan;
  use io;
  use system;
  use strings;
end library make-dylan-app;

define module make-dylan-app
  use common-dylan,
    exclude: { format-to-string };
  use format,
    import: { format,
              format-to-string };
  use file-system,
    import: { create-directory,
              with-open-file,
              working-directory };
  use standard-io,
    import: { *standard-error* };
  use locators,
    import: { <directory-locator>,
              <file-locator>,
              merge-locators };
  use strings,
    import: { alphabetic?,
              alphanumeric?,
              decimal-digit? };
end module make-dylan-app;
