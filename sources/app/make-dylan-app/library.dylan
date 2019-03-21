module: dylan-user
synopsis: Generate the boilerplate for a new Dylan library.

define library make-dylan-app
  use common-dylan;
  use io;
  use regular-expressions;
  use system;
end library;

define module make-dylan-app
  use common-dylan;
  use format-out,
    import: { format-err };
  use format,
    import: { format,
              format-to-string };
  use file-system,
    import: { create-directory,
              with-open-file,
              working-directory };
  use locators,
    import: { <directory-locator>,
              <file-locator>,
              merge-locators };
  use regular-expressions,
    import: { compile-regex, regex-pattern, regex-search };
end module;
