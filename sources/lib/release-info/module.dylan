Module:    Dylan-User
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module release-info
  use common-dylan;
  use machine-words;
  use operating-system;
  use locators;
  use settings;
  use simple-io;
  use simple-xml;
  use file-source-records, import: {read-file-header};
  use file-system; // import: {file-exists?, do-directory};
  use streams; // import: {\with-open-file};

  // Release information
  export <release-info>,
         <named-release-info>,
         <described-release-info>,
         release-product-name,
         release-version,
         release-short-version,
         release-copyright,
         release-web-address,
         release-full-name,
         release-full-copyright;

  // Disk layout information
  export release-directory,
         release-subdirectory,
         release-file,
         release-runtime-directory,
         release-examples-directory,
         release-sources-directory,
         release-library-packs-directory,
         release-templates-directory,
         release-source-templates-directory,
         release-license-agreement-location,
         release-help-location,
         release-bug-report-template-location;

  // Information accessors
  export info-author,
         info-binary,
         info-categories,
         info-company,
         info-copyright,
         info-database,
         info-description,
         info-examples,
         info-binary-name,
         info-lib,
         info-libraries,
         info-library-pack,
         info-location,
         info-manual,
         info-merged-libraries,
         info-merge-parent,
         info-modules,
         info-name,
         info-platform,
         info-product,
         info-project,
         info-relative-location,
         info-releases,
         info-requires,
         info-source-directory,
         info-subcategories,
         info-test-suites,
         info-title,
         info-version;

  // Library Pack information
  export \library-pack-definer,
         <library-pack-info>,
         <basic-library-pack-info>,
         <numbered-library-pack-info>,
         find-library-pack-info,
         installed-library-packs,
         release-contains-library-pack?,
         library-pack-name,
         library-pack-full-name,
         library-pack-number;

  // Merged library DLL information
  export merged-project-name,
         merged-project-libraries;

  // Library category information
  export <library-category-info>,
         installed-library-categories;

  // Library information
  export <library-info>,
         <library-binary-info>,
         <library-release-info>,
         find-library-info;

  // Example information
  export <example-info>;

  // Test suite information
  export <test-suite-info>;

  /*
  // Library group information
  export \library-group-definer,
         <library-group-info>,
         release-library-groups;

  // Dynamic example information
  export invalidate-converted-example-info;

  // Example group information
  export \example-group-definer,
         <example-group-info>,
         release-example-groups;

  // DLL grouping information
  export \dll-group-definer,
         \renamed-dll-definer,
         \renamed-dlls-definer,
         <dll-group-info>,
         find-dll-group-info,
         release-dll-groups;
  */

  // Settings
  export <open-dylan-local-settings>,
         <open-dylan-user-settings>;
end module release-info;
