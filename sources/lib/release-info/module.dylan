Module:    Dylan-User
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module release-info
  // Release information
  create <release-info>,
         <named-release-info>,
         <described-release-info>,
         release-name,
         release-product-name,
         release-edition,
         release-contains-console-tools?,
         release-service-pack,
         release-version,
         release-copyright,
         release-support-address,
         release-web-address,
         release-full-name,
         release-full-copyright;

  // Disk layout information
  create release-directory,
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
  create // info-available?,
         info-author,
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
  create \library-pack-definer,
         <library-pack-info>,
         <basic-library-pack-info>,
         <numbered-library-pack-info>,
         find-library-pack-info,
         installed-library-packs,
	 release-library-packs,
         release-required-library-packs,
	 release-optional-library-packs,
         release-contains-library-pack?,
         library-pack-name,
	 library-pack-full-name,
         library-pack-number,
         library-pack-required?;

  // Merged library DLL information
  export merged-project-name,
         merged-project-libraries;

  // Library category information
  export <library-category-info>,
         installed-library-categories;

  // Library information
  create <library-info>,
         <library-binary-info>,
         <library-release-info>,
         find-library-info;

  // Example information
  create <example-info>;

  // Test suite information
  create <test-suite-info>;

  /*
  // Library group information
  create \library-group-definer,
         <library-group-info>,
         release-library-groups;

  // Dynamic example information
  create invalidate-converted-example-info;

  // Example group information
  create \example-group-definer,
         <example-group-info>,
         release-example-groups;

  // DLL grouping information
  create \dll-group-definer,
         \renamed-dll-definer,
         \renamed-dlls-definer,
         <dll-group-info>,
         find-dll-group-info,
         release-dll-groups;
  */

  // Settings
  create <general-open-dylan-local-settings>,
	 <general-open-dylan-user-settings>,
	 <unversioned-open-dylan-local-settings>,
         <unversioned-open-dylan-user-settings>,
         <open-dylan-local-settings-1-0>,
         <open-dylan-local-settings-1-1>,
         <open-dylan-local-settings-1-2>,
         <open-dylan-local-settings-2-0>,
         <open-dylan-local-settings-2-1>,
	 <open-dylan-local-settings>,
	 <open-dylan-user-settings-1-0>,
	 <open-dylan-user-settings-1-1>,
	 <open-dylan-user-settings-1-2>,
	 <open-dylan-user-settings-2-0>,
	 <open-dylan-user-settings-2-1>,
	 <open-dylan-user-settings>;
end module release-info;

// BOOTSTRAP: required until everyone is bootstrapped to 2.1a1
define module release-info-xml
  create <xml-error>;

  create <xml-document>,
         document-location, document-location-setter,
         document-element, document-element-setter,
         read-xml-document;

  create <xml-node>,
         node-attribute, node-attribute-setter,
         node-attributes,
         node-children,
         node-name,
         node-text, node-text-setter;

  create <xml-element>;

  create select-node-text,
         select-nodes,
         select-single-node;
end module release-info-xml;

define module release-info-internals
  use functional-dylan;
  use machine-words;
  use simple-format;
  use operating-system;
  use locators;
  use settings;
  use release-info, export: all;
  use release-info-xml;		// BOOTSTRAP: should be simple-xml
  use file-source-records, import: {read-file-header};
  use file-system; // import: {file-exists?, do-directory};
  use streams; // import: {\with-open-file};

  // Internal only protocols
  export release-full-version;
end module release-info-internals;
