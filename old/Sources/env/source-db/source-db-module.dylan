Module: dylan-user
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module source-db
  use dylan;
  use functional-extensions;
  use streams-format;
  use streams-printing;
  use standard-io-streams;
  use internal, import: {read-file-header, maybe-read-file-header};
  use streams, rename: {  count => stream-count,
			  name => stream-name,
			  force-output => stream-force-output,
			  close => stream-close,
			  read => stream-read };
  use stream, prefix: "emulator/";
  use infix-reader;
  use doss;
  use locators, rename: {  translate => locator-translate,
			   default => default-value,
			   base => base-name };
//  use front-end, import: { include-slot-descriptors };
  export
    <db-file-error>,
    <section-reader>,
    reader-stream,
    section-length,
    section-code,
    create-file-reader,
    read-section,
    find-next-section,
    section-id,
    get-sgnature,
    <db-file>,
    describe,
    <metafile>,
    new-branch-from-disk,
    update-from-disk,
    boot-from-disk,
    newest-file,
    <section>,
    <branch>,
    <branched-container>,
    <source-db-error>,
    <no-file-in-branch>,
    <no-file-in-system>,
    <db-source>,
    <db-source-system>,
    find-branch,
    create-branch,
    branch-search-list-setter,
    boot-file,
    find-file,
    update-file-in-branch,
    update-file,
    default-branch,
    create-snapshot,
    open-source-system,
    open-source-snapshot,
    boot-dylan-library,
    update-dylan-library-file,
    update-source-file-sequence,
    update-dylan-library,
    file-code,
    section-text,
    save-source-system,
    retrieve-one-section,
    retrieve-some-sections,
    retrieve-all-sections;
end;

//define module dylan-source
//  use dylan;
//  use internal, import: {read-file-header, maybe-read-file-header};
//  use source-db;
//end;


