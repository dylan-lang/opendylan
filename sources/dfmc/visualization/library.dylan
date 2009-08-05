module: dylan-user
author: Hannes Mehnert
copyright: 2009, all rights reversed
synopsis: Dylan side of graphical visualization of DFM control flow graphs

define library dfmc-visualization
  use dylan;
  use common-dylan;
  use io;
  use network;
  use lisp-reader;
  use system;

  use file-source-records;

  use dfmc-core;
  use dfmc-management;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-debug-back-end;
  use projects;
  use dfmc-environment-projects;
  use dfmc-browser-support;
  use dfmc-reader;

  export dfmc-visualization;
end;

define module dfmc-visualization
  use dylan;
  use dylan-extensions, import: { debug-name };
  use common-dylan, exclude: { format-to-string };
  use threads, import: { dynamic-bind };
  use format;
  use streams;
  use standard-io;
  use print, import: { print-object };
  use sockets;
  use lisp-reader;
  use locators, import: { locator-name };

  use file-source-records, import: { <flat-file-source-record>, source-record-location };

  use dfmc-core;
  use dfmc-management;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-debug-back-end;
  use projects;
  use projects-implementation, import: { project-build-settings, project-current-compilation-context };
  use dfmc-environment-projects; //for with-progress-reporting
  use dfmc-project-compilation, import: { compilation-context-project };
  use dfmc-reader, import: { fragment-name, <name-fragment> };

  export <dfmc-graph-visualization>,
    system-info,
    connect-to-server,
    process-request,
    read-from-visualizer,
    write-to-visualizer;

  export visualizing-compiler;
end;
