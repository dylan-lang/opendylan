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

  export dfmc-visualization;
end;

define module dfmc-visualization
  use dylan;
  use common-dylan, exclude: { format-to-string };
  use format;
  use streams;
  use standard-io;
  use sockets;
  use lisp-reader;  

  export <dfmc-graph-visualization>, connect-to-server;
end;
