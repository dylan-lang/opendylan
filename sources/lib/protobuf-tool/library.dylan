Module: dylan-user

define library protobuf-tool
  use protocol-buffers;

  use common-dylan;
  use dylan, import: { simple-debugging };
  use file-source-records;
  use io;
  use system;
  use tools-interface;
end library;

define module protobuf-tool
  use protocol-buffers;

  use common-dylan;
  use date;
  use file-source-records;
  use file-system;
  use format;
  use format-out;
  use locators;
  use simple-debugging, import: { debug-out };
  use streams;
  use tools-interface;
end module;

