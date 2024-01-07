Module: dylan-user
Synopsis: Test building an app containing .proto files, compiling the proto files into the using library

define library pbtool-test-one
  use common-dylan;
  use io, import: { format-out };
  use protocol-buffers;
end library;
