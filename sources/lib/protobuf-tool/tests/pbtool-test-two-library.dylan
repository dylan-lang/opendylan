Module: dylan-user
Synopsis: Test building an app containing .proto files, in its own library

define library pbtool-test-two
  use common-dylan;
  use io, import: { format-out };
  use protocol-buffers;
  use pbtool-test-two-pb;       // generated from pbtool-test-two-a.proto
end library;

define module pbtool-test-two
  use common-dylan;
  use format-out;
  use protocol-buffers;
  use pbtool-test-two-pb;       // generated from pbtool-test-two-a.proto
end module;
