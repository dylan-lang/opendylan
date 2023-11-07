Module: dylan-user

define module pbtool-test-one
  use common-dylan;
  use format-out;
  use protocol-buffers;
  use pbtool-test-one-pb;       // generated from pbtool-test-one-{a,b}.proto
end module;
