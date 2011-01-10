Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test corba-services-idl-Concurrency ()
  check("", test-idl-file, *corba-services-files*, "Concurrency");
end test;

add-idl-file!(
  *corba-services-files*,
  "Concurrency",
"// CosConcurrency Control Module, p 7-8 CORBAservices, \n"
"// Concurrency Control Service V1.0, 3/94\n"
"\n"
"#ifndef COSCONCURRENCYCONTROL\n"
"#define COSCONCURRENCYCONTROL\n"
"#include <CosTransactions.idl>\n"
"module CosConcurrencyControl {\n"
"\n"
"    enum lock_mode {\n"
"        read,\n"
"        write,\n"
"        upgrade,\n"
"        intention_read,\n"
"        intention_write\n"
"    };\n"
"\n"
"    exception LockNotHeld{};\n"
"\n"
"    interface LockCoordinator\n"
"    {\n"
"\t        void drop_locks();\n"
"    };\n"
"\n"
"    interface LockSet\n"
"    {\n"
"        void lock(in lock_mode mode);\n"
"        boolean try_lock(in lock_mode mode);\n"
"\n"
"        void unlock(in lock_mode mode)\n"
"            raises(LockNotHeld);\n"
"        void change_mode(in lock_mode held_mode,\n"
"                         in lock_mode new_mode)\n"
"           raises(LockNotHeld);\n"
"        LockCoordinator get_coordinator(\n"
"            in CosTransactions::Coordinator which);\n"
"    };\n"
"\n"
"    interface TransactionalLockSet\n"
"    {\n"
"        void lock(in CosTransactions::Coordinator current,\n"
"                  in lock_mode mode);\n"
"        boolean try_lock(in CosTransactions::Coordinator current,\n"
"                         in lock_mode mode);\n"
"        void unlock(in CosTransactions::Coordinator current,\n"
"                    in lock_mode mode)\n"
"           raises(LockNotHeld);\n"
"        void change_mode(in CosTransactions::Coordinator current,\n"
"                         in lock_mode held_mode,\n"
"                         in lock_mode new_mode)\n"
"           raises(LockNotHeld);\n"
"        LockCoordinator get_coordinator(\n"
"            in CosTransactions::Coordinator which);\n"
"    };\n"
"\n"
"    interface LockSetFactory\n"
"    {\n"
"        LockSet create();\n"
"        LockSet create_related(in LockSet which);\n"
"        TransactionalLockSet create_transactional();\n"
"        TransactionalLockSet create_transactional_related(in\n"
"            TransactionalLockSet which);\n"
"    };\n"
"};\n"
"#endif\n"
"\n");
