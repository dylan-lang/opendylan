Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* template
define method gen-primitive
    (prim-name == #"primitive-",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
*/


define method gen-primitive
    (prim-name == #"primitive-current-thread",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $find-dylanthread-meth$)
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-simple-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-simple-lock-timed",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // lose the time
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-recursive-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-recursive-lock-timed",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // lose the time
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-release-simple-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-synchronize-side-effects",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-raw-expression-leaf (jbb, 0) ;  // not sure if this is meant to push anything
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-make-simple-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the lock
  emit-pop (jbb) ; // pop the name
  emit-raw-expression-leaf (jbb, 0) ;
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-release-semaphore",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-release-recursive-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-release-notification",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the lock
//  emit-pop (jbb) ; // pop the notification, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-release-all-notification",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the lock
//  emit-pop (jbb) ; // pop the notification, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-notification",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the lock
//  emit-pop (jbb) ; // pop the notification, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-semaphore",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//  emit-pop (jbb) ; // pop the sema, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-notification-timed",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the number of ms
  emit-pop (jbb) ; // pop the lock
//  emit-pop (jbb) ; // pop the notification, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-wait-for-semaphore-timed",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the number of ms
//  emit-pop (jbb) ; // pop the sema, push it again
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-destroy-thread",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//  emit-pop (jbb) ; // pop the thread
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-thread-yield",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, #f)
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-sleep",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the ms
  emit-expression-leaf (jbb, #f)
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-thread-join-single",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the other thread
  emit-expression-leaf (jbb, #f)
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-thread-join-multiple",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // pop the vector
  emit-expression-leaf (jbb, #f)
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-owned-simple-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-owned-recursive-lock",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
// DUMMY
define method gen-primitive
    (prim-name == #"primitive-make-notification",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
end;

/*

 primitive-release-semaphore(D s);
 primitive-owned-recursive-lock(D l);
 primitive-destroy-simple-lock(D l);
 primitive-wait-for-semaphore-timed(D s, D ms);
 primitive-wait-for-semaphore(D s);
 primitive-wait-for-simple-lock-timed(D l, D ms);

 primitive-make-recursive-lock(D l, D n);
 primitive-release-recursive-lock(D l);
 primitive-make-semaphore(D l, D n, D i, D m);
 primitive-destroy-recursive-lock(D l);
 primitive-owned-simple-lock(D l);
 primitive-destroy-semaphore(D l);
 primitive-wait-for-recursive-lock-timed(D l, D ms);
 primitive-wait-for-recursive-lock(D l);
 primitive-thread-join-multiple(D v);
 primitive-thread-join-single(D t);
 primitive-initialize-current-thread(D t);
 primitive-initialize-special-thread(D t);
// primitive-current-thread();
 primitive-make-thread(D t, D n, D p, D f);
 primitive-destroy-thread(D t);
 primitive-destroy-notification(D n);
 primitive-release-all-notification(D n, D l);
 primitive-make-notification(D l, D n);
 primitive-wait-for-notification-timed(D n, D l, D ms);
 primitive-wait-for-notification(D n, D l);
// primitive-release-notification(D n, D l);
   primitive-thread-yield();
   primitive-sleep(D ms);
 primitive-make-simple-lock(D l, D n);
 primitive-allocate-thread-variable(D i);
 primitive-read-thread-variable(D h);
 primitive-write-thread-variable(D h, D nv);
   primitive-sequence-point()
*/





define method gen-primitive
    (prim-name == #"primitive-gc-state",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-raw-expression-leaf (jbb, 0)
end;

define method gen-primitive
    (prim-name == #"primitive-mps-ld-reset",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;

define method gen-primitive
    (prim-name == #"primitive-mps-ld-add",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
end;

define method gen-primitive
    (prim-name == #"primitive-mps-ld-merge",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
end;

define method gen-primitive
    (prim-name == #"primitive-mps-ld-isstale",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
  emit-raw-expression-leaf (jbb, 0)
end;

define method gen-primitive
    (prim-name == #"primitive-mps-finalize",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // can call Java for finalization??  Only per class though!
//  emit-pop (jbb) ;
end;

define method gen-primitive
    (prim-name == #"primitive-mps-finalization-queue-first",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-raw-expression-leaf (jbb, 0)
end;

define method gen-primitive
    (prim-name == #"primitive-pin-object",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;

define method gen-primitive
    (prim-name == #"primitive-unpin-object",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb)
end;

define constant $garbage-collect-method$ =
  meth-spec ($java/lang/System$, "gc", 
             meth-type ($java-void-type$),
             j-invokestatic) ;


define method gen-primitive
    (prim-name == #"primitive-mps-collect",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $garbage-collect-method$)
end;

define method gen-primitive
    (prim-name == #"primitive-mps-committed",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // not sure what this needs to return
  emit-raw-expression-leaf (jbb, 0)
end;




// eof
