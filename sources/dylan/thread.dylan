module:    threads-internal
Synopsis:  The implementation of the <thread> class
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $low-priority         = -1000;
define constant $background-priority  =  -500;
define constant $normal-priority      =     0;
define constant $interactive-priority =   500;
define constant $high-priority        =  1000;


define sealed class <thread> (<portable-double-container>)

  constant slot priority :: <integer>, 
    init-value: $normal-priority, init-keyword: priority:;

  constant slot thread-name :: <optional-name>,
    init-value: #f, init-keyword: name:;
  
  constant slot function :: <function>, 
    required-init-keyword: function:;

  slot function-results :: <simple-object-vector> = #[];

end class;

define sealed class <synchronous-thread> (<thread>)
end class;

define method debug-name (thread :: <thread>)
  thread.thread-name;
end method debug-name;

define method finalize (thread :: <thread>) => ()
  unless (primitive-destroy-thread(thread) == $success)
    error(make(<thread-finalization-error>, thread: thread));
  end;
end method;


define function trampoline-function (thread :: <thread>) => (result :: <function>)
  method () => ()
    primitive-initialize-current-thread
      (thread,
       primitive-boolean-as-raw(instance?(thread, <synchronous-thread>)));
    internal-initialize-thread();
    let (#rest results) = thread.function();
    thread.function-results := as(<simple-object-vector>, results);
    primitive-detach-thread
      (thread);
  end method;
end;


// Special-thread-function is a dummy function object. It's sole purpose 
// is to uniquely identify the very first thread at object initialization
// time - to give the illusion that the first thread was created by this 
// library.
// [actually, now it's also used for foreign threads]
define function special-thread-function () => ()
end;


define sealed method initialize (thr :: <thread>, #key) => ()
  drain-finalization-queue();
  next-method();
  if (thr.function == special-thread-function)
    primitive-initialize-special-thread(thr); // This is the first thread
  else
    let res =
      primitive-make-thread(thr, thr.thread-name, thr.priority, 
			    thr.trampoline-function,
			    primitive-boolean-as-raw
			      (instance?(thr, <synchronous-thread>)));
    if  (res ~= $success)
      let class = select (res)
                    $creation-error => <thread-creation-error>;
                    $priority-error => <thread-priority-error>;
                    otherwise       => <unexpected-thread-error>;
                  end select;
      error(make(class, thread: thr));
    end if;
  end if;
  finalize-when-unreachable(thr);
end method;


define function make-first-thread () => (thread :: <thread>)
  make(<thread>, name: "Master thread", function: special-thread-function);
end;


define variable *master-thread* = #f;

// Don't do this as part of the define variable init, because it's really
// done for side effect. (e.g. it shouldn't be eliminated just because
// the variable is unreferenced)
*master-thread* := make-first-thread();


// make-foreign-thread may be called by the runtime system to 
// create a Dylan <thread> object, and initialize any Dylan thread-local 
// data, when a thread created by foreign code calls into Dylan for the 
// first time.
//
define function make-foreign-thread () => (thread :: <thread>)
  let thread = make(<thread>, name: "Foreign thread", 
                    function: special-thread-function);
  internal-initialize-thread();
  thread
end;

define function join-thread (thread1 :: <thread>, #rest more-threads)
 => (thread :: <thread>, #rest thread-values)
  let signal-join-error 
    = method (res :: <integer>, thread) 
        error(make(<duplicate-join-error>, thread: thread))
      end method;
  let joined-thread
    = if (more-threads.empty?)
        // Join a single thread
        let res = primitive-thread-join-single(thread1);
        if (res == $success)
          thread1
        else signal-join-error(res, thread1);
        end if;
      else
        // Join one of multiple threads
        let thread-vec = apply(vector, thread1, more-threads);
        let res = primitive-thread-join-multiple(thread-vec);
        if (instance?(res, <thread>))
          res
        else signal-join-error(res, thread-vec);
        end if;
      end if;
  apply(values, joined-thread, joined-thread.function-results);
end function join-thread;


define function thread-yield () => ()
  primitive-thread-yield();
end;


define function current-thread () => (thread :: <thread>)
  primitive-current-thread();
end;



///// Sleep

define function sleep (secs :: <real>) => ()
  primitive-sleep(secs.millisecs);
end;
