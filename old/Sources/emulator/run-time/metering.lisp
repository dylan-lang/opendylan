;;; -*- Mode: LISP; Package: monitor; Syntax: Common-lisp; Base: 10.;  -*- 
;;; Tue Jan 25 18:32:28 1994 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; metering.cl -- 56711 bytes

;;; ****************************************************************
;;; Metering System ************************************************
;;; ****************************************************************
;;;
;;; The Metering System is a portable Common Lisp code profiling tool.
;;; It gathers timing and consing statistics for specified functions 
;;; while a program is running.
;;;
;;; The Metering System is a combination of 
;;;   o  the Monitor package written by Chris McConnell
;;;   o  the Profile package written by Skef Wholey and Rob MacLachlan
;;; The two systems were merged and extended by Mark Kantrowitz.
;;; 
;;; Address: Carnegie Mellon University
;;;          School of Computer Science
;;;          Pittsburgh, PA 15213
;;;
;;; This code is in the public domain and is distributed without warranty
;;; of any kind. 
;;;
;;; Bug reports, comments, and suggestions should be sent to mkant@cs.cmu.edu.
;;; 
;;; 

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; 26-JUN-90  mk       Merged functionality of Monitor and Profile packages.
;;; 26-JUN-90  mk       Now handles both inclusive and exclusive statistics
;;;                     with respect to nested calls. (Allows it to subtract
;;;                     total monitoring overhead for each function, not just
;;;                     the time spent monitoring the function itself.)
;;; 26-JUN-90  mk       The table is now saved so that one may manipulate
;;;                     the data (sorting it, etc.) even after the original
;;;                     source of the data has been cleared.
;;; 25-SEP-90  mk       Added get-cons functions for Lucid 3.0, MACL 1.3.2
;;;                     required-arguments functions for Lucid 3.0,
;;;                     Franz Allegro CL, and MACL 1.3.2.
;;; 25-JAN-91  mk       Now uses fdefinition if available.
;;; 25-JAN-91  mk       Replaced (and :allegro (not :coral)) with :excl.
;;;                     Much better solution for the fact that both call 
;;;                     themselves :allegro.
;;;  5-JUL-91 mk        Fixed warning to occur only when file is loaded 
;;;                     uncompiled.
;;;  5-JUL-91 mk        When many unmonitored functions, print out number 
;;;                     instead of whole list.
;;; 24-MAR-92 mk        Updated for CLtL2 compatibility. space measuring 
;;;                     doesn't work in MCL, but fixed so that timing 
;;;                     statistics do.
;;; 26-MAR-92 mk        Updated for Lispworks. Replaced :ccl with 
;;;                     (and :ccl (not :lispworks)).
;;; 27-MAR-92 mk        Added get-cons for Allegro-V4.0.
;;; 06-NOV-92 kab&drw	Added get-cons for MCL 2.0. Fixed with-monitoring to 
;;;			return values of last form. Fixed monitoring of (setf xxx)
;;;			functions.
;;; 01-JAN-93 mk  v2.0  Support for MCL 2.0, CMU CL 16d, Allegro V3.1/4.0/4.1, 
;;;                     Lucid 4.0, ibcl
;;; 26-JAN-93 dam       Fix column alignment in the report so I can read it.
;;;                     Explain overhead better in the report.
;;;                     Make total-calls, -time, and -cons be the real totals for the run
;;;                     instead of just the totals of what was shown.  Use the real totals
;;;                     to compute percents, show both kinds of totals in the report.  This involved
;;;                     adding some arguments to report-monitoring.
;;;                     Fixed some code malformatting caused by indiscriminate meta-Q.
;;;                     Add dynamic-extent declaration for rest argument in monitoring encapsulations.
;;;                     Add *monitoring-enabled* flag so we don't monitor the compilations
;;;                     involved in installing the monitors!
;;;                     Call set-monitor-overhead the first time we need it, not at
;;;                     load time, to avoid problems when the build server is a different
;;;                     speed from the user machine.
;;;                     Change monitor-form and with-monitoring to take keyword arguments
;;;                     instead of a million confusing optionals.
;;;                     Add backtrace collection (MCL 2.0 implementation only).
;;; 25-OCT-94 kab	Changed with-monitoring to evaluate the list of places to monitor,
;;;			because that seems to be more useful in practice.
;;;			Changed with-monitoring and monitor-form to use a common helper
;;;			that is careful about order of evaluation of the various options,
;;;			and allows packages (and keywords that name packages) as additional
;;;			specifications of places to monitor.
;;;
;;; 25-JAN-94 mk  v2.1  Patches for CLISP from Bruno Haible.

;;;

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;;    - Need get-cons for Allegro, AKCL.
;;;    - Speed up monitoring code. Replace use of hash tables with an embedded
;;;      offset in an array so that it will be faster than using gethash. 
;;;      (i.e., svref/closure reference is usually faster than gethash).
;;;    - Beware of (get-internal-run-time) overflowing. Yikes!
;;;    - Check robustness with respect to profiled functions.
;;;    - Check logic of computing inclusive and exclusive time and consing.
;;;      Especially wrt incf/setf comment below. Should be incf, so we
;;;      sum recursive calls.
;;;    - Add option to record caller statistics -- this would list who
;;;      called which functions and how often.
;;;    - switches to turn timing/CONSING statistics collection on/off.
;;;    - implement backtrace collection for other than MCL.


;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    METERING has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (16d, Python Compiler 1.0 ) :new-compiler
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90) 
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       Macintosh Common Lisp (2.0)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 11/19/90) :allegro-v3.1
;;;       ExCL (Franz Allegro CL 4.0.1 [Sun4] 2/8/91)       :allegro-v4.0
;;;       ExCL (Franz Allegro CL 4.1 [SPARC R1] 8/28/92 14:06) :allegro-v4.1
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;       Lucid Common Lisp (3.0)
;;;       Lucid Common Lisp (4.0.1 HP-700 12-Aug-91)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       Ibuki Common Lisp (Version 2, release 01.027)
;;;       CLISP (January 1994)
;;;
;;;    METERING needs to be tested in the following lisps:
;;;       Symbolics Common Lisp (8.0)
;;;       KCL (June 3, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       VAXLisp (2.0, 3.1)
;;;       Procyon Common Lisp


;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; This system runs in any valid Common Lisp. Four small 
;;; implementation-dependent changes can be made to improve performance
;;; and prettiness. In the section labelled "Implementation Dependent
;;; Changes" below, you should tailor the functions REQUIRED-ARGUMENTS,
;;; GET-CONS, GET-TIME, and TIME-UNITS-PER-SECOND to your implementation
;;; for the best results. If GET-CONS is not specified for your 
;;; implementation, no consing information will be reported. The other
;;; functions will default to working forms, albeit inefficient, in
;;; non-CMU implementations. If you tailor these functions for a particular
;;; version of Common Lisp, we'd appreciate receiving the code.
;;;

;;; ****************************************************************
;;; Usage Notes ****************************************************
;;; ****************************************************************
;;; 
;;; SUGGESTED USAGE:
;;; 
;;; Start by monitoring big pieces of the program, then carefully choose
;;; which functions close to, but not in, the inner loop are to be 
;;; monitored next. Don't monitor functions that are called by other
;;; monitored functions: you will only confuse yourself.
;;;
;;; If the per-call time reported is less than 1/10th of a second, then
;;; consider the clock resolution and profiling overhead before you believe
;;; the time. It may be that you will need to run your program many times
;;; in order to average out to a higher resolution.
;;;
;;; The easiest way to use this package is to load it and execute either
;;;     (mon:with-monitoring '(names*) ()
;;;         your-forms*)
;;; or                      
;;;     (mon:monitor-form your-form)
;;; The former allows you to specify which functions and packages will be
;;; monitored; the latter monitors all functions in the current package.
;;; Both automatically produce a table of statistics. Other variants can
;;; be constructed from the monitoring primitives, which are described
;;; below, along with a fuller description of these two macros.
;;; 
;;; For best results, compile this file before using.
;;;
;;; 
;;; CLOCK RESOLUTION:
;;; 
;;; Unless you are very lucky, the length of your machine's clock "tick" is
;;; probably much longer than the time it takes a simple function to run.
;;; For example, on the IBM RT, the clock resolution is 1/50th of a second.
;;; This means that if a function is only called a few times, then only the
;;; first couple of decimal places are really meaningful.
;;;
;;; 
;;; MONITORING OVERHEAD:
;;;
;;; The added monitoring code takes time to run every time that the monitored
;;; function is called, which can disrupt the attempt to collect timing 
;;; information. In order to avoid serious inflation of the times for functions
;;; that take little time to run, an estimate of the overhead due to monitoring
;;; is subtracted from the times reported for each function. 
;;; 
;;; Although this correction works fairly well, it is not totally accurate,
;;; resulting in times that become increasingly meaningless for functions
;;; with short runtimes. For example, subtracting the estimated overhead
;;; may result in negative times for some functions. This is only a concern
;;; when the estimated profiling overhead is many times larger than 
;;; reported total CPU time.
;;;
;;; If you monitor functions that are called by monitored functions, in
;;; :inclusive mode the monitoring overhead for the inner function is
;;; subtracted from the CPU time for the outer function. [We do this by
;;; counting for each function not only the number of calls to *this*
;;; function, but also the number of monitored calls while it was running.]
;;; In :exclusive mode this is not necessary, since we subtract the
;;; monitoring time of inner functions, overhead & all.
;;;
;;; Otherwise, the estimated monitoring overhead is not represented in the
;;; reported total CPU time. The sum of total CPU time and the estimated
;;; monitoring overhead should be close to the total CPU time for the
;;; entire monitoring run (as determined by TIME).
;;;
;;;
;;; If your times vary widely, possible causes are:
;;;    - Garbage collection.  Try turning it off, then running your code.
;;;      Be warned that monitoring code will probably cons when it does
;;;      (get-internal-run-time).
;;;    - Swapping.  If you have enough memory, execute your form once
;;;      before monitoring so that it will be swapped into memory. Otherwise,
;;;      get a bigger machine! 
;;;    - Resolution of internal-time-units-per-second.  If this value is
;;;      too low, then the timings become wild. You can try executing more
;;;      of whatever your test is, but that will only work if some of your
;;;      paths do not match the timer resolution. 
;;;      internal-time-units-per-second is so coarse -- on a Symbolics it is
;;;      977, in MACL it is 60.
;;;
;;;

;;; ****************************************************************
;;; Interface ******************************************************
;;; ****************************************************************
;;;
;;; WITH-MONITORING places                                    [Macro]
;;;                 (&key (nested :exclusive) 
;;;                       (threshold 0.01)
;;;                       (key :percent-time))
;;;                       (backtrace nil))
;;;                 &body body
;;; The named functions will be set up for monitoring, the body forms executed,
;;; a table of results printed, and the functions unmonitored. The nested,
;;; threshold, and key arguments are passed to report-monitoring below.
;;;
;;; MONITOR-FORM form                                         [Macro]
;;;               &key (nested :exclusive)
;;;                    (threshold 0.01)
;;;                    (key :percent-time)
;;;                    (backtrace nil)
;;; All functions in the current package are set up for monitoring while
;;; the form is executed, and automatically unmonitored after a table of
;;; results has been printed. The nested, threshold, and key arguments 
;;; are passed to report-monitoring below.
;;; If the backtrace argument is true, patterns in the callers of monitored
;;; functions are collected and reported.  This makes the monitoring much
;;; slower and the reported times somewhat less accurate.
;;;
;;; *MONITORED-FUNCTIONS*                                     [Variable]
;;; This holds a list of all functions that are currently being monitored.
;;;
;;; MONITOR &rest names                                       [Macro]
;;; The named functions will be set up for monitoring by augmenting
;;; their function definitions with code that gathers statistical information
;;; about code performance. As with the TRACE macro, the function names are
;;; not evaluated. Calls the function MON::MONITORING-ENCAPSULATE on each
;;; function name. If no names are specified, returns a list of all 
;;; monitored functions.
;;; 
;;; If name is not a symbol, it is evaled to return the appropriate
;;; closure. This allows you to monitor closures stored anywhere like
;;; in a variable, array or structure. Most other monitoring packages 
;;; can't handle this. 
;;;
;;; MONITOR-ALL &optional (package *package*)                 [Function]
;;; Monitors all functions in the specified package, which defaults to
;;; the current package.
;;;
;;; UNMONITOR &rest names                                     [Macro]
;;; Removes monitoring code from the named functions. If no names are
;;; specified, all currently monitored functions are unmonitored.
;;;
;;; RESET-MONITORING-INFO name                                [Function]
;;; Resets the monitoring statistics for the specified function.
;;; 
;;; RESET-ALL-MONITORING                                      [Function]
;;; Resets the monitoring statistics for all monitored functions.
;;;
;;; MONITORED name                                            [Function]
;;; Predicate to test whether a function is monitored.
;;; 
;;; REPORT-MONITORING &optional names                         [Function]
;;;                             (nested :exclusive) 
;;;                             (threshold 0.01)
;;;                             (key :percent-time)
;;;                             ignore-no-calls
;;;                             run-time
;;;                             run-cons
;;;                             run-gc
;;;                             start-gc
;;; Creates a table of monitoring information for the specified list
;;; of names, and displays the table using display-monitoring-results.
;;; If names is :all or nil, uses all currently monitored functions.
;;; Takes the following arguments:
;;;    - NESTED specifies whether nested calls of monitored functions
;;;      are included in the times for monitored functions.
;;;      o  If :inclusive, the per-function information is for the entire
;;;         duration of the monitored function, including any calls to
;;;         other monitored functions. If functions A and B are monitored,
;;;         and A calls B, then the accumulated time and consing for A will
;;;         include the time and consing of B.  Note: if a function calls
;;;         itself recursively, the time spent in the inner call(s) may
;;;         be counted several times.
;;;      o  If :exclusive, the information excludes time attributed to
;;;         calls to other monitored functions. This is the default.
;;;    - THRESHOLD specifies that only functions which have been executed
;;;      more than threshold percent of the time will be reported. Defaults
;;;      to 1%. If a threshold of 0 is specified, all functions are listed,
;;;      even those with 0 or negative running times (see note on overhead).
;;;    - KEY specifies that the table be sorted by one of the following
;;;      sort keys:
;;;         :function       alphabetically by function name
;;;         :percent-time   by percent of total execution time
;;;         :percent-cons   by percent of total consing
;;;         :calls          by number of times the function was called
;;;         :time-per-call  by average execution time per function
;;;         :cons-per-call  by average consing per function
;;;         :time           same as :percent-time
;;;         :cons           same as :percent-cons
;;;    - IGNORE-NO-CALLS is true to inhibit listing functions not called
;;;    - RUN-TIME and RUN-CONS, if non-NIL, are the total time and consing
;;;      consumed by this run.  This is useful if not all the time is spent
;;;      in monitored functions, i.e. the top-level function in the form
;;;      that was measured is not itself monitored.
;;;    - RUN-GC, garbage collection time during the run
;;;    - START-GC, value of get-gc before run, for backtrace validity checking
;;;
;;; DISPLAY-MONITORING-RESULTS &optional (threshold 0.01)     [Function]
;;;                                      (key :percent-time)
;;;                                      (ignore-no-calls t)
;;;                                      (nested :exclusive)
;;;                                      run-calls
;;;                                      run-time
;;;                                      run-consed
;;; Prints a table showing for each named function:
;;;    - the total CPU time used in that function for all calls
;;;    - the total number of bytes consed in that function for all calls
;;;    - the total number of calls
;;;    - the average amount of CPU time per call
;;;    - the average amount of consing per call
;;;    - the percent of total execution time spent executing that function
;;;    - the percent of total consing spent consing in that function
;;; Summary totals of the CPU time, consing, and calls columns are printed.
;;; An estimate of the monitoring overhead is also printed. May be run
;;; even after unmonitoring all the functions, to play with the data.
;;;
;;; SAMPLE TABLE:
#|
                                               Cons
                 %     %                       Per      Total   Total
Function         Time  Cons  Calls  Sec/Call   Call     Time    Cons
----------------------------------------------------------------------
FIND-ROLE:       0.58  0.00    136  0.003521      0  0.478863       0
GROUP-ROLE:      0.35  0.00    365  0.000802      0  0.292760       0
GROUP-PROJECTOR: 0.05  0.00    102  0.000408      0  0.041648       0
FEATURE-P:       0.02  0.00    570  0.000028      0  0.015680       0
----------------------------------------------------------------------
TOTAL:                        1173                   0.828950       0
Estimated total monitoring overhead: 0.88 seconds
|#

;;; ****************************************************************
;;; METERING *******************************************************
;;; ****************************************************************

;;; ********************************
;;; Fix up the *features* list *****
;;; ********************************

(eval-when (compile load eval)
  ;; The *features* list for Macintosh Allegro Common Lisp 1.3.2
  ;; isn't really unambiguous, so we add the :mcl1.3.2 feature.
  (when (or (and (string-equal (lisp-implementation-type)
			       "Macintosh Allegro Common Lisp")
		 (string-equal (lisp-implementation-version)
			       "1.3.2"))
	    (and (find :ccl *features*)
		 (not (find :lispworks *features*))
		 (not (find :mcl *features*))))
    (pushnew :mcl1.3.2 *features*))
  ;; We assume that :mcl means version 2.0 or greater. If it doesn't,
  ;; use :mcl2.0 which is defined by:
  (when (or (and (string-equal (lisp-implementation-type)
			       "Macintosh Common Lisp")
		 (string-equal (lisp-implementation-version)
			       "Version 2.0"))
	    (and (find :ccl *features*)
		 (find :ccl-2 *features*)
		 (not (find :lispworks *features*))
		 (find :mcl *features*)))
    (pushnew :mcl2.0 *features*))
  )

;;; Let's be smart about CLtL2 compatible Lisps:
(eval-when (compile load eval)
  #+(or (and :excl (or :allegro-v4.0 :allegro-v4.1)) 
	:mcl
	(and :cmu :new-compiler)
	:Harlequin-common-lisp)
  (pushnew :cltl2 *features*))

;;; ********************************
;;; Packages ***********************
;;; ********************************

#-:cltl2
(in-package "MONITOR" :nicknames '("MON"))

;;; For CLtL2 compatible lisps

#+(and :excl (or :allegro-v4.0 :allegro-v4.1))
(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP") 
  (:import-from cltl1 provide require))
#+:mcl
(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP") 
  (:import-from ccl provide require))
#+:Harlequin-common-lisp
(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP") 
  (:import-from lw provide require))
#+(and :cmu :new-compiler)
(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP"))
#+(and :cltl2
       (not (or (and :excl (or :allegro-v4.0 :allegro-v4.1))
		:mcl
		(and :cmu :new-compiler)
                :Harlequin-common-lisp)))
(unless (find-package "MONITOR") 
  (make-package "MONITOR" :nicknames '("MON") :use '("COMMON-LISP")))

#+:cltl2
(in-package "MONITOR")


#+(and :excl :allegro-v4.0)
(cltl1:provide "monitor")
#+(and :excl :allegro-v4.1)
(provide "monitor")
#+:mcl
(ccl:provide "monitor")
#+(and :cltl2
       (not (or (and :excl (or :allegro-v4.0 :allegro-v4.1))
		:mcl
		(and :cmu :new-compiler))))
(provide "monitor")
#-:cltl2
(provide "monitor")

(export '(*monitored-functions*
	  monitor monitor-all unmonitor monitor-form
	  with-monitoring
	  reset-monitoring-info reset-all-monitoring
	  monitored
	  report-monitoring
	  display-monitoring-results
	  monitoring-encapsulate monitoring-unencapsulate))


;;; Warn user if they're loading the source instead of compiling it first.
(eval-when (eval)
   (warn "This file should be compiled before loading for best results."))

;;; ********************************
;;; Version ************************
;;; ********************************

(defparameter *metering-version* "v2.1 25-JAN-94"
  "Current version number/date for Metering.")


;;; ****************************************************************
;;; Implementation Dependent Definitions ***************************
;;; ****************************************************************

;;; ********************************
;;; Type Definitions ***************
;;; ********************************

(eval-when (compile load eval)
  #+(and :cmu :new-compiler)
  (deftype time-type () '(unsigned-byte 29))
  #+(and :cmu :new-compiler)
  (deftype consing-type () '(unsigned-byte 29))
  #-(and :cmu :new-compiler)
  (deftype time-type () 'unsigned-byte)
  #-(and :cmu :new-compiler)
  (deftype consing-type () 'unsigned-byte)

  )

;;; ********************************
;;; Timing Functions ***************
;;; ********************************
;;; The get-time function is called to find the total number of ticks since
;;; the beginning of time. time-units-per-second allows us to convert units
;;; to seconds.
;;; reset-time helps to keep the times small.  We'd like them to stay fixnums,
;;; so we don't cons during metering.

#+:Harlequin-common-lisp
(progn
  (defconstant time-units-per-second 100000)

  (defvar *start-time-secs* 0)

  (defun reset-time ()
    (sys::update-resource-table)
    (setq *start-time-secs*
	  (sys::raw-int-fixnum 
	   #-harp::alpha
	   (sys::raw-+
	    (sys::*%system-resource-table (constants::sys-num 0))
	    (sys::*%system-resource-table (constants::sys-num 8)))
	   #+harp::alpha
	   (sys::raw-+
	    (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			   (constants::sys-num 0))
	    (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			   (constants::sys-num 8))))))

  ;; from get-internal-run-time, but use 10-microsecond ticks
  ;; this gives us better resolution, and covers about 80 minutes
  ;; in fixnums
  (defun get-time ()
    (sys::update-resource-table)
    (+ (* 100000
          (sys::-$fixnum
	   (sys::raw-int-fixnum 
	    #-harp::alpha
	    (sys::raw-+
	     (sys::*%system-resource-table (constants::sys-num 0))
	     (sys::*%system-resource-table (constants::sys-num 8)))
	    #+harp::alpha
	    (sys::raw-+
	     (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			    (constants::sys-num 0))
	     (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			    (constants::sys-num 8))))
           *start-time-secs*))
       (sys::raw-int-fixnum
	(sys::raw-/
	 #-harp::alpha
	 (sys::raw-+ (sys::*%system-resource-table (constants::sys-num 4))
		     (sys::*%system-resource-table (constants::sys-num 12)))
	 #+harp::alpha
	 (sys::raw-+
	  (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			 (constants::sys-num 4))
	  (sys::raw-load (sys::*%system-resource-table (constants::sys-num 0))
			 (constants::sys-num 12)))
	 (constants::sys-num 10)))))
    )


(progn
  #-(or :cmu 
        CLISP
	:allegro-v3.1 :allegro-v4.0 :allegro-v4.1
	:mcl :mcl1.3.2
	:lcl3.0 :lcl4.0
        :Harlequin-common-lisp)
  (eval-when (compile eval)
    (warn
     "You may want to supply implementation-specific get-time functions."))

  #-:Harlequin-common-lisp
  (defconstant time-units-per-second internal-time-units-per-second)
  
  #-:Harlequin-common-lisp
  (defmacro get-time ()
    `(the time-type (get-internal-run-time)))

  #-:Harlequin-common-lisp
  (defmacro reset-time ()
    '())

)

;;; NOTE: In Macintosh Common Lisp, CCL::GCTIME returns the number of
;;;       milliseconds spent during GC. We could subtract this from
;;;       the value returned by get-internal-run-time to eliminate
;;;       the effect of GC on the timing values, but we prefer to let
;;;       the user run without GC on. If the application is so big that
;;;       it requires GC to complete, then the GC times are part of the
;;;       cost of doing business, and will average out in the long run.
;;;       If it seems really important to a user that GC times not be 
;;;       counted, then uncomment the following three lines and read-time
;;;       conditionalize the definition of get-time above with #-:mcl.
;#+:mcl 
;(defmacro get-time () 
;  `(the time-type (- (get-internal-run-time) (ccl:gctime))))

;;; ********************************
;;; GC Timing Functions **************
;;; ********************************
;;; The get-gc macro is called to find the amount of time spent in the GC.

#+MCL
(defun get-gc () (ccl:gctime))

#+:Harlequin-common-lisp
(progn
  (defmacro fix-+ (a &rest rest)
    (if (null rest) a
      `(sys::+$fixnum ,a (fix-+ ,@rest))))

  (defun get-gc ()
    (multiple-value-bind
	(main-promote-secs main-promote-micros 
			   main-promote-system-secs 
			   main-promote-system-micros
			   main-promote-calls)
	(sys::main-promote-value)
      (declare (ignore main-promote-calls))
      (multiple-value-bind
	  (mark-and-sweep-secs mark-and-sweep-micros 
			       mark-and-sweep-system-secs
			       mark-and-sweep-system-micros
			       mark-and-sweep-calls)
	  (sys::mark-and-sweep-value)
	(declare (ignore mark-and-sweep-calls))
	(multiple-value-bind
	    (compact-secs compact-micros 
			  compact-system-secs
			  compact-system-micros
			  compact-calls)
	    (sys::compact-value)
	  (declare (ignore compact-calls))
	  (multiple-value-bind
	      (in-promote-secs in-promote-micros 
			       in-promote-system-secs
			       in-promote-system-micros
			       in-promote-calls)
			
	      (sys::in-promote-value)
	    (declare (ignore in-promote-calls))
	    (multiple-value-bind
		(a total-micros)
		(sys::floor$fixnum
		 (fix-+ main-promote-micros
			mark-and-sweep-micros
			compact-micros
			in-promote-micros
			main-promote-system-micros
			mark-and-sweep-system-micros
			compact-system-micros
			in-promote-system-micros)
		 1000000)
	      (+ (* (fix-+ a
			   main-promote-secs
			   mark-and-sweep-secs
			   compact-secs
			   in-promote-secs
			   main-promote-system-secs
			   mark-and-sweep-system-secs
			   compact-system-secs
			   in-promote-system-secs)
		    100000)
		 (sys::raw-int-fixnum
		  (sys::raw-/ total-micros
			      (constants::sys-num 10))))))))))

  (defmacro reset-and-start-gc-monitor ()
    '(progn
       (sys::zero-total-allocation)
       (sys::reset-gc-timer)
       (sys::switch-gc-timer-on)))

  (defmacro stop-gc-monitor ()
    '(sys::switch-gc-timer-off))
  )

#-:Harlequin-common-lisp
(progn
  (defmacro reset-and-start-gc-monitor ()
    '())

  (defmacro stop-gc-monitor ()
    '())
  )

#-(or MCL :Harlequin-common-lisp)
(progn
  (eval-when (compile eval)
    (warn "No GC time will be reported unless a get-gc function is ~
           defined."))
  
  (defmacro get-gc () '0))

;;; ********************************
;;; Consing Functions **************
;;; ********************************
;;; The get-cons macro is called to find the total number of bytes
;;; consed since the beginning of time.

#+:cmu
(defmacro get-cons ()
  "The get-cons macro is called to find the total number of bytes
   consed since the beginning of time."
;  #-:new-compiler
;  '(ext:get-bytes-consed)
;  #+:new-compiler
  '(the consing-type (ext:get-bytes-consed)))

#+CLISP
(defun get-cons ()
  (multiple-value-bind (real1 real2 run1 run2 gc1 gc2 space1 space2 gccount)
      (sys::%%time)
    (declare (ignore real1 real2 run1 run2 gc1 gc2 gccount))
    (dpb space1 (byte 24 24) space2)))

;;; Lucid. 4 bytes/word. This returns bytes.
;;; For some reason this doesn't work properly under Lucid 4.0, but
;;; that's OK, because they have PC-based profiling which is more accurate.
#+(or :lcl3.0 :lcl4.0)
(defmacro get-cons () `(the consing-type (gc-size)))

;;; Allegro V4.0/1. SYS::GSGC-MAP takes one argument, and returns an
;;; array representing the memory state.
#+(or :allegro-v4.0 :allegro-v4.1)
(defvar *gc-space-array* (make-array 4 :element-type '(unsigned-byte 32)))
#+(or :allegro-v4.0 :allegro-v4.1)
(defun bytes-consed ()
  (system:gsgc-totalloc *gc-space-array* t)
  (aref *gc-space-array* 0))

#+:allegro-v3.1
(defun bytes-consed ()
  (let ((gs (sys::gsgc-map)))
    (+ (aref gs 3)			; new space
       (let ((sum 0))			; old space
	 (dotimes (i (1+ (floor (/ (- (length gs) 13) 10))))
	   (incf sum (aref gs (+ (* i 10) 13))))
	 sum)))
  )

#+(or :allegro-v3.1 :allegro-v4.0 :allegro-v4.1)
(defmacro get-cons () `(the consing-type (bytes-consed)))

#+:Harlequin-common-lisp
(defmacro get-cons () `(the consing-type (lw:total-allocation)))

;;; Macintosh Allegro Common Lisp 1.3.2
;;; Based on CCL's sample code for memory usage.
;;; They key trick here is that we maintain the information about total
;;; consing since time zero by keeping track of how much memory was free
;;; before and after gc (by advising gc). Luckily, MACL's garbage collection
;;; seems to always be invoked internally by calling GC.
;;;
;;; Maybe instead of showing bytes consed since time zero, we should
;;; return bytes consed since the first time the function is called?
;;; And the first time the function is called, it should set the
;;; value to zero. No real need to do this -- what we have works fine,
;;; and involves less code.
#+:mcl1.3.2
(in-package :ccl)

#+:mcl1.3.2
(defvar *bytes-consed-chkpt* 0)

#+:mcl1.3.2
(defun reset-consing () (setq *bytes-consed-chkpt* 0))

(eval-when (eval compile)
  #+:mcl1.3.2(defconstant $currentA5 #x904)
  #+:mcl1.3.2(defconstant $pagecounts #x-18e)
  #+:mcl1.3.2(defconstant $lstFP #x-a42)
  #+:mcl1.3.2(defconstant $consfirstob 64)
  #+:mcl1.3.2(defconstant $pagesize 4096))

#+:mcl1.3.2
(let ((old-gc (symbol-function 'gc))
      (ccl:*warn-if-redefine-kernel* nil))
  (setf (symbol-function 'gc)
        #'(lambda ()
            (let ((old-consing (total-bytes-consed)))
              (prog1
                (funcall old-gc)
                (incf *bytes-consed-chkpt*
		      (- old-consing (total-bytes-consed))))))))

#+:mcl1.3.2
(defun total-bytes-consed (&aux pages fp)
  "Returns number of conses (8 bytes each)"
  (let* ((a5 (%get-ptr $currentA5))
         (ptr (%inc-ptr a5 $pagecounts)))
    (%ilsr 3 (%i+ (%i- (%ilsl 12 (%i- (setq pages (%get-word ptr 0)) 1))
		       (%i* pages $consfirstob))
                   (if (eq 0 (setq fp (%get-long a5 $lstFP)))
		       $pagesize
		     (%ilogand2 #xfff fp))))))

#+:mcl1.3.2
(in-package "MONITOR")

#+:mcl1.3.2
(defun get-cons ()
  (the consing-type (+ (ccl::total-bytes-consed) ccl::*bytes-consed-chkpt*)))

;;; Macintosh Common Lisp 2.0
;;; Note that this includes bytes that were allocated during GC.
;;; We could subtract this out by advising GC like we did under
;;; MCL 1.3.2, but I'd rather users ran without GC. If they can't
;;; run without GC, then the bytes consed during GC are a cost of
;;; running their program. Metering the code a few times will
;;; avoid the consing values being too lopsided. If a user really really
;;; wants to subtract out the consing during GC, replace the following
;;; two lines with the commented out code.
#+:mcl
(defmacro get-cons () `(the consing-type (ccl::total-bytes-allocated)))
;#+:mcl
;(in-package :ccl)
;#+:mcl
;(defvar *bytes-consed-chkpt* 0)
;#+:mcl
;(defun reset-consing () (setq *bytes-consed-chkpt* 0))
;#+:mcl
;(let ((old-gc (symbol-function 'gc))
;      (ccl:*warn-if-redefine-kernel* nil))
;  (setf (symbol-function 'gc)
;	#'(lambda ()
;	    (let ((old-consing (total-bytes-consed)))
;	      (prog1
;		(funcall old-gc)
;		(incf *bytes-consed-chkpt*
;		      (- old-consing (total-bytes-consed))))))))
;#+:mcl
;(defun total-bytes-consed ()
;  "Returns number of conses (8 bytes each)"
;  (ccl::total-bytes-allocated))
;#+:mcl
;(in-package "MONITOR")
;#+:mcl
;(defun get-cons ()
;  (the consing-type (+ (ccl::total-bytes-consed) ccl::*bytes-consed-chkpt*)))


#-(or :cmu 
      CLISP
      :lcl3.0 :lcl4.0 
      :allegro-v3.1 :allegro-v4.0 :allegro-v4.1
      :mcl1.3.2 :mcl Harlequin-common-lisp)
(progn
  (eval-when (compile eval)
    (warn "No consing will be reported unless a get-cons function is ~
           defined."))

  (defmacro get-cons () '(the consing-type 0)))

;;; ********************************
;;; Required Arguments *************
;;; ********************************
;;;
;;; Required (Fixed) vs Optional Args
;;;
;;; To avoid unnecessary consing in the "encapsulation" code, we find out the
;;; number of required arguments, and use &rest to capture only non-required
;;; arguments.  The function Required-Arguments returns two values: the first
;;; is the number of required arguments, and the second is T iff there are any
;;; non-required arguments (e.g. &optional, &rest, &key).
#+cmu
(progn
  #-new-compiler
  (defun required-arguments (name)
    (let ((function (symbol-function name)))
      (if (eql (system:%primitive get-type function) system:%function-type)
	  (let ((min (ldb system:%function-min-args-byte
			  (system:%primitive header-ref function
					     system:%function-min-args-slot)))
		(max (ldb system:%function-max-args-byte
			  (system:%primitive header-ref function
					     system:%function-max-args-slot)))
		(rest (ldb system:%function-rest-arg-byte
			   (system:%primitive header-ref function
					      system:%function-rest-arg-slot)))
		(key (ldb system:%function-keyword-arg-byte
			  (system:%primitive
			   header-ref function
			   system:%function-keyword-arg-slot))))
	    (values min (or (/= min max) (/= rest 0) (/= key 0))))
	  (values 0 t))))

  #| #+new-compiler
  (defun required-arguments (name)
    (let* ((function (symbol-function name))
	   (stype (system:%primitive get-vector-subtype function)))
      (if (eql stype system:%function-entry-subtype)
	  (let* ((args (cadr (system:%primitive
			      header-ref
			      function
			      system:%function-entry-type-slot)))		 (pos (position-if #'(lambda (x)
				       (and (symbolp x)
					    (let ((name (symbol-name x)))
					      (and (>= (length name) 1)
						   (char= (schar name 0)
							  #\&)))))
				   args)))
	    (if pos
		(values pos t)
		(values (length args) nil)))
	  (values 0 t)))))|#

  #+new-compiler
  (defun required-arguments (name)
    (let ((type (ext:info function type name)))
      (cond ((not (kernel:function-type-p type))
	     (warn "No argument count information available for:~%  ~S~@
		  Allow for &rest arg consing."
		   name)
	     (values 0 t))
	    (t
	     (values (length (kernel:function-type-required type))
		     (if (or (kernel:function-type-optional type)
			     (kernel:function-type-keyp type)
			     (kernel:function-type-rest type))
			 t nil))))))
)

;;; Lucid, Allegro, and Macintosh Common Lisp
#+(OR :lcl3.0 :lcl4.0 :excl :mcl) 
(defun required-arguments (name)
  (let* ((function (symbol-function name))
         (args #+:excl(excl::arglist function)
	       #+:mcl(ccl:arglist function)
	       #-(or :excl :mcl)(arglist function))
         (pos (position-if #'(lambda (x)
                               (and (symbolp x)
                                    (let ((name (symbol-name x)))
                                      (and (>= (length name) 1)
                                           (char= (schar name 0)
                                                  #\&)))))
                           args)))
    (if pos
        (values pos t)
        (values (length args) nil))))

;;; Macintosh Allegro Common Lisp version 1.3.2
#+:mcl1.3.2
(defun required-arguments (name)
  (let ((arguments-string
         (let ((the-string
                (with-output-to-string (*standard-output*)
		    (ccl:arglist-to-stream name *standard-output*))))
           (cond ((and (>=  (length the-string) 23)
                       (string-equal (subseq the-string 0 22)
                                     "Can't find arglist for")) nil)
                 ((position  #\( the-string :test 'char-equal) the-string)
                 (T  (concatenate 'string "(" the-string ")"))))))
    (if (null arguments-string)
	(values 0 t)
      (let* ((pos (position #\& arguments-string))
             (args (length (read-from-string
                            (concatenate 'string
					 (subseq arguments-string 0 pos)
					 ")")))))
        (if pos
	    (values args t)
          (values args nil))))))

#+CLISP
(defun required-arguments (name)
  (let ((function (symbol-function name)))
    (case (type-of function)
      (FUNCTION
       (if (compiled-function-p function)
	   (multiple-value-bind (req-anz opt-anz rest-p key-p
					 keyword-list allow-other-keys-p)
	       (sys::signature function)
	     (declare (ignore keyword-list allow-other-keys-p))
	     (values req-anz (or (plusp opt-anz) rest-p key-p)))
	   (let ((lambdalist (car (sys::%record-ref function 1))))
	     (values (or (position-if #'(lambda (x) 
					  (member x lambda-list-keywords))
				      lambdalist)
			 (length lambdalist))
		     (and (intersection lambdalist lambda-list-keywords) t)))))
      (COMPILED-FUNCTION
       (multiple-value-bind (name req-anz opt-anz rest-p
				  keywords allow-other-keys)
	   (sys::subr-info function)
	 (declare (ignore allow-other-keys))
	 (if name
	     (values req-anz (or (plusp opt-anz) rest-p keywords))
	     (values 0 t))))
      (T (values 0 t)))))

#+:Harlequin-common-lisp
(defun required-arguments (name)
  (let ((lambda-list (lw:function-lambda-list name)))
    (if lambda-list
        (let ((key-position (position-if #'(lambda (x) 
					     (member x lambda-list-keywords))
				         lambda-list)))
          (if key-position
	      (values key-position t)
            (values (length lambda-list) nil)))
      ;; work around a bug in LispWorks 3.2, where arglists which are
      ;; shaken out disappear without a trace
      (values 0 t))))

#-(or :cmu CLISP :lcl3.0 :lcl4.0 :mcl1.3.2 :mcl :excl :Harlequin-common-lisp)
(progn
 (eval-when (compile eval)
   (warn
    "You may want to add an implementation-specific Required-Arguments function."))
 (eval-when (load eval)
   (defun required-arguments (name)
     (declare (ignore name))
     (values 0 t))))

#|
;;;Examples
(defun square (x) (* x x))
(defun square2 (x &optional y) (* x x y))
(defun test (x y &optional (z 3)) 3)
(defun test2 (x y &optional (z 3) &rest fred) 3)

(required-arguments 'square) => 1 nil
(required-arguments 'square2) => 1 t
(required-arguments 'test) => 2 t
(required-arguments 'test2) => 2 t
|#

;;; Implementation-specific backtrace support

#+MCL
(defun get-encoded-backtrace (vector)   ; returns number of elements filled in
  (declare (optimize (speed 3) (space 0) (safety 1))
           (simple-vector vector))
  (let ((i -1)                          ; don't count our direct caller
        (n (length vector))
        (end (ccl::last-frame-ptr))
        (cfp (ccl::lap-inline () (ccl::address->index ccl::sp ccl::arg_z)))
        (endptr (ccl::lap-inline () (ccl::address->index (ccl::a5 ccl::$csarea) ccl::arg_z))))
    (declare (fixnum i n))
    (loop until (or (null cfp) (eq cfp end) (>= i n))
          do (when (>= i 0)
               (setf (svref vector i)
                     (ccl::lap-inline ()
                       (:variable cfp)
                       ;; Get this call frame's return PC
                       (ccl::move.l (ccl::varg cfp) ccl::dtemp0)
                       (ccl::index->address ccl::dtemp0 ccl::atemp1)
                       (ccl::move.l (ccl::atemp1 4) ccl::acc)     ; return PC
                       (ccl::cmp.l (ccl::a5 ccl::$mvexpect) ccl::acc)
                       (ccl::if# ccl::eq
                         (ccl::move.l (ccl::atemp1 12) ccl::acc))
                       ;; Encode PC as a fixnum, since finding the enclosing
                       ;; function object is much too slow to do here
                       ;; Assume high two bits of 32 bit address space not used
                       ;; Assume GC moving functions around is rare enough that
                       ;; we can still get useful information out of the metering
                       (ccl::bclr (ccl::$ 0) ccl::acc)    ; should be zero already
                       (ccl::lsl.l (ccl::$ 2) ccl::acc))))
             (incf i)
             (multiple-value-setq (cfp endptr) (ccl::%next-cfp endptr cfp)))
    i))

#+MCL
(defun decode-backtrace-element (x)     ; returns function name and offset
  (let* ((offset nil)
         (f (ccl::lap-inline ()
             (:variable x offset)
             (ccl::move.l (ccl::varg x) ccl::dtemp0)
             (ccl::lsr.l (ccl::$ 2) ccl::dtemp0)
             (ccl::spush ccl::dtemp0)
             (ccl::move.l ccl::sp ccl::atemp1)
             (ccl::jsr_subprim ccl::$sp-findlfunv)
             (ccl::if# (and (ccl::ne (ccl::move.l ccl::atemp0 ccl::da))
                            (eq (ccl::vsubtypep (ccl::$ ccl::$v_nlfunv) ccl::atemp0)))
               (ccl::move.l (ccl::$ ccl::$lfv_lfun) ccl::acc)
               (ccl::add.l ccl::atemp0 ccl::acc)
               (ccl::move.l (ccl::sp) ccl::dtemp1)
               (ccl::sub.l ccl::acc ccl::dtemp1)
               (ccl::lsl.l (ccl::$ 3) ccl::dtemp1)
               (ccl::move.l ccl::dtemp1 (ccl::varg offset))
              ccl::else#
               (ccl::move.l ccl::nilreg ccl::acc))
             (ccl::add.l (ccl::$ 4) ccl::sp))))
    (values (and f (or (ccl:function-name f) f)) offset)))

;;; Adjustable parameters

(defvar *backtrace-length* 15 "Length of backtraces to collect")
(defvar *number-of-backtraces* 10 "Number of different backtraces to keep")
(defvar *minimum-breakdown-size* 5 "List this many items regardless of thresholds")

;;; A backtrace is a simple-vector containing an encoded-backtrace
;;; The first seven elements in a backtrace are special
(defun    make-backtrace () (make-array (+ 7 *backtrace-length*) :initial-element 0))
(defmacro backtrace-inclusive-time (bt) `(svref ,bt 0))
(defmacro backtrace-inclusive-cons (bt) `(svref ,bt 1))
(defmacro backtrace-exclusive-time (bt) `(svref ,bt 2))
(defmacro backtrace-exclusive-cons (bt) `(svref ,bt 3))
(defmacro backtrace-calls            (bt) `(svref ,bt 4))
(defmacro backtrace-nested-calls    (bt) `(svref ,bt 5))
(defmacro backtrace-length           (bt) `(svref ,bt 6))
(defmacro backtrace-ref (bt i) `(svref ,bt (+ ,i 7)))


;;; ********************************
;;; Fdefinition ********************
;;; ********************************
;;; fdefinition is a CLtL2 addition. 
#+(and :cmu (not (or new-compiler :new-compiler)))
(eval-when (compile eval)
  ;; Need to worry about extensions:encapsulate in CMU CL
  ;; Note: We should really be defining fdefinition as a function
  ;; in the "LISP" package and export it. But this will do for now,
  ;; especially since we only define it while compiling this code.
  ;; The use of (fboundp 'fdefinition) later in this file works
  ;; because (fboundp <macro>) returns t.
  ;; (export 'lisp::fdefinition "LISP")
  (defmacro fdefinition (x)
    `(lisp::careful-symbol-function ,x))
  (defsetf fdefinition lisp::set-symbol-function-carefully))


;;; ****************************************************************
;;; Main METERING Code *********************************************
;;; ****************************************************************

;;; ********************************
;;; Global Variables ***************
;;; ********************************
(defvar *MONITORING-ENABLED* nil
  "Bind this to true to enable the monitors to collect data")
(defvar *MONITOR-TIME-OVERHEAD* nil
  "The amount of time an empty monitored function costs.")
(defvar *MONITOR-CONS-OVERHEAD* nil
  "The amount of cons an empty monitored function costs.")

(defvar *TOTAL-TIME* 0
  "Total amount of time monitored so far.")
(defvar *TOTAL-CONS* 0
  "Total amount of consing monitored so far.")
(defvar *TOTAL-CALLS* 0
  "Total number of calls monitored so far.")
(proclaim '(type time-type *total-time*))
(proclaim '(type consing-type *total-cons*))
(proclaim '(fixnum *total-calls*))
(defvar *backtrace-time* 0)             ; time spent collecting backtraces
(proclaim '(type time-type *backtrace-time*))
(defvar *backtrace-cons* 0)
(proclaim '(type consing-type *backtrace-cons*))

;;; ********************************
;;; Accessor Functions *************
;;; ********************************
;;; Perhaps the SYMBOLP should be FBOUNDP? I.e., what about variables
;;; containing closures.
(defmacro PLACE-FUNCTION (function-place)
  "Return the function found at FUNCTION-PLACE. Evals FUNCTION-PLACE
if it isn't a symbol, to allow monitoring of closures located in
variables/arrays/structures."
  ;; Note that (fboundp 'fdefinition) returns T even if fdefinition
  ;; is a macro, which is what we want.
  (if (fboundp 'fdefinition)
      `(if (fboundp ,function-place)
	   (fdefinition ,function-place)
	   (eval ,function-place))
      `(if (symbolp ,function-place)
	   (symbol-function ,function-place)
	   (eval ,function-place))))

(defsetf PLACE-FUNCTION (function-place) (function)
  "Set the function in FUNCTION-PLACE to FUNCTION."
  (if (fboundp 'fdefinition)
      ;; If we're conforming to CLtL2, use fdefinition here.
      `(if (fboundp ,function-place)
	   (setf (fdefinition ,function-place) ,function) 
	   (eval '(setf ,function-place ',function)))
      `(if (symbolp ,function-place)
	   (setf (symbol-function ,function-place) ,function) 
	   (eval '(setf ,function-place ',function)))))

#|
;; special version for Dylan emulator
(defsetf PLACE-FUNCTION (function-place) (function)
  "Set the function in FUNCTION-PLACE to FUNCTION."
  ;; If we're conforming to CLtL2, use fdefinition here.
  `(if (fboundp ,function-place)
       (if (and (symbolp ,function-place)
		(boundp ,function-place)
		(eq (symbol-function ,function-place)
		    (symbol-value ,function-place)))
	   (setf (symbol-value ,function-place)
		 (setf (symbol-function ,function-place) ,function))
	 (setf (fdefinition ,function-place) ,function))
     (eval '(setf ,function-place ',function))))

|#

#|
;;; before using fdefinition
(defun PLACE-FUNCTION (function-place)
  "Return the function found at FUNCTION-PLACE. Evals FUNCTION-PLACE
if it isn't a symbol, to allow monitoring of closures located in
variables/arrays/structures."
  (if (symbolp function-place)
      (symbol-function function-place)
      (eval function-place)))

(defsetf PLACE-FUNCTION (function-place) (function)
  "Set the function in FUNCTION-PLACE to FUNCTION."
  `(if (symbolp ,function-place)
       (setf (symbol-function ,function-place) ,function)
       (eval '(setf ,function-place ',function))))
|#

(defun PLACE-FBOUNDP (function-place)
  "Test to see if FUNCTION-PLACE is a function."
  ;; probably should be 
  #|(or (and (symbolp function-place)(fboundp function-place))
      (functionp (place-function function-place)))|#
  (if (symbolp function-place)
      (fboundp function-place)
      (functionp (place-function function-place))))

(defun PLACE-MACROP (function-place)
  "Test to see if FUNCTION-PLACE is a macro."
  (when (symbolp function-place)
    (macro-function function-place)))

;;; ********************************
;;; Measurement Tables *************
;;; ********************************
(defvar *monitored-functions* nil
  "List of monitored symbols.")

;;; We associate a METERING-FUNCTIONS structure with each monitored function
;;; name or other closure. This holds the functions that we call to manipulate
;;; the closure which implements the encapsulation.
;;;
(defstruct metering-functions
  (name nil)
  (old-definition #-(and :cmu :new-compiler) nil
		  #+(and :cmu :new-compiler) 
		  (error "Missing required keyword argument :old-definition")
		  :type function)
  (new-definition #-(and :cmu :new-compiler) nil
		  #+(and :cmu :new-compiler) 
		  (error "Missing required keyword argument :new-definition")
		  :type function)
  (read-metering #-(and :cmu :new-compiler) nil
		  #+(and :cmu :new-compiler) 
		  (error "Missing required keyword argument :read-metering")
		  :type function)
  (reset-metering #-(and :cmu :new-compiler) nil
		  #+(and :cmu :new-compiler) 
		  (error "Missing required keyword argument :reset-metering")
		  :type function))

;;; In general using hash tables in time-critical programs is a bad idea,
;;; because when one has to grow the table and rehash everything, the
;;; timing becomes grossly inaccurate. In this case it is not an issue
;;; because all inserting of entries in the hash table occurs before the
;;; timing commences. The only circumstance in which this could be a 
;;; problem is if the lisp rehashes on the next reference to the table,
;;; instead of when the entry which forces a rehash was inserted. 
;;;
;;; Note that a similar kind of problem can occur with GC, which is why
;;; one should turn off GC when monitoring code. 
;;;
(defvar *monitor* (make-hash-table :test #'equal)
  "Hash table in which METERING-FUNCTIONS structures are stored.")
(defun get-monitor-info (name)
  (gethash name *monitor*))
(defsetf get-monitor-info (name) (info)
  `(setf (gethash ,name *monitor*) ,info))

(defun MONITORED (function-place)
  "Test to see if a FUNCTION-PLACE is monitored."
  (and (place-fboundp function-place)	; this line necessary?
       (get-monitor-info function-place)))

(defun reset-monitoring-info (name)
  "Reset the monitoring info for the specified function."
  (let ((finfo (get-monitor-info name)))
    (when finfo
      (funcall (metering-functions-reset-metering finfo)))))

(defun reset-all-monitoring () 
  "Reset monitoring info for all functions."
  (setq *total-time* 0
	*total-cons* 0
	*total-calls* 0
        *backtrace-time* 0
        *backtrace-cons* 0)
  (dolist (symbol *monitored-functions*)
    (when (monitored symbol)
      (reset-monitoring-info symbol)))
  (reset-time)
  (reset-and-start-gc-monitor))

(defun monitor-info-values (name &optional (nested :exclusive) warn backtracep)
  "Returns monitoring information values for the named function,
adjusted for overhead."
  (let ((finfo (get-monitor-info name)))
    (if finfo
	(multiple-value-bind (inclusive-time inclusive-cons
					     exclusive-time exclusive-cons
					     calls nested-calls backtrace)
	    (funcall (metering-functions-read-metering finfo) backtracep)
	  (unless (or (null warn)
		      (eq (place-function name)
			  (metering-functions-new-definition finfo)))
	    (warn "Function ~S has been redefined, so times may be inaccurate.~@
                   MONITOR it again to record calls to the new definition."
		  name))
	  (case nested
	    (:exclusive (values calls
				nested-calls
				(- exclusive-time 
				   (* calls *monitor-time-overhead*))
				(- exclusive-cons 
				   (* calls *monitor-cons-overhead*))
                                backtrace))
	    ;; In :inclusive mode, subtract overhead for all the
	    ;; called functions as well. Nested-calls includes the
	    ;; calls of the function as well. [Necessary 'cause of
	    ;; functions which call themselves recursively.]
	    (:inclusive (values calls
				nested-calls
				(- inclusive-time 
				   (* nested-calls ;(+ calls)
				      *monitor-time-overhead*))
				(- inclusive-cons 
				   (* nested-calls ;(+ calls)
				      *monitor-cons-overhead*))
                                backtrace))))
	(values 0 0 0 0 nil))))

;;; ********************************
;;; Encapsulate ********************
;;; ********************************
(eval-when (compile load eval)
  ;; Returns a lambda expression for a function that, when called with the
  ;; function name, will set up that function for metering.
  ;;
  ;; A function is monitored by replacing its definition with a closure
  ;; created by the following function. The closure records the monitoring
  ;; data, and updates the data with each call of the function.
  ;;
  ;; Other closures are used to read and reset the data.
  (defun make-monitoring-encapsulation (min-args optionals-p backtracep)
    (let (required-args)
      (dotimes (i min-args) (push (gensym) required-args))
      `(lambda (name)
         (let (,@(if backtracep
                   `((backtraces (make-array *number-of-backtraces*))
                     (n-backtraces *number-of-backtraces*)
                     (this-backtrace (make-array *backtrace-length*)))
                   `((inclusive-time 0)
	             (inclusive-cons 0)
	             (exclusive-time 0)
	             (exclusive-cons 0)
	             (calls 0)
	             (nested-calls 0)))
	       (old-definition (place-function name)))
	   ,@(when backtracep
               `((declare (simple-vector backtraces this-backtrace)
                          (fixnum n-backtraces))
                 (dotimes (i *number-of-backtraces*)
                   (setf (svref backtraces i) (make-backtrace)))))
           (pushnew name *monitored-functions* :test #'equal)    ; --- for setf names
           
	   (setf (place-function name)
	         (#-MCL function
                  #+MCL ccl::nfunction #+MCL ,(format nil "Monitor-~D~A~A" ;see monitoring-encapsulation-p
                                                      min-args
                                                      (if optionals-p "-opt" "")
                                                      (if backtracep "-bt" ""))
                   (lambda (,@required-args
			    ,@(when optionals-p `(&rest optional-args)))
                     ,@(when optionals-p `((declare (dynamic-extent optional-args))))
		     (let ((prev-total-time *total-time*)
			   (prev-total-cons *total-cons*)
			   (prev-total-calls *total-calls*)
                           (prev-backtrace-time *backtrace-time*)
                           (prev-backtrace-cons *backtrace-cons*)
;		  	   (old-time inclusive-time)
;			   (old-cons inclusive-cons)
;			   (old-nested-calls nested-calls)
			   (start-time (get-time))
			   (start-cons (get-cons)))
		       (multiple-value-prog1
			 ,(if optionals-p
                            `(apply old-definition 
                                    ,@required-args optional-args)
                            `(funcall old-definition ,@required-args))
		        (when *monitoring-enabled*
                         (let* ((end-time (get-time))
                                (end-cons (get-cons))
                                (delta-time (- end-time start-time (- *backtrace-time* prev-backtrace-time)))
			        (delta-cons (- end-cons start-cons (- *backtrace-cons* prev-backtrace-cons))))
                           ,@(if backtracep
                               `(;; Determine how we got called
                                 (let ((this-length (get-encoded-backtrace this-backtrace))
                                       (bt this-backtrace)   ;bt is setq'ed later
                                       (hole nil))
                                   (declare (fixnum this-length)
                                            (simple-vector bt)
                                            (optimize (speed 3) (space 0) (safety 1)))
                                   ;; Find this-backtrace in backtraces, set bt to it
                                   ;; We might not find it exactly, so find the longest one that matches
                                   (block found
                                     (let ((nn -1))
                                       (declare (fixnum nn))
                                       (dotimes (i n-backtraces)
                                         (declare (fixnum i))
                                         (let ((b (svref backtraces i)))
                                           (declare (simple-vector b))
                                           (if (zerop (backtrace-calls b))
                                             (unless hole (setq hole b))
                                             (let ((n (backtrace-length b)))
                                               (declare (fixnum n))
                                               (when (> n nn)
                                                 (unless (< this-length n)
                                                   (dotimes (j n (setq bt b nn n))
                                                     (declare (fixnum j))
                                                     (unless (eq (svref this-backtrace j)
                                                                 (backtrace-ref b j))
                                                       (return)))))))))
                                       (unless (< nn 0)         ; if we found anything
                                         (unless (and hole      ; unless we can do better
                                                      (< nn this-length))
                                           (return-from found))))
                                     ;; Didn't find it, have to add it
                                     (block found-hole
                                       (when hole
                                         (setq bt hole)
                                         (return-from found-hole))
                                       ;; No room, merge the backtrace with the smallest count into the
                                       ;; one that shares the most with it, and has the next smallest count
                                       (let ((k 0)
                                             (c most-positive-fixnum))
                                         (declare (fixnum k c))
                                         (dotimes (i n-backtraces)
                                           (declare (fixnum i))
                                           (let ((cc (backtrace-calls (svref backtraces i))))
                                             (declare (fixnum cc))
                                             (when (< cc c)
                                               (setq c cc k i))))
                                         ;; bt is the backtrace that will get reused
                                         (setq bt (svref backtraces k)))
                                       (let ((k 0)
                                             (c most-positive-fixnum)
                                             (d 0)
                                             (n (backtrace-length bt)))
                                         (declare (fixnum k c d n))
                                         (dotimes (i n-backtraces)
                                           (declare (fixnum i))
                                           (let* ((bb (svref backtraces i))     ; do I look like a
                                                  (cc (backtrace-calls bb))     ; Fortran programmer?
                                                  (bn (backtrace-length bb))
                                                  (ln (if (< n bn) n bn))
                                                  (nn (dotimes (j ln ln)
                                                        (declare (fixnum j))
                                                        (unless (eq (backtrace-ref bb j) (backtrace-ref bt j))
                                                          (return j)))))
                                             (declare (fixnum cc bn ln nn) (simple-vector bb))
                                             (unless (eq bb bt)
                                               (when (or (> nn d) (and (= nn d) (< cc c)))
                                                 (setq d nn c cc k i)))))
                                         ;; Merge backtrace bt into backtrace bt2
                                         ;--- maybe look for another matching backtrace of the same
                                         ;--- length and merge it as well?
                                         (let ((bt2 (svref backtraces k)))
                                           (declare (simple-vector bt2))
                                           (setf (backtrace-length bt2) d)      ; length of matching part
                                           (incf (backtrace-inclusive-time bt2) (backtrace-inclusive-time bt))
                                           (incf (backtrace-inclusive-cons bt2) (backtrace-inclusive-cons bt))
                                           (incf (backtrace-exclusive-time bt2) (backtrace-exclusive-time bt))
                                           (incf (backtrace-exclusive-cons bt2) (backtrace-exclusive-cons bt))
                                           (incf (backtrace-calls bt2) (backtrace-calls bt))
                                           (incf (backtrace-nested-calls bt2) (backtrace-nested-calls bt))
                                           (fill bt 0))))
                                     ;; Found a place to put it
                                     (setf (backtrace-length bt) this-length)
                                     (dotimes (j this-length)
                                       (declare (fixnum j))
                                       (setf (backtrace-ref bt j) (svref this-backtrace j))))

                                   ;; Discount resources spent collecting backtrace
                                   (incf *backtrace-time* (- (get-time) end-time))
                                   (incf *backtrace-cons* (- (get-cons) end-cons))

                                   ;; Found the backtrace, record the information in it
                                   (incf (backtrace-calls bt))
                                   (incf *total-calls*)
                                   ;;; nested-calls includes this call
			           (incf (backtrace-nested-calls bt) (- *total-calls* prev-total-calls))
                                   ;; Time
                                   (incf (backtrace-inclusive-time bt) delta-time)
                                   (incf (backtrace-exclusive-time bt)
                                         (+ delta-time (- prev-total-time *total-time*)))
                                   (setf *total-time* (+ delta-time prev-total-time))
                                   ;; Consing
                                   (incf (backtrace-inclusive-cons bt) delta-cons)
                                   (incf (backtrace-exclusive-cons bt)
                                         (+ delta-cons (- prev-total-cons *total-cons*)))
                                   (setf *total-cons* (+ delta-cons prev-total-cons))))
                               `(;; Calls
			         (incf calls)
			         (incf *total-calls*)
                                 ;;; nested-calls includes this call
			         (incf nested-calls (- *total-calls*
					               prev-total-calls))
;       			 (setf nested-calls (+ old-nested-calls
;					            (- *total-calls*
;						       prev-total-calls)))
			         ;; Time
                                 ;;; Problem with inclusive time is that it
                                 ;;; currently doesn't add values from recursive
                                 ;;; calls to the same function. Change the
                                 ;;; setf to an incf to fix this?
			         (incf inclusive-time delta-time)
;			         (setf inclusive-time (+ delta-time old-time))
			         (incf exclusive-time (+ delta-time
						         (- prev-total-time 
						            *total-time*)))
			         (setf *total-time* (+ delta-time prev-total-time))
			         ;; Consing
			         (incf inclusive-cons delta-cons)
;			         (setf inclusive-cons (+ delta-cons old-cons))
			         (incf exclusive-cons (+ delta-cons
						         (- prev-total-cons 
						            *total-cons*)))
			         (setf *total-cons* (+ delta-cons prev-total-cons)))))))))))
	   (setf (get-monitor-info name)
	         (make-metering-functions
		  :name name
		  :old-definition old-definition
		  :new-definition (place-function name)
		  :read-metering ,(if backtracep
                                    `#'(lambda (&optional backtracep)
                                         (read-backtrace-metering backtraces backtracep))
                                    `#'(lambda (&optional backtracep)
                                         (declare (ignore backtracep))
				         (values inclusive-time
					         inclusive-cons
					         exclusive-time
					         exclusive-cons
					         calls
					         nested-calls)))
		  :reset-metering ,(if backtracep
                                     `#'(lambda ()
                                          (dotimes (i (length backtraces))
                                            (fill (svref backtraces i) 0))
                                          t)
                                     `#'(lambda ()
				          (setq inclusive-time 0
					        inclusive-cons 0
					        exclusive-time 0 
					        exclusive-cons 0
					        calls 0
					        nested-calls 0)
				          t))))))))
  );; End of EVAL-WHEN

;;; backtracep is nil to read a summary or the index of a backtrace to get
;;; returns seven values: inclusive-time inclusive-cons exclusive-time exclusive-cons
;;;                       calls nested-calls backtraces
(defun read-backtrace-metering (backtraces backtracep)
  (if backtracep
    (let ((bt (svref backtraces backtracep)))
      (values (backtrace-inclusive-time bt) (backtrace-inclusive-cons bt)
              (backtrace-exclusive-time bt) (backtrace-exclusive-cons bt)
              (backtrace-calls bt) (backtrace-nested-calls bt)
              backtraces))
    (values (loop for bt across backtraces
                  sum (backtrace-inclusive-time bt))
            (loop for bt across backtraces
                  sum (backtrace-inclusive-cons bt))
            (loop for bt across backtraces
                  sum (backtrace-exclusive-time bt))
            (loop for bt across backtraces
                  sum (backtrace-exclusive-cons bt))
            (loop for bt across backtraces
                  sum (backtrace-calls bt))
            (loop for bt across backtraces
                  sum (backtrace-nested-calls bt))
            backtraces)))

(defun monitoring-encapsulation-p (function-name)
  (and (stringp function-name)
       (> (length function-name) 8)
       (not (mismatch function-name "Monitor-" :end1 8))))


;;; For efficiency reasons, we precompute the encapsulation functions
;;; for a variety of combinations of argument structures 
;;; (min-args . optional-p). These are stored in the following hash table
;;; along with any new ones we encounter. Since we're now precomputing
;;; closure functions for common argument signatures, this eliminates
;;; the former need to call COMPILE for each monitored function.  
(eval-when (compile eval)
   (defconstant precomputed-encapsulations 8))

(defvar *existing-encapsulations* (make-hash-table :test #'equal))
(defun find-encapsulation (min-args optionals-p backtracep)
  (let ((key (if backtracep (cons optionals-p min-args) (cons min-args optionals-p))))
    (or (gethash key *existing-encapsulations*)
        (setf (gethash key *existing-encapsulations*)
	      (compile nil
		       (make-monitoring-encapsulation min-args optionals-p backtracep))))))

(macrolet ((frob ()
	     (let ((res ()))
	       (dotimes (i precomputed-encapsulations)
		 (push `(setf (gethash '(,i . nil) *existing-encapsulations*)
			      #',(make-monitoring-encapsulation i nil nil))
		       res)
		 (push `(setf (gethash '(,i . t) *existing-encapsulations*)
			      #',(make-monitoring-encapsulation i t nil))
		       res))
	       `(progn ,@res))))
  (frob))

(defun monitoring-encapsulate (name &optional warn backtracep)
  "Monitor the function Name. If already monitored, unmonitor first."
  ;; Saves the current definition of name and inserts a new function which
  ;; returns the result of evaluating body. 
  (unless *monitor-time-overhead*
    (set-monitor-overhead))
  (cond ((not (place-fboundp name))	; not a function
	 (when warn
	   (warn "Ignoring undefined function ~S." name)))
	((place-macrop name)		; a macro
	 (when warn
	   (warn "Ignoring macro ~S." name)))
	(t				; tis a function
	 (when (get-monitor-info name) ; monitored
	   (when warn
	     (warn "~S already monitored, so unmonitoring it first." name))
	   (monitoring-unencapsulate name))
	 (multiple-value-bind (min-args optionals-p)
	                      (required-arguments name)
	   (funcall (find-encapsulation min-args optionals-p backtracep) name)))))

(defun monitoring-unencapsulate (name &optional warn)
  "Removes monitoring encapsulation code from around Name."
  (let ((finfo (get-monitor-info name)))
    (when finfo				; monitored
      ;(remprop name 'metering-functions) --- uses a hash table now, breaks for setf names
      (setq *monitored-functions* 
	    (remove name *monitored-functions* :test #'equal))
      (if (eq (place-function name)
	      (metering-functions-new-definition finfo))
        (setf (place-function name)
              (metering-functions-old-definition finfo)) 
        (when warn
          (warn "Preserving current definition of redefined function ~S."
                name))))))
    
;;; ********************************
;;; Main Monitoring Functions ******
;;; ********************************
(defmacro MONITOR (&rest names)
  "Monitor the named functions. As in TRACE, the names are not evaluated.
   If a function is already monitored, then unmonitor and remonitor (useful
   to notice function redefinition). If a name is undefined, give a warning
   and ignore it. See also unmonitor, report-monitoring, 
   display-monitoring-results and reset-time."
  `(progn
     ,@(mapcar #'(lambda (name) `(monitoring-encapsulate ',name)) names)
     *monitored-functions*))

(defmacro UNMONITOR (&rest names)
  "Remove the monitoring on the named functions. 
   Names defaults to the list of all currently monitored functions."
  `(progn
     (stop-gc-monitor)
     (dolist (name ,(if names `',names '*monitored-functions*) (values))
       (monitoring-unencapsulate name))))		 

(defun MONITOR-ALL (&optional (package *package*) (backtracep nil))
  "Monitor all functions in the specified package."
  (let ((package (if (symbolp package)
                   (find-package package)
                   package)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
	(monitoring-encapsulate symbol nil backtracep)))))

(defmacro with-monitoring-1 (monitored-places (&rest keys) &body body)
  "Monitor the specified functions during the execution of the body."
  (let ((places (gensym))
        (place (gensym))
        (nested (gensym))
        (threshold (gensym))
        (key (gensym))
        (backtrace (gensym))
        (start-time (gensym))
        (end-time (gensym))
        (start-cons (gensym))
        (end-cons (gensym))
        (start-gc (gensym))
        (end-gc (gensym)))
    `(unwind-protect
       ((lambda (,places
                 &key ((:nested ,nested) :exclusive)
                      ((:threshold ,threshold) 0.01)
                      ((:key ,key) :percent-time)
                      ((:backtrace ,backtrace) nil))
          (dolist (,place ,places)
            (if (or (keywordp ,place)
                    (packagep ,place))
              (monitor-all ,place ,backtrace)
              (monitoring-encapsulate ,place nil ,backtrace)))
          (reset-all-monitoring)
          (let ((,start-time (get-time))
                (,start-cons (get-cons))
                (,start-gc (get-gc))
                ,end-time ,end-cons ,end-gc)
            (multiple-value-prog1
              (let ((*monitoring-enabled* t))
		(time (multiple-value-prog1
		       (progn ,@body)
		       (setf ,end-time (get-time)
			     ,end-cons (get-cons)
			     ,end-gc (get-gc)))))
              (report-monitoring :all ,nested ,threshold ,key nil
                                 (- (get-time) ,start-time) (- (get-cons) ,start-cons)
                                 (- (get-gc) ,start-gc) ,start-gc))))
        ,monitored-places ,@keys)
       (unmonitor))))

(defmacro MONITOR-FORM (form &rest keys)
  "Monitor the execution of all functions in the current package
during the execution of FORM.  All functions that are executed above
THRESHOLD % will be reported."
  `(with-monitoring-1 (list *package*) (,@keys) ,form))

(defmacro WITH-MONITORING (places (&rest keys) &body body)
  "Monitor the specified functions during the execution of the body."
  `(with-monitoring-1 ,places (,@keys) ,@body))

;;; ********************************
;;; Overhead Calculations **********
;;; ********************************
(defconstant overhead-iterations 5000
  "Number of iterations over which the timing overhead is averaged.")

;;; Perhaps this should return something to frustrate clever compilers.
(defun STUB-FUNCTION (x)
  (declare (ignore x))
  nil)
(proclaim '(notinline stub-function))

(defun SET-MONITOR-OVERHEAD ()
  "Determines the average overhead of monitoring by monitoring the execution
of an empty function many times." 
  (setq *monitor-time-overhead* 0
	*monitor-cons-overhead* 0)
  (stub-function nil)
  (monitor stub-function)
  (reset-all-monitoring)
  (let ((overhead-function (symbol-function 'stub-function))
	(*monitoring-enabled* t))
    (dotimes (x overhead-iterations)
      (funcall overhead-function overhead-function)))
;  (dotimes (x overhead-iterations)
;    (stub-function nil))
  (let ((fiter (float overhead-iterations)))
    (multiple-value-bind (calls nested-calls time cons)
	(monitor-info-values 'stub-function)
      (declare (ignore calls nested-calls))
      (setq *monitor-time-overhead* (/ time fiter)
	    *monitor-cons-overhead* (/ cons fiter))))
  (unmonitor stub-function))

;;; ********************************
;;; Report Data ********************
;;; ********************************
(defvar *monitor-results* nil
  "A table of monitoring statistics is stored here.")
(defvar *no-calls* nil
  "A list of monitored functions which weren't called.")
(defvar *estimated-total-overhead* 0)
(proclaim '(type time-type *estimated-total-overhead*))

(defstruct (monitoring-info 
             (:conc-name m-info-)
             (:constructor make-monitoring-info
                              (name calls time cons
                                     percent-time percent-cons
                                     time-per-call cons-per-call 
                                     backtraces)))
  name
  calls
  time
  cons
  percent-time
  percent-cons
  time-per-call
  cons-per-call
  backtraces)

(defun REPORT-MONITORING (&optional names 
                                       (nested :exclusive) 
				       (threshold 0.01)
				       (key :percent-time)
				       ignore-no-calls
                                       run-time
                                       run-cons
                                       run-gc
                                       start-gc)
  "Report the current monitoring state.
The percentage of the total time spent executing unmonitored code
in each function (:exclusive mode), or total time (:inclusive mode)
will be printed together with the number of calls and
the unmonitored time per call.  Functions that have been executed
below THRESHOLD % of the time will not be reported."
  (when (or (null names) (eq names :all)) (setq names *monitored-functions*))

  (let ((total-time (or run-time 0))
	(total-cons (or run-cons 0))
	(total-calls *total-calls*))
    (if run-time
      ;; Subtract off overhead
      (setq total-time (- total-time (* total-calls *monitor-time-overhead*) *backtrace-time*)
            total-cons (- total-cons (* total-calls *monitor-cons-overhead*) *backtrace-cons*))
      ;; Compute overall time and consing if not supplied as arguments
      (dolist (name names)
        (multiple-value-bind (calls nested-calls time cons)
	                     (monitor-info-values name nested :warn)
	  (declare (ignore calls nested-calls))
	  (incf total-time time)
	  (incf total-cons cons))))
    ;; Total overhead.
    (setq *estimated-total-overhead* 	    
	  (/ (* *monitor-time-overhead* total-calls)
	     time-units-per-second))
    ;; Assemble data for only the specified names (all monitored functions)
    (if (zerop total-time)
      (format *trace-output* "Not enough execution time to monitor.")
      (progn
        (setq *monitor-results* nil *no-calls* nil)
        (labels ((record (name calls time cons backtraces)
                   (when (minusp time) (setq time 0.0))
                   (when (minusp cons) (setq cons 0.0))
                   (if (zerop calls)
                     (push (if (symbolp name) 
                             (symbol-name name)
                             (format nil "~S" name))
                           *no-calls*)
                     (push (record-1 name calls time cons backtraces) *monitor-results*)))
                 (record-1 (name calls time cons backtraces)
                   (make-monitoring-info
                     name                                       ; name
                     calls                                      ; calls
                     (/ time (float time-units-per-second))     ; time in secs
                     (round cons)                               ; consing
                     (/ time (float total-time))                ; percent-time
                     (if (zerop total-cons) 0
                         (/ cons (float total-cons)))           ; percent-cons
                     (/ (/ time (float calls))                  ; time-per-call
                        time-units-per-second)                  ; sec/call
                     (round (/ cons (float calls)))             ; cons-per-call
                     backtraces)))                              ; backtraces
          (dolist (name names)
            (multiple-value-bind (calls nested-calls time cons backtraces)
		                 (monitor-info-values name nested)
              (declare (ignore nested-calls))
              (record (format nil "~S" name) calls time cons
                      (and backtraces
                           (loop for i from 0 below (length backtraces)
                                 as backtrace = (svref backtraces i)
                                 unless (zerop (backtrace-calls backtrace))
                                 collect (multiple-value-bind (calls nested-calls time cons)
                                                              (monitor-info-values name nested nil i)
                                           (declare (ignore nested-calls))
                                           (record-1 (format nil "~S #~D" name (1+ i))
                                                     calls time cons backtrace))))))))
        (display-monitoring-results threshold key ignore-no-calls nested
                                    total-calls total-time total-cons run-gc start-gc)))))

(defun display-monitoring-results (&optional (threshold 0.01)
					     (key :percent-time)
					     (ignore-no-calls t)
					     (nested :exclusive)
					     run-calls
					     run-time
					     run-consed 
					     run-gc
					     start-gc)
  (let ((max-length 8)			; Function header size
        (total-time 0.0)
	(total-consed 0)
	(total-calls 0)
	(total-percent-time 0)
	(total-percent-cons 0)
        (ones-with-backtraces nil))			
    (labels
       ((display-internal (*monitor-results* totalize total-result)
         (let ((n-items 0))
          (setq max-length 8)
          (sort-results key)
          (dolist (result *monitor-results*)
            (when (or (< n-items *minimum-breakdown-size*)
                      (zerop threshold)
                      (> (m-info-percent-time result) threshold)
                      (> (m-info-percent-cons result) threshold))
              (incf n-items)
              (setq max-length
                    (max max-length
                         (length (m-info-name result))))))
          (incf max-length 2)
          (format *trace-output*
                  "~&        ~VT                                     Cons~
                   ~&        ~VTfrac   frac            ~Aclusive     Per      Total       Total~
                   ~&Function~VTTime   Cons    Calls    Sec/Call     Call     Time        Cons~
                   ~&~V,,,'-A"
                  max-length max-length (if (eq nested :exclusive) "ex" "in")
                  max-length
                  (+ max-length 63) "-")	
          (dolist (result *monitor-results*)
            (when (or (>= (decf n-items) 0)
                      (zerop threshold)
  		      (> (m-info-percent-time result) threshold)
                      (> (m-info-percent-cons result) threshold))
              (display-1 result)
              (when totalize
  	        (incf total-time (m-info-time result))
  	        (incf total-consed (m-info-cons result))
  	        (incf total-calls (m-info-calls result))
  	        (incf total-percent-time (m-info-percent-time result))
  	        (incf total-percent-cons (m-info-percent-cons result)))
              (when (m-info-backtraces result) (push result ones-with-backtraces))))
          (when total-result
            (format *trace-output* "~&~V,,,'-A" (+ max-length 63) "-")
            (display-1 total-result))))
        (display-1 (result)
          (format *trace-output* 
                  "~&~A:~VT~5,3F  ~5,3F  ~6D  ~10,6F ~8D  ~10,6F  ~8D"
                  (m-info-name result)
                  max-length
                  (m-info-percent-time result)
                  (m-info-percent-cons result)
                  (m-info-calls result)
                  (m-info-time-per-call result)
                  (m-info-cons-per-call result)
                  (m-info-time result)
                  (m-info-cons result))))
      (display-internal *monitor-results* t nil)
      (format *trace-output* 
	      "~&~V,,,'-A~
	       ~&TOTAL: ~VT~5,3F  ~5,3F  ~6D                       ~10,6F  ~8D~
	       ~&THIS RUN:~VT              ~6D                       ~10,6F  ~8D~
               ~&~V,,,'-A~
               ~&Estimated monitoring overhead per call: ~,6F seconds, ~,2F conses~
               ~&Estimated monitoring overhead for calls shown: ~,2F seconds~
               ~&Estimated total monitoring overhead for this run: ~,2F seconds"
	      (+ max-length 63) "-"
	      max-length 
	      total-percent-time total-percent-cons 
	      total-calls total-time total-consed
              max-length run-calls (/ run-time time-units-per-second) (round run-consed)
              (+ max-length 63) "-"
              (/ *monitor-time-overhead* time-units-per-second) *monitor-cons-overhead*
	      (/ (* *monitor-time-overhead* total-calls)
	         time-units-per-second)
	      *estimated-total-overhead*)
      (when (and run-GC (> run-GC 0))
        (format *trace-output* "~&Garbage collection occurred in this run, took ~,2F seconds."
                (/ run-GC time-units-per-second)))
      (unless (zerop *backtrace-time*)
        (format *trace-output* 
	        "~&Estimated backtrace collection overhead for this run: ~,2F seconds, ~:D conses~
                 ~&Note: collecting backtraces makes all time measurements less accurate."
                (/ *backtrace-time* time-units-per-second) *backtrace-cons*))

      (when ones-with-backtraces
        (dolist (result (nreverse ones-with-backtraces))
          (setq ones-with-backtraces nil)
          (cond ((cdr (m-info-backtraces result))
                 (format *trace-output* "~2%Breakdown for ~A:" (m-info-name result))
                 (display-internal (m-info-backtraces result) nil result))
                (t (setq ones-with-backtraces (m-info-backtraces result))))
          (format *trace-output* "~2%Backtrace~P for ~A:" 
                  (length ones-with-backtraces) (m-info-name result))
          (let ((max-length 0))
            (dolist (result ones-with-backtraces)
              (setq max-length (max max-length (length (m-info-name result)))))
            (incf max-length 2)
            (dolist (result (nreverse ones-with-backtraces))
              (format *trace-output* "~%~A:~VT~:[no pattern found~;~:*~<~@{~S + ~D~^ <- ~:_~}~:>~]"
                      (m-info-name result) max-length
                      (loop with bt = (m-info-backtraces result)
                            for i from 0 below (backtrace-length bt)
                            append (multiple-value-bind (f o)
                                       (decode-backtrace-element (backtrace-ref bt i))
                                     (unless (monitoring-encapsulation-p f)
                                       (list f o))))))))
        (unless (= (get-gc) start-gc)
          (format *trace-output*
                  "~2&Warning: garbage collection has occurred, backtrace information might be wrong.")))

      (when (and (not ignore-no-calls) *no-calls*)
        (setq *no-calls* (sort *no-calls* #'string<))
        (let ((num-no-calls (length *no-calls*)))
	  (if (> num-no-calls 20)
            (format *trace-output*
                    "~%~@(~r~) monitored functions were not called. ~
                     ~%See the variable mon::*no-calls* for a list."
                    num-no-calls)
            (format *trace-output*
                    "~%The following monitored functions were not called:~
                     ~%~{~<~%~:; ~A~>~}~%"
                    *no-calls*))))
      (values))))

(defun sort-results (&optional (key :percent-time))
  (setq *monitor-results* 
	(case key
	  (:function             (sort *monitor-results* #'string>
				       :key #'m-info-name))
	  ((:percent-time :time) (sort *monitor-results* #'>
				       :key #'m-info-time))
	  ((:percent-cons :cons) (sort *monitor-results* #'>
				       :key #'m-info-cons))
	  (:calls	         (sort *monitor-results* #'>
				       :key #'m-info-calls))
	  (:time-per-call	 (sort *monitor-results* #'> 
				       :key #'m-info-time-per-call))
	  (:cons-per-call        (sort *monitor-results* #'>
				       :key #'m-info-cons-per-call)))))  

;;; *END OF FILE*




#|
	Change History (most recent last):
	1	11/09/92	Derek	Mark Kantrowitz's metering tools fixed by KAB to work with MCL 2.0
				  and setf functions.
	2	11/16/92	Derek	Made the threshold for displaying results depend on %consing as well
				  as %time.
	3	1/26/93	Moon	Improve readability and accuracy of results.  Add backtrace collection feature.  Use keyword arguments instead of a million confusing optionals.
|# ;(do not edit past this line!!)
