;;;; Module: Interactive Mode for Dylan Console Debugger
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

(require 'dylan-mode)
(require 'comint)
(require 'cl)
(require 'info)

(defvar dylan-debug-program-name 
  (tilde-dylan-install-file-name "/bin/devel-dbg-ui.exe")
  "*The filename of the Dylan console debugger program.")

(defvar dylan-debug-raw-buffer-name "Dylan Debugger"
  "The basic buffer name for the Dylan console debugger, sans *\'s.")

(defvar dylan-debug-buffer-name (concat "*" dylan-debug-raw-buffer-name "*")
  "The full buffer name for the Dylan console debugger.")

(defvar dylan-debug-mode-reopen-map (make-sparse-keymap)
  "The dynamic menu built from applications opened via the open menu.
")

(defvar dylan-debug-last-command nil
  "The last command issued to the Dylan console debugger.")

(defvar dylan-debug-find-file-function 'find-file
  "*User control over how the Dylan console debugger visits files for editing.")

(defvar dylan-debug-auto-find-file-function 'find-file-other-window
  "*User control over how the Dylan console debugger visits files for
editing when synchronized with stack frame.")

(defvar dylan-debug-switch-to-buffer-function 'switch-to-buffer
  "*User control over how the Dylan console debugger finds the debugger
window for running commands.")

(defvar dylan-debug-auto-switch-to-buffer-function 'pop-to-buffer
  "*User control over how the Dylan console debugger finds the debugger
window for running commands when doing synchronized editing with stack frame.")

(defvar dylan-debug-in-auto-edit-p nil
  "Internal flag for indicating auto-edit context.")

(defvar dylan-debug-source-change-commands
  '(
    dylan-debug-up
    dylan-debug-down
    dylan-debug-top
    dylan-debug-bottom
    dylan-debug-step-out
    dylan-debug-step-over
    dylan-debug-step-into
    )
  "Commands that change the current frame.")

(defvar dylan-debug-source-display-commands
  '(
    dylan-debug-frame
    dylan-debug-up
    dylan-debug-down
    dylan-debug-top
    dylan-debug-bottom
    dylan-debug-edit
    )
  "Commands that display the currently associated source as part of their output.")

(defun setup-dylan-debug-mode-map ()
  "Setup key sequences and menu items specific to Dylan console
debugger\'s interactive mode.
"
  (let ((map (copy-keymap comint-mode-map)))

    (define-key map "\C-a" 'comint-bol)
    (define-key map "\C-i" 'comint-dynamic-complete)

    ;; stepping coming so reserve s, and n.

    (define-key map "\C-c\C-b" 'dylan-debug-break) ; Break C-u C-c C-b clears
    (define-key map "\C-c\C-l" 'dylan-debug-break-line) ; Line
    (define-key map "\C-c\C-t" 'dylan-debug-trace) ; C-u C-c C-t untraces
    (define-key map "\C-c\C-c" 'dylan-debug-continue) ; Continue
    (define-key map "\C-c\C-u" 'dylan-debug-up) ; Up
    (define-key map "\C-c\C-d" 'dylan-debug-down) ; Down
    (define-key map "\C-c\C-p" 'dylan-debug-print) ; Print
    (define-key map "\C-c\C-i" 'dylan-debug-describe) ; Inspect
    (define-key map "\C-c\C-e" 'dylan-debug-edit) ; Edit
    (define-key map "\C-c\C-k" 'dylan-debug-backtrace) ; stacK bacKtrace
    (define-key map "\C-c\C-f" 'dylan-debug-step-out) ; Finish Function (GUD)
    (define-key map "\C-c\C-n" 'dylan-debug-step-over) ; Next
    (define-key map "\C-c\C-s" 'dylan-debug-step-into) ; Step

    (define-key map [menu-bar dylan-debug] (cons "Dylan/Debug" (make-sparse-keymap)))

    (define-key map [menu-bar dylan-debug dylan-debug-options] '("Options..." . dylan-debug-options))
    (define-key map [menu-bar dylan-debug dylan-debug-info] '("Info" . dylan-debug-info))
    (define-key map [menu-bar dylan-debug dylan-debug-help] '("Help" . dylan-debug-help))

    (define-key map [menu-bar dylan-debug dylan-debug-misc] (cons "Misc" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-profiler] (cons "Profile" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-stepper] (cons "Step" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-printer] (cons "Print" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker] (cons "Break" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-libraries] (cons "Libraries" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-frames] (cons "Frames" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-threads] (cons "Threads" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-debug dylan-debug-app] (cons "Application" (make-sparse-keymap)))

    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-nearto] '("Find Symbols Near To..." . dylan-debug-nearto))
    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-show-registers] '("Show Registers" . dylan-debug-show-registers))
    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-set] '("Set Value..." . dylan-debug-set))
    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-display] '("Display Memory..." . dylan-debug-display))
    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-load-init-file] '("Load Command File..." . dylan-debug-load-command-file))
    (define-key map [menu-bar dylan-debug dylan-debug-misc dylan-debug-load-command-file] '("ReLoad Init File" . dylan-debug-load-init-file))
    
    (define-key map [menu-bar dylan-debug dylan-debug-profiler dylan-debug-profile-results] '("Stop..." . dylan-debug-profile-results)) 
    (define-key map [menu-bar dylan-debug dylan-debug-profiler dylan-debug-profile] '("Start..." . dylan-debug-profile)) 

    (define-key map [menu-bar dylan-debug dylan-debug-stepper dylan-debug-step-into] '("Step Into" . dylan-debug-step-into))
    (define-key map [menu-bar dylan-debug dylan-debug-stepper dylan-debug-step-out] '("Step Out" . dylan-debug-step-out))
    (define-key map [menu-bar dylan-debug dylan-debug-stepper dylan-debug-step-over] '("Step Over" . dylan-debug-step-over))

    (define-key map [menu-bar dylan-debug dylan-debug-printer dylan-debug-evaluate] '("Evaluate..." . dylan-debug-evaluate))
    (define-key map [menu-bar dylan-debug dylan-debug-printer dylan-debug-describe] '("Describe..." . dylan-debug-describe))
    (define-key map [menu-bar dylan-debug dylan-debug-printer dylan-debug-print] '("Print..." . dylan-debug-print))

    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-breakpoints] '("List All" . dylan-debug-breakpoints))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-breaker-divider-1] '("--"))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-untrace-all] '("Clear All Traces" . dylan-debug-untrace-all))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-untrace] '("Clear Trace..." . dylan-debug-untrace))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-trace] '("Set Trace On Function..." . dylan-debug-trace))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-breaker-divider-2] '("--"))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-clear-all] '("Clear All Breaks" . dylan-debug-clear-all))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-ignore] '("Ignore Break..." . dylan-debug-ignore))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-disable] '("Disable Break..." . dylan-debug-disable))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-enable] '("Enable Break..." . dylan-debug-enable))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-clear-line] '("Clear Break At Line..." . dylan-debug-clear-line))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-break-line] '("Set Break At Line..." . dylan-debug-break-line))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-clear] '("Clear Break On Function..." . dylan-debug-clear))
    (define-key map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-break] '("Set Break On Function..." . dylan-debug-break))

    (define-key map [menu-bar dylan-debug dylan-debug-libraries dylan-debug-show-libraries] '("Show All Libraries" . dylan-debug-show-libraries))
    (define-key map [menu-bar dylan-debug dylan-debug-libraries dylan-debug-in-library] '("Select Library..." . dylan-debug-in-library))
    (define-key map [menu-bar dylan-debug dylan-debug-libraries dylan-debug-in-module] '("Select Module..." . dylan-debug-in-module))

    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-bottom] '("Move to Bottom" . dylan-debug-bottom))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-down] '("Move Down" . dylan-debug-down))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-up] '("Move Up" . dylan-debug-up))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-top] '("Move to Top" . dylan-debug-top))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-divider-1] '("--"))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-frame-at] '("Show Frame At..." . dylan-debug-frame-at))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-frame] '("Show Frame" . dylan-debug-frame))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-divider-2] '("--"))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-backtrace-to] '("Backtrace To..." . dylan-debug-backtrace-to))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-backtrace-verbose] '("Verbose Backtrace" . dylan-debug-backtrace-verbose))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-backtrace] '("Backtrace" . dylan-debug-backtrace))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-divider-3] '("--"))
    (define-key map [menu-bar dylan-debug dylan-debug-frames dylan-debug-edit] '("Edit Source" . dylan-debug-edit))

    (define-key map [menu-bar dylan-debug dylan-debug-threads dylan-debug-resume] '("Resume..." . dylan-debug-resume))
    (define-key map [menu-bar dylan-debug dylan-debug-threads dylan-debug-suspend] '("Suspend..." . dylan-debug-suspend))
    (define-key map [menu-bar dylan-debug dylan-debug-threads dylan-debug-in-thread] '("Select..." . dylan-debug-in-thread))
    (define-key map [menu-bar dylan-debug dylan-debug-threads dylan-debug-show-threads] '("Show All" . dylan-debug-show-threads))

    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-quit] '("Quit" . dylan-debug-quit))
    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-kill] '("Kill" . dylan-debug-kill))
    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-restart] '("Restart" . dylan-debug-restart))
    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-continue] '("Continue" . dylan-debug-continue))
    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-reopen] (cons "ReOpen" dylan-debug-mode-reopen-map))
    (define-key map [menu-bar dylan-debug dylan-debug-app dylan-debug-open] '("Open..." . dylan-debug-open))

    map))

(defvar dylan-debug-mode-map (setup-dylan-debug-mode-map)
  "The key sequences and menu items specific to the Dylan console
debugger\'s interactive mode.
")

(defvar dylan-debug-prompt "Command> "
  "A regexp for finding the Dylan console debugger\'s prompt.")

(defvar dylan-debug-font-lock-keywords
  (list (cons dylan-debug-prompt font-lock-keyword-face)
	(cons "^;;; .*" font-lock-comment-face))
  "A description of what should be highlighed in the Dylan console
debugger\'s interactive mode. See FONT-LOCK-KEYWORDS for the format.
")

(defun dylan-debug (&optional wait-p)
  "Starts up the Dylan console debugger in an interactive buffer. If
the debugger is already running in a buffer then this command just
switches to it. The user can control which program is run by setting
the variable DYLAN-DEBUG-PROGRAM-NAME. For more information on the
debugger see the DYLAN-DEBUG Info file (C-h i).
"
  (interactive)
  (unless (comint-check-proc dylan-debug-buffer-name)
    (set-buffer (make-comint dylan-debug-raw-buffer-name shell-file-name))
    (dylan-debug-mode)
    (dylan-link-initialize)
    (dylan-debug-initialize)
    (setq dylan-debug-last-source-location nil)
    (setq dylan-debug-last-command nil))
  (funcall (if (and wait-p (member 'dylan-debug-auto-edit dylan-debug-command-hook))
	       dylan-debug-auto-switch-to-buffer-function
	     dylan-debug-switch-to-buffer-function)
	   dylan-debug-buffer-name)
  (when wait-p
    (dylan-wait-for-prompt dylan-debug-prompt)))

(defvar dylan-debug-init-file-path "~/.debugrc"
  "The name of a file that contains debugger commands that should be executed on start up.")

(defun dylan-debug-init-file-exists-p ()
  "Tests if Dylan Debugger init file exists."
  (file-exists-p dylan-debug-init-file-path))

(defun dylan-debug-load-init-file ()
  "Runs Dylan Debugger commands from init file."
  (interactive)
  (dylan-debug-load-command-file dylan-debug-init-file-path))

(defun dylan-debug-load-command-file (file)
  "Runs Dylan Debugger commands from given file."
  (interactive "fDebugger command file to load: ")
  (let ((initfile (expand-file-name file))
	(initstring nil))
    (unless (file-exists-p initfile)
      (error "Command file %s not found" initfile))
    (save-excursion
      (let ((initbuffer (find-file initfile)))
	(setq initstring (buffer-string))
	(kill-buffer initbuffer)))
    (dylan-debug-send-command 'initialize initstring)))

(defun dylan-debug-initialize ()
  (dylan-debug-send-command 'initialize dylan-debug-program-name)
  (if (dylan-debug-init-file-exists-p)
      (dylan-debug-load-init-file)))

(defun dylan-debug-mode ()
  "The major mode for interacting with the Dylan console debugger. The
user can customize the initialization of the mode through
DYLAN-DEBUG-MODE-HOOK. For more information on the debugger see the
DYLAN-DEBUG Info file (C-h i).
"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp dylan-debug-prompt)
  (setq major-mode 'dylan-debug-mode)
  (setq mode-name "Dylan Debug")
  (setq mode-line-process '(":%s"))
  (make-local-variable 'font-lock-defaults)
  (make-local-hook 'dylan-debug-mode-hook)
  (make-local-hook 'dylan-debug-command-hook)
  (setq font-lock-defaults '((dylan-debug-font-lock-keywords)))
  (use-local-map dylan-debug-mode-map)
  (run-hooks 'dylan-debug-mode-hook))

(defun dylan-debug-send-command (command fmt &rest args)
  "Core function for controlling Dylan console debugger. Inserts
formatted command strings into the interactive buffer\'s prompt and
then sends them to the debugger process.
"
  (setq dylan-debug-last-command command)
  (end-of-buffer)
  (insert (apply 'format fmt args))
  (comint-send-input)
  (when (memq command dylan-debug-source-display-commands)
    (dylan-wait-for-prompt dylan-debug-prompt))
  (run-hooks 'dylan-debug-command-hook))

(defun current-word-modulo-prompt (&optional describe-context-p)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at " +\\([^ ]+\\) = [^ ]")
	   (concat "local " (match-string 1)))
	  ((looking-at "0x[0-9a-f]+ +\\(\\$[0-9]+\\) : [^ ]")
	   (match-string 1))
	  (t (let ((word (current-word)))
	       (if (string= (concat word " ") dylan-debug-prompt)
		   ""
		 word))))))

;;; INDIVIDUAL COMMANDS

(defun dylan-debug-open (program)
  "Starts an application in the Dylan console debugger. (Command line
arguments are not yet supported through this interactive invocation.)
"
  (interactive "fDylan Program to Debug: ")
  (dylan-debug-update-reopen-menu program)
  (dylan-debug-send-command 'dylan-debug-open "open \"%s\"" (expand-file-name program)))

(defvar *dylan-debug-programs* nil
  "Cache of previously opened applications.
")

(defun dylan-debug-update-reopen-menu (program)
  "Rebuilds ReOpen menu from cache of previously opened applications.
"
  (pushnew program *dylan-debug-programs* :test 'string-equal :key 'downcase)
  (setcdr dylan-debug-mode-reopen-map
	  (mapcar '(lambda (program)
		     (cons program	; HACK: use program as event
					; (RMS Himself does the same,
					; and I'm not worthy ...)
			   (cons program 'dylan-debug-reopen)))
		  *dylan-debug-programs*)))
	  
(defun dylan-debug-reopen ()
  "Starts an application in the Dylan console debugger. The
application is assumed to be the last \'event\' (left there by the use
of the associated menu item).
"
  (interactive)
  (dylan-debug-open last-command-event))

(defun dylan-debug-quit ()
  "Confirms and then quits the Dylan console debugger.
"
  (interactive)
  (if (y-or-n-p "Quit the Dylan console debugger? ")
      (dylan-debug-send-command 'dylan-debug-quit "quit")))

(defun dylan-debug-continue ()
  "Lets the application run in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-continue "continue"))

(defun dylan-debug-restart ()
  "Restarts the application from the beginning in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-restart "restart"))

(defun dylan-debug-kill ()
  "Kills the current application in the Dylan console debugger.
"
  (interactive)
  (if (y-or-n-p "Kill the debugger\'s current application? ")
      (dylan-debug-send-command 'dylan-debug-kill "kill")))

(defun dylan-debug-show-threads ()
  "Show the current threads in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-show-threads "show threads"))

(defun dylan-debug-in-thread (thread)
  "Select a different thread context in the Dylan console debugger.
"
  (interactive "nSelect Thread: ")
  (dylan-debug-send-command 'dylan-debug-in-thread "in thread %d" thread))

(defun dylan-debug-suspend (thread)
  "Suspend a thread in the Dylan console debugger.
"
  (interactive "nSuspend Thread: ")
  (dylan-debug-send-command 'dylan-debug-suspend "suspend %d" thread))

(defun dylan-debug-resume (thread)
  "Resume a thread in the Dylan console debugger.
"
  (interactive "nResume Thread: ")
  (dylan-debug-send-command 'dylan-debug-resume "resume %d" thread))

(defvar dylan-debug-last-source-location nil)

(defun dylan-debug-edit ()
  "Edit the source of the current stack frame in the Dylan console
debugger.
" 
  (interactive)
  (apply 'dylan-debug-find-file
	 (if (or (null dylan-debug-last-command)
		 (null dylan-debug-last-source-location)
		 (memq dylan-debug-last-command dylan-debug-source-change-commands))
	     (progn
	       (unless (memq dylan-debug-last-command dylan-debug-source-display-commands)
		 (dylan-wait-for-prompt dylan-debug-prompt)
		 (dylan-debug-send-command 'dylan-debug-edit "frame")
		 (dylan-wait-for-prompt dylan-debug-prompt))
	       (dylan-debug-note-executable-path)
	       (setq dylan-debug-last-source-location (dylan-debug-parse-source-location)))
	   dylan-debug-last-source-location)))    

(defun dylan-debug-auto-edit ()
  (interactive)
  (unless dylan-debug-in-auto-edit-p
    (let ((dylan-debug-in-auto-edit-p t))
      (dylan-debug-edit))))

(defun dylan-debug-backtrace ()
  "Show a stack backtrace in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-backtrace "backtrace"))

(defun dylan-debug-backtrace-verbose ()
  "Show a verbose stack backtrace in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-backtrace-verbose "backtrace verbose"))

(defun dylan-debug-backtrace-to (frames)
  "Show a stack backtrace (up to a certain number of a frames) in the
Dylan console debugger.
"
  (interactive "sBacktrace Frames: ")
  (dylan-debug-send-command 'dylan-debug-backtrace-to "backtrace %s" frames))

(defun dylan-debug-frame ()
  "Show the details of the current stack frame in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-frame "frame"))

(defun dylan-debug-frame-at (frame)
  "Show the details of a selected stack frame in the Dylan console
debugger.
"
  (interactive "sShow Frame: ")
  (dylan-debug-send-command 'dylan-debug-frame-at "frame %s" frame))

(defun dylan-debug-up ()
  "Move up to the next newer stack frame in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-up "up"))

(defun dylan-debug-down ()
  "Move down to the previous older stack frame in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-down "down"))

(defun dylan-debug-top ()
  "Move to the top (newest) stack frame in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-top "top"))

(defun dylan-debug-bottom ()
  "Move to the bottom (oldest) stack frame in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-bottom "bottom"))

(defun dylan-debug-in-module (module)
  "Select another Dylan module context in the Dylan console debugger.
"
  (interactive "sSelect Module: ")
  (dylan-debug-send-command 'dylan-debug-in-module "in module %s" module))

(defun dylan-debug-in-library (library)
  "Select another Dylan library context in the Dylan console debugger.
"
  (interactive "sSelect Library: ")
  (dylan-debug-send-command 'dylan-debug-in-library "in library %s" library))

(defun dylan-debug-show-libraries ()
  "Show the currently loaded libraries in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-show-libraries "show libraries"))

(defun dylan-debug-break (prefix)
  "Set a break point on a given function in the Dylan console
debugger.
"
  (interactive "p")
  (if (= prefix 1)
      (%dylan-debug-break (read-input "Set Break Point On Function: " (current-word-modulo-prompt)))
    (dylan-debug-clear)))

(defun %dylan-debug-break (expression)
  (dylan-debug-send-command 'dylan-debug-break "break %s" expression))

(defun dylan-debug-break-line (filename lineno)
  "Set a break point on a given source location in the Dylan console
debugger.
"
  (interactive "fSet Break Point In File: 
nAnd Set Break At Line (starts at 1): ")
  (dylan-debug-send-command 'dylan-debug-break-line "break line %d of \"%s\"" lineno (file-name-nondirectory filename)))

(defun dylan-debug-clear-line (filename lineno)
  "Clear a break point on a given source location in the Dylan console
debugger.
"
  (interactive "fClear Break Point In File: 
nAnd Clear Break At Line (starts at 1): ")
  (dylan-debug-send-command 'dylan-debug-clear-line "clear line %d of \"%s\"" lineno (file-name-nondirectory filename)))

(defun dylan-debug-clear ()
  "Clear a break point from a given function in the Dylan console
debugger.
"
  (interactive)
  (%dylan-debug-clear (read-input "Clear Break Point On Function: " (current-word-modulo-prompt))))

(defun dylan-debug-clear-all ()
  "Clear all break points in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-clear-all "clear"))

(defun dylan-debug-enable ()
  "Enable a break point in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-enable "enable %s" (read-input "Enable Break Point: " (current-word-modulo-prompt))))

(defun dylan-debug-disable ()
  "Disable a break point in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-disable "disable %s" (read-input "Disable Break Point: " (current-word-modulo-prompt))))

(defun dylan-debug-ignore ()
  "Ignore a break point in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-ignore "ignore %s"  (read-input "Ignore Break Point: " (current-word-modulo-prompt))))

(defun %dylan-debug-clear (expression)
  (dylan-debug-send-command 'dylan-debug-clear "clear %s" expression))

(defun dylan-debug-trace (prefix)
  "Set a trace point on a given function in the Dylan console
debugger.
"
  (interactive "p")
  (if (= prefix 1)
      (%dylan-debug-trace (read-input "Set Trace Point On Function: " (current-word-modulo-prompt)))
    (dylan-debug-untrace)))

(defun %dylan-debug-trace (expression)
  (dylan-debug-send-command 'dylan-debug-trace "trace %s" expression))

(defun dylan-debug-untrace ()
  "Clear a trace point from a given function in the Dylan console
debugger.
"
  (interactive)
  (%dylan-debug-untrace (read-input "Clear Trace Point On Function: " (current-word-modulo-prompt))))

(defun dylan-debug-untrace-all ()
  "Clear all trace points in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-untrace-all "untrace"))

(defun %dylan-debug-untrace (expression)
  (dylan-debug-send-command 'dylan-debug-untrace "untrace %s" expression))

(defun dylan-debug-breakpoints ()
  "List all the current break points in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-breakpoints "breakpoints"))

(defun dylan-debug-print ()
  "Print the current value of the given expression in the Dylan
console debugger.
"
  (interactive)
  (%dylan-debug-print (read-input "Print Value Of: " (current-word-modulo-prompt t))))

(defun %dylan-debug-print (expression)
  (dylan-debug-send-command 'dylan-debug-print "print %s" expression))

(defun dylan-debug-describe ()
  "Describe the structure of the given expression in the Dylan console
debugger.
"
  (interactive)
  (%dylan-debug-describe (read-input "Describe Structure Of: " (current-word-modulo-prompt t))))

(defun %dylan-debug-describe (expression)
  (dylan-debug-send-command 'dylan-debug-describe "describe %s" expression))

(defun dylan-debug-evaluate ()
  "Evaluate the result of the given expression in the Dylan console
debugger.
"
  (interactive)
  (%dylan-debug-evaluate (read-input "sEvaluate Result Of: " (current-word-modulo-prompt t))))

(defun %dylan-debug-evaluate (expression)
  (dylan-debug-send-command 'dylan-debug-evaluate "evaluate %s" expression))

(defun dylan-debug-step-out ()
  "Continue running until the current function returns; in the Dylan
console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-step-out "step out"))

(defun dylan-debug-step-into ()
  "Continue running until the next recorded source location; in the Dylan
console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-step-into "step into"))

(defun dylan-debug-step-over ()
  "Step over the next function call; in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-step-over "step over"))

(defun dylan-debug-profile-results ()
  "Stop profiling and display the accumulated results in the Dylan
console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-profile-results "profile results"))

(defun dylan-debug-profile (threads interval depth function)
  "Start profiling with the given parameters in the Dylan console
debugger.
"
  (interactive "sProfile: Select Threads (default: all) 
sProfile: Sample Interval (default: 100 (msecs)) 
sProfile: Stack Depth Limit (default: all) 
sProfile: On Entry To Function (default: none (start immediately)) ")
  (unless (string= threads "")
    (setq threads (concat "threads " threads)))
  (unless (string= interval "")
    (setq interval (concat "at " interval)))
  (unless (string= depth "")
    (setq depth (concat "depth " depth)))
  (unless (string= function "")
    (setq function (concat "in " function)))
  (dylan-debug-send-command 'dylan-debug-profile "profile %s %s %s %s" threads interval depth function))

(defun dylan-debug-nearto ()
  "Find the symbols near to the address given by the expression in the
Dylan console debugger.
"
  (interactive)
  (%dylan-debug-nearto (read-input "Find Symbols Near To: " (current-word-modulo-prompt))))

(defun %dylan-debug-nearto (expression)
  (dylan-debug-send-command 'dylan-debug-nearto "nearto %s" expression))

(defun dylan-debug-display ()
  "Display the memory contents at the address given by the expression
in the Dylan console debugger.
"
  (interactive)
  (%dylan-debug-display (read-input "Display Memory At: " (current-word-modulo-prompt))))

(defun %dylan-debug-display (expression)
  (dylan-debug-send-command 'dylan-debug-display "display %s" expression))

(defun dylan-debug-set ()
  "Set place given by the RVALUE to value given by the LVALUE in the
Dylan console debugger.
"
  (interactive)
  (%dylan-debug-set (read-input "Set: " (current-word-modulo-prompt))
		    (read-input "To Value: " (current-word-modulo-prompt))))

(defun %dylan-debug-set (lvalue rvalue)
  (dylan-debug-send-command 'dylan-debug-set "set %s %s" lvalue rvalue))

(defun dylan-debug-show-registers ()
  "Show the contents of the registers in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-send-command 'dylan-debug-show-registers "show registers"))

(defun dylan-debug-options ()
  (interactive)
  (if (y-or-n-p "Synchronize source with stack frame? ")
      (add-hook 'dylan-debug-command-hook 'dylan-debug-auto-edit)
    (remove-hook 'dylan-debug-command-hook 'dylan-debug-auto-edit)))

(defun dylan-debug-info ()
  (interactive)
  (Info-goto-node
   (concat "(" 
	   (tilde-dylan-file-name "/doc/misc/devel-dbg-instructions.text")
	   ")top")))

(defun dylan-debug-help ()
  (interactive)
  (dylan-debug-send-command 'dylan-debug-help "help"))

;;; FIND-FILE

(defun dylan-debug-note-executable-path ()
  "Hunt for the status line printed by the debugger and snarf the
directory of the executable to put in the search path that gets used
to find source files.
"
  (save-excursion
    (goto-char (point-max))
    (let ((match (re-search-backward "^;;; \\(.+/\\).+ (.+:.+)"
				     (or (marker-position comint-last-input-end) (point-min))
				     t)))
      (when match
	(unless (member (match-string 1) compilation-search-path)
	  (push (match-string 1) compilation-search-path))))))

(defun dylan-debug-parse-source-location ()
  "Hunt for the filename and line number printed by the debugger\'s
\'frame\' command.
"
  (save-excursion
    (goto-char (point-max))
    (let ((match (re-search-backward "{Line \\([0-9]+\\) of.*[\\\\/]\\(.+\\)}"
				     (or (marker-position comint-last-input-end) (point-min))
				     t)))
      (if match
	  (list (match-string 2) (string-to-int (match-string 1)))
	(list 'unknown 'unknown)))))

(defun dylan-debug-find-file (filename lineno)
  "Grovel around looking for location we just found in the stack
frame. Check locally, then check search path, then ask user for
somewhere else to look.
"
  (if (eq filename 'unknown)
      (message "Unknown source location")
    (if (file-exists-p filename)
	(dylan-debug-find-file-internal filename lineno)
      (unless (dylan-debug-find-file-search filename lineno)
	(when (y-or-n-p (format "File \"%s\" not found. Add New Directory to Search Path? " filename))
	  (call-interactively 'dylan-debug-find-file-add-to-search-path)
	  (dylan-debug-find-file filename lineno))))))
	    
(defmacro with-cache (key alist &rest body)
  `(or (cdr (assoc ,key ,alist))
       (let ((%new (progn ,@body)))
	 (when %new
	   (setq ,alist (cons (cons ,key %new) ,alist)))
	 %new)))

(defvar dylan-debug-source-file-alist nil)

(defun dylan-debug-find-file-search (filename lineno)
  "Look through search path for file we found in stack frame.
"
  (message (format "Searching for file %s line %d ..." filename lineno))
  (let ((fullname (with-cache filename dylan-debug-source-file-alist
			      (dylan-debug-search-files-for-named-file compilation-search-path filename))))
    (message "")
    (when fullname
      (dylan-debug-find-file-internal fullname lineno))))

(defun dylan-debug-search-files-for-named-file (initial-files named-file)
  "Look for file in a list of files.
"
  (dylan-debug-search-files initial-files
			    '(lambda (file)
			       (string= (file-name-nondirectory file) named-file))))

(defvar *dylan-debug-search-recursively-p* t
  "*When non-NIL, recurse down directories when searching for sources
in Dylan console debugger.
")

(defun dylan-debug-search-files (initial-files test &optional collect-p)
  "Search through list of files for something that matches the test.
"
  (do* ((files initial-files)
	(next-file (car files))
	(result nil)
	(collected-files nil))
      ((or (and collected-files (not collect-p)) (null files))
       (when collected-files (if collect-p collected-files (car collected-files))))
    (setq next-file (pop files))
    (when (stringp next-file)
      (setq result (funcall test next-file))
      (when result
	(push next-file collected-files))
      (when (and *dylan-debug-search-recursively-p* (file-directory-p next-file))
	(setq files (nconc (directory-files-without-self-and-parent next-file) files))))))

(defun directory-files-without-self-and-parent (directory)
  "Returns list of full pathnames of files in given directory. Hacked
to strip off . and ..
"
  (butlast (directory-files directory t nil t) 2))

(defun dylan-debug-find-file-internal (filename lineno)
  "Visit file at given line number.
"
  (funcall (if dylan-debug-in-auto-edit-p
	       dylan-debug-auto-find-file-function
	     dylan-debug-find-file-function)
	   filename)
  (goto-line lineno))

(defun dylan-debug-find-file-add-to-search-path (directory)
  "Cadge new directory from user to augment search path.
"
  (interactive "DNew Directory for Search Path: ")
  (unless (member directory compilation-search-path)
    (push directory compilation-search-path)))

;;; STUFF FOR DYLAN-MODE

(add-hook 'dylan-mode-hook 'setup-dylan-mode-debug)

(defun setup-dylan-mode-debug ()
  "Setup key sequences and menu items specific to Dylan console
debugger\'s operations from Dylan mode.
"
  (define-key dylan-mode-map "\C-c\C-b" 'dylan-debug-break-from-file)
  (define-key dylan-mode-map "\C-c\C-l" 'dylan-debug-break-line-from-file)
  (define-key dylan-mode-map "\C-c\C-t" 'dylan-debug-trace-from-file)
  (define-key dylan-mode-map "\C-c\C-c" 'dylan-debug-continue-from-file)
  (define-key dylan-mode-map "\C-c\C-u" 'dylan-debug-up-from-file)
  (define-key dylan-mode-map "\C-c\C-d" 'dylan-debug-down-from-file)
  (define-key dylan-mode-map "\C-c\C-p" 'dylan-debug-print-from-file)
  (define-key dylan-mode-map "\C-c\C-i" 'dylan-debug-describe-from-file)
  (define-key dylan-mode-map "\C-c\C-k" 'dylan-debug-backtrace-from-file)
  (define-key dylan-mode-map "\C-c\C-f" 'dylan-debug-step-out-from-file)
  (define-key dylan-mode-map "\C-c\C-n" 'dylan-debug-step-over-from-file)
  (define-key dylan-mode-map "\C-c\C-s" 'dylan-debug-step-into-from-file)

  (define-key dylan-mode-map [menu-bar dylan-debug] (cons "Dylan/Debug" (make-sparse-keymap)))
  (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-printer] (cons "Print" (make-sparse-keymap)))
  (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker] (cons "Break" (make-sparse-keymap)))
  (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-set-context] '("Set Library and Module" . dylan-debug-set-context-from-file))

  (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-start-debugger] '("Start Debugger" . dylan-debug))
  
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-printer dylan-debug-describe] '("Describe" . dylan-debug-describe-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-printer dylan-debug-print] '("Print" . dylan-debug-print-from-file))

    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-untrace] '("Clear Trace..." . dylan-debug-untrace-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-trace] '("Set Trace On Function..." . dylan-debug-trace-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-clear-line] '("Clear Break At Line..." . dylan-debug-clear-line-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-break-line] '("Set Break At Line..." . dylan-debug-break-line-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-clear] '("Clear Break On Function..." . dylan-debug-clear-from-file))
    (define-key dylan-mode-map [menu-bar dylan-debug dylan-debug-breaker dylan-debug-break] '("Set Break On Function..." . dylan-debug-break-from-file)))  

(defun dylan-debug-continue-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-continue))

(defun dylan-debug-step-out-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-step-out))

(defun dylan-debug-step-into-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-step-into))

(defun dylan-debug-up-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-up))

(defun dylan-debug-down-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-down))

(defun dylan-debug-backtrace-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-backtrace))

(defun dylan-debug-step-over-from-file ()
  (interactive)
  (dylan-debug t)
  (dylan-debug-step-over))

(defun dylan-debug-describe-from-file ()
  "Describe the structure of the selected expression in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-operation-from-file '%dylan-debug-describe))

(defun dylan-debug-print-from-file ()
  "Print the value of the selected expression in the Dylan console
debugger.
"
  (interactive)
  (dylan-debug-operation-from-file '%dylan-debug-print))

(defun dylan-debug-break-from-file (prefix)
  "Break on the selected function in the Dylan console debugger.
"
  (interactive "p")
  (if (= prefix 1)
      (dylan-debug-operation-from-file '%dylan-debug-break)
    (dylan-debug-operation-from-file '%dylan-debug-clear)))
  
(defun dylan-debug-break-line-from-file ()
  "Break on the selected line in the Dylan console debugger.
"
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name (current-buffer))))
	 (lineno (dylan-debug-line-number)))
    (dylan-debug t)
    (dylan-debug-break-line filename lineno)))

(defun dylan-debug-clear-line-from-file ()
  "Clear break on the selected line in the Dylan console debugger.
"
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name (current-buffer))))
	 (lineno (dylan-debug-line-number)))
    (dylan-debug t)
    (dylan-debug-clear-line filename lineno)))

(defun dylan-debug-line-number ()
  "Find line number of cursor."
  (save-excursion
    (beginning-of-line)
    ;; 1 less than Emacs since dbg is 0-based
    (count-lines 1 (point))))

(defun dylan-debug-clear-from-file ()
  "Clear break on the selected function in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-operation-from-file '%dylan-debug-clear))
  
(defun dylan-debug-trace-from-file (prefix)
  "Trace the selected function in the Dylan console debugger.
"
  (interactive "p")
  (if (= prefix 1)
      (dylan-debug-operation-from-file '%dylan-debug-trace)
    (dylan-debug-operation-from-file '%dylan-debug-untrace)))
  
(defun dylan-debug-untrace-from-file ()
  "Clear the trace on the selected function in the Dylan console debugger.
"
  (interactive)
  (dylan-debug-operation-from-file '%dylan-debug-untrace))
  
(defun dylan-debug-operation-from-file (operation)
  "Call the operation on the fully qualified Dylan identifier under the cursor."
  (let* ((module (dylan-debug-guess-module-from-file))
	 (library (dylan-debug-guess-library-from-file module))
	 (identifier (current-word)))
    (dylan-debug t)
    (funcall operation (concat identifier ":"  module ":" library))))

(defun dylan-debug-set-context-from-file ()
  "Set the library and the module context from the currrent file in the Dylan console debugger.
"
  (interactive)
  (let* ((module (dylan-debug-guess-module-from-file))
	 (library (dylan-debug-guess-library-from-file module)))
    (dylan-debug t)
    (when library
      (dylan-debug-in-library library))
    (dylan-wait-for-prompt dylan-debug-prompt)
    (when module
      (dylan-debug-in-module module))))

(defun dylan-debug-guess-module-from-file ()
  "Look for module header in file."
  (save-excursion
    (goto-char (point-min))
    (let ((limit (re-search-forward "^$" nil t)))
      (when limit
	(goto-char (point-min))
	(let ((found (re-search-forward "^Module:[ 	
]*\\([^ 	
]+\\)$" limit t)))
	  (when found
	    (match-string 1)))))))

(defvar *dylan-debug-library-alist* '()
  "Cache of associations between known files and their containing libraries.")

(defun dylan-debug-guess-library-from-file (&optional module)
  "Look up directory hierarchy for files called *.lid. Then look in
each *.lid file to see if current file is referenced. Then if it is
then extract library file from the lid file, and search the library
file for a library form that exports our module.
"
  (unless module
    (setq module (dylan-debug-guess-module-from-file)))
  (let ((source-file (buffer-file-name (current-buffer))))
    (when source-file
      (with-cache source-file *dylan-debug-library-alist*
		  (dylan-debug-guess-library-from-file-search source-file)))))

(defun dylan-debug-guess-library-from-file-search (source-file)
  (let ((lid-files (dylan-debug-find-lid-files source-file)))
    (when lid-files
      (dylan-debug-find-library source-file lid-files))))
    
(defun dylan-debug-find-lid-files (source-file)
  "Look up directory hierarchy for files called *.lid."
  (let ((*dylan-debug-search-recursively-p* nil))
    (do ((directory (file-name-directory source-file))
	 (pattern "\\.lid$")
	 (lid-files nil))
	((root-directory-p directory) lid-files)
      (let ((more-lid-files (dylan-debug-search-files
			     (directory-files-without-self-and-parent directory)
			     '(lambda (file) (string-match pattern file))
			     t)))
	(when more-lid-files
	  (setq lid-files (nconc more-lid-files lid-files))))
      (setq directory
	    (file-name-directory (directory-file-name directory))))))

(defun root-directory-p (directory)
  "Cope with Unix and PC directories."
  (or (string= directory "/")
      (string-match "^[a-z]:[/\\\\]$" directory)))

(defun dylan-debug-find-library (source-file lid-files)
  "Then look in each *.lid file to see if current file is
referenced. Then if it is then extract library name.
"
  (block 'found
    (let ((source-file-name (file-name-sans-extension (file-name-nondirectory source-file))))
      (save-excursion
	(dolist (lid-file lid-files)
	  (set-buffer (find-file-noselect lid-file))
	  (goto-char (point-min))
	  (let ((found-source-file (re-search-forward source-file-name nil nil)))
	    (when found-source-file 
	      (goto-char (point-min))
	      (let ((found-library (re-search-forward "^Library:[ 	
]*\\([^ 	
]+\\)" nil nil)))
		(if found-library
		    (return-from 'found (match-string 1))
		  (return nil))))))))))

(defun dylan-wait-for-prompt (prompt)
  "Keep sleeping until last output line is command prompt"
  (save-excursion
    (message "Waiting for command to complete ...")
    (while (not (dylan-check-for-prompt prompt))
      (sleep-for 1))
    (message "")))

(defun dylan-check-for-prompt (prompt)
  (goto-char (point-max))
  (beginning-of-line)
  (looking-at prompt))

;;; UNIVERSAL STUFF

(define-key menu-bar-tools-menu [dylan-debug] '("Dylan Debugger" . dylan-debug))

(provide 'dylan-debug)

