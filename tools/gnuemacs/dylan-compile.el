;;;; Module: Interactive Mode for Dylan Console Compiler
;;;; Author: Jason Trenouth
;;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.

(require 'dylan-mode)
(require 'dylan-debug)
(require 'comint)
(require 'cl)

(defvar dylan-compile-program-name (tilde-dylan-install-file-name "/bin/pentium-dw.exe")
  "*The filename of the dylan console compiler program.")

(defvar dylan-compile-raw-buffer-name "Dylan Compiler"
  "The basic buffer name for the dylan console compiler, sans *\'s.")

(defvar dylan-compile-buffer-name
  (concat "*" dylan-compile-raw-buffer-name "*")
  "The full buffer name for the dylan console compiler.")

(defvar dylan-compile-mode-recompile-map (make-sparse-keymap)
  "The dynamic menu built from applications opened via the open menu.
")

(defun setup-dylan-compile-mode-map ()
  "Setup key sequences and menu items specific to Dylan console
compiler\'s interactive mode.
"
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-a" 'comint-bol)
    (define-key map "\C-i" 'comint-dynamic-complete)

    (define-key map [menu-bar dylan-compile] (cons "Dylan/Compile" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-compile dylan-compile-exit] '("Exit" . dylan-compile-exit))
    ;; help
    ;; report
    ;; enter-debugger (debug)
    ;; print
    ;; abort
    ;; continue <n>
    (define-key map [menu-bar dylan-compile dylan-compile-options] '("Options..." . dylan-compile-options))
    (define-key map [menu-bar dylan-compile dylan-compile-build-locations] '("Show Build Locations" . dylan-compile-build-locations))
    (define-key map [menu-bar dylan-compile dylan-compile-registries] '("Show Registries" . dylan-compile-registries))
    (define-key map [menu-bar dylan-compile dylan-compile-find-library] '("Find Library..." . dylan-compile-find-library))
    (define-key map [menu-bar dylan-compile dylan-compile-edit] '("Edit Message Source" . dylan-compile-edit))
    (define-key map [menu-bar dylan-compile dylan-compile-update-libraries] '("Update Libraries..." . dylan-compile-update-libraries))
    (define-key map [menu-bar dylan-compile dylan-compile-compile] '("Compile Library..." . dylan-compile-compile))
    map
    ))

(defvar dylan-compile-mode-map (setup-dylan-compile-mode-map)
  "The key sequences and menu items specific to the Dylan console
compiler\'s interactive mode.
")

(defvar dylan-compile-prompt "dw=> \\|dylan => "
  "A regexp for finding the Dylan console compiler\'s prompt.")

(defvar dylan-compile-font-lock-keywords
  (list (cons dylan-compile-prompt font-lock-keyword-face))
  "A description of what should be highlighed in the Dylan console
compiler\'s interactive mode. See FONT-LOCK-KEYWORDS for the format.
")

(defun dylan-compile (&optional wait-p)
  "Starts up the Dylan console compiler in an interactive buffer. If
the compiler is already running in a buffer then this command just
switches to it. The user can control which program is run by setting
the variable DYLAN-COMPILE-PROGRAM-NAME.
"
  (interactive)
  (unless (comint-check-proc dylan-compile-buffer-name)
    (set-buffer (make-comint dylan-compile-raw-buffer-name shell-file-name))
    (dylan-compile-mode)
    (dylan-link-initialize)
    (dylan-compile-initialize))
  (switch-to-buffer dylan-compile-buffer-name)
  (when wait-p
    (dylan-wait-for-prompt dylan-compile-prompt)))

(defun dylan-compile-initialize ()
  (dylan-compile-send-command dylan-compile-program-name))

(defun dylan-compile-mode ()
  "The major mode for interacting with the Dylan console compiler. The
user can customize the initialization of the mode through
DYLAN-COMPILE-MODE-HOOK. For more information on the compiler see the
DYLAN-COMPILE Info file (C-h i).
"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp dylan-compile-prompt)
  (setq major-mode 'dylan-compile-mode)
  (setq mode-name "Dylan Compile")
  (setq mode-line-process '(":%s"))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((dylan-compile-font-lock-keywords)))
  (use-local-map dylan-compile-mode-map)
  (run-hooks 'dylan-compile-mode-hook))

(defun dylan-compile-send-command (command &rest args)
  "Core function for controlling Dylan console compiler. Inserts
formatted command strings into the interactive buffer\'s prompt and
then sends them to the compiler process.
"
  (end-of-buffer)
  (insert (apply 'format command args))
  (comint-send-input))

(defun dylan-compile-exit ()
  (interactive)
  (when (y-or-n-p "Exit Dylan Compiler? ")
    (dylan-compile-send-command "exit")))

(defvar *dylan-compile-force-compile-p* nil)

(defvar *dylan-compile-force-parse-p* nil)

(defvar *dylan-compile-mode* nil)

(defun dylan-compile-compile (library)
  (interactive "sCompile Library: ")
  (let ((options ""))
    (when *dylan-compile-force-compile-p*
      (setq options (concat "-force-compile " options)))
    (when *dylan-compile-force-parse-p*
      (setq options (concat "-force-parse " options)))
    (when *dylan-compile-mode*
      (setq options (concat "-m " *dylan-compile-mode* " " options)))
    (dylan-compile-send-command "compile %s %s" options library)))

(defun dylan-compile-update-libraries (library)
  (interactive "sUpdate Libraries: ")
  (dylan-compile-send-command "update-libraries %s" library))

(defun dylan-compile-edit ()
  (interactive)
  (apply 'dylan-debug-find-file
	 (dylan-compile-parse-compiler-message-location)))

(defun dylan-compile-parse-compiler-message-location ()
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\(.*\\):\\(.*\\):"))
      (list (match-string 1) (string-to-int (match-string 2)))
    '(unknown unknown)))

(defun dylan-compile-options (mode)
  (interactive "sCompilation Mode: ")
  (setq *dylan-compile-mode* (unless (string= mode "") mode))
  (setq *dylan-compile-force-compile-p* (y-or-n-p "Force Compile? "))
  (setq *dylan-compile-force-parse-p* (y-or-n-p "Force Parse? "))
  (if (y-or-n-p "Trace Optimizations? ")
      (dylan-compile-send-command "trace-optimizations")
    (dylan-compile-send-command "untrace-optimizations")))

(defun dylan-compile-build-locations ()
  (interactive)
  (dylan-compile-send-command "build-locations"))

(defun dylan-compile-find-library (library)
  (interactive "sFind Library: ")
  (dylan-compile-send-command "find-library %s" library))

(defun dylan-compile-registries ()
  (interactive)
  (dylan-compile-send-command "registries"))

;;; STUFF FOR DYLAN-MODE

(add-hook 'dylan-mode-hook 'setup-dylan-mode-compile)

(defun setup-dylan-mode-compile ()
  "Setup key sequences and menu items specific to Dylan console
compiler\'s operations from Dylan mode.
"
  (define-key dylan-mode-map [menu-bar dylan-compile] (cons "Dylan/Compile" (make-sparse-keymap)))
  (define-key dylan-mode-map [menu-bar dylan-compile dylan-link-start-shell] '("Start Linker Shell" . dylan-link))
  (define-key dylan-mode-map [menu-bar dylan-compile compile-separator] '("--"))
  (define-key dylan-mode-map [menu-bar dylan-compile dylan-compile-update-libraries-from-file] '("Update Libraries" .  dylan-compile-update-libraries-from-file))
  (define-key dylan-mode-map [menu-bar dylan-compile dylan-compile-compile-from-file] '("Compile Library" . dylan-compile-compile-from-file))
  (define-key dylan-mode-map [menu-bar dylan-compile dylan-compile-start-compiler] '("Start Compiler" . dylan-compile))
  )

(defun dylan-compile-compile-from-file ()
  (interactive)
  (let ((library (dylan-debug-guess-library-from-file)))
    (dylan-compile t)
    (when library
      (dylan-compile-compile library))))

(defun dylan-compile-update-libraries-from-file ()
  (interactive)
  (let ((library (dylan-debug-guess-library-from-file)))
    (dylan-compile t)
    (when library
      (dylan-compile-update-libraries library))))

;;; STUFF FOR DYLAN-LINK

(defvar dylan-link-shell-program-name shell-file-name
  "*The filename of the Windows shell.")

(defvar dylan-link-linker-program-name "build.exe"
  "*The filename of the Windows linker.")

(defvar dylan-link-raw-buffer-name "Dylan Linker"
  "The basic buffer name for the dylan linker shell, sans *\'s.")

(defvar dylan-link-buffer-name
  (concat "*" dylan-link-raw-buffer-name "*")
  "The full buffer name for the dylan linker shell.")

(defvar dylan-link-mode-relink-map (make-sparse-keymap)
  "The dynamic menu built from applications linked via the link menu.
")

(defvar dylan-link-mode-force-relink-map (make-sparse-keymap)
  "The dynamic menu built from applications linked via the link menu.
")

(defun setup-dylan-link-mode-map ()
  "Setup key sequences and menu items specific to Dylan console
linker\'s interactive mode.
"
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-a" 'comint-bol)
    (define-key map "\C-i" 'comint-dynamic-complete)

    (define-key map [menu-bar dylan-link] (cons "Dylan/Link" (make-sparse-keymap)))
    (define-key map [menu-bar dylan-link dylan-link-exit] '("Exit" . dylan-link-exit))
    (define-key map [menu-bar dylan-link dylan-link-run] '("Run Application..." . dylan-link-run))
    (define-key map [menu-bar dylan-link dylan-link-force-relink] (cons "Force ReLink Application" dylan-link-mode-force-relink-map))
    (define-key map [menu-bar dylan-link dylan-link-force] '("Force Link Application..." . dylan-link-force))
    (define-key map [menu-bar dylan-link dylan-link-relink] (cons "ReLink Application" dylan-link-mode-relink-map))
    (define-key map [menu-bar dylan-link dylan-link-link] '("Link Application..." . dylan-link-link))
    (define-key map [menu-bar dylan-link dylan-link-dll] '("Link DLL..." . dylan-link-dll))
    (define-key map [menu-bar dylan-link dylan-link-install] '("Link Library..." . dylan-link-install))
    map
    ))

(defvar dylan-link-mode-map (setup-dylan-link-mode-map)
  "The key sequences and menu items specific to the Dylan console
linker\'s interactive mode.
")

(defun dylan-link (&optional wait-p)
  "Starts up the Dylan console linker in an interactive buffer. If
the linker is already running in a buffer then this command just
switches to it. The user can control which program is run by setting
the variable DYLAN-LINK-SHELL-PROGRAM-NAME.
"
  (interactive)
  (unless (comint-check-proc dylan-link-buffer-name)
    (set-buffer (make-comint dylan-link-raw-buffer-name dylan-link-shell-program-name))
    (dylan-link-mode)
    (dylan-link-initialize))
  (switch-to-buffer dylan-link-buffer-name)
  (when wait-p
    (dylan-wait-for-prompt comint-prompt-regexp)))

(defvar dylan-link-setup-program-name "kan-env.bat")

(defun dylan-link-initialize ()
  (if (or (not (getenv "WEBSTER_SYSTEM_ROOT"))
	  (y-or-n-p "Some Dylan environment variables are already set. Still run kan-env.bat?"))
      (let* ((system-path (maybe-pc-file-name dylan-release-directory))
	     (system-drive (substring system-path 0 2))
	     (system-directory (substring system-path 2)))
	(dylan-compile-send-command 
	 "%s -r %s -sd %s -p %s"
	 (maybe-pc-file-name (tilde-dylan-admin-file-name dylan-link-setup-program-name))
	 system-directory
	 system-drive
	 (maybe-pc-file-name personal-dylan-directory)))))

(defun dylan-link-directory ()
  "If we're somewhere inside the personal build directory then ok,
otherwise use the personal build directory. Canonicalize names to help comparison.
Have to use clunky approach 'cause we don't really have full CL functions here."
  (let ((root (maybe-pc-file-name (personal-dylan-build-file-name "/")))
	(current (maybe-pc-file-name (expand-file-name default-directory))))
    (if (string= root (substring current 0 (min (length root) (length current)))) current root))) 

;;; RUN APPLICATION

(defun dylan-link-run ()
  (interactive)
  (cd
   (let ((default-directory (dylan-link-directory)))
     (call-interactively '%dylan-link-run))))

(defun %dylan-linker-operation (application operation)
  (let ((directory (expand-file-name application)))
    (dylan-compile-send-command "cd /d %s" (maybe-pc-file-name directory))
    (dylan-compile-send-command "%s %s"
				dylan-link-linker-program-name
				operation)
    directory))

(defun %dylan-link-run (application)
  (interactive "DRun Application (enter directory): ")
  (%dylan-linker-operation application "run"))

;;; FORCE APPLICATION

(defun dylan-link-force ()
  (interactive)
  (cd
   (let ((default-directory (dylan-link-directory)))
     (call-interactively '%dylan-link-force))))

(defun %dylan-link-force (application)
  (interactive "DForcibly Link Application, ie delete the .EXE first (enter directory): ")
  (let* ((directory (expand-file-name application)))
    (dylan-compile-send-command "cd /d %s" (maybe-pc-file-name directory))
    (dylan-compile-send-command "del /q /f %s.exe" (file-name-nondirectory (directory-file-name (maybe-pc-file-name directory))))
    (dylan-compile-send-command "%s app" dylan-link-linker-program-name)
    directory))

;;; LINK APPLICATION

(defun dylan-link-link ()
  (interactive)
  (cd
   (let ((default-directory (dylan-link-directory)))
     (call-interactively '%dylan-link-link))))

(defun %dylan-link-link (application)
  (interactive "DLink Application (enter directory): ")
  (let* ((directory (%dylan-linker-operation application "app")))
    (dylan-link-update-relink-menus directory)
    directory))

;;; LINK LIBRARY (INSTALL)

(defun dylan-link-install ()
  (interactive)
  (cd 
   (let ((default-directory (dylan-link-directory)))
     (call-interactively '%dylan-link-install))))

(defun %dylan-link-install (application)
  (interactive "DLink Library (enter directory): ")
  (%dylan-linker-operation application "install"))

(defun dylan-link-dll ()
  (interactive)
  (cd 
   (let ((default-directory (dylan-link-directory)))
     (call-interactively '%dylan-link-dll))))

(defun %dylan-link-dll (application)
  (interactive "DLink DLL (enter directory): ")
  (%dylan-linker-operation application "dll"))

(defvar *dylan-link-directories* nil
  "Cache of previously linked applications.
")

(defun dylan-link-update-relink-menus (directory)
  "Rebuilds ReLink menu from cache of previously linked applications.
"
  (pushnew directory *dylan-link-directories* :test 'string-equal :key 'downcase)
  (setcdr dylan-link-mode-relink-map
	  (mapcar '(lambda (directory)
		     (cons directory	; HACK: use program as event
					; (RMS Himself does the same,
					; and I'm not worthy ...)
			   (cons directory 'dylan-link-relink)))
		  *dylan-link-directories*))
  (setcdr dylan-link-mode-force-relink-map
	  (mapcar '(lambda (directory)
		     (cons directory
			   (cons directory 'dylan-link-force-relink)))
		  *dylan-link-directories*)))
	  
(defun dylan-link-relink ()
  "Links an application in the Dylan console linker. The
application is assumed to be the last \'event\' (left there by the use
of the associated menu item).
"
  (interactive)
  (%dylan-link-link last-command-event))

(defun dylan-link-force-relink ()
  "ReLinks an application in the Dylan console linker. The
application is assumed to be the last \'event\' (left there by the use
of the associated menu item).
"
  (interactive)
  (%dylan-link-force last-command-event))

;;; EXIT

(defun dylan-link-exit ()
  (interactive)
  (dylan-compile-send-command "exit"))

;;; MODE

(defun dylan-link-mode ()
  "The major mode for interacting with the Dylan console linker. The
user can customize the initialization of the mode through
DYLAN-LINK-MODE-HOOK. For more information on the linker see the
DYLAN-LINK Info file (C-h i).
"
  (interactive)
  (comint-mode)
  (setq major-mode 'dylan-link-mode)
  (setq mode-name "Dylan Link")
  (setq mode-line-process '(":%s"))
  (use-local-map dylan-link-mode-map)
  (run-hooks 'dylan-link-mode-hook))

;;; UNIVERSAL STUFF

(define-key menu-bar-tools-menu [dylan-link] '("Dylan Linker" . dylan-link))
(define-key menu-bar-tools-menu [dylan-compile] '("Dylan Compiler" . dylan-compile))

(provide 'dylan-compile)

