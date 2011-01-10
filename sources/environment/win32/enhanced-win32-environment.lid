Library:   enhanced-win32-environment
Synopsis:  Win32 environment
Author:	   Andy Armstrong
Target-Type:	executable
Files:	enhanced-library
	module
	initialization
        environment-frames
	start
Major-Version: 2
Minor-Version: 1
Linker-Options: $(guilflags)
C-Libraries:    devnub.lib
	        dbghelp.lib
Comment:	'C-Header-Files' is a kludge to get the bitmaps linked in
RC-Files:	bitmaps.rc
C-Header-Files: bitmaps/about.bmp
		bitmaps/splash-screen.bmp
		bitmaps/wizard.bmp
                bitmaps/dylan-app.ico
		bitmaps/dylan-source.ico
		bitmaps/dylan-project.ico
		bitmaps/dylan-database.ico
		bitmaps/lid-file.ico
                bitmaps/spec-file.ico
		bitmaps/lp-spec-file.ico
		bitmaps/application.ico
		bitmaps/back.ico
		bitmaps/bottom-of-stack.ico
		bitmaps/browser.ico
		bitmaps/build.ico
		bitmaps/check-mark.ico
		bitmaps/canonical-source.ico
		bitmaps/current-source.ico
		bitmaps/class.ico
		bitmaps/clients-folder.ico
		bitmaps/clone.ico
		bitmaps/compile-all.ico
		bitmaps/compile-changes.ico
		bitmaps/compile.ico
		bitmaps/constant.ico
		bitmaps/copy.ico
		bitmaps/current-location.ico
		bitmaps/cut.ico
		bitmaps/debug.ico
		bitmaps/debugger.ico
		bitmaps/default.ico
		bitmaps/definition.ico
		bitmaps/disabled-break.ico
		bitmaps/disabled-trace.ico
		bitmaps/down-stack.ico
		bitmaps/dylan-file.ico
		bitmaps/edit-source.ico
		bitmaps/enabled-break.ico
		bitmaps/enabled-trace.ico
		bitmaps/error.ico
		bitmaps/examples.ico
		bitmaps/find-next.ico
		bitmaps/find-previous.ico
		bitmaps/find.ico
		bitmaps/foreign.ico
		bitmaps/forward.ico
		bitmaps/function.ico
		bitmaps/generic.ico
		bitmaps/help.ico
		bitmaps/home.ico
		bitmaps/ierror.ico
		bitmaps/interact.ico
		bitmaps/library.ico
		bitmaps/link.ico
		bitmaps/load.ico
		bitmaps/macro-def.ico
		bitmaps/method.ico
		bitmaps/module.ico
		bitmaps/new-project.ico
		bitmaps/new-text.ico
		bitmaps/new.ico
		bitmaps/object.ico
		bitmaps/open.ico
		bitmaps/page-setup.ico
		bitmaps/paste.ico
		bitmaps/pause.ico
		bitmaps/play.ico
		bitmaps/playground.ico
		bitmaps/potential-break.ico
		bitmaps/print.ico
		bitmaps/profile.ico
		bitmaps/profile-point.ico
		bitmaps/project.ico
		bitmaps/prompt.ico
		bitmaps/redo.ico
		bitmaps/replace.ico
		bitmaps/restart.ico
		bitmaps/run.ico
		bitmaps/save-all.ico
		bitmaps/save.ico
		bitmaps/serious-warning.ico
		bitmaps/slot.ico
		bitmaps/stack-frame.ico
		bitmaps/step-break.ico
		bitmaps/step-into.ico
		bitmaps/step-out.ico
		bitmaps/step-over.ico
		bitmaps/stop.ico
		bitmaps/test-break.ico
		bitmaps/text-file.ico
		bitmaps/threads.ico
		bitmaps/top-of-stack.ico
		bitmaps/tutorial.ico
		bitmaps/unbound.ico
		bitmaps/uncheck-mark.ico
		bitmaps/undo.ico
		bitmaps/up-stack.ico
		bitmaps/uses-folder.ico
		bitmaps/values.ico
		bitmaps/variable.ico
		bitmaps/warning.ico
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

