Module:    environment-tools
Synopsis:  Environment options
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment options

define constant <environment-start-action>
  = one-of(#"no",           // Do nothing
           #"start-dialog", // Show startup options dialog
           #"open-file");   // Open a file (shows Open dialog)

define constant <environment-linker-option>
  = one-of(#"microsoft", // Microsoft linker
           #"gnu");      // GNU linker

define settings <environment-settings> (<functional-developer-user-settings>)
  key-name "Environment";
  slot start-action             :: <symbol>  = #"start-dialog";
  slot start-dialog-action      :: <symbol>  = #"example";
  slot about-box-action         :: <symbol>  = #"initial";
  slot default-save-databases   :: <boolean> = #t;
  slot default-copy-sources     :: <boolean> = #f;
  slot default-link-mode        :: <symbol>  = #"ask";
  slot default-upgrade-warnings :: <boolean> = #f;
  slot application-confirm-stop :: <boolean> = #t;
  slot opened-project-active    :: <boolean> = #t;
  slot started-project-active   :: <boolean> = #t;
  slot qualify-names            :: <boolean> = #t;
  // The type of the next two is really <trigger-frame>...
  slot auto-raise-all-frames    :: <symbol>  = #"primary-frame";
  slot auto-lower-all-frames    :: <symbol>  = #"primary-frame";
end settings <environment-settings>;

define constant $environment-settings = make(<environment-settings>);

define function environment-start-action
    () => (action :: <symbol>)
  $environment-settings.start-action
end function environment-start-action;

define function environment-start-dialog-action
    () => (action :: <symbol>)
  $environment-settings.start-dialog-action
end function environment-start-dialog-action;

define function environment-start-dialog-action-setter
    (action :: <symbol>) => (action :: <symbol>)
  $environment-settings.start-dialog-action := action
end function environment-start-dialog-action-setter;

/*---*** andrewa: we aren't using this, for the moment
define function environment-about-box-action
    () => (action :: <symbol>)
  $environment-settings.about-box-action
end function environment-about-box-action;
*/

define function environment-default-save-databases
    () => (save? :: <boolean>)
  $environment-settings.default-save-databases
end function environment-default-save-databases;

define function environment-default-copy-sources
    () => (copy? :: <boolean>)
  $environment-settings.default-copy-sources
end function environment-default-copy-sources;

define function environment-default-link-mode
    () => (mode :: <symbol>)
  $environment-settings.default-link-mode
end function environment-default-link-mode;

define function environment-default-upgrade-warnings
    () => (upgrade? :: <boolean>)
  $environment-settings.default-upgrade-warnings
end function environment-default-upgrade-warnings;

define function environment-application-confirm-stop?
    () => (confirm? :: <boolean>)
  $environment-settings.application-confirm-stop
end function environment-application-confirm-stop?;

define function environment-active-on-starting?
    () => (confirm? :: <boolean>)
  $environment-settings.started-project-active
end function environment-active-on-starting?;

define function environment-active-on-opening?
    () => (confirm? :: <boolean>)
  $environment-settings.opened-project-active
end function environment-active-on-opening?;

define function environment-qualify-names?
    () => (qualify? :: <boolean>)
  $environment-settings.qualify-names
end function environment-qualify-names?;

define function environment-auto-raise-all-frames
    () => (auto-raise :: <trigger-frame>)
  $environment-settings.auto-raise-all-frames
end function environment-auto-raise-all-frames;

define function environment-auto-lower-all-frames
    () => (auto-lower :: <trigger-frame>)
  $environment-settings.auto-lower-all-frames
end function environment-auto-lower-all-frames;


/// Environment options dialog

define pane <environment-general-options-page> ()
  sealed slot %start-action :: <environment-start-action>,
    required-init-keyword: start-action:;
  sealed slot %confirm-stop? :: <boolean>,
    required-init-keyword: confirm-stop?:;
  sealed slot %qualify-names? :: <boolean>,
    required-init-keyword: qualify-names?:;
  sealed slot %active-on-opening? :: <boolean>,
    required-init-keyword: active-on-opening?:;
  sealed slot %active-on-starting? :: <boolean>,
    required-init-keyword: active-on-starting?:;
  pane %start-action-pane (pane)
    make(<radio-box>,
         orientation: #"vertical",
         items: #[#["Show start options &dialog", #"start-dialog"],
                  #["&Open a file",               #"open-file"],
                  #["Do &nothing",                #"no"]],
         value: pane.%start-action,
         label-key: first,
         value-key: second,
         value-changed-callback: method (box)
                                   pane.%start-action := gadget-value(box)
                                 end);
  pane %qualify-names-pane (pane)
    make(<check-button>,
	 label: "&Qualify object names using module and/or library",
	 value: pane.%qualify-names?,
	 value-changed-callback: method (button)
				   pane.%qualify-names? := gadget-value(button)
				 end);
  pane %confirm-stop-pane (pane)
    make(<check-button>,
	 label: "&Confirm before stopping application",
	 value: pane.%confirm-stop?,
	 value-changed-callback: method (button)
				   pane.%confirm-stop? := gadget-value(button)
				 end);
  pane %active-on-opening-pane (pane)
    make(<check-button>,
	 label: "Project becomes active when &opened",
	 value: pane.%active-on-opening?,
	 value-changed-callback: method (button)
				   pane.%active-on-opening? := gadget-value(button)
				 end);
  pane %active-on-starting-pane (pane)
    make(<check-button>,
	 label: "Project becomes active when application &started",
	 value: pane.%active-on-starting?,
	 value-changed-callback: method (button)
				   pane.%active-on-starting? := gadget-value(button)
				 end);
  layout (pane)
    vertically (spacing: 2)
      grouping ("Startup", max-width: $fill)
        vertically (spacing: 8)
          make(<label>,
               label: "When Functional Developer starts:");
          pane.%start-action-pane;
        end vertically
      end grouping;
      grouping ("Options", max-width: $fill)
        vertically (spacing: 8)
          pane.%qualify-names-pane;
          pane.%confirm-stop-pane;
          pane.%active-on-opening-pane;
          pane.%active-on-starting-pane;
        end vertically
      end grouping;
    end vertically;
end pane <environment-general-options-page>;

define pane <environment-build-options-page> ()
  sealed slot %linker :: <environment-linker-option>,
    required-init-keyword: linker:;
  sealed slot %copy-sources? :: <boolean>,
    required-init-keyword: copy-sources?:;
  sealed slot %save-databases? :: <boolean>,
    required-init-keyword: save-databases?:;
  sealed slot %link-mode :: <link-mode>,
    required-init-keyword: link-mode:;
  sealed slot %upgrade-warnings? :: <boolean>,
    required-init-keyword: upgrade-warnings?:;
  pane %linker-pane (pane)
    make(<radio-box>,
         orientation: #"vertical",
         items: #[#["&Microsoft linker", #"microsoft"],
                  #["&GNU linker",       #"gnu"]],
	 enabled?: release-contains-library-pack?(#"MSLinker"),
         value: pane.%linker,
         label-key: first,
         value-key: second,
         value-changed-callback: method (box)
                                   pane.%linker := gadget-value(box)
                                 end);
  /* ---*** Removed for 2.0 Beta 1 -- put it back in later
  pane %copy-sources-pane (pane)
    make(<check-button>,
	 label: "Sa&ve 'canonical' sources in the build area for builds",
	 value: pane.%copy-sources?,
	 value-changed-callback:
	   method (b)
	     pane.%copy-sources? := gadget-value(b)
	   end method); */
  pane %save-databases-pane (pane)
    make(<check-button>,
	 label: "&Save compiler databases after builds",
	 value: pane.%save-databases?,
	 value-changed-callback:
	   method (b)
	     pane.%save-databases? := gadget-value(b)
	   end method);
  pane %save-databases-now-pane (pane)
    make(<push-button>,
         label: "Save Databases Now",
         activate-callback:
           method (b)
             let frame = sheet-frame(b);
             with-busy-cursor (frame)
               save-all-project-databases(owner: frame);
             end;
           end);
  pane %link-mode-pane (pane)
    make(<option-box>,
	 items: $link-modes,
	 value-key: first, label-key: second,
	 value: pane.%link-mode,
	 value-changed-callback:
	   method (b)
	     pane.%link-mode := gadget-value(b)
	   end method);
  pane %upgrade-warnings-pane (pane)
    make(<check-button>,
	 label: "Treat all &warnings as serious warnings",
	 value: pane.%upgrade-warnings?,
	 value-changed-callback:
	   method (b)
	     pane.%upgrade-warnings? := gadget-value(b)
	   end method);
  layout (pane)
    vertically (spacing: 5, equalize-widths?: #t)
      make(<group-box>,
           label: "Database options",
           child: vertically (spacing: 8)
                    /* pane.%copy-sources-pane; */
                    pane.%save-databases-pane;
		    pane.%save-databases-now-pane;
                  end);
      make(<group-box>,
           label: "Warnings options",
           child: vertically (spacing: 8)
                    pane.%link-mode-pane;
                    pane.%upgrade-warnings-pane;
                  end);
      make(<group-box>,
	   label: "Link object files using",
	   max-width: $fill,
	   child: pane.%linker-pane);
    end vertically;
end pane <environment-build-options-page>;

define pane <environment-windows-options-page> ()
  sealed slot %raise-frames :: <trigger-frame>,
    required-init-keyword: raise-frames:;
  sealed slot %lower-frames :: <trigger-frame>,
    required-init-keyword: lower-frames:;
  pane %raise-frames-pane (pane)
    make(<radio-box>,
         orientation: #"vertical",
	 max-width: $fill,
         items: #[#["Never", #"none"],
		  #["When primary window is selected", #"primary-frame"],
		  #["When any window is selected", #"all-frames"]],
         value: pane.%raise-frames,
         label-key: first,
         value-key: second,
         value-changed-callback: method (box)
                                   pane.%raise-frames := gadget-value(box)
                                 end);
  pane %lower-frames-pane (pane)
    make(<radio-box>,
         orientation: #"vertical",
	 max-width: $fill,
         items: #[#["Never", #"none"],
		  #["When primary window is minimized", #"primary-frame"],
		  #["When any window is minimized", #"all-frames"]],
         value: pane.%lower-frames,
         label-key: first,
         value-key: second,
         value-changed-callback: method (box)
                                   pane.%lower-frames := gadget-value(box)
                                 end);
  layout (pane)
    vertically (spacing: 5, equalize-widths?: #t)
      make(<group-box>,
           label: "Raise other windows",
	   max-width: $fill,
           child: pane.%raise-frames-pane);
      make(<group-box>,
           label: "Minimize other windows",
	   max-width: $fill,
           child: pane.%lower-frames-pane);
    end vertically;
end pane <environment-windows-options-page>;

define method frame-edit-options (frame :: <environment-frame>) => ()
  let framem = frame.frame-manager;
  with-frame-manager (framem)
    let general-page
      = make(<environment-general-options-page>,
             start-action: environment-start-action(),
	     qualify-names?:      environment-qualify-names?(),
	     confirm-stop?:       environment-application-confirm-stop?(),
	     active-on-opening?:  environment-active-on-opening?(),
	     active-on-starting?: environment-active-on-starting?());
    let build-page
      = make(<environment-build-options-page>,
	     save-databases?:   environment-default-save-databases(),
	     copy-sources?:     environment-default-copy-sources(),
             linker:            default-linker(),
	     link-mode:         environment-default-link-mode(),
	     upgrade-warnings?: environment-default-upgrade-warnings());
    let windows-page
      = make(<environment-windows-options-page>,
	     raise-frames: environment-auto-raise-all-frames(),
	     lower-frames: environment-auto-lower-all-frames());
    let pages
      = vector(make(<property-page>, child: general-page, label: "General"),
	       make(<property-page>, child: build-page,   label: "Build"),
	       make(<property-page>, child: windows-page, label: "Windows"));
    local method update-environment-options (dialog) => ()
            ignore(dialog);
	    let settings = $environment-settings;

	    settings.start-action             := general-page.%start-action;
	    settings.qualify-names            := general-page.%qualify-names?;
	    settings.application-confirm-stop := general-page.%confirm-stop?;
	    settings.opened-project-active    := general-page.%active-on-opening?;
	    settings.started-project-active   := general-page.%active-on-starting?;
	    settings.default-save-databases   := build-page.%save-databases?;
	    settings.default-copy-sources     := build-page.%copy-sources?;
	    settings.default-link-mode        := build-page.%link-mode;
	    settings.default-upgrade-warnings := build-page.%upgrade-warnings?;
	    settings.auto-raise-all-frames    := windows-page.%raise-frames;
	    settings.auto-lower-all-frames    := windows-page.%lower-frames;

            default-linker() := build-page.%linker;
          end method update-environment-options;
    let dialog
      = make(<property-frame>,
             title: "Environment Options",
	     owner: frame,
	     mode:  #"modal",
             pages: pages,
             exit-callback: method (dialog)
                              update-environment-options(dialog);
                              exit-dialog(dialog);
                            end);
    start-dialog(dialog);
  end with-frame-manager;
end method frame-edit-options;
