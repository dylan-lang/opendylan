Module: 	dylan-user
Author:		1997-07-17: Seth LaForge
Synopsis:	Interface between spec file tools and the project manager.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library tools-interface
  export tools-interface;

  use functional-dylan;
  use system;
  use io;
end library tools-interface;


define module tools-interface
  export tool-register, tool-name-from-specification, tool-find;
  export <tool-yes-no-question>, tool-ask-yes-no-question;
  export <tool-warning-condition>, tool-warning-serious?,
	 tool-warning-recoverable?, tool-warning-file, tool-warning-line,
	 tool-warning-library, tool-warning-definition,
	 tool-warning-serious?-setter, tool-warning-recoverable?-setter,
	 tool-warning-file-setter, tool-warning-line-setter,
	 tool-warning-library-setter, tool-warning-definition-setter;
  export tool-warning, tool-error;

  export $current-project-file-version, 
	 write-project-file, write-project-to-stream, 
	 read-project-file, read-project-from-stream;
  export <project-information>, project-information-files,
	 project-information-subprojects, project-information-library,
	 project-information-target-type, project-information-executable,
	 project-information-compilation-mode,
	 project-information-major-version, project-information-minor-version,
         project-information-library-pack,
	 project-information-base-address, project-information-remaining-keys,
	 project-information-files-setter,
	 project-information-subprojects-setter,
	 project-information-library-setter,
	 project-information-target-type-setter,
	 project-information-executable-setter,
	 project-information-compilation-mode-setter,
	 project-information-major-version-setter,
	 project-information-minor-version-setter,
         project-information-library-pack-setter,
	 project-information-base-address-setter,
	 project-information-remaining-keys-setter;

  export read-keyword-pair-file, read-keyword-pair-stream;
  export write-keyword-pair-file, write-keyword-pair-stream;
  export <keyword-file-element>, 
  	 keyword-file-element-value, keyword-file-element-value-setter,
  	 keyword-file-element-line, keyword-file-element-line-setter;

  use functional-dylan;
  use machine-words;
  use format;
  use format-out;
  use locators;
  use streams;
  use file-system;
end module tools-interface;
