Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the stream module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-module stream
  (use internal
    export:
      (

        <stream>

        $eof

	standard-input *standard-input*
	standard-output *standard-output*
        
        with-open-file print-unreadable-object print-unreadable-named-object 

        probe-file
        existing-dylan-filename
        open
        close
        
        force-output
        input-line
        fresh-line
        
        input
        undo-input
        output
        clear-input
        
        <simple-reader-error>
        <reader-error>
        <end-of-file>

        reader-error

        <reader-buffer>
        reset 

        <reader>
	buffer
	
        char-kind
        char-kind-setter
        secondary-char-kind
        secondary-char-kind-setter
        action
        action-setter
	char-actions

	read
        print
        format

        ;; Don't belong here!!!

	standard?
        graphic?
        alpha?
        digit?
        alphanumeric?
        lowercase?
        uppercase?
        both-case?
        whitespace?

      )
  )
)

;; eof
