Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Libraries loaded by default into the emulator

(ensure-emulator-library 'macro-expander)
(ensure-emulator-library 'emulator-patches-1)
(ensure-emulator-library 'emulator-patches-2)

(ensure-emulator-library 'functional-dylan)
(ensure-emulator-library 'transcendentals)
(ensure-emulator-library 'locators)
(ensure-emulator-library 'streams)
(ensure-emulator-library 'format)
(ensure-emulator-library 'standard-io)
(ensure-emulator-library 'emulator-release-info)

;; eof
