module: win32-kernel-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("win32_kernel");
module-namer("win32_kernel");

constant-namer("getlargestconsolewindowsize", getlargestconsolewindowsize);

variable-namer("_D_generic_read", $generic-read);

variable-namer("_D_generic_write", $generic-write);

constant-namer("createfile", createfile);

constant-namer("getfileattributes", getfileattributes);

constant-namer("getmodulefilename", getmodulefilename);

constant-namer("getmodulehandle", getmodulehandle);

constant-namer("getcommandline", getcommandline);

constant-namer("outputdebugstring", outputdebugstring);

constant-namer("setnamedpipehandlestate", setnamedpipehandlestate);

constant-namer("loadlibrary", loadlibrary);

constant-namer("freelibrary", freelibrary);

constant-namer("getprocaddress", getprocaddress);

constant-namer("getversion", getversion);

constant-namer("globalalloc", globalalloc);

constant-namer("globalsize", globalsize);

constant-namer("globallock", globallock);

constant-namer("globalunlock", globalunlock);

constant-namer("globalfree", globalfree);

constant-namer("localalloc", localalloc);

constant-namer("locallock", locallock);

constant-namer("localunlock", localunlock);

constant-namer("localfree", localfree);

constant-namer("exitprocess", exitprocess);

constant-namer("getlasterror", getlasterror);

constant-namer("debugbreak", debugbreak);

constant-namer("getfilesize", getfilesize);

constant-namer("writefile", writefile);

constant-namer("readfile", readfile);

constant-namer("flushfilebuffers", flushfilebuffers);

constant-namer("setfilepointer", setfilepointer);

constant-namer("closehandle", closehandle);

variable-namer("_D_ghnd", $ghnd);

variable-namer("_D_gptr", $gptr);

variable-namer("_D_lhnd", $lhnd);

variable-namer("_D_lptr", $lptr);

constant-namer("wprocessorarchitecture_value", wprocessorarchitecture-value);

constant-namer("wprocessorarchitecture_value_setter", wprocessorarchitecture-value-setter);

constant-namer("dwpagesize_value", dwpagesize-value);

constant-namer("dwpagesize_value_setter", dwpagesize-value-setter);

constant-namer("lpminimumapplicationaddress_value", lpminimumapplicationaddress-value);

constant-namer("lpminimumapplicationaddress_value_setter", lpminimumapplicationaddress-value-setter);

constant-namer("lpmaximumapplicationaddress_value", lpmaximumapplicationaddress-value);

constant-namer("lpmaximumapplicationaddress_value_setter", lpmaximumapplicationaddress-value-setter);

constant-namer("dwactiveprocessormask_value", dwactiveprocessormask-value);

constant-namer("dwactiveprocessormask_value_setter", dwactiveprocessormask-value-setter);

constant-namer("dwnumberofprocessors_value", dwnumberofprocessors-value);

constant-namer("dwnumberofprocessors_value_setter", dwnumberofprocessors-value-setter);

constant-namer("dwprocessortype_value", dwprocessortype-value);

constant-namer("dwprocessortype_value_setter", dwprocessortype-value-setter);

constant-namer("dwallocationgranularity_value", dwallocationgranularity-value);

constant-namer("dwallocationgranularity_value_setter", dwallocationgranularity-value-setter);

constant-namer("wprocessorlevel_value", wprocessorlevel-value);

constant-namer("wprocessorlevel_value_setter", wprocessorlevel-value-setter);

constant-namer("wprocessorrevision_value", wprocessorrevision-value);

constant-namer("wprocessorrevision_value_setter", wprocessorrevision-value-setter);

constant-namer("_L_system_info_G_", <system-info>);

constant-namer("_L_lpsystem_info_G_", <lpsystem-info>);

constant-namer("internal_value", internal-value);

constant-namer("internal_value_setter", internal-value-setter);

constant-namer("internalhigh_value", internalhigh-value);

constant-namer("internalhigh_value_setter", internalhigh-value-setter);

constant-namer("offset_value", offset-value);

constant-namer("offset_value_setter", offset-value-setter);

constant-namer("offsethigh_value", offsethigh-value);

constant-namer("offsethigh_value_setter", offsethigh-value-setter);

constant-namer("hevent_value", hevent-value);

constant-namer("hevent_value_setter", hevent-value-setter);

constant-namer("_L_overlapped_G_", <overlapped>);

constant-namer("_L_lpoverlapped_G_", <lpoverlapped>);

constant-namer("nlength_value", nlength-value);

constant-namer("nlength_value_setter", nlength-value-setter);

constant-namer("lpsecuritydescriptor_value", lpsecuritydescriptor-value);

constant-namer("lpsecuritydescriptor_value_setter", lpsecuritydescriptor-value-setter);

constant-namer("binherithandle_value", binherithandle-value);

constant-namer("binherithandle_value_setter", binherithandle-value-setter);

constant-namer("_L_security_attributes_G_", <security-attributes>);

constant-namer("_L_psecurity_attributes_G_", <psecurity-attributes>);

constant-namer("_L_lpsecurity_attributes_G_", <lpsecurity-attributes>);

constant-namer("dwlowdatetime_value", dwlowdatetime-value);

constant-namer("dwlowdatetime_value_setter", dwlowdatetime-value-setter);

constant-namer("dwhighdatetime_value", dwhighdatetime-value);

constant-namer("dwhighdatetime_value_setter", dwhighdatetime-value-setter);

constant-namer("_L_filetime_G_", <filetime>);

constant-namer("_L_pfiletime_G_", <pfiletime>);

constant-namer("_L_lpfiletime_G_", <lpfiletime>);

variable-namer("_D_invalid_handle_value", $invalid-handle-value);
