OLE Automation Training Materials
---------------------------------

The OLE Automation session was presented by Hugh Greene.


* Contents

This directory contains the following files and subdirectories.

File				Description
----				-----------

readme.txt			This file
OLEAutomation.doc			Presentation "slides" as MS Word doc
OLEAutomation.txt			Notes for presentation and practical
select-viewer-server [dir]		Server side of exercise
select-viewer-controller [dir]	Controller (client) side of exercise


In the original session, there were also doc files for the following OLE
macros but, as of release 1.1, these are documented in the release notes
(and in later releases, in the product doc, I assume). 

Macro			1.1 Release notes section
-----			-------------------------

custom-interface-definer	5.7 COM library
coclass-definer		5.8.8 New macros
dispatch-client-definer	5.8.8 New macros
dispatch-server-definer	5.8.8 New macros
dual-interface-definer	5.8.8 New macros


* Notes

** Client example

A "feature" of our implementation means that COM/OLE operations can only be
carried out on the first thread to call OLE-Initialize.  (I'll report this as
a bug.)  The notes on writing the server-side in the practical describe how
to work around this.  However, in the 1.1 release, the main thread always
calls OLE-Initialize (during initialization of the OLE-AUTOMATION library),
so you have to make that your query thread.  The pre-written client solution
has *not* been updated to do things this way, so it won't work for now.

Also, the client is buggy in that it uses AS to convert from <BSTR> to
<byte-string>, but never frees the <BSTR>s afterwards.

