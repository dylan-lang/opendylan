Summary: Functional Developer for Linux
Name: functional-developer
Version: 2.1alpha3
Release: 1
URL: http://www.functionalobjects.com/
Source0: functional-developer-2.1alpha3-1.tgz
Vendor: Functional Objects, Inc.
License: (c) 1995-2003 Functional Objects, Inc.  All Rights Reserved.
Group:  Development/Languages
BuildRoot: %{_tmppath}/%{name}-root

%description
This is the third alpha release of the Functional Developer Dylan
compiler for Linux.  Please see the README file for a list of
limitations in this release.

%prep
%setup -q

%build

%install
[ $RPM_BUILD_ROOT == / ] || rm -rf $RPM_BUILD_ROOT
make ROOT="$RPM_BUILD_ROOT" install

%pre
[ $1 -gt 1 ] && exit 0
more <<EOF
Functional Developer(TM) Beta Software License Agreement

IMPORTANT: IN ORDER TO INSTALL THIS SOFTWARE, YOU MUST FIRST READ THE
FOLLOWING TERMS AND CONDITIONS.  BY TYPING "YES" AT THE END OF THIS
DOCUMENT, YOU ARE EXPRESSLY AGREEING TO BE BOUND BY ALL OF THE TERMS
AND CONDITIONS OF THIS AGREEMENT.  IF YOU DO NOT AGREE TO ALL OF THE
TERMS AND CONDITIONS OF THIS AGREEMENT, TYPE "NO"; THE INSTALLATION
PROCESS WILL BE ABORTED AND YOU MAY RETURN THE SOFTWARE TO FUNCTIONAL
OBJECTS.

1.  License Grant; Copying.  Functional Objects, Inc. ("Functional
Objects") grant to the Customer ("you") a non-exclusive,
non-transferable license to use one (1) copy of the Functional
Developer Beta software program ("Software") solely for the purposes
of developing applications written in the DYLAN programming language.
You may use the Software only on a single computer at a time.  You may
not copy the Software except as necessary to exercise your rights
under this Agreement and to make one (1) copy of the Software in
machine readable form for back-up or archival purposes only.  This
license does not permit the distribution of the Software, or any part
thereof, to any third party.  You may not modify, reverse compile,
disassemble, or otherwise reverse engineer the Software, except that
in the European Community, you may reverse engineer only for
interoperability purposes and then only if all conditions of Article 6
of Council Directive 91/250/EEC are met.

2.  Distribution of Runtime Libraries.  You may not copy or distribute
any runtime libraries.

3. Non-Commercial Use Only.  The Software is licensed for academic,
personal, and commercial evaluation and testing purposes only.  Use
for the purposes of developing any commercial software product is
strictly prohibited and shall be ground for termination of this
license.

4.  Ownership; Confidentiality. You acknowledge and agree that the
Software contains the confidential and proprietary information of
Functional Objects and its licensors and is provided solely under the
terms and conditions of this Agreement.  All right, title to,
ownership of and all patent, copyright, trade secret, trademark and
all other proprietary rights in the Software shall remain in
Functional Objects or its licensors. You shall not remove any product
identification, copyright notices, or other legends set forth on the
Software and shall reproduce all such notices on any copies.  You
shall have no right Functional Objects' or its third party licensors'
trademarks in connection with the Software, or with its promotion or
publication, without Functional Objects' prior written approval.  You
agree not to use the trademark DYLAN on the "About Box", splash
screen, packaging or other visible location on any Application.

5. Warranty Exclusion.  THE SOFTWARE IS PRE-RELEASE SOFTWARE AND IS
LICENSED "AS IS" WITHOUT WARRANTY OR REPRESENTATION OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. Functional Objects reserves the right to change the Software,
including its specifications and documentation, at any time.

6.  Limitation of Liability. IN NO EVENT SHALL FUNCTIONAL OBJECTS OR
ITS LICENSORS BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
CONSEQUENTIAL DAMAGES EVEN IF FUNCTIONAL OBJECTS HAS BEEN ADVISED OF
THE POSSIBILITY OF SUCH DAMAGES IN ADVANCE.  IN NO EVENT SHALL
FUNCTIONAL OBJECTS' LIABILITY EXCEED THE AMOUNT OF LICENSE FEES PAID
BY YOU FOR USE OF THE SOFTWARE.

7.  Government Supply. This Software is a commercial computer software
program developed at private expense and is subject to the following
restricted rights legend: "Use, duplication, or disclosure by the
United States Government is subject to restrictions as set forth in
(i) FAR 52.227-14 Alt III, (ii) FAR 52.227-19; as applicable.  Use by
agencies of the Department of Defense (DOD) is subject to Functional
Objects' commercial license as contained in the accompanying license
agreement, in accordance with DFAR 227.7202-1 (a).  For purposes of
the FAR, the Software shall be deemed 'unpublished' and licensed with
disclosure prohibitions, rights reserved under the copyright laws of
the United States.  Functional Objects, Inc., 84 Chandler Street,
Somerville, Massachusetts 02144."  You agree to include this language
in any Application, including any associated documentation, you
distribute written using this Software.

8.  Export Control. You may not export or re-export the Software or
any underlying information or technology except in full compliance
with all United States and other applicable laws and regulations of
all applicable countries.

9.  Governing Law. This Agreement shall be construed in accordance
with the substantive laws of the Commonwealth of Massachusetts.

10.  Non-Assignment; Entire Agreement. Except as in accordance with
this Agreement, you shall not sell, assign, sublicense or otherwise
transfer the Software without Functional Objects' prior written
consent. Any such attempted transfer shall be void.  This Agreement
constitutes the entire agreement between the parties and supersedes
all other communications between the parties relating to the subject
matter hereof.  This Agreement may only be modified by a writing
signed by Functional Objects.

11.  Termination. Functional Objects may terminate this Agreement if
you fail to comply with any of the terms and conditions of this
Agreement.  Upon termination, you shall cease using the Software and
shall destroy or return to Functional Objects all copies of the
Software.

BY INSTALLING THIS SOFTWARE, YOU ACKNOWLEDGE THAT YOU HAVE READ THIS
AGREEMENT, THAT YOU UNDERSTAND IT AND THAT YOU AGREE TO BE BOUND BY
ITS TERMS AND CONDITIONS.

Copyright (C) 2001  Functional Objects, Inc.  All Rights Reserved.
FUNCTIONAL DEVELOPER is a trademark of Functional Objects, Inc.
EOF
while true; do
  echo
  echo -n "Agree to the license? (yes/no) "
  read res < /dev/stdout
  case "$res" in
    yes|y)
      exit 0;;
    no|n)
      exit 1;;
  esac
done

%post
[ -d /usr/local/lib/functional-developer/profiles ] || mkdir /usr/local/lib/functional-developer/profiles
exit 0

%preun
[ $1 = 0 ] && [ -d /usr/local/lib/functional-developer/profiles ] && rm -rf /usr/local/lib/functional-developer/profiles
exit 0

%clean
[ $RPM_BUILD_ROOT == / ] || rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/local/lib/functional-developer/
/usr/local/bin/fdcompile
/usr/local/bin/xfdcompile

%changelog
* Sun Jan 19 2003 Gary Palter <Palter@FunctionalObjects.COM>
- Third alpha release of Functional Developer for x86 Linux:
  - Implement support for both x86 Linux thread models
  - Declare the Network library and its test suite to be open source
  - Expires on January 1, 2004

* Tue Sep 25 2001 Gary Palter <Palter@FunctionalObjects.COM>
- Second alpha release of Functional Developer for x86 Linux:
  - Implement support for threads
  - Add condition classes for arithmetic errors
  - Fix bug in floating point exception handling
  - Expires on January 1, 2002

* Sat Sep 1 2001 Gary Palter <Palter@FunctionalObjects.COM>
- First alpha release of Functional Developer for x86 Linux

