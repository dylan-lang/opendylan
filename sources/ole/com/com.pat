
! Additional translation rules specific to the COM library

! Copyright: 1998 Functional Objects, Inc.  All rights reserved.



\L\#define\ REFGUID const GUID \* const=\N\
	define constant @export{\<REFGUID\>} \= \<LPGUID\>\;\n
\L\#define\sREF\J<K>\JID\L <type>\N=\
	define constant @export{\<REF$1ID\>} \= $2\;\n
