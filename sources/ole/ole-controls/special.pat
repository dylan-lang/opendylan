! Additional translation rules for "olectl.h" and "ocidl.h".
! Copyright: 1997, 1998 Functional Objects, Inc.  All rights reserved.


\N\#if\ defined(_WIN32) \&\& \!defined(OLE2ANSI)<endline><matchcond><elsepart>=@{$2}

def-flag:FACILITY_CONTROL=$0@end

! special case for PICTYPE_UNINITIALIZED
number:(UINT)-1=\$FFFFFFFF@end

! for constants using STD_CTL_SCODE, CUSTOM_CTL_SCODE, or MAKE-SCODE
number:<K2>\J<i>(<number>)=@map-name{$1$2}($3)@end
number:MAKE_SCODE(\W<number>,\W<number>,\W<number>)=MAKE-SCODE($1, $2, $3)@end
number:C\J<K>\J_E_FIRST\+<D>=u\%\+(@number{C$1_E_FIRST}, $2)@end
number:S\J<K>\J_E_FIRST\+<D>=u\%\+(@number{S$1_E_FIRST}, $2)@end
number:P\J<K>\J_E_FIRST\+<D>=u\%\+(@number{P$1_E_FIRST}, $2)@end

type:size_t<stars>=\<C-unsigned-int\>$1@end
type:struct\ IBindHost\ __RPC_FAR\W\*=\<Interface\> \/\* IBindHost \*\/@end

! embedded structs for PICTDESC:
fields:struct\{<fields>\}\W<I>\;=@incr{gencount}\
	@out{define C-struct \<$2\%${gencount}\>\n$1\Nend\;\n}\
	\ \ slot @export{$2-value} @tab{20}\:\: \<$2\%${gencount}\>,\ \
		setter\: \#f\;\n
fields:union\{<fields>\}\;=@incr{gencount}\
	@out{define C-union \<u\%${gencount}\>\n$1\Nend\;\n}\
       \ \ slot @export-slot{u} @tab{20}\:\: \<u\%${gencount}\>\;\n

DECLARE_INTERFACE_(<matchparen>)\{<matchparen>\}\;=

outok:ppUnk=@fail! OleCreatePropertyFrame - array pointer

! to use output parameter in OleTranslateColor:
scalar:\JCOLORREF=$0@end

! omit functions defined by user instead of by the Microsoft library:
STDAPI\ DllRegisterServer(void)\;=
STDAPI\ DllUnregisterServer(void)\;=

! Omit OleLoadPicturePath because it is undocumented and not in NT 3.51.
! It is still described as a ``near-future API'' in the October 1997 MSDN.
undef-flag:ASYNCPICTURE=@end

! Newly declared (Oct. '97), but not yet defined (NT 4.0 sp2) or documented:
relevant-function:OleLoadPictureEx=@fail
relevant-function:OleLoadPictureFileEx=@fail

! These are defined in NT 4.0 (sp2) but are not in Windows 95:
relevant-function:OleCreateFontIndirect=@fail
relevant-function:OleCreatePictureIndirect=@fail
relevant-function:OleCreatePropertyFrame=@fail
relevant-function:OleCreatePropertyFrameIndirect=@fail
relevant-function:OleIconToCursor=@fail
relevant-function:OleLoadPicture=@fail
relevant-function:OleTranslateColor=@fail
bad-struct:tagOCPFIPARAMS=$0@end

! skip DEFINE_GUID forms
def-flag:INITGUID=$0@end

! Skip forward declarations that would become duplicate definitions in Dylan:
typedef\ struct\ tag\J<K> FAR\* LP\J$1\;=

! 30 bits should be plenty for these; don't need <C-both-long>:
typedef\ long OLE_\J<K>\J_\J<K>\;=\N\
	define constant @export{@type{OLE_$1_$2}} @tab{38}\= \<C-long\>\;\n

\N\/\/\/\/\/*\n=@export-end{}

\L\#define <relevant-constant> *\n=@err{Not translated\: $0}
\L\#define <relevant-function>\J(*\n=@err{Not translated\: $0}
\S=
\;=
\L<T>\n=@err{line @line\: Not translated\: $0}

WINOLECTLAPI <relevant-function>\G(<args>)\;=@stdfun{\<C-HRESULT\>\d$1\d$2}
WINOLECTLAPI_(<type>)\W<relevant-function>\G(<args>)\;=@stdfun{$1\d$2\d$3}

! skip arglist of omitted functions to avoid warnings:
WINOLECTLAPI <omitted-function>(<matchparen>)\;=
WINOLECTLAPI_(<type>)\W<omitted-function>(<matchparen>)\;=

omitted-function:<relevant-function>=@fail
omitted-function:<I>=$1@end
omitted-function:=@fail
