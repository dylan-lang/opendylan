
! Additional translation rules specific to "shellapi.h"

SHSTDAPI <relevant-function>\G(<args>)\;=@stdfun{\<C-HRESULT\>\d$1\d$2}
SHSTDAPI_(<type>)\W<relevant-function>\G(<args>)\;=@stdfun{$1\d$2\d$3}

! stdfun{type name args}
stdfun:*\d*\d*=\
	\N\ndefine inline-only C-function @export{@fnname{$2}}\n$3\N\
	@result{$1}\
	\ \ c-name\: \"$2\",@wrap{\ c-modifiers\: \"__stdcall\"\;}\n\
	end\;\n

type:__int64=\<ULARGE-INTEGER\>@end

resolve-type:\*ULARGE_INTEGER=PULARGE-INTEGER

DECLARE_HANDLE(<L>)\;=\Ndefine C-subtype @export{\<$1\>} ( \<HANDLE\> ) end\;\n

! undocumented flag, assume not set:
undef-flag:WINNT=@end

! prevent duplicate definition:
relevant-function:Shell_NotifyIconW=@fail

! These functions are undefined and undocumented:
relevant-function:FindEnvironmentStringA=@fail
relevant-function:WinExecErrorA=@fail
relevant-function:SHGetDiskFreeSpace\J<opta>\I=@fail

! newly defined but not documented:
relevant-function:DuplicateIcon=@fail
bad-struct:_DRAGINFO\J<opta>\I=$0@end
relevant-name:DRAGINFO\J<opta>\I=@fail
relevant-name:LPDRAGINFO=@fail

! Exclude functions new in NT 5.0:
relevant-function:DoEnvironmentSubst\J<opta>\I=@fail
relevant-function:SHEmptyRecycleBin\J<opta>\I=@fail
relevant-function:SHGetNewLinkInfo\J<opta>\I=@fail
relevant-function:SHInvokePrinterCommand\J<opta>\I=@fail
relevant-function:SHQueryRecycleBin\J<opta>\I=@fail
bad-struct:_SHQUERYRBINFO\J<opta>\I=$0@end
relevant-constant:SHERB_NO\J<K>\I=@fail
relevant-constant:PRINTACTION_\J<K>\I=@fail




