#include "dylan.h"

/**********************************************************************\
*                                                                      *
* windbg ( )                                                           *
*                                                                      *
* Arguments:                                                           *
*                                                                      *
*    hCurrentProcess - Supplies a handle to the current process (at    *
*        the time the extension was called).                           *
*                                                                      *
*    hCurrentThread - Supplies a handle to the current thread (at      *
*        the time the extension was called).                           *
*                                                                      *
*    CurrentPc - Supplies the current pc at the time the extension is  *
*        called.                                                       *
*                                                                      *
*    lpExtensionApis - Supplies the address of the functions callable  *
*        by this extension.                                            *
*                                                                      *
*    lpArgumentString   Supplies the pattern and expression for this   *
*        command.                                                      *
*                                                                      *
* Return Value:                                                        *
*                                                                      *
*    None.                                                             *
\**********************************************************************/

WINDBG_EXTENSION_APIS ExtensionApis;
static extensionApisInitialized = 0;

char *dylan_current_library = (char*)0;

void maybeInitializeLibrary () {
  if (!dylan_current_library) {
    dylan_current_library = (char*)malloc(MAX_NAME_SIZE);
    strcpy(dylan_current_library, "dylan");
  }
}

char *dylan_current_module = (char*)0;

void maybeInitializeModule () {
  if (!dylan_current_module) {
    dylan_current_module = (char*)malloc(MAX_NAME_SIZE);
    strcpy(dylan_current_module, "internal");
  }
}

void maybeInitializeTopLevel (WINDBG_EXTENSION_APIS *lpExtensionApis) {
  if (!extensionApisInitialized) {
    memcpy(&ExtensionApis, lpExtensionApis, sizeof(WINDBG_EXTENSION_APIS));
    maybeInitializeLibrary();
    maybeInitializeModule();
    extensionApisInitialized = 1;
  }
}

int dylan_print_length = DEFAULT_PRINT_LENGTH;

VOID length (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    int new_print_length;

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (sscanf(lpArgumentString, "%d", &new_print_length) > 0)
      dylan_print_length = new_print_length;
    else
      dprintf("%d\n", dylan_print_length);
}

int dylan_print_depth = DEFAULT_PRINT_DEPTH;

VOID depth (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    int new_print_depth;

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (sscanf(lpArgumentString, "%d", &new_print_depth) > 0)
      dylan_print_depth = new_print_depth;
    else
      dprintf("%d\n", dylan_print_depth);
}

VOID library (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    char new_library[MAX_NAME_SIZE];

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (sscanf(lpArgumentString, "%s", new_library) > 0)
      strcpy(dylan_current_library, new_library);
    else
      dprintf("%s\n", dylan_current_library);
}

VOID module (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    char new_module[MAX_NAME_SIZE];

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (sscanf(lpArgumentString, "%s", new_module) > 0)
      strcpy(dylan_current_module, new_module);
    else
      dprintf("%s\n", dylan_current_module);
}

CORE_ADDR GetDylanExpression (LPSTR lpArgumentString) {
  char argumentString[MAX_NAME_SIZE];
  CORE_ADDR addr;

  if (sscanf(lpArgumentString, "%s", argumentString) == 1) {
    if (argumentString[0] == '$') {
      int index;
      if (sscanf(&argumentString[1], "%d", &index) == 1 &&
	  index >= 0 && index < max_values_index) {
	return(values[index]);
      } else 
        return(false);
    } else if (addr = GetExpression(argumentString)) 
      return(addr);
    else {
      /*
      char mangled[MAX_NAME_SIZE];
      char mangledVar[MAX_NAME_SIZE];
      mangle_dylan_name(argumentString, mangled, false);
      sprintf(mangledVar, "%s@", mangled);
      if (addr = GetExpression(mangledVar))
        return(addr);
      else
        return(GetExpression(mangled));
      */
      char mangled[MAX_NAME_SIZE];
      mangle_dylan_name(argumentString, mangled, false);
      addr = GetExpression(mangled);
      if (addr && dylan_object_p(addr))
        return(addr);
      else {
        char mangledVar[MAX_NAME_SIZE];
        sprintf(mangledVar, "%s@", mangled);
        return(GetExpression(mangledVar));
      }
    }
  } else
     return(false);
}

VOID print (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    CORE_ADDR address;

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (address = GetDylanExpression(lpArgumentString)) {
      dprintf("0x%lx $%d = ", address, values_index);
      add_value(address);
      print_dylan_object(0, address, true, 0);
    }
}

VOID describe (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    CORE_ADDR address;

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    if (address = GetDylanExpression(lpArgumentString)) {
      dprintf("$%d = 0x%lx\n", values_index, address);
      add_value(address);
      describe_dylan_object(0, address, true, 0);
    }
}

VOID mangle (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    char mangled[MAX_NAME_SIZE];

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    dprintf(mangle_dylan_name(lpArgumentString, mangled, false));
}

VOID demangle (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    char demangled[MAX_NAME_SIZE];

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    dprintf(demangle_dylan_name(lpArgumentString, demangled, false));
}

VOID where (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )
{
    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    maybeInitializeTopLevel(lpExtensionApis);

    dprintf("where %s\n", lpArgumentString);
}

/*
VOID str (
    HANDLE hCurrentProcess,
    HANDLE hCurrentThread,
    DWORD dwCurrentPc,
    PWINDBG_EXTENSION_APIS lpExtensionApis,
    LPSTR lpArgumentString
    )

{
    CHAR String[MAXLEN];
    size_t Length;
    DWORD dwAddrString;
    CHAR Symbol[64];
    LPSTR StringData;
    DWORD Displacement;
    BOOL b;
    PNTSD_OUTPUT_ROUTINE lpOutputRoutine;
    PNTSD_GET_EXPRESSION lpGetExpressionRoutine;
    PNTSD_GET_SYMBOL lpGetSymbolRoutine;

    UNREFERENCED_PARAMETER( hCurrentProcess );
    UNREFERENCED_PARAMETER( hCurrentThread );
    UNREFERENCED_PARAMETER( dwCurrentPc );

    lpOutputRoutine = lpExtensionApis->lpOutputRoutine;
    lpGetExpressionRoutine = lpExtensionApis->lpGetExpressionRoutine;
    lpGetSymbolRoutine = lpExtensionApis->lpGetSymbolRoutine;

    //
    // Evaluate the argument string to get the address of
    // the string to dump.
    //

    dwAddrString = (lpGetExpressionRoutine)(lpArgumentString);
    if (!dwAddrString) {
	(lpOutputRoutine)( "Invalid Expression." );
	return;
    }

    //
    // Get the symbolic name of the string
    //

    (lpGetSymbolRoutine)((LPVOID)dwAddrString,(PUCHAR)Symbol,&Displacement);

    //
    // Read current process memory and handle remote read as well
    //

    b = (lpExtensionApis->lpReadProcessMemoryRoutine)(
                                                     dwAddrString,
						     String,
						     MAXLEN,
                                                     NULL
                                                     );

    if (!b) {
	(lpOutputRoutine)( "ReadProcessMemory failed." );
	return;
    }

    Length = strlen( String );

    StringData = (LPSTR)LocalAlloc(LMEM_ZEROINIT,Length+1);

    if (!StringData) {
	(lpOutputRoutine)( "LocalAlloc failed. Error = %x", GetLastError());
        return;
    }

    (lpOutputRoutine)(
	"String: %s ; %d bytes at %lx\n",
	String,
	Length,
	dwAddrString
        );

    LocalFree( StringData );
}

 
void
print_dylan_val (struct type *type, char *valaddr, CORE_ADDR address, FILE *stream)
{
  if (type->code == TYPE_CODE_PTR) {
    CORE_ADDR val = *((CORE_ADDR*)valaddr);
    print_dylan_object(stream, val, true, 0);
  } else {
    val_print (type, valaddr, address, stream, 0, 0, 2, Val_no_prettyprint);
  }
}
*/
