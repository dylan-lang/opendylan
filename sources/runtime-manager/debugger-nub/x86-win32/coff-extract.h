/* ********************************************************************** */
/* ** coff-extract.h                                                   ** */
/* ** Descriptors for COFF and CodeView formats.                       ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* ********************************************************************** */

#define OBJECT_FILE 1
#define IMAGE_FILE 2

// Windows NT limits the number of COFF sections to 96. The debugger nub 
// does not enforce this, but it does assume that there *probably* won't
// be more. Therefore, space for 96 sections is initially allocated, with
// a further malloc if more sections exist.

#define MAX_SECTIONS 96
#define MAX_DEBUG_DIRECTORIES 10
#define SECTION_NOT_FOUND 100000

// SECTION_NOT_FOUND should safely be larger than the number of sections
// in any case. If it isn't, we're in trouble.

#define LOCATION_OF_SIGNATURE_POINTER 0x3c

#define NAME_OF_DEBUG_SECTION ".debug"
#define NAME_OF_SYM_DEBUG_SECTION ".debug$S"

#define LENGTH_OF_DEBUG_STRING 6
#define LENGTH_OF_SYM_DEBUG_STRING 8

typedef struct _COFF_HEADER {

 WORD         Machine;
 WORD         NumberOfSections;
 DWORD        TimeStamp;
 DWORD        PointerToSymbolTable;
 DWORD        NumberOfSymbols;
 WORD         OptionalHeaderSize;
 WORD         Characteristics;

} COFF_HEADER, *PCOFF_HEADER;


typedef struct _OPT_HDR_STANDARD {

 WORD         Magic;
 BYTE         LMajor;
 BYTE         LMinor;
 DWORD        CodeSize;
 DWORD        InitializedDataSize;
 DWORD        UninitializedDataSize;
 DWORD        EntryPointRVA;
 DWORD        BaseOfCode;
 DWORD        BaseOfData;

} OPT_HDR_STANDARD;


typedef struct _OPT_HDR_WINDOWS_NT {

 DWORD        ImageBase;
 DWORD        SectionAlignment;
 DWORD        FileAlignment;
 WORD         OSMajor;
 WORD         OSMinor;
 WORD         UserMajor;
 WORD         UserMinor;
 WORD         SubSysMajor;
 WORD         SubSysMinor;
 DWORD        Reserved;
 DWORD        ImageSize;
 DWORD        HeaderSize;
 DWORD        FileChecksum;
 WORD         SubSystem;
 WORD         DLLFlags;
 DWORD        StackReserveSize;
 DWORD        StackCommitSize;
 DWORD        HeapReserveSize;
 DWORD        HeapCommitSize;
 DWORD        LoaderFlags;
 DWORD        NumberOfDataDirectories;

} OPT_HDR_WINDOWS_NT;


typedef struct _DATA_DIRECTORY {

 DWORD        RVA;
 DWORD        Size;

} DATA_DIRECTORY;


typedef struct _OPT_HDR_DATA_DIRECTORIES {

 DATA_DIRECTORY   ExportTable;
 DATA_DIRECTORY   ImportTable;
 DATA_DIRECTORY   ResourceTable;
 DATA_DIRECTORY   ExceptionTable;
 DATA_DIRECTORY   SecurityTable;
 DATA_DIRECTORY   BaseRelocationTable;
 DATA_DIRECTORY   Debug;
 DATA_DIRECTORY   Copyright;
 DATA_DIRECTORY   GlobalPtr;
 DATA_DIRECTORY   TLSTable;
 DATA_DIRECTORY   LoadConfigTable;
 BYTE             Reserved[40];

} OPT_HDR_DATA_DIRECTORIES;


typedef struct _DEBUG_DIRECTORY {

 DWORD            Characteristics;
 DWORD            TimeStamp;
 WORD             MajorVersion;
 WORD             MinorVersion;
 DWORD            DebugType;
 DWORD            SizeOfData;
 DWORD            RVAOfRawData;
 DWORD            PointerToRawData;

} DEBUG_DIRECTORY;


typedef struct _OPTIONAL_HEADER {

 OPT_HDR_STANDARD         StandardFields;
 OPT_HDR_WINDOWS_NT       WindowsNTFields;
 OPT_HDR_DATA_DIRECTORIES DataDirectories;

} OPTIONAL_HEADER;


typedef struct _COFF_SECTION_HEADER {

 char         SectionName[8];
 DWORD        VirtualSize;
 DWORD        RVA;
 DWORD        SizeOfRawData;
 DWORD        PointerToRawData;
 DWORD        PointerToRelocs;
 DWORD        PointerToLineNumbers;
 WORD         NumberOfRelocs;
 WORD         NumberOfLineNumbers;
 DWORD        SectionFlags;

} COFF_SECTION_HEADER, *PCOFF_SECTION_HEADER;


typedef struct _COFF_RELOCATION {

 DWORD        Offset;
 DWORD        SymbolTableIndex;
 WORD         Type;

} COFF_RELOCATION, *PCOFF_RELOCATION;


typedef union _COFF_LINENUMBER_TYPE {

 DWORD        SymbolTableIndex;
 DWORD        RVA;

} COFF_LINENUMBER_TYPE;


typedef struct _COFF_LINENUMBER {

 COFF_LINENUMBER_TYPE   Type;
 WORD                   LineNumber;

} COFF_LINENUMBER, *PCOFF_LINENUMBER;


typedef struct _IMAGE_SIGNATURE {

 char         Signature[4];

} IMAGE_SIGNATURE, *PIMAGE_SIGNATURE;


typedef struct _COFF_SECTION_SUMMARY {

 COFF_HEADER           Header;
 PCOFF_SECTION_HEADER  SectionHeaders;

} COFF_SECTION_SUMMARY;


typedef union _SYMBOL_NAME {

 char          Name[8];

 struct {

  DWORD     NullDWord;
  DWORD     StringTableOffset;
 }
               Pointer;
} SYMBOL_NAME;

#define SIZEOF_SYMBOL_ENTRY 18

#define COFF_SYMBOL_TYPE_FUNCTION 0x20
#define COFF_SYMBOL_TYPE_NOT_FUNCTION 0x0

#define COFF_ITERATOR_NEXT_SYMBOL 1
#define COFF_ITERATOR_NEXT_STANDARD_SYMBOL 2
#define COFF_ITERATOR_ABORT 3

// COFF_SYMBOL_ENTRY
// A standard symbol-defining record in a COFF symbol table.

typedef struct _COFF_SYMBOL_ENTRY {

 SYMBOL_NAME   Name;
 DWORD         Value;
 WORD          SectionNumber;
 WORD          Type;
 BYTE          StorageClass;
 BYTE          NumberOfAuxSymbols;

} COFF_SYMBOL_ENTRY;

// COFF_AUX_FUNCTION_DEFINITION
// An auxiliary COFF symbol following a function-defining standard symbol

typedef struct _COFF_AUX_FUNCTION_DEFINITION {
  DWORD        TagIndex;
  DWORD        TotalSize;
  DWORD        PointerToLinenumbers;
  DWORD        PointerToNextFunction;
  WORD         Unused;
} COFF_AUX_FUNCTION_DEFINITION;

// COFF_AUX_BF_EF
// An auxiliary COFF symbol following a .bf or .ef symbol.

typedef struct _COFF_AUX_BF_EF {
  DWORD        Unused1;
  WORD         LineNumber;
  BYTE         Unused2[6];
  DWORD        PointerToNextFunction;
  WORD         Unused3;
} COFF_AUX_BF_EF;

// COFF_AUX_WEAK_EXTERNAL
// An auxiliary COFF symbol for a weak external reference.

typedef struct _COFF_AUX_WEAK_EXTERNAL {
  DWORD        TagIndex;
  DWORD        Characteristics;
  BYTE         Unused[10];
} COFF_AUX_WEAK_EXTERNAL;

// COFF_AUX_FILENAME
// An auxiliary COFF symbol for the record following a .file symbol.
// Just contains characters!

typedef struct _COFF_AUX_FILENAME {
  char         FileName[18];
} COFF_AUX_FILENAME;

// COFF_AUX_SECTION_DEFINITION
// An auxiliary COFF symbol for the record following a section-defining
// symbol (such as .text or .drectve)

typedef struct _COFF_AUX_SECTION_DEFINITION {
  DWORD        Length;
  WORD         NumberOfRelocs;
  WORD         NumberOfLinenumbers;
  DWORD        CheckSum;
  WORD         Number;
  BYTE         Selection;
  BYTE         Unused[3];
} COFF_AUX_SECTION_DEFINITION;

// Types and structures for SubSection Directories.

typedef struct _SS_DIR_HEADER {

 WORD          cbDirHeader;
 WORD          cbDirEntry;
 DWORD         cDir;
 DWORD         lfoNextDir;
 DWORD         flags;

} SS_DIR_HEADER;

typedef struct _SS_DIR_ENTRY {

 WORD          subsection;
 WORD          iMod;
 DWORD         lfo;
 DWORD         cb;

} SS_DIR_ENTRY;

// IP_CACHE_ENTRY is a record that points you to the CodeView data for
// the function whose code location lies between Start and End
// (inclusive).

#define IP_CACHE_SIZE 200

typedef struct _IP_CACHE_ENTRY {

 DWORD         FilePointer;
 DWORD         Start;
 DWORD         DebugStart;
 DWORD         End;
 DWORD         DebugEnd;

} IP_CACHE_ENTRY;

typedef struct _LEXICAL *LPLEXICAL;

typedef struct _LEXICAL {

 DWORD         FilePointer;
 int           Offset;
 LPLEXICAL     Next;

} LEXICAL;

typedef struct _IMAGE_INFORMATION {

 IMAGE_SIGNATURE      ImageSignature;
 COFF_HEADER          CoffHeader;
 OPTIONAL_HEADER      OptionalHeader;
 COFF_SECTION_HEADER  *SectionHeaders;
 COFF_SYMBOL_ENTRY    *SymbolTable;
 DWORD                lfaBase;
 DWORD                ImageBase;
 HANDLE               ImageFileHandle;
 int                  NumberOfDebugDirectories;
 DEBUG_DIRECTORY      DebugDirectories[MAX_DEBUG_DIRECTORIES];
 int                  NumberOfSubSectionDirEntries;
 int                  IPCacheIndex;
 BOOL                 IPCacheFull;

 // Note: Zero is NOT a valid value of IPCacheIndex.
 // (this is because zero is returned as a failure code from
 // the function find_function.)

 IP_CACHE_ENTRY       IPCache[IP_CACHE_SIZE + 1];
 SS_DIR_ENTRY         *SubSectionDirEntries;

 // Coff Symbolic Information

 DWORD                NumberOfCoffSymbols;
 BYTE                *CoffSymbolTable;
 char                *CoffStringTable;
 BOOL                 CoffTableExplicitlyAllocated;

} IMAGE_INFORMATION;

void test_print_image_information (IMAGE_INFORMATION *info);

int section_number_called (char *target, IMAGE_INFORMATION *info);
