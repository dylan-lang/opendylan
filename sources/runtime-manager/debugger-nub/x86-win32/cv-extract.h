/* ********************************************************************** */
/* ** cv-extract.h                                                     ** */
/* ** Descriptors for COFF and CodeView formats.                       ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* ********************************************************************** */

// The macro CVMAP generates a pointer by combining the pointer to the base
// of CodeView data with the required offset to something in that data.
// So, if there is a SOME_STRUCT object at offset X from the base of the
// image information (info), then a pointer to it is obtained with the
// expression:

//      (SOME_STRUCT*) (CVMAP(info, X))

// (Gotta love those C macros...)


#define CVMAP(i, y) (((DWORD) ((i)->CodeViewSymbols)) + ((DWORD) (y)))

// Constants - CV field indices.

#define CV_S_COMPILE 0x0001
#define CV_S_REGISTER 0x0002
#define CV_S_REGISTER_NEW 0x1001
#define CV_S_CONSTANT 0x0003
#define CV_S_CONSTANT_NEW 0x1002
#define CV_S_UDT 0x0004
#define CV_S_UDT_NEW 0x1003
#define CV_S_SSEARCH 0x0005
#define CV_S_END 0x0006
#define CV_S_SKIP 0x0007
#define CV_S_CVRESERVE 0x0008
#define CV_S_OBJNAME 0x0009
#define CV_S_ENDARG 0x000a
#define CV_S_COBOLUDT 0x000b
#define CV_S_COBOLUDT_NEW 0x1004
#define CV_S_MANYREG 0x000c
#define CV_S_MANYREG_NEW 0x1005
#define CV_S_RETURN 0x000d
#define CV_S_ENTRYTHIS 0x000e

#define CV_S_BPREL16 0x0100
#define CV_S_LDATA16 0x0101
#define CV_S_GDATA16 0x0102
#define CV_S_PUB16 0x0103
#define CV_S_LPROC16 0x0104
#define CV_S_GPROC16 0x0105
#define CV_S_THUNK16 0x0106
#define CV_S_BLOCK16 0x0107
#define CV_S_WITH16 0x0108
#define CV_S_LABEL16 0x0109
#define CV_S_CEXMODEL16 0x010a
#define CV_S_VFTPATH16 0x010b
#define CV_S_REGREL16 0x010c

#define CV_S_BPREL32 0x0200
#define CV_S_BPREL32_NEW 0x1006
#define CV_S_LDATA32 0x0201
#define CV_S_LDATA32_NEW 0x1007
#define CV_S_GDATA32 0x0202
#define CV_S_GDATA32_NEW 0x1008
#define CV_S_PUB32 0x0203
#define CV_S_PUB32_NEW 0x1009
#define CV_S_LPROC32 0x0204
#define CV_S_LPROC32_NEW 0x100a
#define CV_S_GPROC32 0x0205
#define CV_S_GPROC32_NEW 0x100b
#define CV_S_THUNK32 0x0206
#define CV_S_BLOCK32 0x0207
#define CV_S_WITH32 0x0208
#define CV_S_LABEL32 0x0209
#define CV_S_CEXMODEL32 0x020a
#define CV_S_VFTPATH32 0x020b
#define CV_S_VFTPATH32_NEW 0x100c
#define CV_S_REGREL32 0x020c
#define CV_S_REGREL32_NEW 0x100d
#define CV_S_LTHREAD32 0x020d
#define CV_S_LTHREAD32_NEW 0x100e
#define CV_S_GTHREAD32 0x020e
#define CV_S_GTHREAD32_NEW 0x100f

#define CV_S_LPROCMIPS 0x0300
#define CV_S_LPROCMIPS_NEW 0x1010
#define CV_S_GPROCMIPS 0x0301
#define CV_S_GPROCMIPS_NEW 0x1011

#define CV_S_PROCREF 0x0400
#define CV_S_DATAREF 0x0401
#define CV_S_ALIGN 0x0402

// Constants: Subsection types

#define SST_MODULE 0x120
#define SST_TYPES 0x121
#define SST_PUBLIC 0x122
#define SST_PUBLIC_SYM 0x123
#define SST_SYMBOLS 0x124
#define SST_ALIGN_SYM 0x125
#define SST_STATIC_SYM 0x134
#define SST_SRC_LN_SEG 0x126
#define SST_SRC_MODULE 0x127
#define SST_LIBRARIES 0x128
#define SST_GLOBAL_SYM 0x129
#define SST_GLOBAL_PUB 0x12a
#define SST_GLOBAL_TYPES 0x12b
#define SST_MPC 0x12c

#define DEFAULT_NAME_SIZE 256

// Structures of CodeView records.

typedef struct _CV_HEADER {

 // The only fields common to all codeview records.

 WORD          Length; // The length of the codeview record.
 WORD          Index;  // One of the above values.

} CV_HEADER;


typedef struct _CV_COMPILE_FLAG {

 CV_HEADER     Header;
 BYTE          Machine;
 BYTE          Flags[3];
 BYTE          VersionNameLength;
 char          Version[DEFAULT_NAME_SIZE];

} CV_COMPILE_FLAG;

typedef struct _CV_REGISTER {

 CV_HEADER     Header;
 WORD          Type;
 WORD          Register;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_REGISTER;

typedef struct _CV_REGISTER_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 WORD          Register;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_REGISTER_NEW;

typedef struct _CV_CONSTANT {

 CV_HEADER     Header;
 WORD          Type;

} CV_CONSTANT;

typedef struct _CV_CONSTANT_NEW {

 CV_HEADER     Header;
 DWORD         Type;

} CV_CONSTANT_NEW;

typedef struct _CV_UDT {

 CV_HEADER     Header;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_UDT;

typedef struct _CV_UDT_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_UDT_NEW;

typedef struct _CV_SSEARCH {

 CV_HEADER     Header;
 DWORD         SymOff;
 WORD          Segment;

} CV_SSEARCH;

typedef struct _CV_OBJECT_FILENAME {

 CV_HEADER     Header;
 DWORD         Signature;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_OBJECT_FILENAME;

typedef struct _CV_COBOLUDT {

 CV_HEADER     Header;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_COBOLUDT;

typedef struct _CV_COBOLUDT_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_COBOLUDT_NEW;

typedef struct _CV_MANYREG {

 CV_HEADER     Header;
 WORD          Type;
 BYTE          Count;
 BYTE          Data[DEFAULT_NAME_SIZE];

} CV_MANYREG;

typedef struct _CV_MANYREG_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 BYTE          Count;
 BYTE          Data[DEFAULT_NAME_SIZE];

} CV_MANYREG_NEW;

typedef struct _CV_RETURN {

 CV_HEADER     Header;
 WORD          Flags;
 BYTE          Style;
 BYTE          Data[DEFAULT_NAME_SIZE];

} CV_RETURN;

typedef struct _CV_BPREL16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_BPREL16;

typedef struct _CV_LDATA16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LDATA16;

typedef struct _CV_GDATA16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GDATA16;

typedef struct _CV_PUB16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_PUB16;

typedef struct _CV_LPROC16 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 WORD          ProcLength;
 WORD          DebugStart;
 WORD          DebugEnd;
 WORD          Offset;
 WORD          Segment;
 WORD          ProcType;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LPROC16;

typedef struct _CV_GPROC16 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 WORD          ProcLength;
 WORD          DebugStart;
 WORD          DebugEnd;
 WORD          Offset;
 WORD          Segment;
 WORD          ProcType;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GPROC16;

typedef struct _CV_THUNK16 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 WORD          pNext;
 WORD          Offset;
 WORD          Segment;
 WORD          Length;
 BYTE          Ordinal;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_THUNK16;

typedef struct _CV_BLOCK16 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 WORD          Length;
 WORD          Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_BLOCK16;

typedef struct _CV_WITH16 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 WORD          Length;
 WORD          Offset;
 WORD          Segment;
 BYTE          StringLength;
 char          String[DEFAULT_NAME_SIZE];

} CV_WITH16;

typedef struct _CV_LABEL16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Segment;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LABEL16;

typedef struct _CV_CEXMODEL16 {

 CV_HEADER      Header;
 WORD           Offset;
 WORD           Segment;
 WORD           Model;
 BYTE           DataLength;
 BYTE           Data[DEFAULT_NAME_SIZE];

} CV_CEXMODEL16;

typedef struct _CV_VFTPATH16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Segment;
 WORD          Root;
 WORD          Path;

} CV_VFTPATH16;

typedef struct _CV_REGREL16 {

 CV_HEADER     Header;
 WORD          Offset;
 WORD          Register;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_REGREL16;

typedef struct _CV_BPREL32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_BPREL32;

typedef struct _CV_LDATA32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LDATA32;

typedef struct _CV_GDATA32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GDATA32;

typedef struct _CV_PUB32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_PUB32;

typedef struct _CV_LPROC32 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 DWORD         ProcLength;
 DWORD         DebugStart;
 DWORD         DebugEnd;
 DWORD         Offset;
 WORD          Segment;
 WORD          ProcType;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LPROC32;

typedef struct _CV_GPROC32 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 DWORD         ProcLength;
 DWORD         DebugStart;
 DWORD         DebugEnd;
 DWORD         Offset;
 WORD          Segment;
 WORD          ProcType;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GPROC32;

typedef struct _CV_BPREL32_NEW {

 CV_HEADER     Header;
 DWORD         Offset;
 DWORD         Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_BPREL32_NEW;

typedef struct _CV_LDATA32_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LDATA32_NEW;

typedef struct _CV_GDATA32_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GDATA32_NEW;

typedef struct _CV_PUB32_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_PUB32_NEW;

typedef struct _CV_LPROC32_NEW {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 DWORD         ProcLength;
 DWORD         DebugStart;
 DWORD         DebugEnd;
 DWORD         ProcType;
 DWORD         Offset;
 WORD          Segment;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LPROC32_NEW;

typedef struct _CV_GPROC32_NEW {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 DWORD         ProcLength;
 DWORD         DebugStart;
 DWORD         DebugEnd;
 DWORD         ProcType;
 DWORD         Offset;
 WORD          Segment;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GPROC32_NEW;

typedef struct _CV_THUNK32 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         pNext;
 DWORD         Offset;
 WORD          Segment;
 WORD          ThunkLength;
 BYTE          Ordinal;
 BYTE          Data[DEFAULT_NAME_SIZE];

} CV_THUNK32;

typedef struct _CV_BLOCK32 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         Length;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_BLOCK32;

typedef struct _CV_WITH32 {

 CV_HEADER     Header;
 DWORD         pParent;
 DWORD         pEnd;
 DWORD         Length;
 DWORD         Offset;
 WORD          Segment;
 BYTE          StringLength;
 char          String[DEFAULT_NAME_SIZE];

} CV_WITH32;

typedef struct _CV_LABEL32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 BYTE          Flags;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LABEL32;

typedef struct _CV_CEXMODEL32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Model;
 BYTE          Variant[DEFAULT_NAME_SIZE];

} CV_CEXMODEL32;

typedef struct _CV_VFTPATH32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Root;
 WORD          Path;

} CV_VFTPATH32;

typedef struct _CV_VFTPATH32_NEW {

 CV_HEADER     Header;
 DWORD         Root;
 DWORD         Path;
 DWORD         Offset;
 WORD          Segment;

} CV_VFTPATH32_NEW;

typedef struct _CV_REGREL32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Register;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_REGREL32;

typedef struct _CV_REGREL32_NEW {

 CV_HEADER     Header;
 DWORD         Offset;
 DWORD         Type;
 WORD          Register;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_REGREL32_NEW;

typedef struct _CV_LTHREAD32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LTHREAD32;

typedef struct _CV_GTHREAD32 {

 CV_HEADER     Header;
 DWORD         Offset;
 WORD          Segment;
 WORD          Type;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GTHREAD32;

typedef struct _CV_LTHREAD32_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_LTHREAD32_NEW;

typedef struct _CV_GTHREAD32_NEW {

 CV_HEADER     Header;
 DWORD         Type;
 DWORD         Offset;
 WORD          Segment;
 BYTE          NameLength;
 char          Name[DEFAULT_NAME_SIZE];

} CV_GTHREAD32_NEW;

// Missing out MIPS ones for now. Surely they aren't useful!

typedef struct _CV_PROCREF {

 CV_HEADER     Header;
 DWORD         Checksum;
 DWORD         Offset;
 WORD          Module;

} CV_PROCREF;

typedef struct _CV_DATAREF {

 CV_HEADER     Header;
 DWORD         Checksum;
 DWORD         Offset;
 WORD          Module;

} CV_DATAREF;

typedef struct _BASE_POINTER {

 char          DebugSignature[4];
 DWORD         lfoBase;

} BASE_POINTER;

typedef struct _HASH_INFO_RECORD {

 WORD          symhash;
 WORD          addrhash;
 DWORD         cbSymbol;
 DWORD         cbSymHash;
 DWORD         cbAddrHash;

} HASH_INFO_RECORD;




