/* ********************************************************************** */
/* ** cv-types.h                                                       ** */
/* ** Helpful structs and constants for CodeView type info.            ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* ********************************************************************** */


// Basic data about primitive types.

#define PRIMITIVE_TYPE_INDEX_SIZE_MASK 0x0007
#define PRIMITIVE_TYPE_INDEX_TYPE_MASK 0x00f0
#define PRIMITIVE_TYPE_INDEX_MODE_MASK 0x0700

#define SHIFT_FOR_SIZE 0
#define SHIFT_FOR_TYPE 4
#define SHIFT_FOR_MODE 8

#define PRIM_TYPE_SPECIAL 0x00
#define PRIM_TYPE_SIGNED_INTEGRAL 0x01
#define PRIM_TYPE_UNSIGNED_INTEGRAL 0x02
#define PRIM_TYPE_BOOLEAN 0x03
#define PRIM_TYPE_REAL 0x04
#define PRIM_TYPE_COMPLEX 0x05
#define PRIM_TYPE_SPECIAL2 0x06
#define PRIM_TYPE_REALLY_INT 0x07

// Sizes for PRIM_TYPE_SPECIAL

#define PRIM_SIZE_NOTYPE 0x00
#define PRIM_SIZE_ABSOLUTE_SYM 0x01
#define PRIM_SIZE_SEGMENT 0x02
#define PRIM_SIZE_VOID 0x03
#define PRIM_SIZE_BASIC_8_BYTE 0x04
#define PRIM_SIZE_NEAR_BASIC_STRING 0x05
#define PRIM_SIZE_FAR_BASIC_STRING 0x06
#define PRIM_SIZE_NOTTRANS 0x07

// Sizes for PRIM_TYPE_SIGNED_INTEGRAL
//           PRIM_TYPE_UNSIGNED_INTEGRAL
//           PRIM_TYPE_BOOLEAN

#define PRIM_SIZE_ONE_BYTE 0x00
#define PRIM_SIZE_TWO_BYTE 0x01
#define PRIM_SIZE_FOUR_BYTE 0x02
#define PRIM_SIZE_EIGHT_BYTE 0x03

// Sizes for PRIM_TYPE_REAL
//           PRIM_TYPE_COMPLEX

#define PRIM_SIZE_32_BIT 0x00
#define PRIM_SIZE_64_BIT 0x01
#define PRIM_SIZE_80_BIT 0x02
#define PRIM_SIZE_128_BIT 0x03
#define PRIM_SIZE_48_BIT 0x04

// Sizes for PRIM_TYPE_SPECIAL_2

#define PRIM_SIZE_BIT 0x00
#define PRIM_SIZE_PASCAL_CHAR 0x01

// Sizes for PRIM_TYPE_REALLY_INT

#define PRIM_SIZE_CHAR 0x00
#define PRIM_SIZE_WIDE_CHAR 0x01
#define PRIM_SIZE_TWO_BYTE_SIGNED_INT 0x02
#define PRIM_SIZE_TWO_BYTE_UNSIGNED_INT 0x03
#define PRIM_SIZE_FOUR_BYTE_SIGNED_INT 0x04
#define PRIM_SIZE_FOUR_BYTE_UNSIGNED_INT 0x05
#define PRIM_SIZE_EIGHT_BYTE_SIGNED_INT 0x06
#define PRIM_SIZE_EIGHT_BYTE_UNSIGNED_INT 0x07

// Modes

#define PRIM_MODE_DIRECT 0x00
#define PRIM_MODE_NEAR_POINTER 0x01
#define PRIM_MODE_FAR_POINTER 0x02
#define PRIM_MODE_HUGE_POINTER 0x03
#define PRIM_MODE_32_BIT_NEAR_POINTER 0x04
#define PRIM_MODE_32_BIT_FAR_POINTER 0x05
#define PRIM_MODE_64_BIT_NEAR_POINTER 0x06

// Leaf Codes

#define CVT_LF_MODIFIER 0x001
#define CVT_LF_POINTER 0x002
#define CVT_LF_ARRAY 0x003
#define CVT_LF_CLASS 0x004
#define CVT_LF_STRUCTURE 0x005
#define CVT_LF_UNION 0x006
#define CVT_LF_ENUM 0x007
#define CVT_LF_PROCEDURE 0x008
#define CVT_LF_MFUNCTION 0x009
#define CVT_LF_VTSHAPE 0x00a
#define CVT_LF_COBOL0 0x00b
#define CVT_LF_COBOL1 0x00c
#define CVT_LF_BARRAY 0x00d
#define CVT_LF_LABEL 0x00e
#define CVT_LF_NULL 0x00f
#define CVT_LF_NOTTRAN 0x010
#define CVT_LF_DIMARRAY 0x011
#define CVT_LF_VFTPATH 0x012
#define CVT_LF_PRECOMP 0x013
#define CVT_LF_ENDPRECOMP 0x014
#define CVT_LF_OEM 0x015
#define CVT_LF_TYPESERVER 0x016

#define CVT_LF_SKIP 0x0200
#define CVT_LF_ARGLIST 0x0201
#define CVT_LF_DEFARG 0x0202
#define CVT_LF_LIST 0x0203
#define CVT_LF_FIELDLIST 0x0204
#define CVT_LF_DERIVED 0x0205
#define CVT_LF_BITFIELD 0x0206
#define CVT_LF_METHODLIST 0x0207
#define CVT_LF_DIMCONU 0x0208
#define CVT_LF_DIMCONLU 0x0209
#define CVT_LF_DIMVARU 0x020a
#define CVT_LF_DIMVARLU 0x020b
#define CVT_LF_REFSYM 0x020c

#define CVT_LF_BCLASS 0x0400
#define CVT_LF_VBCLASS 0x0401
#define CVT_LF_IVBCLASS 0x0402
#define CVT_LF_ENUMERATE 0x0403
#define CVT_LF_FRIENDFCN 0x0404
#define CVT_LF_INDEX 0x0405
#define CVT_LF_MEMBER 0x0406
#define CVT_LF_STMEMBER 0x0407
#define CVT_LF_METHOD 0x0408
#define CVT_LF_NESTTYPE 0x0409
#define CVT_LF_VFUNCTAB 0x040a
#define CVT_LF_FRIENDCLS 0x040b
#define CVT_LF_ONEMETHOD 0x040c
#define CVT_LF_VFUNCOFF 0x040d

#define CVT_LF_NUMERIC 0x8000
#define CVT_LF_CHAR 0x8000
#define CVT_LF_SHORT 0x8001
#define CVT_LF_USHORT 0x8002
#define CVT_LF_LONG 0x8003
#define CVT_LF_ULONG 0x8004
#define CVT_LF_REAL32 0x8005
#define CVT_LF_REAL64 0x8006
#define CVT_LF_REAL80 0x8007
#define CVT_LF_REAL128 0x8008
#define CVT_LF_QUADWORD 0x8009
#define CVT_LF_UQUADWORD 0x800a
#define CVT_LF_REAL48 0x800b
#define CVT_LF_COMPLEX32 0x800c
#define CVT_LF_COMPLEX64 0x800d
#define CVT_LF_COMPLEX80 0x800e
#define CVT_LF_COMPLEX128 0x800f
#define CVT_LF_VARSTRING 0x8010

#define CVT_LF_PAD0 0xf0
#define CVT_LF_PAD1 0xf1
#define CVT_LF_PAD2 0xf2
#define CVT_LF_PAD3 0xf3
#define CVT_LF_PAD4 0xf4
#define CVT_LF_PAD5 0xf5
#define CVT_LF_PAD6 0xf6
#define CVT_LF_PAD7 0xf7
#define CVT_LF_PAD8 0xf8
#define CVT_LF_PAD9 0xf9
#define CVT_LF_PAD10 0xfa
#define CVT_LF_PAD11 0xfb
#define CVT_LF_PAD12 0xfc
#define CVT_LF_PAD13 0xfd
#define CVT_LF_PAD14 0xfe
#define CVT_LF_PAD15 0xff

// Structures for type records in the $$TYPES table.

typedef struct _CV_GLOBAL_TYPE_TABLE { // The table itself

 BYTE        Reserved1;
 BYTE        Reserved2;
 BYTE        Reserved3;
 BYTE        Signature;
 DWORD       NumberOfTypeStrings;
 DWORD       TypeStringOffset[256]; // Could be more.

} CV_GLOBAL_TYPE_TABLE;

typedef struct _CV_TYPE_MODIFIER {

 WORD        LfModifier;
 WORD        Attribute;
 WORD        Index;

} CV_TYPE_MODIFIER;

typedef struct _CV_TYPE_POINTER {
 
 WORD        LfPointer;
 WORD        Attribute;
 WORD        Index;
 BYTE        Variant;

} CV_TYPE_POINTER;

typedef struct _CV_TYPE_SIMPLE_ARRAY {

 WORD        LfArray;
 WORD        ElemTypeIndex;
 WORD        IndxTypeIndex;
 BYTE        LfLengthAndName;

} CV_TYPE_SIMPLE_ARRAY;

typedef struct _CV_TYPE_CLASS {

 WORD        LfClass;
 WORD        ElementCount;
 WORD        FieldListIndex;
 WORD        Property;
 WORD        DerivationListIndex;
 WORD        VirtualFunctionTableIndex;
 BYTE        LfLengthAndName;

} CV_TYPE_CLASS, CV_TYPE_STRUCT;

typedef struct _CV_TYPE_UNION {

 WORD        LfUnion;
 WORD        MemberCount;
 WORD        FieldListIndex;
 WORD        Property;
 BYTE        LfLengthAndName;

} CV_TYPE_UNION;

typedef struct _CV_TYPE_ENUMERATION {

 WORD        LfEnum;
 WORD        Count;
 WORD        UnderlyingTypeIndex;
 WORD        EnumListIndex;
 WORD        Property;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_ENUMERATION;

typedef struct _CV_TYPE_PROCEDURE {

 WORD        LfProcedure;
 WORD        ReturnValueTypeIndex;
 BYTE        CallingConvention;
 BYTE        Reserved;
 WORD        ParameterCount;
 WORD        ParameterListIndex;

} CV_TYPE_PROCEDURE;

typedef struct _CV_TYPE_MEMBER_FUNCTION {

 WORD        LfMFunction;
 WORD        ReturnValueTypeIndex;
 WORD        OwnerClassIndex;
 WORD        ThisIndex;
 BYTE        CallingConvention;
 BYTE        Reserved;
 WORD        ParameterCount;
 WORD        PararameterListIndex;
 DWORD       ThisAdjustor;

} CV_TYPE_MEMBER_FUNCTION;

typedef struct _CV_TYPE_VIRTUAL_FUNCTION_TABLE_SHAPE {

 WORD        LfVTShape;
 WORD        DescriptorCount;
 BYTE        FirstDescriptor;

} CV_TYPE_VIRTUAL_FUNCTION_TABLE_SHAPE;

typedef struct _CV_TYPE_COBOL_0 {

 WORD        LfCobol0;
 WORD        ParentIndex;
 BYTE        Data;

} CV_TYPE_COBOL_0;

typedef struct _CV_TYPE_COBOL_1 {

 WORD        LfCobol1;
 BYTE        Data;

} CV_TYPE_COBOL_1;

typedef struct _CV_TYPE_BASIC_ARRAY {

 WORD        LfBArray;
 WORD        ElementTypeIndex;

} CV_TYPE_BASIC_ARRAY;

typedef struct _CV_TYPE_LABEL {

 WORD        LfLabel;
 WORD        AddressingMode;

} CV_TYPE_LABEL;

typedef struct _CV_TYPE_NULL {

 WORD        LfNull;

} CV_TYPE_NULL;

typedef struct _CV_TYPE_NOT_TRANSLATED {

 WORD        LfNotTrans;

} CV_TYPE_NOT_TRANSLATED;

typedef struct _CV_TYPE_MULTI_DIM_ARRAY {

 WORD        LfDimArray;
 WORD        UnderlyingTypeIndex;
 WORD        DimensionInfoIndex;
 BYTE        NameLength;
 char        *Name;

} CV_TYPE_MULTI_DIM_ARRAY;

typedef struct _CV_TYPE_PATH_TO_VFUNC {

 WORD        LfVFTPath;
 WORD        Count;
 WORD        TypeIndices[256];
 
} CV_TYPE_PATH_TO_VFUNC;

typedef struct _CV_TYPE_REF_PRECOMPILED {

 WORD        LfPrecomp;
 WORD        StartIndex;
 WORD        Count;
 DWORD       Signature;
 BYTE        ModuleNameLength;
 char        ModuleName[256];

} CV_TYPE_REF_PRECOMPILED;

typedef struct _CV_TYPE_END_PRECOMPILED {

 WORD        LfEndPrecomp;
 DWORD       Signature;

} CV_TYPE_END_PRECOMPILED;

typedef struct _CV_TYPE_OEM_GENERIC {

 WORD        LfOEM;
 WORD        OEMIdentifier;
 WORD        OEMRecordIdentifier;
 WORD        TypeIndexCount;
 WORD        TypeIndicesAndRest;

} CV_TYPE_OEM_GENERIC;

typedef struct _CV_TYPE_SKIP {

 WORD        LfSkip;
 WORD        NextValidIndex;
 BYTE        Pad;

} CV_TYPE_SKIP;

typedef struct _CV_TYPE_ARGUMENT_LIST {

 WORD        LfArgList;
 WORD        ArgumentCount;
 WORD        ArgumentTypeIndex[256]; // There could be more, but we can
                                     // deal with it as a worse-case scenario
} CV_TYPE_ARGUMENT_LIST;

typedef struct _CV_TYPE_DEFAULT_ARGUMENT {

 WORD        LfDefArg;
 WORD        DefaultTypeIndex;
 BYTE        StringLength;
 char        String[256];

} CV_TYPE_DEFAULT_ARGUMENT;

typedef struct _CV_TYPE_ARBITRARY_LIST {

 WORD        LfList;
 BYTE        DataEtc;

} CV_TYPE_ARBITRARY_LIST;

typedef struct _CV_TYPE_FIELD_LIST {

 WORD        LfFieldList;
 BYTE        DataEtc;

} CV_TYPE_FIELD_LIST;  // Not quite sure about this one!!

typedef struct _CV_TYPE_DERIVED_CLASSES {

 WORD        LfDerived;
 WORD        Count;
 WORD        TypeIndex[256]; // There could be more...

} CV_TYPE_DERIVED_CLASSES;

typedef struct _CV_TYPE_BIT_FIELDS {

 WORD        LfBitField;
 BYTE        Length;
 BYTE        Position;
 WORD        TypeIndex;

} CV_TYPE_BIT_FIELDS;

typedef struct _CV_T_METHOD {

 WORD        Attribute;
 WORD        TypeIndex;
 DWORD       VirtualTableOffset;

} CV_T_METHOD;

typedef struct _CV_TYPE_METHOD_LIST {

 WORD        LfMList;
 CV_T_METHOD Method[256]; // Sigh...there could be more...

} CV_TYPE_METHOD_LIST;

typedef struct _CV_TYPE_DIM_ARRAY_CONST_UPPER {

 WORD        LfDimConU;
 WORD        NumberOfDimensions;
 WORD        IndxTypeIndex;
 BYTE        BoundsData;

} CV_TYPE_DIM_ARRAY_CONST_UPPER;

typedef struct _CV_TYPE_DIM_ARRAY_CONST_UPPER_LOWER {

 WORD        LfDimConLU;
 WORD        NumberOfDimensions;
 WORD        IndxTypeIndex;
 BYTE        BoundsData;

} CV_TYPE_DIM_ARRAY_CONST_UPPER_LOWER;

typedef struct _CV_TYPE_DIM_ARRAY_VAR_UPPER {

 WORD        LfDimVarU;
 WORD        NumberOfDimensions;
 WORD        IndxTypeIndex;
 WORD        RefSymIndex[256]; // Could be more...
 
} CV_TYPE_DIM_ARRAY_VAR_UPPER;

typedef struct _CV_TYPE_DIM_ARRAY_VAR_UPPER_LOWER {

 WORD        LfDimVarLU;
 WORD        NumberOfDimensions;
 WORD        IndxTypeIndex;
 WORD        RefSymIndex[256]; // Could be more...

} CV_TYPE_DIM_ARRAY_VAR_UPPER_LOWER;

typedef struct _CV_TYPE_REF_SYM {

 WORD        LfRefSym;
 WORD        Length;
 WORD        Index;
 BYTE        SymbolDataEtc;

} CV_TYPE_REF_SYM;

typedef struct _CV_TYPE_REAL_BASE_CLASS {

 WORD        LfBClass;
 WORD        ClassRecordIndex;
 WORD        Attribute;
 WORD        LfOffset;

} CV_TYPE_REAL_BASE_CLASS;

typedef struct _CV_TYPE_DIRECT_VIRTUAL_BASE_CLASS {

 WORD        LfVBClass;
 WORD        BaseClassIndex;
 WORD        BasePointerTypeIndex;
 WORD        Attribute;
 WORD        LfFirst;

} CV_TYPE_DIRECT_VIRTUAL_BASE_CLASS;

typedef struct _CV_TYPE_INDIRECT_VIRTUAL_BASE_CLASS {

 WORD        LfIVBClass;
 WORD        BaseClassIndex;
 WORD        BasePointerTypeIndex;
 WORD        Attribute;
 WORD        LfFirst;

} CV_TYPE_INDIRECT_VIRTUAL_BASE_CLASS;

typedef struct _CV_TYPE_ENUMERATE {

 WORD        LfEnumerate;
 WORD        Attribute;
 WORD        ValueAndName;

} CV_TYPE_ENUMERATE;

typedef struct _CV_TYPE_FRIEND_FUNCTION {

 WORD        LfFriendFcn;
 WORD        FunctionTypeIndex;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_FRIEND_FUNCTION;

typedef struct _CV_TYPE_RECORD_INDEX {

 WORD        LfIndex;
 WORD        Index;

} CV_TYPE_RECORD_INDEX;

typedef struct _CV_TYPE_DATA_MEMBER {

 WORD        LfMember;
 WORD        MemberTypeIndex;
 WORD        Attribute;
 WORD        OffsetAndName;

} CV_TYPE_DATA_MEMBER;

typedef struct _CV_TYPE_STATIC_DATA_MEMBER {

 WORD        LfStMember;
 WORD        MemberTypeIndex;
 WORD        Attribute;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_STATIC_DATA_MEMBER;

typedef struct _CV_TYPE_METHOD {

 WORD        LfMethod;
 WORD        OverloadCount;
 WORD        MethodListIndex;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_METHOD;

typedef struct _CV_TYPE_NESTED_TYPE {

 WORD        LfNestedType;
 WORD        NestedTypeIndex;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_NESTED_TYPE;

typedef struct _CV_TYPE_VFT_POINTER {

 WORD        LfVFuncTab;
 WORD        TypeIndex;

} CV_TYPE_VFT_POINTER;

typedef struct _CV_TYPE_FRIEND_CLASS {

 WORD        LfFriendCls;
 WORD        ClassTypeIndex;

} CV_TYPE_FRIEND_CLASS;

typedef struct _CV_TYPE_ONE_METHOD {

 WORD        LfOneMethod;
 WORD        Attribute;
 WORD        MethodTypeIndex;
 DWORD       VirtualBaseOffset;
 BYTE        NameLength;
 char        Name[256];

} CV_TYPE_ONE_METHOD;

typedef struct _CV_TYPE_VIRTUAL_FUNCTION_OFFSET {

 WORD        LfVFuncOff;
 WORD        VFTIndex;
 DWORD       Offset;

} CV_TYPE_VIRTUAL_FUNCTION_OFFSET;


// The numeric leaf.

typedef union _VARSTRING {

 WORD                Length;
 char                StringData[256];

} VARSTRING;

typedef union _CV_TYPE_NUMERIC_LEAF_DATA {

 char                lfChar;
 short               lfShort;
 unsigned short      lfUnsignedShort;
 long                lfLong;
 unsigned long       lfUnsignedLong;
 float               lfReal32;
 double              lfReal64;

 // Those were easy enough to model. Some of the following are
 // a bit dodgy, but the right size.

 BYTE                lfReal80[10];
 BYTE                lfReal128[16];
 DWORD               lfQuadWord[2];
 DWORD               lfUnsignedQuadword[2];
 BYTE                lfReal48[6];
 float               lfComplex32[2];
 double              lfComplex64[2];
 BYTE                lfComplex80[20];
 BYTE                lfComplex128[32];
 VARSTRING           lfVarString;

} CV_TYPE_NUMERIC_LEAF_DATA;

typedef struct _CV_TYPE_NUMERIC_LEAF {

 WORD                      lfIndex;
 CV_TYPE_NUMERIC_LEAF_DATA u;

} CV_TYPE_NUMERIC_LEAF;

