/* ********************************************************************** */
/* ** describe.c                                                       ** */
/* ** Stuff for giving printed representations of debug info.          ** */
/* ** THIS FILE IS FOR TESTING ONLY AND NOT PART OF THE DEBUGGER NUB   ** */
/* ** ---------------------------------------------------------------- ** */
/* ** Author: Paul Howard, Copyright: Functional Objects, Inc. 1996 ** */
/* ********************************************************************** */

#include "nub-core.h"

void print_length_prefixed_string (BYTE length, char *string);

void print_length_prefixed_string (BYTE length, char *string)
{
 BYTE i;

 for (i = 0; i < length; i++) printf("%c", string[i]);
}

void print_cv_symbol_record (CV_HEADER *header)
{

 if (header == NULL) {
  printf ("<NO RECORD>\n");
  return;
 }

 switch (header->Index) {

 case CV_S_COMPILE:
  {
   CV_COMPILE_FLAG *compile_flag = (CV_COMPILE_FLAG*) header;
   printf("CV_S_COMPILE (Compile flag) ");
   printf ("Machine: %x  Version: \n", compile_flag->Machine);
   printf ("Flags[0] = 0x%x\n", compile_flag->Flags[0]);
   printf ("Flags[1] = 0x%x\n", compile_flag->Flags[1]);
   printf ("Flags[2] = 0x%x\n", compile_flag->Flags[2]);
   print_length_prefixed_string (compile_flag->VersionNameLength,
                              compile_flag->Version);
  }
  break;

 case CV_S_REGISTER:
  {
   CV_REGISTER *reg = (CV_REGISTER*) header;
   printf ("CV_S_REGISTER (Enregistered symbol) ");
   printf ("Type: %x  Register: %x  Name: ", reg->Type, reg->Register);
   print_length_prefixed_string (reg->NameLength, reg->Name);
  }
  break;

 case CV_S_CONSTANT:
  {
   CV_CONSTANT *constant = (CV_CONSTANT*) header;
   printf ("CV_S_CONSTANT (Constant) ");
   printf ("Type: %x", constant->Type);
  }
  break;

 case CV_S_UDT:
  {
   CV_UDT *udt = (CV_UDT*) header;
   printf ("CV_S_UDT (User defined type) ");
   printf ("Type: %x  Name: ", udt->Type);
   print_length_prefixed_string (udt->NameLength, udt->Name);
  }
  break;

 case CV_S_SSEARCH:
  {
   CV_SSEARCH *ssearch = (CV_SSEARCH*) header;
   printf ("CV_S_SSEARCH (Start Search) ");
   printf ("Sym Offset: %x  Segment: %x ", ssearch->SymOff, ssearch->Segment);
  }
  break;

 case CV_S_END:
  {
   printf ("CV_S_END (End of scope)");
  }
  break;

 case CV_S_SKIP:
  {
   printf ("CV_S_SKIP (Skip record)");
  }
  break;

 case CV_S_OBJNAME:
  {
   CV_OBJECT_FILENAME *fname = (CV_OBJECT_FILENAME*) header;
   printf ("CV_S_OBJNAME (Object file name) ");
   printf ("Signature: %x  Name: ", fname->Signature);
   print_length_prefixed_string (fname->NameLength, fname->Name);
  }
  break;

 case CV_S_BPREL16:
  {
   CV_BPREL16 *rec = (CV_BPREL16*) header;
   printf ("CV_S_BPREL16 (BP Relative 16:16) ");
   printf ("Frame offset: %d  Type: %x  Name: ", rec->Offset, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_LDATA16:
  {
   CV_LDATA16 *rec = (CV_LDATA16*) header;
   printf ("CV_S_LDATA16 (Local data 16:16) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_GDATA16:
  {
   CV_GDATA16 *rec = (CV_GDATA16*) header;
   printf ("CV_S_GDATA16 (Global data 16:16) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_PUB16:
  {
   CV_PUB16 *rec = (CV_PUB16*) header;
   printf ("CV_S_PUB16 (Public symbol 16:16) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_LPROC16:
  {
   CV_LPROC16 *rec = (CV_LPROC16*) header;
   printf ("CV_S_LPROC16 (Local procedure start 16:16) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->ProcType);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_GPROC16:
  {
   CV_GPROC16 *rec = (CV_GPROC16*) header;
   printf ("CV_S_GPROC16 (Global procedure start 16:16) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->ProcType);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_BPREL32:
  {
   CV_BPREL32 *rec = (CV_BPREL32*) header;
   printf ("CV_S_BPREL32 (BP Relative 16:32) ");
   printf ("Frame offset: %d  Type: %x  Name: ", rec->Offset, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_LDATA32:
  {
   CV_LDATA32 *rec = (CV_LDATA32*) header;
   printf ("CV_S_LDATA32 (Local data 16:32) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_GDATA32:
  {
   CV_GDATA32 *rec = (CV_GDATA32*) header;
   printf ("CV_S_GDATA32 (Global data 16:32) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_PUB32:
  {
   CV_PUB32 *rec = (CV_PUB32*) header;
   printf ("CV_S_PUB32 (Public symbol 16:32) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->Type);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_LPROC32:
  {
   CV_LPROC32 *rec = (CV_LPROC32*) header;
   printf ("CV_S_LPROC32 (Local procedure start 16:32) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->ProcType);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 case CV_S_GPROC32:
  {
   CV_GPROC32 *rec = (CV_GPROC32*) header;
   printf ("CV_S_GPROC32 (Global procedure start 16:32) ");
   printf ("Offset: %x  Segment:  %x  Type: %x  Name: ",
        rec->Offset, rec->Segment, rec->ProcType);
   print_length_prefixed_string (rec->NameLength, rec->Name);
  }
  break;

 default:

  printf ("CV_UNKOWN (Don't know or don't care)");
 }

 printf ("\n");
}

