#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_LEN(x)            (sizeof(x)/sizeof((x)[0]))

#ifdef HAVE_LIBUNWIND_H
#define UNW_LOCAL_ONLY
#include <libunwind.h>

#include "demangle.h"

// The dispatch engine is not interesting
static int interesting_iep(const char *demangled)
{
  const char suffix[] = ":dispatch-engine:dylan";
  size_t suffix_len = strlen(suffix);
  size_t demangled_len = strlen(demangled);
  return
    demangled_len < suffix_len
      || strcmp(demangled + demangled_len - suffix_len, suffix) != 0;
}

// Keep sorted!
const char * const uninteresting[] = {
  "apply_mep_1",
  "apply_mep_10",
  "apply_mep_11",
  "apply_mep_12",
  "apply_mep_13",
  "apply_mep_14",
  "apply_mep_15",
  "apply_mep_16",
  "apply_mep_17",
  "apply_mep_18",
  "apply_mep_19",
  "apply_mep_2",
  "apply_mep_20",
  "apply_mep_3",
  "apply_mep_4",
  "apply_mep_5",
  "apply_mep_6",
  "apply_mep_7",
  "apply_mep_8",
  "apply_mep_9",
  "apply_xep_1",
  "apply_xep_10",
  "apply_xep_11",
  "apply_xep_12",
  "apply_xep_13",
  "apply_xep_14",
  "apply_xep_15",
  "apply_xep_16",
  "apply_xep_17",
  "apply_xep_18",
  "apply_xep_19",
  "apply_xep_2",
  "apply_xep_20",
  "apply_xep_3",
  "apply_xep_4",
  "apply_xep_5",
  "apply_xep_6",
  "apply_xep_7",
  "apply_xep_8",
  "apply_xep_9",
  "discriminate_engine_1_1",
  "discriminate_engine_1_2",
  "discriminate_engine_1_3",
  "discriminate_engine_1_4",
  "discriminate_engine_1_5",
  "discriminate_engine_1_6",
  "discriminate_engine_1_7",
  "discriminate_engine_2_2",
  "discriminate_engine_2_3",
  "discriminate_engine_2_4",
  "discriminate_engine_2_5",
  "discriminate_engine_2_6",
  "discriminate_engine_2_7",
  "discriminate_engine_3_3",
  "discriminate_engine_3_4",
  "discriminate_engine_3_5",
  "discriminate_engine_3_6",
  "discriminate_engine_3_7",
  "discriminate_engine_4_4",
  "discriminate_engine_4_5",
  "discriminate_engine_4_6",
  "discriminate_engine_4_7",
  "discriminate_engine_5_5",
  "discriminate_engine_5_6",
  "discriminate_engine_5_7",
  "discriminate_engine_6_6",
  "discriminate_engine_6_7",
  "discriminate_engine_7_7",
  "discriminate_engine_n_n",
  "general_engine_node_n",
  "general_engine_node_n_engine",
  "general_engine_node_n_optionals",
  "general_engine_node_spread",
  "general_engine_node_spread_engine",
  "gf_iep_0",
  "gf_iep_1",
  "gf_iep_2",
  "gf_iep_3",
  "gf_iep_4",
  "gf_iep_5",
  "gf_iep_6",
  "gf_iep_7",
  "gf_optional_xep_0",
  "gf_optional_xep_1",
  "gf_optional_xep_10",
  "gf_optional_xep_11",
  "gf_optional_xep_12",
  "gf_optional_xep_13",
  "gf_optional_xep_14",
  "gf_optional_xep_15",
  "gf_optional_xep_16",
  "gf_optional_xep_17",
  "gf_optional_xep_18",
  "gf_optional_xep_19",
  "gf_optional_xep_2",
  "gf_optional_xep_20",
  "gf_optional_xep_3",
  "gf_optional_xep_4",
  "gf_optional_xep_5",
  "gf_optional_xep_6",
  "gf_optional_xep_7",
  "gf_optional_xep_8",
  "gf_optional_xep_9",
  "gf_xep_0",
  "gf_xep_1",
  "gf_xep_10",
  "gf_xep_11",
  "gf_xep_12",
  "gf_xep_13",
  "gf_xep_14",
  "gf_xep_15",
  "gf_xep_16",
  "gf_xep_17",
  "gf_xep_18",
  "gf_xep_19",
  "gf_xep_2",
  "gf_xep_20",
  "gf_xep_3",
  "gf_xep_4",
  "gf_xep_5",
  "gf_xep_6",
  "gf_xep_7",
  "gf_xep_8",
  "gf_xep_9",
  "iep_apply",
  "if_type_discriminator_engine_1_1",
  "if_type_discriminator_engine_1_2",
  "if_type_discriminator_engine_1_3",
  "if_type_discriminator_engine_1_4",
  "if_type_discriminator_engine_1_5",
  "if_type_discriminator_engine_1_6",
  "if_type_discriminator_engine_1_7",
  "if_type_discriminator_engine_2_2",
  "if_type_discriminator_engine_2_3",
  "if_type_discriminator_engine_2_4",
  "if_type_discriminator_engine_2_5",
  "if_type_discriminator_engine_2_6",
  "if_type_discriminator_engine_2_7",
  "if_type_discriminator_engine_3_3",
  "if_type_discriminator_engine_3_4",
  "if_type_discriminator_engine_3_5",
  "if_type_discriminator_engine_3_6",
  "if_type_discriminator_engine_3_7",
  "if_type_discriminator_engine_4_4",
  "if_type_discriminator_engine_4_5",
  "if_type_discriminator_engine_4_6",
  "if_type_discriminator_engine_4_7",
  "if_type_discriminator_engine_5_5",
  "if_type_discriminator_engine_5_6",
  "if_type_discriminator_engine_5_7",
  "if_type_discriminator_engine_6_6",
  "if_type_discriminator_engine_6_7",
  "if_type_discriminator_engine_7_7",
  "if_type_discriminator_engine_n_n",
  "implicit_keyed_single_method_1",
  "implicit_keyed_single_method_10",
  "implicit_keyed_single_method_11",
  "implicit_keyed_single_method_12",
  "implicit_keyed_single_method_13",
  "implicit_keyed_single_method_14",
  "implicit_keyed_single_method_15",
  "implicit_keyed_single_method_16",
  "implicit_keyed_single_method_17",
  "implicit_keyed_single_method_18",
  "implicit_keyed_single_method_19",
  "implicit_keyed_single_method_2",
  "implicit_keyed_single_method_20",
  "implicit_keyed_single_method_3",
  "implicit_keyed_single_method_4",
  "implicit_keyed_single_method_5",
  "implicit_keyed_single_method_6",
  "implicit_keyed_single_method_7",
  "implicit_keyed_single_method_8",
  "implicit_keyed_single_method_9",
  "implicit_keyed_single_method_engine_0",
  "implicit_keyed_single_method_engine_1",
  "implicit_keyed_single_method_engine_2",
  "implicit_keyed_single_method_engine_3",
  "implicit_keyed_single_method_engine_4",
  "implicit_keyed_single_method_engine_5",
  "implicit_keyed_single_method_engine_6",
  "implicit_keyed_single_method_engine_n",
  "key_mep",
  "key_mep_0",
  "key_mep_1",
  "key_mep_2",
  "key_mep_3",
  "key_mep_4",
  "key_mep_5",
  "key_mep_6",
  "key_mep_7",
  "key_mep_8",
  "key_mep_9",
  "monomorphic_discriminator_engine_1_1",
  "monomorphic_discriminator_engine_1_2",
  "monomorphic_discriminator_engine_1_3",
  "monomorphic_discriminator_engine_1_4",
  "monomorphic_discriminator_engine_1_5",
  "monomorphic_discriminator_engine_1_6",
  "monomorphic_discriminator_engine_1_7",
  "monomorphic_discriminator_engine_2_2",
  "monomorphic_discriminator_engine_2_3",
  "monomorphic_discriminator_engine_2_4",
  "monomorphic_discriminator_engine_2_5",
  "monomorphic_discriminator_engine_2_6",
  "monomorphic_discriminator_engine_2_7",
  "monomorphic_discriminator_engine_3_3",
  "monomorphic_discriminator_engine_3_4",
  "monomorphic_discriminator_engine_3_5",
  "monomorphic_discriminator_engine_3_6",
  "monomorphic_discriminator_engine_3_7",
  "monomorphic_discriminator_engine_4_4",
  "monomorphic_discriminator_engine_4_5",
  "monomorphic_discriminator_engine_4_6",
  "monomorphic_discriminator_engine_4_7",
  "monomorphic_discriminator_engine_5_5",
  "monomorphic_discriminator_engine_5_6",
  "monomorphic_discriminator_engine_5_7",
  "monomorphic_discriminator_engine_6_6",
  "monomorphic_discriminator_engine_6_7",
  "monomorphic_discriminator_engine_7_7",
  "monomorphic_discriminator_engine_n_n",
  "primitive_apply_spread",
  "primitive_apply_using_buffer",
  "primitive_engine_node_apply_with_optionals",
  "primitive_invoke_debugger",
  "primitive_mep_apply_with_optionals",
  "primitive_xep_apply",
  "rest_key_mep_1",
  "rest_key_mep_10",
  "rest_key_mep_11",
  "rest_key_mep_12",
  "rest_key_mep_13",
  "rest_key_mep_14",
  "rest_key_mep_15",
  "rest_key_mep_16",
  "rest_key_mep_17",
  "rest_key_mep_18",
  "rest_key_mep_19",
  "rest_key_mep_2",
  "rest_key_mep_20",
  "rest_key_mep_3",
  "rest_key_mep_4",
  "rest_key_mep_5",
  "rest_key_mep_6",
  "rest_key_mep_7",
  "rest_key_mep_8",
  "rest_key_mep_9",
  "rest_key_mep_n",
  "rest_key_xep",
  "rest_key_xep_0",
  "rest_key_xep_1",
  "rest_key_xep_10",
  "rest_key_xep_11",
  "rest_key_xep_12",
  "rest_key_xep_13",
  "rest_key_xep_14",
  "rest_key_xep_15",
  "rest_key_xep_16",
  "rest_key_xep_17",
  "rest_key_xep_18",
  "rest_key_xep_19",
  "rest_key_xep_2",
  "rest_key_xep_20",
  "rest_key_xep_3",
  "rest_key_xep_4",
  "rest_key_xep_5",
  "rest_key_xep_6",
  "rest_key_xep_7",
  "rest_key_xep_8",
  "rest_key_xep_9",
  "rest_key_xep_n",
  "rest_xep_0",
  "rest_xep_1",
  "rest_xep_10",
  "rest_xep_11",
  "rest_xep_12",
  "rest_xep_13",
  "rest_xep_14",
  "rest_xep_15",
  "rest_xep_16",
  "rest_xep_17",
  "rest_xep_18",
  "rest_xep_19",
  "rest_xep_2",
  "rest_xep_20",
  "rest_xep_3",
  "rest_xep_4",
  "rest_xep_5",
  "rest_xep_6",
  "rest_xep_7",
  "rest_xep_8",
  "rest_xep_9",
  "single_method_0",
  "single_method_1",
  "single_method_10",
  "single_method_11",
  "single_method_12",
  "single_method_13",
  "single_method_14",
  "single_method_15",
  "single_method_16",
  "single_method_17",
  "single_method_18",
  "single_method_19",
  "single_method_2",
  "single_method_20",
  "single_method_3",
  "single_method_4",
  "single_method_5",
  "single_method_6",
  "single_method_7",
  "single_method_8",
  "single_method_9",
  "unrestricted_keyed_single_method_engine_0",
  "unrestricted_keyed_single_method_engine_1",
  "unrestricted_keyed_single_method_engine_2",
  "unrestricted_keyed_single_method_engine_3",
  "unrestricted_keyed_single_method_engine_4",
  "unrestricted_keyed_single_method_engine_5",
  "unrestricted_keyed_single_method_engine_6",
  "unrestricted_keyed_single_method_engine_n",
  "xep_0",
  "xep_1",
  "xep_10",
  "xep_11",
  "xep_12",
  "xep_13",
  "xep_14",
  "xep_15",
  "xep_16",
  "xep_17",
  "xep_18",
  "xep_19",
  "xep_2",
  "xep_20",
  "xep_3",
  "xep_4",
  "xep_5",
  "xep_6",
  "xep_7",
  "xep_8",
  "xep_9",
};

int entrycmp(const void *a, const void *b)
{
  const char **ap = (const char **) a;
  const char **bp = (const char **) b;
  return strcmp(*ap, *bp);
}

static int interesting_function(const char *name)
{
  return bsearch(&name, uninteresting, ARRAY_LEN(uninteresting),
                 sizeof(uninteresting[0]), entrycmp) == NULL;
}

void dylan_dump_callstack(void *ctxt)
{
  unw_context_t context;
  if (ctxt == NULL) {
    // If no explicit context was passed in, start "here"
    unw_getcontext(&context);
    ctxt = &context;
  }

  fprintf(stderr, "Backtrace:\n");
  unw_cursor_t cursor;
  int rc = unw_init_local(&cursor, ctxt);
  do {
    // Find a symbol for the current frame
    unw_word_t offset;
    char buf[256];
    if (unw_get_proc_name(&cursor, buf, sizeof buf, &offset) == 0) {
      // Is this an IEP?
      size_t namelen = strlen(buf);
      char demangled[256];
      if (namelen > 0
          && buf[namelen - 1] == 'I'
          && dylan_demangle(demangled, sizeof demangled, buf) == 0) {
        if (interesting_iep(demangled)) {
          fprintf(stderr, "  %s + %#jx\n", demangled, (uintmax_t) offset);
        }
      }
      else if (interesting_function(buf)) {
        fprintf(stderr, "  %s + %#jx\n", buf, (uintmax_t) offset);
      }
    }
    else {
      // Read the raw instruction pointer address
      unw_word_t ip;
      unw_get_reg(&cursor, UNW_REG_IP, &ip);

      fprintf(stderr, "  %#jx\n", (uintmax_t) ip);
    }

    // On to the next enclosing frame
    rc = unw_step(&cursor);
  } while (rc > 0);
}

#else  // !HAVE_LIBUNWIND_H

void dylan_dump_callstack(void *ctxt)
{
}

#endif
