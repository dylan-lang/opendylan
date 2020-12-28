#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ARRAY_LEN(x)            (sizeof(x)/sizeof((x)[0]))

#ifdef HAVE_LIBUNWIND_H
#define UNW_LOCAL_ONLY
#include <libunwind.h>

#include <unistd.h>
#include <signal.h>
#include <sys/signal.h>
#include <sys/ucontext.h>

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
  "cache_header_0",
  "cache_header_1",
  "cache_header_10",
  "cache_header_11",
  "cache_header_12",
  "cache_header_13",
  "cache_header_14",
  "cache_header_15",
  "cache_header_2",
  "cache_header_3",
  "cache_header_4",
  "cache_header_5",
  "cache_header_6",
  "cache_header_7",
  "cache_header_8",
  "cache_header_9",
  "cache_header_engine_0",
  "cache_header_engine_1",
  "cache_header_engine_2",
  "cache_header_engine_3",
  "cache_header_engine_4",
  "cache_header_engine_5",
  "cache_header_engine_6",
  "cache_header_engine_7",
  "cache_header_engine_n",
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
  "discriminate_on_argument_0_1",
  "discriminate_on_argument_0_10",
  "discriminate_on_argument_0_11",
  "discriminate_on_argument_0_12",
  "discriminate_on_argument_0_13",
  "discriminate_on_argument_0_14",
  "discriminate_on_argument_0_15",
  "discriminate_on_argument_0_2",
  "discriminate_on_argument_0_3",
  "discriminate_on_argument_0_4",
  "discriminate_on_argument_0_5",
  "discriminate_on_argument_0_6",
  "discriminate_on_argument_0_7",
  "discriminate_on_argument_0_8",
  "discriminate_on_argument_0_9",
  "discriminate_on_argument_10_11",
  "discriminate_on_argument_10_12",
  "discriminate_on_argument_10_13",
  "discriminate_on_argument_10_14",
  "discriminate_on_argument_10_15",
  "discriminate_on_argument_11_12",
  "discriminate_on_argument_11_13",
  "discriminate_on_argument_11_14",
  "discriminate_on_argument_11_15",
  "discriminate_on_argument_12_13",
  "discriminate_on_argument_12_14",
  "discriminate_on_argument_12_15",
  "discriminate_on_argument_13_14",
  "discriminate_on_argument_13_15",
  "discriminate_on_argument_14_15",
  "discriminate_on_argument_1_10",
  "discriminate_on_argument_1_11",
  "discriminate_on_argument_1_12",
  "discriminate_on_argument_1_13",
  "discriminate_on_argument_1_14",
  "discriminate_on_argument_1_15",
  "discriminate_on_argument_1_2",
  "discriminate_on_argument_1_3",
  "discriminate_on_argument_1_4",
  "discriminate_on_argument_1_5",
  "discriminate_on_argument_1_6",
  "discriminate_on_argument_1_7",
  "discriminate_on_argument_1_8",
  "discriminate_on_argument_1_9",
  "discriminate_on_argument_2_10",
  "discriminate_on_argument_2_11",
  "discriminate_on_argument_2_12",
  "discriminate_on_argument_2_13",
  "discriminate_on_argument_2_14",
  "discriminate_on_argument_2_15",
  "discriminate_on_argument_2_3",
  "discriminate_on_argument_2_4",
  "discriminate_on_argument_2_5",
  "discriminate_on_argument_2_6",
  "discriminate_on_argument_2_7",
  "discriminate_on_argument_2_8",
  "discriminate_on_argument_2_9",
  "discriminate_on_argument_3_10",
  "discriminate_on_argument_3_11",
  "discriminate_on_argument_3_12",
  "discriminate_on_argument_3_13",
  "discriminate_on_argument_3_14",
  "discriminate_on_argument_3_15",
  "discriminate_on_argument_3_4",
  "discriminate_on_argument_3_5",
  "discriminate_on_argument_3_6",
  "discriminate_on_argument_3_7",
  "discriminate_on_argument_3_8",
  "discriminate_on_argument_3_9",
  "discriminate_on_argument_4_10",
  "discriminate_on_argument_4_11",
  "discriminate_on_argument_4_12",
  "discriminate_on_argument_4_13",
  "discriminate_on_argument_4_14",
  "discriminate_on_argument_4_15",
  "discriminate_on_argument_4_5",
  "discriminate_on_argument_4_6",
  "discriminate_on_argument_4_7",
  "discriminate_on_argument_4_8",
  "discriminate_on_argument_4_9",
  "discriminate_on_argument_5_10",
  "discriminate_on_argument_5_11",
  "discriminate_on_argument_5_12",
  "discriminate_on_argument_5_13",
  "discriminate_on_argument_5_14",
  "discriminate_on_argument_5_15",
  "discriminate_on_argument_5_6",
  "discriminate_on_argument_5_7",
  "discriminate_on_argument_5_8",
  "discriminate_on_argument_5_9",
  "discriminate_on_argument_6_10",
  "discriminate_on_argument_6_11",
  "discriminate_on_argument_6_12",
  "discriminate_on_argument_6_13",
  "discriminate_on_argument_6_14",
  "discriminate_on_argument_6_15",
  "discriminate_on_argument_6_7",
  "discriminate_on_argument_6_8",
  "discriminate_on_argument_6_9",
  "discriminate_on_argument_7_10",
  "discriminate_on_argument_7_11",
  "discriminate_on_argument_7_12",
  "discriminate_on_argument_7_13",
  "discriminate_on_argument_7_14",
  "discriminate_on_argument_7_15",
  "discriminate_on_argument_7_8",
  "discriminate_on_argument_7_9",
  "discriminate_on_argument_8_10",
  "discriminate_on_argument_8_11",
  "discriminate_on_argument_8_12",
  "discriminate_on_argument_8_13",
  "discriminate_on_argument_8_14",
  "discriminate_on_argument_8_15",
  "discriminate_on_argument_8_9",
  "discriminate_on_argument_9_10",
  "discriminate_on_argument_9_11",
  "discriminate_on_argument_9_12",
  "discriminate_on_argument_9_13",
  "discriminate_on_argument_9_14",
  "discriminate_on_argument_9_15",
  "explicit_keyed_single_method_0",
  "explicit_keyed_single_method_1",
  "explicit_keyed_single_method_10",
  "explicit_keyed_single_method_11",
  "explicit_keyed_single_method_12",
  "explicit_keyed_single_method_13",
  "explicit_keyed_single_method_14",
  "explicit_keyed_single_method_15",
  "explicit_keyed_single_method_2",
  "explicit_keyed_single_method_3",
  "explicit_keyed_single_method_4",
  "explicit_keyed_single_method_5",
  "explicit_keyed_single_method_6",
  "explicit_keyed_single_method_7",
  "explicit_keyed_single_method_8",
  "explicit_keyed_single_method_9",
  "general_engine_node_1",
  "general_engine_node_2",
  "general_engine_node_3",
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
  "if_type_discriminator_0_1",
  "if_type_discriminator_0_10",
  "if_type_discriminator_0_11",
  "if_type_discriminator_0_12",
  "if_type_discriminator_0_13",
  "if_type_discriminator_0_14",
  "if_type_discriminator_0_15",
  "if_type_discriminator_0_2",
  "if_type_discriminator_0_3",
  "if_type_discriminator_0_4",
  "if_type_discriminator_0_5",
  "if_type_discriminator_0_6",
  "if_type_discriminator_0_7",
  "if_type_discriminator_0_8",
  "if_type_discriminator_0_9",
  "if_type_discriminator_10_11",
  "if_type_discriminator_10_12",
  "if_type_discriminator_10_13",
  "if_type_discriminator_10_14",
  "if_type_discriminator_10_15",
  "if_type_discriminator_11_12",
  "if_type_discriminator_11_13",
  "if_type_discriminator_11_14",
  "if_type_discriminator_11_15",
  "if_type_discriminator_12_13",
  "if_type_discriminator_12_14",
  "if_type_discriminator_12_15",
  "if_type_discriminator_13_14",
  "if_type_discriminator_13_15",
  "if_type_discriminator_14_15",
  "if_type_discriminator_1_10",
  "if_type_discriminator_1_11",
  "if_type_discriminator_1_12",
  "if_type_discriminator_1_13",
  "if_type_discriminator_1_14",
  "if_type_discriminator_1_15",
  "if_type_discriminator_1_2",
  "if_type_discriminator_1_3",
  "if_type_discriminator_1_4",
  "if_type_discriminator_1_5",
  "if_type_discriminator_1_6",
  "if_type_discriminator_1_7",
  "if_type_discriminator_1_8",
  "if_type_discriminator_1_9",
  "if_type_discriminator_2_10",
  "if_type_discriminator_2_11",
  "if_type_discriminator_2_12",
  "if_type_discriminator_2_13",
  "if_type_discriminator_2_14",
  "if_type_discriminator_2_15",
  "if_type_discriminator_2_3",
  "if_type_discriminator_2_4",
  "if_type_discriminator_2_5",
  "if_type_discriminator_2_6",
  "if_type_discriminator_2_7",
  "if_type_discriminator_2_8",
  "if_type_discriminator_2_9",
  "if_type_discriminator_3_10",
  "if_type_discriminator_3_11",
  "if_type_discriminator_3_12",
  "if_type_discriminator_3_13",
  "if_type_discriminator_3_14",
  "if_type_discriminator_3_15",
  "if_type_discriminator_3_4",
  "if_type_discriminator_3_5",
  "if_type_discriminator_3_6",
  "if_type_discriminator_3_7",
  "if_type_discriminator_3_8",
  "if_type_discriminator_3_9",
  "if_type_discriminator_4_10",
  "if_type_discriminator_4_11",
  "if_type_discriminator_4_12",
  "if_type_discriminator_4_13",
  "if_type_discriminator_4_14",
  "if_type_discriminator_4_15",
  "if_type_discriminator_4_5",
  "if_type_discriminator_4_6",
  "if_type_discriminator_4_7",
  "if_type_discriminator_4_8",
  "if_type_discriminator_4_9",
  "if_type_discriminator_5_10",
  "if_type_discriminator_5_11",
  "if_type_discriminator_5_12",
  "if_type_discriminator_5_13",
  "if_type_discriminator_5_14",
  "if_type_discriminator_5_15",
  "if_type_discriminator_5_6",
  "if_type_discriminator_5_7",
  "if_type_discriminator_5_8",
  "if_type_discriminator_5_9",
  "if_type_discriminator_6_10",
  "if_type_discriminator_6_11",
  "if_type_discriminator_6_12",
  "if_type_discriminator_6_13",
  "if_type_discriminator_6_14",
  "if_type_discriminator_6_15",
  "if_type_discriminator_6_7",
  "if_type_discriminator_6_8",
  "if_type_discriminator_6_9",
  "if_type_discriminator_7_10",
  "if_type_discriminator_7_11",
  "if_type_discriminator_7_12",
  "if_type_discriminator_7_13",
  "if_type_discriminator_7_14",
  "if_type_discriminator_7_15",
  "if_type_discriminator_7_8",
  "if_type_discriminator_7_9",
  "if_type_discriminator_8_10",
  "if_type_discriminator_8_11",
  "if_type_discriminator_8_12",
  "if_type_discriminator_8_13",
  "if_type_discriminator_8_14",
  "if_type_discriminator_8_15",
  "if_type_discriminator_8_9",
  "if_type_discriminator_9_10",
  "if_type_discriminator_9_11",
  "if_type_discriminator_9_12",
  "if_type_discriminator_9_13",
  "if_type_discriminator_9_14",
  "if_type_discriminator_9_15",
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
  "monomorphic_by_class_discriminator_0_1",
  "monomorphic_by_class_discriminator_0_10",
  "monomorphic_by_class_discriminator_0_11",
  "monomorphic_by_class_discriminator_0_12",
  "monomorphic_by_class_discriminator_0_13",
  "monomorphic_by_class_discriminator_0_14",
  "monomorphic_by_class_discriminator_0_15",
  "monomorphic_by_class_discriminator_0_2",
  "monomorphic_by_class_discriminator_0_3",
  "monomorphic_by_class_discriminator_0_4",
  "monomorphic_by_class_discriminator_0_5",
  "monomorphic_by_class_discriminator_0_6",
  "monomorphic_by_class_discriminator_0_7",
  "monomorphic_by_class_discriminator_0_8",
  "monomorphic_by_class_discriminator_0_9",
  "monomorphic_by_class_discriminator_10_11",
  "monomorphic_by_class_discriminator_10_12",
  "monomorphic_by_class_discriminator_10_13",
  "monomorphic_by_class_discriminator_10_14",
  "monomorphic_by_class_discriminator_10_15",
  "monomorphic_by_class_discriminator_11_12",
  "monomorphic_by_class_discriminator_11_13",
  "monomorphic_by_class_discriminator_11_14",
  "monomorphic_by_class_discriminator_11_15",
  "monomorphic_by_class_discriminator_12_13",
  "monomorphic_by_class_discriminator_12_14",
  "monomorphic_by_class_discriminator_12_15",
  "monomorphic_by_class_discriminator_13_14",
  "monomorphic_by_class_discriminator_13_15",
  "monomorphic_by_class_discriminator_14_15",
  "monomorphic_by_class_discriminator_1_10",
  "monomorphic_by_class_discriminator_1_11",
  "monomorphic_by_class_discriminator_1_12",
  "monomorphic_by_class_discriminator_1_13",
  "monomorphic_by_class_discriminator_1_14",
  "monomorphic_by_class_discriminator_1_15",
  "monomorphic_by_class_discriminator_1_2",
  "monomorphic_by_class_discriminator_1_3",
  "monomorphic_by_class_discriminator_1_4",
  "monomorphic_by_class_discriminator_1_5",
  "monomorphic_by_class_discriminator_1_6",
  "monomorphic_by_class_discriminator_1_7",
  "monomorphic_by_class_discriminator_1_8",
  "monomorphic_by_class_discriminator_1_9",
  "monomorphic_by_class_discriminator_2_10",
  "monomorphic_by_class_discriminator_2_11",
  "monomorphic_by_class_discriminator_2_12",
  "monomorphic_by_class_discriminator_2_13",
  "monomorphic_by_class_discriminator_2_14",
  "monomorphic_by_class_discriminator_2_15",
  "monomorphic_by_class_discriminator_2_3",
  "monomorphic_by_class_discriminator_2_4",
  "monomorphic_by_class_discriminator_2_5",
  "monomorphic_by_class_discriminator_2_6",
  "monomorphic_by_class_discriminator_2_7",
  "monomorphic_by_class_discriminator_2_8",
  "monomorphic_by_class_discriminator_2_9",
  "monomorphic_by_class_discriminator_3_10",
  "monomorphic_by_class_discriminator_3_11",
  "monomorphic_by_class_discriminator_3_12",
  "monomorphic_by_class_discriminator_3_13",
  "monomorphic_by_class_discriminator_3_14",
  "monomorphic_by_class_discriminator_3_15",
  "monomorphic_by_class_discriminator_3_4",
  "monomorphic_by_class_discriminator_3_5",
  "monomorphic_by_class_discriminator_3_6",
  "monomorphic_by_class_discriminator_3_7",
  "monomorphic_by_class_discriminator_3_8",
  "monomorphic_by_class_discriminator_3_9",
  "monomorphic_by_class_discriminator_4_10",
  "monomorphic_by_class_discriminator_4_11",
  "monomorphic_by_class_discriminator_4_12",
  "monomorphic_by_class_discriminator_4_13",
  "monomorphic_by_class_discriminator_4_14",
  "monomorphic_by_class_discriminator_4_15",
  "monomorphic_by_class_discriminator_4_5",
  "monomorphic_by_class_discriminator_4_6",
  "monomorphic_by_class_discriminator_4_7",
  "monomorphic_by_class_discriminator_4_8",
  "monomorphic_by_class_discriminator_4_9",
  "monomorphic_by_class_discriminator_5_10",
  "monomorphic_by_class_discriminator_5_11",
  "monomorphic_by_class_discriminator_5_12",
  "monomorphic_by_class_discriminator_5_13",
  "monomorphic_by_class_discriminator_5_14",
  "monomorphic_by_class_discriminator_5_15",
  "monomorphic_by_class_discriminator_5_6",
  "monomorphic_by_class_discriminator_5_7",
  "monomorphic_by_class_discriminator_5_8",
  "monomorphic_by_class_discriminator_5_9",
  "monomorphic_by_class_discriminator_6_10",
  "monomorphic_by_class_discriminator_6_11",
  "monomorphic_by_class_discriminator_6_12",
  "monomorphic_by_class_discriminator_6_13",
  "monomorphic_by_class_discriminator_6_14",
  "monomorphic_by_class_discriminator_6_15",
  "monomorphic_by_class_discriminator_6_7",
  "monomorphic_by_class_discriminator_6_8",
  "monomorphic_by_class_discriminator_6_9",
  "monomorphic_by_class_discriminator_7_10",
  "monomorphic_by_class_discriminator_7_11",
  "monomorphic_by_class_discriminator_7_12",
  "monomorphic_by_class_discriminator_7_13",
  "monomorphic_by_class_discriminator_7_14",
  "monomorphic_by_class_discriminator_7_15",
  "monomorphic_by_class_discriminator_7_8",
  "monomorphic_by_class_discriminator_7_9",
  "monomorphic_by_class_discriminator_8_10",
  "monomorphic_by_class_discriminator_8_11",
  "monomorphic_by_class_discriminator_8_12",
  "monomorphic_by_class_discriminator_8_13",
  "monomorphic_by_class_discriminator_8_14",
  "monomorphic_by_class_discriminator_8_15",
  "monomorphic_by_class_discriminator_8_9",
  "monomorphic_by_class_discriminator_9_10",
  "monomorphic_by_class_discriminator_9_11",
  "monomorphic_by_class_discriminator_9_12",
  "monomorphic_by_class_discriminator_9_13",
  "monomorphic_by_class_discriminator_9_14",
  "monomorphic_by_class_discriminator_9_15",
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
  "profiling_cache_header_0",
  "profiling_cache_header_1",
  "profiling_cache_header_10",
  "profiling_cache_header_11",
  "profiling_cache_header_12",
  "profiling_cache_header_13",
  "profiling_cache_header_14",
  "profiling_cache_header_15",
  "profiling_cache_header_2",
  "profiling_cache_header_3",
  "profiling_cache_header_4",
  "profiling_cache_header_5",
  "profiling_cache_header_6",
  "profiling_cache_header_7",
  "profiling_cache_header_8",
  "profiling_cache_header_9",
  "profiling_cache_header_engine_0",
  "profiling_cache_header_engine_1",
  "profiling_cache_header_engine_2",
  "profiling_cache_header_engine_3",
  "profiling_cache_header_engine_4",
  "profiling_cache_header_engine_5",
  "profiling_cache_header_engine_6",
  "profiling_cache_header_engine_7",
  "profiling_cache_header_engine_n",
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
  "typecheck_discriminator_0_1",
  "typecheck_discriminator_0_10",
  "typecheck_discriminator_0_11",
  "typecheck_discriminator_0_12",
  "typecheck_discriminator_0_13",
  "typecheck_discriminator_0_14",
  "typecheck_discriminator_0_15",
  "typecheck_discriminator_0_2",
  "typecheck_discriminator_0_3",
  "typecheck_discriminator_0_4",
  "typecheck_discriminator_0_5",
  "typecheck_discriminator_0_6",
  "typecheck_discriminator_0_7",
  "typecheck_discriminator_0_8",
  "typecheck_discriminator_0_9",
  "typecheck_discriminator_10_11",
  "typecheck_discriminator_10_12",
  "typecheck_discriminator_10_13",
  "typecheck_discriminator_10_14",
  "typecheck_discriminator_10_15",
  "typecheck_discriminator_11_12",
  "typecheck_discriminator_11_13",
  "typecheck_discriminator_11_14",
  "typecheck_discriminator_11_15",
  "typecheck_discriminator_12_13",
  "typecheck_discriminator_12_14",
  "typecheck_discriminator_12_15",
  "typecheck_discriminator_13_14",
  "typecheck_discriminator_13_15",
  "typecheck_discriminator_14_15",
  "typecheck_discriminator_1_10",
  "typecheck_discriminator_1_11",
  "typecheck_discriminator_1_12",
  "typecheck_discriminator_1_13",
  "typecheck_discriminator_1_14",
  "typecheck_discriminator_1_15",
  "typecheck_discriminator_1_2",
  "typecheck_discriminator_1_3",
  "typecheck_discriminator_1_4",
  "typecheck_discriminator_1_5",
  "typecheck_discriminator_1_6",
  "typecheck_discriminator_1_7",
  "typecheck_discriminator_1_8",
  "typecheck_discriminator_1_9",
  "typecheck_discriminator_2_10",
  "typecheck_discriminator_2_11",
  "typecheck_discriminator_2_12",
  "typecheck_discriminator_2_13",
  "typecheck_discriminator_2_14",
  "typecheck_discriminator_2_15",
  "typecheck_discriminator_2_3",
  "typecheck_discriminator_2_4",
  "typecheck_discriminator_2_5",
  "typecheck_discriminator_2_6",
  "typecheck_discriminator_2_7",
  "typecheck_discriminator_2_8",
  "typecheck_discriminator_2_9",
  "typecheck_discriminator_3_10",
  "typecheck_discriminator_3_11",
  "typecheck_discriminator_3_12",
  "typecheck_discriminator_3_13",
  "typecheck_discriminator_3_14",
  "typecheck_discriminator_3_15",
  "typecheck_discriminator_3_4",
  "typecheck_discriminator_3_5",
  "typecheck_discriminator_3_6",
  "typecheck_discriminator_3_7",
  "typecheck_discriminator_3_8",
  "typecheck_discriminator_3_9",
  "typecheck_discriminator_4_10",
  "typecheck_discriminator_4_11",
  "typecheck_discriminator_4_12",
  "typecheck_discriminator_4_13",
  "typecheck_discriminator_4_14",
  "typecheck_discriminator_4_15",
  "typecheck_discriminator_4_5",
  "typecheck_discriminator_4_6",
  "typecheck_discriminator_4_7",
  "typecheck_discriminator_4_8",
  "typecheck_discriminator_4_9",
  "typecheck_discriminator_5_10",
  "typecheck_discriminator_5_11",
  "typecheck_discriminator_5_12",
  "typecheck_discriminator_5_13",
  "typecheck_discriminator_5_14",
  "typecheck_discriminator_5_15",
  "typecheck_discriminator_5_6",
  "typecheck_discriminator_5_7",
  "typecheck_discriminator_5_8",
  "typecheck_discriminator_5_9",
  "typecheck_discriminator_6_10",
  "typecheck_discriminator_6_11",
  "typecheck_discriminator_6_12",
  "typecheck_discriminator_6_13",
  "typecheck_discriminator_6_14",
  "typecheck_discriminator_6_15",
  "typecheck_discriminator_6_7",
  "typecheck_discriminator_6_8",
  "typecheck_discriminator_6_9",
  "typecheck_discriminator_7_10",
  "typecheck_discriminator_7_11",
  "typecheck_discriminator_7_12",
  "typecheck_discriminator_7_13",
  "typecheck_discriminator_7_14",
  "typecheck_discriminator_7_15",
  "typecheck_discriminator_7_8",
  "typecheck_discriminator_7_9",
  "typecheck_discriminator_8_10",
  "typecheck_discriminator_8_11",
  "typecheck_discriminator_8_12",
  "typecheck_discriminator_8_13",
  "typecheck_discriminator_8_14",
  "typecheck_discriminator_8_15",
  "typecheck_discriminator_8_9",
  "typecheck_discriminator_9_10",
  "typecheck_discriminator_9_11",
  "typecheck_discriminator_9_12",
  "typecheck_discriminator_9_13",
  "typecheck_discriminator_9_14",
  "typecheck_discriminator_9_15",
  "unrestricted_keyed_single_method_0",
  "unrestricted_keyed_single_method_1",
  "unrestricted_keyed_single_method_10",
  "unrestricted_keyed_single_method_11",
  "unrestricted_keyed_single_method_12",
  "unrestricted_keyed_single_method_13",
  "unrestricted_keyed_single_method_14",
  "unrestricted_keyed_single_method_15",
  "unrestricted_keyed_single_method_2",
  "unrestricted_keyed_single_method_3",
  "unrestricted_keyed_single_method_4",
  "unrestricted_keyed_single_method_5",
  "unrestricted_keyed_single_method_6",
  "unrestricted_keyed_single_method_7",
  "unrestricted_keyed_single_method_8",
  "unrestricted_keyed_single_method_9",
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
  unw_cursor_t cursor;
#if (defined (OPEN_DYLAN_PLATFORM_DARWIN) && defined(OPEN_DYLAN_ARCH_X86_64))
  // We can use the passed-in ucontext_t as the libunwind context
  if (ctxt == NULL) {
    unw_getcontext(&context);
    ctxt = &context;
  }
  int rc = unw_init_local(&cursor, ctxt);
#else
  unw_getcontext(&context);
  int rc = unw_init_local(&cursor, &context);
  if (ctxt != NULL) {
    // If an explicit context was passed in, copy register values from it
    const ucontext_t *uc = (ucontext_t *) ctxt;
    const mcontext_t *mc = &uc->uc_mcontext;
#if (defined(OPEN_DYLAN_ARCH_X86_64) && defined(OPEN_DYLAN_PLATFORM_LINUX))
    unw_set_reg(&cursor, UNW_X86_64_RAX, mc->gregs[REG_RAX]);
    unw_set_reg(&cursor, UNW_X86_64_RDX, mc->gregs[REG_RDX]);
    unw_set_reg(&cursor, UNW_X86_64_RCX, mc->gregs[REG_RCX]);
    unw_set_reg(&cursor, UNW_X86_64_RBX, mc->gregs[REG_RBX]);
    unw_set_reg(&cursor, UNW_X86_64_RSI, mc->gregs[REG_RSI]);
    unw_set_reg(&cursor, UNW_X86_64_RDI, mc->gregs[REG_RDI]);
    unw_set_reg(&cursor, UNW_X86_64_RBP, mc->gregs[REG_RBP]);
    unw_set_reg(&cursor, UNW_X86_64_RSP, mc->gregs[REG_RSP]);
    unw_set_reg(&cursor, UNW_X86_64_R8 , mc->gregs[REG_R8]);
    unw_set_reg(&cursor, UNW_X86_64_R9 , mc->gregs[REG_R9]);
    unw_set_reg(&cursor, UNW_X86_64_R10, mc->gregs[REG_R10]);
    unw_set_reg(&cursor, UNW_X86_64_R11, mc->gregs[REG_R11]);
    unw_set_reg(&cursor, UNW_X86_64_R12, mc->gregs[REG_R12]);
    unw_set_reg(&cursor, UNW_X86_64_R13, mc->gregs[REG_R13]);
    unw_set_reg(&cursor, UNW_X86_64_R14, mc->gregs[REG_R14]);
    unw_set_reg(&cursor, UNW_X86_64_R15, mc->gregs[REG_R15]);

    unw_set_reg(&cursor, UNW_REG_IP, mc->gregs[REG_RIP]);
    unw_set_reg(&cursor, UNW_REG_SP, mc->gregs[REG_RSP]);
#elif (defined(OPEN_DYLAN_ARCH_X86_64) && defined(OPEN_DYLAN_PLATFORM_FREEBSD))
    unw_set_reg(&cursor, UNW_X86_64_RAX, mc->mc_rax);
    unw_set_reg(&cursor, UNW_X86_64_RDX, mc->mc_rdx);
    unw_set_reg(&cursor, UNW_X86_64_RCX, mc->mc_rcx);
    unw_set_reg(&cursor, UNW_X86_64_RBX, mc->mc_rbx);
    unw_set_reg(&cursor, UNW_X86_64_RSI, mc->mc_rsi);
    unw_set_reg(&cursor, UNW_X86_64_RDI, mc->mc_rdi);
    unw_set_reg(&cursor, UNW_X86_64_RBP, mc->mc_rbp);
    unw_set_reg(&cursor, UNW_X86_64_RSP, mc->mc_rsp);
    unw_set_reg(&cursor, UNW_X86_64_R8 , mc->mc_r8);
    unw_set_reg(&cursor, UNW_X86_64_R9 , mc->mc_r9);
    unw_set_reg(&cursor, UNW_X86_64_R10, mc->mc_r10);
    unw_set_reg(&cursor, UNW_X86_64_R11, mc->mc_r11);
    unw_set_reg(&cursor, UNW_X86_64_R12, mc->mc_r12);
    unw_set_reg(&cursor, UNW_X86_64_R13, mc->mc_r13);
    unw_set_reg(&cursor, UNW_X86_64_R14, mc->mc_r14);
    unw_set_reg(&cursor, UNW_X86_64_R15, mc->mc_r15);

    unw_set_reg(&cursor, UNW_REG_IP, mc->mc_rip);
    unw_set_reg(&cursor, UNW_REG_SP, mc->mc_rsp);
#elif (defined(OPEN_DYLAN_ARCH_AARCH64) && defined(OPEN_DYLAN_PLATFORM_LINUX))
    unw_set_reg(&cursor, UNW_ARM64_X0, mc->regs[0]);
    unw_set_reg(&cursor, UNW_ARM64_X1, mc->regs[1]);
    unw_set_reg(&cursor, UNW_ARM64_X2, mc->regs[2]);
    unw_set_reg(&cursor, UNW_ARM64_X3, mc->regs[3]);
    unw_set_reg(&cursor, UNW_ARM64_X4, mc->regs[4]);
    unw_set_reg(&cursor, UNW_ARM64_X5, mc->regs[5]);
    unw_set_reg(&cursor, UNW_ARM64_X6, mc->regs[6]);
    unw_set_reg(&cursor, UNW_ARM64_X7, mc->regs[7]);
    unw_set_reg(&cursor, UNW_ARM64_X8, mc->regs[8]);
    unw_set_reg(&cursor, UNW_ARM64_X9, mc->regs[9]);
    unw_set_reg(&cursor, UNW_ARM64_X10, mc->regs[10]);
    unw_set_reg(&cursor, UNW_ARM64_X11, mc->regs[11]);
    unw_set_reg(&cursor, UNW_ARM64_X12, mc->regs[12]);
    unw_set_reg(&cursor, UNW_ARM64_X13, mc->regs[13]);
    unw_set_reg(&cursor, UNW_ARM64_X14, mc->regs[14]);
    unw_set_reg(&cursor, UNW_ARM64_X15, mc->regs[15]);
    unw_set_reg(&cursor, UNW_ARM64_X16, mc->regs[16]);
    unw_set_reg(&cursor, UNW_ARM64_X17, mc->regs[17]);
    unw_set_reg(&cursor, UNW_ARM64_X18, mc->regs[18]);
    unw_set_reg(&cursor, UNW_ARM64_X19, mc->regs[19]);
    unw_set_reg(&cursor, UNW_ARM64_X20, mc->regs[20]);
    unw_set_reg(&cursor, UNW_ARM64_X21, mc->regs[21]);
    unw_set_reg(&cursor, UNW_ARM64_X22, mc->regs[22]);
    unw_set_reg(&cursor, UNW_ARM64_X23, mc->regs[23]);
    unw_set_reg(&cursor, UNW_ARM64_X24, mc->regs[24]);
    unw_set_reg(&cursor, UNW_ARM64_X25, mc->regs[25]);
    unw_set_reg(&cursor, UNW_ARM64_X26, mc->regs[26]);
    unw_set_reg(&cursor, UNW_ARM64_X27, mc->regs[27]);
    unw_set_reg(&cursor, UNW_ARM64_X28, mc->regs[28]);
    unw_set_reg(&cursor, UNW_ARM64_X29, mc->regs[29]);
    unw_set_reg(&cursor, UNW_ARM64_X30, mc->regs[30]);
    unw_set_reg(&cursor, UNW_ARM64_SP, mc->sp);

    unw_set_reg(&cursor, UNW_REG_IP, mc->pc);
    unw_set_reg(&cursor, UNW_REG_SP, mc->sp);
#elif (defined(OPEN_DYLAN_ARCH_RISCV64) && defined(OPEN_DYLAN_PLATFORM_LINUX))
    unw_set_reg(&cursor, UNW_RISCV_X1, mc->__gregs[1]);
    unw_set_reg(&cursor, UNW_RISCV_X2, mc->__gregs[2]);
    unw_set_reg(&cursor, UNW_RISCV_X3, mc->__gregs[3]);
    unw_set_reg(&cursor, UNW_RISCV_X4, mc->__gregs[4]);
    unw_set_reg(&cursor, UNW_RISCV_X5, mc->__gregs[5]);
    unw_set_reg(&cursor, UNW_RISCV_X6, mc->__gregs[6]);
    unw_set_reg(&cursor, UNW_RISCV_X7, mc->__gregs[7]);
    unw_set_reg(&cursor, UNW_RISCV_X8, mc->__gregs[8]);
    unw_set_reg(&cursor, UNW_RISCV_X9, mc->__gregs[9]);
    unw_set_reg(&cursor, UNW_RISCV_X10, mc->__gregs[10]);
    unw_set_reg(&cursor, UNW_RISCV_X11, mc->__gregs[11]);
    unw_set_reg(&cursor, UNW_RISCV_X12, mc->__gregs[12]);
    unw_set_reg(&cursor, UNW_RISCV_X13, mc->__gregs[13]);
    unw_set_reg(&cursor, UNW_RISCV_X14, mc->__gregs[14]);
    unw_set_reg(&cursor, UNW_RISCV_X15, mc->__gregs[15]);
    unw_set_reg(&cursor, UNW_RISCV_X16, mc->__gregs[16]);
    unw_set_reg(&cursor, UNW_RISCV_X17, mc->__gregs[17]);
    unw_set_reg(&cursor, UNW_RISCV_X18, mc->__gregs[18]);
    unw_set_reg(&cursor, UNW_RISCV_X19, mc->__gregs[19]);
    unw_set_reg(&cursor, UNW_RISCV_X20, mc->__gregs[20]);
    unw_set_reg(&cursor, UNW_RISCV_X21, mc->__gregs[21]);
    unw_set_reg(&cursor, UNW_RISCV_X22, mc->__gregs[22]);
    unw_set_reg(&cursor, UNW_RISCV_X23, mc->__gregs[23]);
    unw_set_reg(&cursor, UNW_RISCV_X24, mc->__gregs[24]);
    unw_set_reg(&cursor, UNW_RISCV_X25, mc->__gregs[25]);
    unw_set_reg(&cursor, UNW_RISCV_X26, mc->__gregs[26]);
    unw_set_reg(&cursor, UNW_RISCV_X27, mc->__gregs[27]);
    unw_set_reg(&cursor, UNW_RISCV_X28, mc->__gregs[28]);
    unw_set_reg(&cursor, UNW_RISCV_X29, mc->__gregs[29]);
    unw_set_reg(&cursor, UNW_RISCV_X30, mc->__gregs[30]);
    unw_set_reg(&cursor, UNW_RISCV_X31, mc->__gregs[31]);

    unw_set_reg(&cursor, UNW_REG_IP, mc->__gregs[REG_PC]);
    unw_set_reg(&cursor, UNW_REG_SP, mc->__gregs[REG_SP]);
#elif (defined(OPEN_DYLAN_ARCH_X86) && defined(OPEN_DYLAN_PLATFORM_LINUX))
    unw_set_reg(&cursor, UNW_X86_EAX, mc->gregs[REG_EAX]);
    unw_set_reg(&cursor, UNW_X86_ECX, mc->gregs[REG_ECX]);
    unw_set_reg(&cursor, UNW_X86_EDX, mc->gregs[REG_EDX]);
    unw_set_reg(&cursor, UNW_X86_EBX, mc->gregs[REG_EBX]);
    unw_set_reg(&cursor, UNW_X86_EBP, mc->gregs[REG_EBP]);
    unw_set_reg(&cursor, UNW_X86_ESP, mc->gregs[REG_ESP]);
    unw_set_reg(&cursor, UNW_X86_ESI, mc->gregs[REG_ESI]);
    unw_set_reg(&cursor, UNW_X86_EDI, mc->gregs[REG_EDI]);

    unw_set_reg(&cursor, UNW_REG_IP, mc->gregs[REG_EIP]);
    unw_set_reg(&cursor, UNW_REG_SP, mc->gregs[REG_ESP]);
#elif (defined(OPEN_DYLAN_ARCH_X86) && defined(OPEN_DYLAN_PLATFORM_FREEBSD))
    unw_set_reg(&cursor, UNW_X86_EAX, mc->mc_eax);
    unw_set_reg(&cursor, UNW_X86_ECX, mc->mc_ecx);
    unw_set_reg(&cursor, UNW_X86_EDX, mc->mc_edx);
    unw_set_reg(&cursor, UNW_X86_EBX, mc->mc_ebx);
    unw_set_reg(&cursor, UNW_X86_EBP, mc->mc_ebp);
    unw_set_reg(&cursor, UNW_X86_ESP, mc->mc_esp);
    unw_set_reg(&cursor, UNW_X86_ESI, mc->mc_esi);
    unw_set_reg(&cursor, UNW_X86_EDI, mc->mc_edi);

    unw_set_reg(&cursor, UNW_REG_IP, mc->mc_eip);
    unw_set_reg(&cursor, UNW_REG_SP, mc->mc_esp);
#else
#error No implementation for transferring register context
#endif
  }
#endif

  fprintf(stderr, "Backtrace:\n");
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
