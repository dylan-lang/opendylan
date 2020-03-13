#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "llvm-runtime.h"

#include <unwind.h>

#define CONTAINER_OF(p, type, member)           \
  (type *)((char *)(p) - offsetof(type, member))

#define OPENDYLAN_EXCEPTION_CLASS               \
  (((uint64_t) 'O' << 56)                       \
   | ((uint64_t) 'p' << 48)                     \
   | ((uint64_t) 'e' << 40)                     \
   | ((uint64_t) 'n' << 32)                     \
   | ((uint64_t) 'D' << 24)                     \
   | ((uint64_t) 'y' << 16)                     \
   | ((uint64_t) 'l' << 8)                      \
   | ((uint64_t) 'n' << 0))

#define DW_EH_PE_absptr   0x00
#define DW_EH_PE_uleb128  0x01
#define DW_EH_PE_udata2   0x02
#define DW_EH_PE_udata4   0x03
#define DW_EH_PE_udata8   0x04
#define DW_EH_PE_sleb128  0x09
#define DW_EH_PE_sdata2   0x0A
#define DW_EH_PE_sdata4   0x0B
#define DW_EH_PE_sdata8   0x0C
#define DW_EH_PE_signed   0x08
#define DW_EH_PE_pcrel    0x10
#define DW_EH_PE_textrel  0x20
#define DW_EH_PE_datarel  0x30
#define DW_EH_PE_funcrel  0x40
#define DW_EH_PE_aligned  0x50
#define DW_EH_PE_indirect 0x80
#define DW_EH_PE_omit     0xff

static uintptr_t get_ULEB128(const uint8_t **pp)
{
  const uint8_t *p = *pp;
  uintptr_t value = 0, shift = 0;
  do {
    value |= (*p & 0x7F) << shift;
    shift += 7;
  } while (*p++ & 0x80);
  *pp = p;
  return value;
}

static intptr_t get_SLEB128(const uint8_t **pp)
{
  const uint8_t *p = *pp;
  intptr_t value = 0, shift = 0;
  do {
    value |= (*p & 0x7F) << shift;
    shift += 7;
  } while (*p++ & 0x80);
  if (p[-1] & 0x40)
    value |= -1 << shift;
  *pp = p;
  return value;
}

static uintptr_t get_encoded_size(uint8_t encoding)
{
  if (encoding == DW_EH_PE_omit) {
    return 0;
  }
  switch (encoding & 0xF) {
  case DW_EH_PE_udata4:
  case DW_EH_PE_sdata4:
    return 4;
    break;
  case DW_EH_PE_udata8:
  case DW_EH_PE_sdata8:
    return 8;
    break;
  default:
    fprintf(stderr, "Encoding %#x (size)\n", encoding);
    abort();
  }
}

static uintptr_t get_encoded(const uint8_t **pp, uint8_t encoding)
{
  const uint8_t *p = *pp;
  uintptr_t result;
  if (encoding == DW_EH_PE_omit) {
    return 0;
  }

  // Base type
  switch (encoding & 0xF) {
  case DW_EH_PE_uleb128:
    result = get_ULEB128(&p);
    break;
  case DW_EH_PE_udata4:
    result = *((const uint32_t *) p);
    p += 4;
    break;
  case DW_EH_PE_sdata4:
    result = *((const int32_t *) p);
    p += 4;
    break;
  case DW_EH_PE_sdata8:
    result = *((const int64_t *) p);
    p += 8;
    break;
  default:
    fprintf(stderr, "Encoding %#x (base)\n", encoding);
    abort();
  }

  // Type modifier
  switch (encoding & 0x70) {
  case 0:
    break;
  case DW_EH_PE_pcrel:
    result += (uintptr_t) *pp;
    break;
  default:
    fprintf(stderr, "Encoding %#x (mod)\n", encoding);
    abort();
  }

  // Indirection flag
  if (encoding & DW_EH_PE_indirect) {
    result = *((uintptr_t *) result);
  }

  *pp = p;
  return result;
}

static void opendylan_exception_cleanup(_Unwind_Reason_Code reason,
                                        struct _Unwind_Exception *exception)
{
  // Do nothing (BEF is stack-allocated)
}

// Initiate a non-local exit by unwinding
void primitive_nlx(D bind_exit_frame)
{
  struct dylan_bef *bef = (struct dylan_bef *) bind_exit_frame;
  struct _Unwind_Exception *bef_unwind_exception
    = (struct _Unwind_Exception *) &bef->bef_unwind_exception;

  bef_unwind_exception->exception_class = OPENDYLAN_EXCEPTION_CLASS;
  bef_unwind_exception->exception_cleanup = opendylan_exception_cleanup;

  _Unwind_Reason_Code code = _Unwind_RaiseException(bef_unwind_exception);
  fprintf(stderr, "primitive_nlx: unwind error %d\n", code);
  abort();
}

// Notify the unwinder that a handler was found
static _Unwind_Reason_Code handler_found(_Unwind_Action actions,
                                         struct _Unwind_Exception *ex,
                                         struct _Unwind_Context *context,
                                         uintptr_t ip,
                                         uintptr_t selector)
{
  if (actions & _UA_SEARCH_PHASE) {
    return selector != 0 ? _URC_HANDLER_FOUND : _URC_CONTINUE_UNWIND;
  }
  else {
    _Unwind_SetGR(context, __builtin_eh_return_data_regno(0),
                  (uintptr_t) ex);
    _Unwind_SetGR(context, __builtin_eh_return_data_regno(1), selector);
    _Unwind_SetIP(context, ip);
    return _URC_INSTALL_CONTEXT;
  }
}

// Callback for the unwind system, referenced by dfmc-llvm-backend
// generated landingpad instructions
_Unwind_Reason_Code __opendylan_personality_v0(int version,
                                               _Unwind_Action actions,
                                               uint64_t exceptionClass,
                                               struct _Unwind_Exception *ex,
                                               struct _Unwind_Context *context)
{
  // Only Dylan exceptions will be explicitly caught
  if (exceptionClass != OPENDYLAN_EXCEPTION_CLASS
      && (actions & _UA_SEARCH_PHASE)) {
    return _URC_CONTINUE_UNWIND;
  }

  // Locate the landing pad information (i.e., the Language Specific
  // Data Area)
  const uint8_t *lsda
    = (const uint8_t *) _Unwind_GetLanguageSpecificData(context);
  if (lsda == NULL) {
    return _URC_CONTINUE_UNWIND;
  }

  // Locate the exception object's containing Bind Exit Frame
  // structure
  struct dylan_bef *bef
    = CONTAINER_OF(ex, struct dylan_bef, bef_unwind_exception);

  // Locate where we are in the current function
  uintptr_t region_start = _Unwind_GetRegionStart(context);
  uintptr_t ip_offset = _Unwind_GetIP(context) - region_start;

  // Read header
  uint8_t lpstart_encoding = *lsda++;
  if (lpstart_encoding != DW_EH_PE_omit) {
    get_encoded(&lsda, lpstart_encoding);
  }
  uint8_t ttype_encoding = *lsda++;
  uintptr_t ttype_entry_size = get_encoded_size(ttype_encoding);
  const uint8_t *ttable = NULL;
  if (ttype_encoding != DW_EH_PE_omit) {
    uintptr_t ttbase = get_ULEB128(&lsda);
    ttable = lsda + ttbase;
  }

  // Record if a cleanup was found
  bool cleanup_found = false;
  uintptr_t cleanup_ip;

  // Locate the current instruction pointer within the call site table
  uint8_t call_site_encoding = *lsda++;
  uintptr_t call_site_size = get_ULEB128(&lsda);
  const uint8_t *call_site_limit = lsda + call_site_size;
  while (lsda < call_site_limit) {
    uintptr_t start = get_encoded(&lsda, call_site_encoding);
    uintptr_t length = get_encoded(&lsda, call_site_encoding);
    uintptr_t landing_pad = get_encoded(&lsda, call_site_encoding);
    uintptr_t action = get_ULEB128(&lsda);

    if (landing_pad != 0 && start < ip_offset && ip_offset <= start + length) {
      // Found a matching call site, now determine which corresponding
      // action entry applies
      if (action == 0) {
        cleanup_found = true;
        cleanup_ip = region_start + landing_pad;
      }
      else {
        const uint8_t *action_entry = call_site_limit + action - 1;
        while (action_entry) {
          intptr_t filter = get_SLEB128(&action_entry);

          if (filter > 0) {
            // Catch types precede the TType offset
            const uint8_t *type_entry = ttable - filter * ttype_entry_size;
            uintptr_t type = get_encoded(&type_entry, ttype_encoding);
            if (type == (uintptr_t) bef->bef_typeid) {
              // Found a catch
              return handler_found(actions, ex, context,
                                   region_start + landing_pad, filter);
            }
          }
          else if (filter == 0) {
            // Found a cleanup
            if (cleanup_found && cleanup_ip != region_start + landing_pad) {
              abort();
            }
            else {
              cleanup_found = true;
              cleanup_ip = region_start + landing_pad;
            }
          }

          // This one didn't match, so try the next entry by following
          // the next link
          const uint8_t *next_base = action_entry;
          intptr_t next = get_SLEB128(&action_entry);
          action_entry = (next != 0) ? next_base + next : 0;
        }
      }
    }
  }

  if (cleanup_found) {
    return handler_found(actions, ex, context, cleanup_ip, 0);
  }

  // Nothing was found, so keep unwinding
  return _URC_CONTINUE_UNWIND;
}
