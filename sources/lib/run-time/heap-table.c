/* A dictionary mapping a void *to a void*
 * Munged unashamedly from the MPS table code
 */


#include <assert.h>
#include "heap-utils.h"

typedef unsigned long ulong;


#define TABLE_UNUSED        ((ulong)0x2AB7E040)
#define TABLE_DELETED   ((ulong)0x2AB7EDE7)
#define TABLE_ACTIVE    ((ulong)0x2AB7EAC2)


typedef struct table_entry_s *table_entry_t;
typedef struct table_entry_s {
  ulong status;
  void *key;
  void *value;
} table_entry_s;


typedef struct table_s {
  size_t length;
  size_t count;
  table_entry_t array;
} table_s;


static ulong table_hash(void *key)
{
  return (ulong)key;
}


/* table_find -- finds the entry for this key, or NULL */

static table_entry_t table_find(table_t table, void *key, int skip_deleted)
{
  ulong hash;
  size_t i;

  hash = table_hash(key) & (table->length - 1);
  i = hash;
  do {
    switch (table->array[i].status) {
    case TABLE_ACTIVE:
      if (table->array[i].key == key)
        return &table->array[i];
      break;
    case TABLE_DELETED:
      if (!skip_deleted)
        return &table->array[i];
      break;
    case TABLE_UNUSED:
      return &table->array[i];
      break;
    default:
      assert(0);
    }
    i = (i + (hash | 1)) & (table->length - 1);
  } while(i != hash);

  return NULL;
}


/* table_grow -- doubles the size of the table */

static BOOL table_grow(table_t table)
{
  table_entry_t oldArray, newArray;
  size_t i, oldLength, newLength;

  oldLength = table->length;
  oldArray = table->array;
  newLength = table->length * 2;
  newArray = alloc_obj(sizeof(table_entry_s) * newLength);
  if(newArray == NULL) return FALSE;

  for(i = 0; i < newLength; ++i) {
    newArray[i].key = 0;
    newArray[i].value = NULL;
    newArray[i].status = TABLE_UNUSED;
  }

  table->length = newLength;
  table->array = newArray;

  for(i = 0; i < oldLength; ++i) {
    table_entry_t entry;
    assert(oldArray[i].status == TABLE_ACTIVE); /* should be full */
    entry = table_find(table, oldArray[i].key, 0 /* none deleted */);
    assert(entry->status == TABLE_UNUSED); /* shouldn't be defined yet */
    entry->key = oldArray[i].key;
    entry->value = oldArray[i].value;
    entry->status = TABLE_ACTIVE;
  }
  free_obj(oldArray, sizeof(table_entry_s) * oldLength);

  return TRUE;
}


/* table_create -- makes a new table */

extern BOOL table_create(table_t *tableReturn, size_t length)
{
  table_t table;
  size_t i;

  assert(tableReturn != NULL);

  table = alloc_obj(sizeof(table_s));
  if(table == NULL) goto failMallocTable;
  table->length = length; table->count = 0;
  table->array = alloc_obj(sizeof(table_entry_s) * length);
  if(table->array == NULL) goto failMallocArray;
  for(i = 0; i < length; ++i) {
    table->array[i].key = 0;
    table->array[i].value = NULL;
    table->array[i].status = TABLE_UNUSED;
  }

  *tableReturn = table;
  return TRUE;

failMallocArray:
  free_obj(table, sizeof(table_s));
failMallocTable:
  return FALSE;
}


extern void table_destroy(table_t table)
{
  assert(table != NULL);
  free_obj(table->array, sizeof(table_entry_s) * table->length);
  free_obj(table, sizeof(table_s));
}


/* table_lookup -- look up */

extern BOOL table_lookup(void **valueReturn, table_t table, void *key)
{
  table_entry_t entry = table_find(table, key, 1 /* skip deleted */);

  if(entry == NULL || entry->status != TABLE_ACTIVE)
    return FALSE;
  *valueReturn = entry->value;
  return TRUE;
}


/* table_define -- add a new mapping */

extern BOOL table_define(table_t table, void *key, void *value)
{
  table_entry_t entry = table_find(table, key, 1 /* skip deleted */);

  if (entry != NULL && entry->status == TABLE_ACTIVE)
    return FALSE;

  if (entry == NULL) {
    BOOL res;
    entry = table_find(table, key, 0 /* do not skip deletions */);
    if (entry == NULL) {
      /* table is full.  Must grow the table to make room. */
      res = table_grow(table);
      if(res != TRUE) return res;
      entry = table_find(table, key, 0 /* do not skip deletions */);
    }
  }
  assert(entry != NULL && entry->status != TABLE_ACTIVE);

  entry->status = TABLE_ACTIVE;
  entry->key = key;
  entry->value = value;
  ++table->count;

  return TRUE;
}


/* table_redefine -- redefine an existing mapping */

extern BOOL table_redefine(table_t table, void *key, void *value)
{
  table_entry_t entry = table_find(table, key, 1 /* skip deletions */);

  if (entry == NULL || entry->status != TABLE_ACTIVE)
    return FALSE;
  assert(entry->key == key);
  entry->value = value;
  return TRUE;
}


/* table_remove -- remove a mapping */

extern BOOL table_remove(table_t table, void *key)
{
  table_entry_t entry = table_find(table, key, 1);

  if (entry == NULL || entry->status != TABLE_ACTIVE)
    return FALSE;
  entry->status = TABLE_DELETED;
  --table->count;
  return TRUE;
}


/* table_map -- apply a function to all the mappings */

extern void table_map(table_t table, void(*fun)(void *key, void*value))
{
  size_t i;
  for (i = 0; i < table->length; i++)
    if (table->array[i].status == TABLE_ACTIVE)
      (*fun)(table->array[i].key, table->array[i].value);
}


/* table_count -- count the number of mappings in the table */

extern size_t table_count(table_t table)
{
  return table->count;
}
