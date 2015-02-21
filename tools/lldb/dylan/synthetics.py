import lldb
from accessors import *

class SyntheticHideChildren(object):
  """A synthetic that shows no children."""
  def __init__(self, value, internal_dict):
    self.value = self.cast_value(value)
    self.update()

  def cast_value(self, value):
    return value

  def num_children(self):
    return 0

  def get_child_index(self, name):
    return -1

  def get_child_at_index(self, index):
    return None

  def has_children(self):
    return False

  def update(self):
    pass

class SyntheticDylanValue(SyntheticHideChildren):
  """A shape-shifting synthetic that doesn't do anything by itself, but changes
     class to an appropriate synthetic when there is one.
  """
  def __init__(self, value, internal_dict):
    tag = dylan_tag_bits(value)
    new_class = None
    if tag == OBJECT_TAG:
      wrapper_symbol_name = dylan_object_wrapper_symbol_name(value)
      new_class = SYNTHETIC_CLASS_TABLE.get(wrapper_symbol_name, SyntheticObject)
      if new_class:
         self.__class__ = new_class
    else:
      pass
    self.value = self.cast_value(value)
    self.update()

class SyntheticObject(object):
  """A synthetic that knows how to walk the slots of an arbitrary Dylan
     object."""
  def cast_value(self, value):
    return dylan_value_as_object(value)

  def num_children(self):
    return len(self.slots)

  def get_child_index(self, name):
    return -1

  def get_child_at_index(self, index):
    if index >= 0 and index < len(self.slots):
      slot_name = self.slots[index]
      offset = (index + 1) * self.dylan_value_type.GetByteSize()
      return self.value.CreateChildAtOffset('[%s]' % slot_name,
                                            offset, self.dylan_value_type)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')
    self.slots = dylan_object_class_slot_names(self.value)

class SyntheticSimpleObjectVector(object):
  """A synthetic for representing a <simple-object-vector>."""
  def cast_value(self, value):
    target = lldb.debugger.GetSelectedTarget()
    vector_type = target.FindFirstType('dylan_simple_object_vector').GetPointerType()
    return value.Cast(vector_type)

  def num_children(self):
    return self.element_count

  def get_child_index(self, name):
    try:
      return int(name.lstrip('[').rstrip(']'))
    except:
      return -1

  def get_child_at_index(self, index):
    if index >= 0 and index < self.element_count:
      # +1 is to skip past the object header
      offset = (index + 1) * self.dylan_value_type.GetByteSize()
      return self.value.CreateChildAtOffset('[%d]' % index, offset, self.dylan_value_type)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')
    self.element_count = dylan_vector_size(self.value)

SYNTHETIC_CLASS_TABLE = {
  'KLbyte_stringGVKdW': SyntheticHideChildren,
  'KLempty_listGVKdW': SyntheticHideChildren,
  'KLsimple_object_vectorGVKdW': SyntheticSimpleObjectVector,
  'KLsymbolGVKdW': SyntheticHideChildren
}
