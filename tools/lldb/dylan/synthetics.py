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
      class_name = dylan_object_class_name(value)
      new_class = SYNTHETIC_CLASS_TABLE.get(class_name, SyntheticObject)
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
      slot_data = self.slots[index]
      offset = (slot_data[0] + 1) * self.dylan_value_type.GetByteSize()
      return self.value.CreateChildAtOffset('[%s]' % slot_data[1],
                                            offset, self.dylan_value_type)
    return None

  def has_children(self):
    return True

  def update(self):
    target = lldb.debugger.GetSelectedTarget()
    self.dylan_value_type = target.FindFirstType('dylan_value')

    iclass = dylan_object_implementation_class(self.value)
    slot_descriptors = dylan_implementation_class_instance_slot_descriptors(iclass)
    slot_count = dylan_vector_size(slot_descriptors)
    self.slots = []
    for index in range(0, slot_count):
      slot_descriptor = dylan_vector_element(slot_descriptors, index)
      slot_name = dylan_slot_descriptor_name(slot_descriptor)
      self.slots.append([index, slot_name])

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
  '<empty-list>': SyntheticHideChildren,
  '<simple-object-vector>': SyntheticSimpleObjectVector
}
