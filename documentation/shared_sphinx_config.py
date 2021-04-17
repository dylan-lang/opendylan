# Sphinx configuration shared by all Dylan docs.

# This file is imported by each Sphinx conf.py, usually with "from ... import *",
# so remember to use leading underscore for _private variables.

# Add any Sphinx extension module names here, as strings. They can be extensions
# coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = [
    'dylan.domain',
    'sphinx.ext.intersphinx'    # eases linking to other document sets.
]

# For any key in this map you can link to objects in the corresponding document
# using syntax like :func:`http:add-resource` or
# :doc:`building-with-duim:intro` where "http" and "building-with-duim" are
# keys in the map. HOWEVER, I (cgay) have not been able to get :class: links to
# work. Maybe something to do with the < and > characters? Needs investigation,
# but SOME working links are better than none. I have verified that :func: and
# :var: refs work.
#
# Note: use `python -m sphinx.ext.intersphinx
# https://opendylan.org/documentation/http/objects.inv` (for example) to print
# a list of targets in one of these mappings. Useful for debugging.
#
intersphinx_mapping = {
    'building-with-duim': (
        'https://opendylan.org/documentation/building-with-duim/', None),
    'corba': (
        'https://opendylan.org/documentation/corba-guide/', None),
    'duim': (
        'https://opendylan.org/documentation/duim-reference/', None),
    'getting-started-cli': (
        'https://opendylan.org/documentation/getting-started-cli/', None),
    'getting-started-ide': (
        'https://opendylan.org/documentation/getting-started-ide/', None),
    'hacker-guide': (
        'https://opendylan.org/documentation/hacker-guide/', None),
    'intro-dylan': (
        'https://opendylan.org/documentation/intro-dylan/', None),
    'library-reference': (
        'https://opendylan.org/documentation/library-reference/', None),
    'release-notes': (
        'https://opendylan.org/documentation/release-notes/', None),
    'style-guide': (
        'https://opendylan.org/documentation/style-guide/', None),

    # So-called "external libraries"
    'binary-data': ('https://opendylan.org/documentation/binary-data/', None),
    'concurrency': ('https://opendylan.org/documentation/concurrency/', None),
    'http': ('https://opendylan.org/documentation/http/', None),
    'melange': ('https://opendylan.org/documentation/melange/', None),
    'objc-dylan': ('https://opendylan.org/documentation/objc-dylan/', None),
    'statistics': ('https://opendylan.org/documentation/statistics/', None),
    'testworks': ('https://opendylan.org/documentation/testworks/', None),
    'tracing': ('https://opendylan.org/documentation/tracing/', None)
}
