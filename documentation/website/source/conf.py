# These configuration settings are documented here:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import os
import sys
sys.path.insert(0, os.path.abspath('../sphinx-extensions/sphinxcontrib'))
extensions = [
    'dylan.domain',
    'sphinx.ext.intersphinx',
]
primary_domain = 'dylan'

project = 'Open Dylan'
copyright = '2011-2023, Dylan Hackers'
version = '2023.1'
release = '2023.1'
show_authors = True

html_theme = 'furo'             # https://pradyunsg.me/furo/customisation/
html_theme_options = {
    'sidebar_hide_name': True,
    'light_logo': 'images/opendylan-light.png',
    'dark_logo': 'images/opendylan-dark.png',
}
html_theme_path = ['_themes']   # still needed? add furo submodule here?

htmlhelp_basename = 'OpenDylandoc'
html_title = 'Open Dylan'
html_static_path = ['_static']
html_favicon = '_static/favicon.ico'

templates_path = ['_templates']
