# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html


# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'Open Dylan'
copyright = '2011-2026, Dylan Hackers'
author = 'Dylan Hackers'
release = '2026.1.0'


# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

import os
import sys
sys.path.insert(0, os.path.abspath('../sphinx-extensions/sphinxcontrib'))
extensions = [
    'dylan.domain',
    'sphinx.ext.graphviz',
    'sphinx.ext.intersphinx',
    'sphinx_copybutton',
]
primary_domain = 'dylan'
exclude_patterns = [
    '**/sphinx_rtd_theme/**',
    '**/README.rst',
]
show_authors = True
templates_path = ['_templates']


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'furo'             # https://pradyunsg.me/furo/customisation/
html_theme_options = {
    'sidebar_hide_name': True,
    'light_logo': 'images/opendylan-light.png',
    'dark_logo': 'images/opendylan-dark.png',

    # https://pradyunsg.me/furo/customisation/edit-button/
    'source_repository': 'https://github.com/dylan-lang/opendylan',
    'source_branch': 'master',
    'source_directory': 'documentation/source',
}
html_theme_path = ['_themes']   # still needed? add furo submodule here?
html_title = 'Open Dylan'
html_static_path = ['_static']
html_favicon = '_static/favicon.ico'

# -- Options for copybutton -------------------------------------------------
# https://sphinx-copybutton.readthedocs.io/en/latest/use.html

# Skip line numbers and prompt characters
copybutton_exclude = '.linenos, .gp'

# -- Options for linkcheck --------------------------------------------------
# Ignore links that work but cannot be checked with linkcheck

linkcheck_ignore = [
    r'https://zenodo.org/.*',
    r'http://citeseerx.ist.psu.edu/.*',
    r'https://citeseerx.ist.psu.edu/.*',
    r'https://app.element.io/#/room/#dylan-language:matrix.org',
    r'https://dl.acm.org/.*',
    r'https://doi.org/.*',
    r'https://www.researchgate.net/.*',
    r'https://www.computerhope.com/issues/ch000549.htm',
    r'https://software.intel.com/.*',
    r'https://github.com/dylan-lang/opendylan/blob/.*',
    r'https://wiki.gnome.org/.*',
    r'https://matrix.to/.*',
    r'https://www.gnu.org/prep/standards/html_node/Errors.html',
    r'https://web.archive.org/web/20150216030253/http://use.perl.org/use.perl.org/_autrijus/journal/25768.html',
    r'https://stackoverflow.com/questions/tagged/dylan',
    r'https://www.corba.org/',
    r'https://www.reddit.com/r/dylanlang/',
    r'https://app.element.io/.*', # The initial '#' in their URLs confuses linkcheck
    r'https://news.ycombinator.com/item?id=8853095'
]
