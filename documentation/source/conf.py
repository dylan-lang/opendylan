# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html


# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'Open Dylan'
copyright = '2011-2024, Dylan Hackers'
author = 'Dylan Hackers'
release = '2024.2.0'


# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

import os
import sys
sys.path.insert(0, os.path.abspath('../sphinx-extensions/sphinxcontrib'))
extensions = [
    'dylan.domain',
    'sphinx.ext.graphviz',
    'sphinx.ext.intersphinx',
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
    'source_directory': 'source/',
}
html_theme_path = ['_themes']   # still needed? add furo submodule here?
html_title = 'Open Dylan'
html_static_path = ['_static']
html_favicon = '_static/favicon.ico'
