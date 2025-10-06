# docs/conf.py
import sys
import os
sys.path.insert(0, os.path.abspath('sphinx'))

project = "DeMoD Communications Framework (DCF)"
copyright = "2025, DeMoD LLC"
author = "DCF Team"
release = "5.0.0"

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.viewcode',
    'sphinx.ext.githubpages',
    'sphinx.ext.autosectionlabel',
    'myst_parser',
    'custom_directives',  # Custom directive module
]

templates_path = ['_templates']
exclude_patterns = []

needs_sphinx = "3.4.3"
myst_enable_extensions = ["colon_fence"]

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

latex_elements = {
    "papersize": "a4paper",
    "pointsize": "11pt"
}
