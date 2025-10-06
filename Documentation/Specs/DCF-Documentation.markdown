# DCF Documentation System Design Specification
**Author:** ALH477  
**Date:** 2025-10-06  
**Version:** 1.0  

## 1. Overview

This specification outlines a documentation system for the DeMoD Communications Framework (DCF) mono repo, based on the Linux Kernel's documentation approach and adapted for a multi-language, modular FOSS project. The system uses Sphinx for generation, supporting Markdown and reStructuredText inputs, with outputs in multiple formats. It accommodates DCF's mono repo structure, including SDK submodules (e.g., C SDK, Python SDK) and shared docs like design specs, READMEs, and contribution guides.

## 2. Core Components

### 2.1 Directory Structure
```
docs/
├── conf.py                 # Sphinx configuration
├── index.rst              # Root documentation file (indexes README.md, design specs)
├── _static/              # Static assets (images, diagrams, logos)
│   └── dcf-logo.svg      # DCF project logo
├── _templates/           # Custom Sphinx templates (e.g., for SDK-specific layouts)
├── sphinx/               # Custom extensions (e.g., for Protobuf rendering)
├── guides/              # User, developer, and SDK guides
│   ├── sdk-development.rst
│   └── interoperability.md
├── api/                 # API documentation (auto-generated from source comments)
│   ├── c-sdk/
│   └── python-sdk/
├── process/            # Development process docs (e.g., CONTRIBUTING.md, CODE_OF_CONDUCT.md)
└── specs/              # Design specifications (e.g., dcf_design_spec.md)
```

Integrate with mono repo: Link to SDK-specific READMEs (e.g., c_sdk/C-SDKreadme.markdown) via Sphinx includes.

### 2.2 Configuration System

The `conf.py` file should include:

```python
# Basic project information
project = "DeMoD Communications Framework (DCF)"
copyright = "2025, DeMoD LLC"
author = "DCF Team"

# Minimum Sphinx version
needs_sphinx = "3.4.3"

# Extensions (add myst_parser for Markdown support)
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.viewcode',
    'sphinx.ext.githubpages',
    'sphinx.ext.autosectionlabel',
    'myst_parser',  # For .md files
]

# Output options
html_theme = "sphinx_rtd_theme"  # ReadTheDocs theme for DCF branding
latex_elements = {
    "papersize": "a4paper",
    "pointsize": "11pt"
}
myst_enable_extensions = ["colon_fence"]  # For Markdown code blocks
```

## 3. Documentation Types

### 3.1 Source Documentation
- Structured comments in code (e.g., Doxygen-style for C/C++, docstrings for Python/Perl)
- Support for multi-language APIs (e.g., Protobuf/gRPC defs via custom extensions)
- Automatic generation for SDKs (e.g., C SDK headers, Python modules)

### 3.2 Process Documentation
- Contribution guidelines (CONTRIBUTING.md)
- Code of conduct (CODE_OF_CONDUCT.md)
- Code style guides (e.g., clang-format for C, black for Python)
- Review processes (PR templates, CI checks)

### 3.3 User Documentation
- Installation guides (multi-language SDK setup)
- Usage tutorials (CLI/TUI examples, AUTO mode setup)
- Configuration references (JSON schema, config.json.example)
- Troubleshooting guides (e.g., P2P failover, plugin loading errors)
- SDK-specific docs (e.g., mobile bindings for Android/iOS)

## 4. Build System Integration

### 4.1 Make Targets
```makefile
# Documentation build targets
docs-html:
    sphinx-build -b html docs/ docs/_build/html

docs-pdf:
    sphinx-build -b latex docs/ docs/_build/latex
    cd docs/_build/latex && make

docs-epub:
    sphinx-build -b epub docs/ docs/_build/epub
```

Add to repo root Makefile or SDK-specific CMakeLists.txt.

### 4.2 CI/CD Integration
```yaml
documentation:
  stage: build
  script:
    - pip install -r docs/requirements.txt
    - make docs-html
  artifacts:
    paths:
      - docs/_build/html
```

Integrate with .github/workflows/ for GitHub Pages deployment.

## 5. File Formats

### 5.1 Supported Input Formats
- Markdown (.md) for READMEs and specs
- reStructuredText (.rst) for structured guides
- Source code comments (via autodoc)
- YAML/JSON for configs (rendered as tables)

### 5.2 Output Formats
- HTML (primary, for GitHub Pages/ReadTheDocs)
- PDF (for printable specs)
- ePub (for offline reading)
- Man pages (for CLI tools like dcf)

## 6. Extension System

### 6.1 Custom Directives
```python
from docutils import nodes
from sphinx.directives import Directive

class ProtobufDirective(Directive):
    """Custom directive for rendering Protobuf schemas"""
    has_content = True
    
    def run(self):
        return [nodes.literal_block(text='\n'.join(self.content), language='protobuf')]
```

### 6.2 Custom Roles
- Code references (e.g., :func:`dcf_client_send_message`)
- API links across SDKs
- Version tags (e.g., :version:`5.0.0`)
- Status indicators (e.g., :status:`experimental` for plugins)

## 7. Search and Navigation

### 7.1 Search Features
- Full-text search across SDKs
- API symbol search (e.g., DCFClient)
- Cross-reference lookup (e.g., link to C SDK from Python guide)

### 7.2 Navigation Elements
- Side navigation (grouped by SDK/language)
- Breadcrumb trails
- Version selector (for releases)
- Language selector (for code examples)

## 8. Versioning and Translation

### 8.1 Version Control
- Align docs with code releases (e.g., tag v5.0.0)
- Branch-specific docs (e.g., main for latest)
- Archive older versions via ReadTheDocs

### 8.2 Translation Support
- Multi-language builds (start with English)
- Translation workflow (e.g., gettext for rst)
- Community contributions for translations

## 9. Implementation Guidelines

### 9.1 Documentation Standards
```rst
Component Name
=============

Description
----------
Brief component description (e.g., AUTO mode for dynamic roles).

Usage
-----
Example usage code or instructions.

.. code-block:: c

   DCFClient* client = dcf_client_new();
   dcf_client_send_message(client, "Hello", "peer1", &response);

Parameters
----------
:param name: Parameter description
:type name: Parameter type
:return: Return value description
:rtype: Return type
```

Use Markdown equivalents for .md files.

### 9.2 Review Process
1. Documentation review checklist (accuracy, completeness)
2. Technical verification (e.g., test code examples)
3. Style compliance (e.g., Markdown linting)
4. Cross-reference validation (no broken links)

## 10. Deployment

### 10.1 Hosting
- GitHub Pages for live docs
- ReadTheDocs integration
- Custom server for internal/preview builds

### 10.2 Access Control
- Public docs for FOSS community
- Version-specific access (e.g., latest vs. archived)
- API docs open (no auth)

## 11. Maintenance

### 11.1 Regular Tasks
- Dead link checking (sphinx-build -b linkcheck)
- Version updates on releases
- Style consistency (e.g., mdl for Markdown)
- Search index updates

### 11.2 Quality Metrics
- Coverage (e.g., APIs documented >80%)
- Build success rate (CI monitoring)
- Search effectiveness (user queries)
- Feedback tracking (GitHub issues)
