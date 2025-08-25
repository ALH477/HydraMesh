# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import importlib.util
import os

class PluginManager:
    def __init__(self, plugin_paths):
        self.plugins = {}
        for name, rel_path in (plugin_paths or {}).items():
            if rel_path:
                abs_path = os.path.abspath(rel_path)
                spec = importlib.util.spec_from_file_location(name, abs_path)
                module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(module)
                if hasattr(module, 'get_plugin_version') and module.get_plugin_version() != "1.0":
                    raise ValueError(f"Incompatible plugin {name}")
                self.plugins[name] = module

    def get(self, name):
        return self.plugins.get(name)