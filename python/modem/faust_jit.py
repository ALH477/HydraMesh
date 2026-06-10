#!/usr/bin/env python3
"""FaustJIT — compile a Faust DSP to a native shared library and drive it via ctypes.

Extracted verbatim from the DCF acoustic modem (main.py) so other tools — e.g. the
DCF-Audio PM codec (codec_id 2) reference synthesis — can reuse Faust JIT compilation
without importing the whole modem. See codec/faust/dcf_pm_codec.dsp and DCF_AUDIO_SPEC.md.

License: LGPL-3.0 | (c) 2025 DeMoD LLC
"""
import ctypes
import hashlib
import platform
import subprocess
import tempfile
from pathlib import Path

SAMPLE_RATE = 48000

# ANSI colors (kept so the extracted prints match the modem's output).
C_RESET = "\033[0m"
C_CYAN = "\033[96m"
C_GREEN = "\033[92m"
C_RED = "\033[91m"
C_DIM = "\033[90m"


class FaustJIT:
    """
    Compiles a Faust DSP file to a native shared library via:
        faust -lang c -cn DCFModem -> .c -> gcc -shared -> .so/.dylib
    Then wraps the compiled DSP via ctypes for real-time use.
    """

    # Faust C API function pointer types for UIGlue
    OPEN_BOX_FN  = ctypes.CFUNCTYPE(None, ctypes.c_void_p, ctypes.c_char_p)
    CLOSE_BOX_FN = ctypes.CFUNCTYPE(None, ctypes.c_void_p)
    ADD_WIDGET_FN = ctypes.CFUNCTYPE(
        None, ctypes.c_void_p, ctypes.c_char_p,
        ctypes.POINTER(ctypes.c_float),
        ctypes.c_float, ctypes.c_float, ctypes.c_float, ctypes.c_float
    )
    ADD_BUTTON_FN = ctypes.CFUNCTYPE(
        None, ctypes.c_void_p, ctypes.c_char_p,
        ctypes.POINTER(ctypes.c_float)
    )
    ADD_BARGRAPH_FN = ctypes.CFUNCTYPE(
        None, ctypes.c_void_p, ctypes.c_char_p,
        ctypes.POINTER(ctypes.c_float),
        ctypes.c_float, ctypes.c_float
    )
    ADD_SOUNDFILE_FN = ctypes.CFUNCTYPE(
        None, ctypes.c_void_p, ctypes.c_char_p,
        ctypes.c_char_p, ctypes.c_void_p
    )
    DECLARE_FN = ctypes.CFUNCTYPE(
        None, ctypes.c_void_p,
        ctypes.POINTER(ctypes.c_float),
        ctypes.c_char_p, ctypes.c_char_p
    )

    class UIGlue(ctypes.Structure):
        """Faust C API UIGlue struct for parameter discovery."""
        pass

    def __init__(self, dsp_path, class_name="DCFModem", sample_rate=SAMPLE_RATE):
        self.dsp_path = Path(dsp_path)
        self.cn = class_name
        self.sr = sample_rate
        self.lib = None
        self.dsp = None
        self.params = {}  # name -> ctypes float pointer
        self._callbacks = []  # prevent GC of callback refs

        if not self.dsp_path.exists():
            raise FileNotFoundError(f"DSP file not found: {self.dsp_path}")

        self._check_tools()
        self._compile()
        self._load()
        self._discover_params()
        self._init_dsp()

    def _check_tools(self):
        """Verify faust and gcc are available."""
        for tool in ("faust", "gcc"):
            if subprocess.run(["which", tool], capture_output=True).returncode != 0:
                raise RuntimeError(
                    f"'{tool}' not found on PATH. "
                    f"Install via your package manager or NixOS configuration."
                )

    def _compile(self):
        """Faust -> C -> shared library."""
        # Use a cache directory based on DSP content hash
        dsp_hash = hashlib.md5(self.dsp_path.read_bytes()).hexdigest()[:12]
        self.build_dir = Path(tempfile.gettempdir()) / f"dcf_modem_{dsp_hash}"
        self.build_dir.mkdir(exist_ok=True)

        self.c_path = self.build_dir / f"{self.cn}.c"
        ext = ".dylib" if platform.system() == "Darwin" else ".so"
        self.so_path = self.build_dir / f"{self.cn}{ext}"

        # Skip if already compiled (cache hit)
        if self.so_path.exists():
            print(f"  {C_DIM}Cache hit: {self.so_path}{C_RESET}")
            return

        print(f"  {C_CYAN}Compiling Faust -> C ...{C_RESET}")
        r = subprocess.run(
            ["faust", "-lang", "c", "-cn", self.cn,
             "-o", str(self.c_path), str(self.dsp_path)],
            capture_output=True, text=True
        )
        if r.returncode != 0:
            print(f"  {C_RED}Faust compilation failed:{C_RESET}")
            print(r.stderr)
            raise RuntimeError("Faust compilation failed")

        print(f"  {C_CYAN}Compiling C -> native ({ext}) ...{C_RESET}")
        gcc_flags = ["-shared", "-fPIC", "-O3", "-march=native"]
        if platform.system() == "Darwin":
            gcc_flags.append("-dynamiclib")
        r = subprocess.run(
            ["gcc"] + gcc_flags +
            ["-o", str(self.so_path), str(self.c_path), "-lm"],
            capture_output=True, text=True
        )
        if r.returncode != 0:
            print(f"  {C_RED}GCC compilation failed:{C_RESET}")
            print(r.stderr)
            raise RuntimeError("GCC compilation failed")

        print(f"  {C_GREEN}Compiled: {self.so_path}{C_RESET}")

    def _load(self):
        """Load the shared library and bind Faust C API functions."""
        self.lib = ctypes.CDLL(str(self.so_path))
        cn = self.cn

        # Bind API functions
        self._new = getattr(self.lib, f"newC{cn}")
        self._new.restype = ctypes.c_void_p

        self._init = getattr(self.lib, f"initC{cn}")
        self._init.argtypes = [ctypes.c_void_p, ctypes.c_int]

        self._compute = getattr(self.lib, f"computeC{cn}")
        self._compute.argtypes = [
            ctypes.c_void_p, ctypes.c_int,
            ctypes.POINTER(ctypes.POINTER(ctypes.c_float)),
            ctypes.POINTER(ctypes.POINTER(ctypes.c_float)),
        ]

        self._delete = getattr(self.lib, f"deleteC{cn}")
        self._delete.argtypes = [ctypes.c_void_p]

        self._num_inputs = getattr(self.lib, f"getNumInputsC{cn}")
        self._num_inputs.restype = ctypes.c_int
        self._num_inputs.argtypes = [ctypes.c_void_p]

        self._num_outputs = getattr(self.lib, f"getNumOutputsC{cn}")
        self._num_outputs.restype = ctypes.c_int
        self._num_outputs.argtypes = [ctypes.c_void_p]

        self._build_ui = getattr(self.lib, f"buildUserInterfaceC{cn}")
        self._build_ui.argtypes = [ctypes.c_void_p, ctypes.c_void_p]

        # Allocate DSP instance
        self.dsp = self._new()
        self.n_inputs = self._num_inputs(self.dsp)
        self.n_outputs = self._num_outputs(self.dsp)

    def _discover_params(self):
        """Use UIGlue to discover parameter names and memory addresses."""
        params = self.params

        # Define UIGlue struct fields AFTER callback types are ready
        FaustJIT.UIGlue._fields_ = [
            ("uiInterface",       ctypes.c_void_p),
            ("openTabBox",        FaustJIT.OPEN_BOX_FN),
            ("openHorizontalBox", FaustJIT.OPEN_BOX_FN),
            ("openVerticalBox",   FaustJIT.OPEN_BOX_FN),
            ("closeBox",          FaustJIT.CLOSE_BOX_FN),
            ("addButton",         FaustJIT.ADD_BUTTON_FN),
            ("addCheckButton",    FaustJIT.ADD_BUTTON_FN),
            ("addVerticalSlider", FaustJIT.ADD_WIDGET_FN),
            ("addHorizontalSlider", FaustJIT.ADD_WIDGET_FN),
            ("addNumEntry",       FaustJIT.ADD_WIDGET_FN),
            ("addHorizontalBargraph", FaustJIT.ADD_BARGRAPH_FN),
            ("addVerticalBargraph",   FaustJIT.ADD_BARGRAPH_FN),
            ("addSoundfile",      FaustJIT.ADD_SOUNDFILE_FN),
            ("declare",           FaustJIT.DECLARE_FN),
        ]

        def noop_open(ui, label):
            pass
        def noop_close(ui):
            pass
        def add_slider(ui, label, zone, init, mn, mx, step):
            name = label.decode("utf-8") if label else "?"
            params[name] = zone
        def add_button(ui, label, zone):
            name = label.decode("utf-8") if label else "?"
            params[name] = zone
        def add_bargraph(ui, label, zone, mn, mx):
            name = label.decode("utf-8") if label else "?"
            params[name] = zone
        def noop_soundfile(ui, label, url, sf):
            pass
        def noop_declare(ui, zone, key, value):
            pass

        # Create callback instances and prevent garbage collection
        cb_open  = FaustJIT.OPEN_BOX_FN(noop_open)
        cb_close = FaustJIT.CLOSE_BOX_FN(noop_close)
        cb_slider = FaustJIT.ADD_WIDGET_FN(add_slider)
        cb_button = FaustJIT.ADD_BUTTON_FN(add_button)
        cb_barg  = FaustJIT.ADD_BARGRAPH_FN(add_bargraph)
        cb_sf    = FaustJIT.ADD_SOUNDFILE_FN(noop_soundfile)
        cb_decl  = FaustJIT.DECLARE_FN(noop_declare)
        self._callbacks = [cb_open, cb_close, cb_slider, cb_button, cb_barg, cb_sf, cb_decl]

        glue = FaustJIT.UIGlue(
            uiInterface=None,
            openTabBox=cb_open,
            openHorizontalBox=cb_open,
            openVerticalBox=cb_open,
            closeBox=cb_close,
            addButton=cb_button,
            addCheckButton=cb_button,
            addVerticalSlider=cb_slider,
            addHorizontalSlider=cb_slider,
            addNumEntry=cb_slider,
            addHorizontalBargraph=cb_barg,
            addVerticalBargraph=cb_barg,
            addSoundfile=cb_sf,
            declare=cb_decl,
        )

        self._build_ui(self.dsp, ctypes.byref(glue))

    def _init_dsp(self):
        """Initialize the DSP at the configured sample rate."""
        self._init(self.dsp, self.sr)

    def set_param(self, name, value):
        """Set a Faust parameter by name."""
        if name in self.params:
            self.params[name][0] = ctypes.c_float(value)
        else:
            raise KeyError(f"Unknown parameter: '{name}'. Available: {list(self.params.keys())}")

    def get_param(self, name):
        """Read a Faust parameter by name."""
        if name in self.params:
            return float(self.params[name][0])
        raise KeyError(f"Unknown parameter: '{name}'")

    def compute(self, n_frames, input_buf, output_bufs):
        """
        Run the Faust DSP compute function.

        input_buf:   numpy float32 array, shape (n_frames,) for mono
        output_bufs: list of numpy float32 arrays, each shape (n_frames,)
        """
        # Build input pointer array
        in_ptrs = (ctypes.POINTER(ctypes.c_float) * self.n_inputs)()
        in_ptrs[0] = input_buf.ctypes.data_as(ctypes.POINTER(ctypes.c_float))

        # Build output pointer array
        out_ptrs = (ctypes.POINTER(ctypes.c_float) * self.n_outputs)()
        for i in range(self.n_outputs):
            out_ptrs[i] = output_bufs[i].ctypes.data_as(ctypes.POINTER(ctypes.c_float))

        self._compute(self.dsp, n_frames, in_ptrs, out_ptrs)

    def cleanup(self):
        if self.dsp:
            self._delete(self.dsp)
            self.dsp = None

    def __del__(self):
        self.cleanup()
