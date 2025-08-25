# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import curses
from .client import DCFClient

def run_tui():
    client = DCFClient()
    def tui_wrapper(stdscr):
        curses.curs_set(0)
        stdscr.clear()
        stdscr.addstr(0, 0, "DCF TUI - Press q to quit")
        row = 2
        for peer in client.config.get("peers", []):
            stdscr.addstr(row, 0, peer)
            row += 1
        stdscr.refresh()
        while stdscr.getch() != ord('q'):
            pass
    curses.wrapper(tui_wrapper)