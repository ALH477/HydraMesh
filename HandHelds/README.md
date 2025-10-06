# DCF Framework for Cross-Platform Multiplayer Games (Nintendo DSi & Sony PSP)

![License: GPL-3.0](https://img.shields.io/badge/License-GPL%20v3-blue.svg)
![Rust](https://img.shields.io/badge/Rust-1.81+-orange.svg)
![Platforms: DSi, PSP](https://img.shields.io/badge/Platforms-DSi%2C%20PSP-green.svg)

## Overview

The **DCF Framework** is a lightweight, `no_std` Rust implementation of the DeMoD Communication Framework (DCF) and StreamDB, designed for ad-hoc multiplayer games on the Nintendo DSi and Sony PSP. It supports cross-platform play between these handhelds via a shared UDP protocol, optimized for low-resource environments (DSi: 16MB RAM, ARM9@133MHz; PSP: 32MB RAM, MIPS@333MHz). The framework includes a proof-of-concept **Tic-Tac-Toe** game, demonstrating turn-based multiplayer with persistent state, chat, and file transfers (e.g., images for game assets).

Built for robustness and modularity, it leverages DCF's peer-to-peer networking (with rooms and self-healing routing) and StreamDB's key-value storage (for game state and configuration). Key features include reliable message delivery (ACKs), peer discovery, and platform-abstracted GUI/input, all tailored for handheld constraints.

## Features

- **Cross-Platform Compatibility**: Seamless multiplayer between DSi and PSP using a unified UDP protocol (port 50051).
- **Ad-Hoc Networking**: Low-latency P2P communication with self-healing routing (Dijkstra-based) and rooms for session isolation.
- **Persistent Storage**: StreamDB saves game state, configuration, and metrics to SD card (DSi: FAT32, PSP: Memory Stick).
- **GUI and Input**: Platform-agnostic interface (DSi: dual-screen console/touch, PSP: single-screen text/buttons).
- **Reliability**: ACK/retry mechanism for UDP, peer discovery via broadcast, and robust error handling.
- **Modularity**: `Game` trait for pluggable game mechanics (Tic-Tac-Toe included; extendable to Pong, Chess, etc.).
- **Optimized for Handhelds**: Minimal memory (~100-200KB heap), small data (64B messages, 4KB files), VBlank sync for battery efficiency.
- **Production-Ready**: Configuration persistence, metrics, and power management (DSi lid detection).

## Supported Game Mechanics

The framework supports lightweight multiplayer games, optimized for ad-hoc WiFi (20-100ms latency, 2-4 players):

- **Turn-Based**: Tic-Tac-Toe, Chess, Cards (e.g., Poker). Low data, latency-tolerant.
- **Simple Real-Time**: Pong, Racing (with prediction/dead-reckoning). Sync positions/inputs.
- **Co-op Puzzles**: Jigsaw, multi-device displays. Share gestures/state.
- **Asymmetric**: Spaceteam-style tasks. Use rooms for teams.
- **File-Based**: Share assets (e.g., 32x32 BMP images) for maps or sprites.

## Prerequisites

- **Rust**: Nightly (for `no_std` features).
- **DSi**:
  - BlocksDS toolchain (`libnds-rs`).
  - Modded DSi with SD card (TWiLight Menu++ for .nds).
  - `cargo-nds` (`cargo install --git https://github.com/SeleDreams/cargo-nds`).
- **PSP**:
  - PSP SDK and `rust-psp` crate (`cargo install --git https://github.com/overdrivenpotato/rust-psp`).
  - Modded PSP with Memory Stick.
- **Dependencies**:
  ```toml
  [dependencies]
  libnds = { git = "https://github.com/oxcabe/libnds-rs", optional = true }
  psp = { git = "https://github.com/overdrivenpotato/rust-psp", optional = true }
  heapless = "0.8"
  lru = { version = "0.12", default-features = false }
  crc = { version = "3.2", default-features = false }
  byteorder = { version = "1.5", default-features = false }

  [features]
  dsi = ["libnds"]
  psp = ["psp"]
  ```

## Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/yourusername/dcf-framework.git
   cd dcf-framework
   ```

2. **Set Up Toolchains**:
   - DSi: Install BlocksDS (`blocksds.github.io/docs`).
   - PSP: Install PSP SDK and `rust-psp` (see `rust-psp` repo).

3. **Build**:
   - DSi: `cargo nds build --release --features=dsi`
     - Output: `target/armv5te-none-eabi/release/dcf-framework.nds`
   - PSP: `cargo build --release --target=mipsel-sony-psp-elf --features=psp`
     - Output: `target/mipsel-sony-psp-elf/release/dcf-framework`

4. **Run**:
   - DSi: Copy `.nds` to SD card, launch via TWiLight Menu++.
   - PSP: Copy `EBOOT.PBP` to Memory Stick (`ms0:/PSP/GAME/`), launch via homebrew loader.

## Usage

1. **Start the Game**:
   - On launch, devices join the "tictactoe_room" room (configurable via "/config" on SD).
   - DSi: Top screen shows board/history, bottom has keyboard/touch.
   - PSP: Single screen shows text board, buttons for input.

2. **Tic-Tac-Toe**:
   - **Input**: Enter "row,col" (e.g., "1,2") via keyboard (DSi) or buttons (PSP). Press START to send move.
   - **Reset**: Press 'r' or send "RESET" to restart.
   - **Win/Draw**: Checked locally, synced via UDP ("MOVE:pos").
   - **State**: Saved to "/game/board" (StreamDB).

3. **Chat and Files**:
   - Send messages via keyboard/buttons (ENTER to send).
   - Send files (e.g., "asset.bmp") with X button (DSi) or Cross (PSP).
   - Join rooms with Y button (DSi) or Circle (PSP).

4. **Configuration**:
   - Stored in "dcf.streamdb" ("/config"): node_id, peers, current_room.
   - Edit manually or via in-game room join.

## Example: Tic-Tac-Toe

- **Setup**: Two devices (DSi, PSP, or mixed) join "tictactoe_room".
- **Gameplay**:
  - Host (node_id="host") is X, client is O.
  - Enter move (e.g., "1,2" for row 1, col 2).
  - Board syncs via "MOVE:pos" messages (e.g., "MOVE:5").
  - Win/draw displayed in history.
- **Persistence**: Board saved to "/game/board", restored on join.
- **GUI**: DSi: Board on top screen, input on bottom. PSP: Text board, button input.

## Extending the Framework

To add new games:
1. Implement the `Game` trait:
   ```rust
   struct NewGame {
       // Game state
   }
   impl Game for NewGame {
       fn init(&mut self) { /* Reset state */ }
       fn handle_message(&mut self, msg: &DcfMessage) -> Option<heapless::String<U32>> { /* Process moves */ }
       fn update_gui(&mut self, gui: &mut dyn Gui) { /* Render */ }
       fn process_input(&mut self, input: char) -> Option<DcfMessage> { /* Handle input */ }
   }
   ```
2. Set in `DcfFramework`:
   ```rust
   framework.game = Some(Box::new(NewGame::new()));
   ```
3. Add mechanics (e.g., Pong: sync x,y,vel; use middleware for prediction).

## Optimizations for Handhelds

- **Memory**: ~100-200KB heap via `heapless` (`U8` maps, `U32` strings). 512B pages, 4KB files, 8-entry cache (~4KB).
- **CPU**: VBlank sync (60FPS), lightweight Dijkstra (O(4²)), trie lookups (~10µs).
- **Power**: Lid detection (DSi), VBlank wait for battery.
- **Network**: Reliable UDP with ACKs (3 retries, 500ms timeout). Peer discovery via "HELLO"/"HELLO_ACK".
- **Storage**: Single-file SD ("dcf.streamdb") minimizes I/O.

## Limitations

- Max 4 peers/rooms (extendable to `U8` if RAM allows).
- File transfers limited to 4KB (no chunking).
- PSP image display stubbed (needs `gu` texture rendering).
- No encryption (per DCF spec; add in middleware if needed).

## Contributing

1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/new-game`).
3. Commit changes (`git commit -m "Add Chess game"`).
4. Push (`git push origin feature/new-game`).
5. Open a Pull Request.

See `CONTRIBUTING.md` for details.

## Testing

- **Emulators**: DeSmuME (DSi), PPSSPP (PSP). Test cross-play with PPSSPP + real DSi.
- **Hardware**: Modded DSi (SD card), PSP (Memory Stick).
- **Steps**:
  1. Build and deploy for both platforms.
  2. Join "tictactoe_room" on two devices.
  3. Play Tic-Tac-Toe, send chat/files, verify persistence.

## License

Licensed under the GNU General Public License v3.0 (GPL-3.0). See `LICENSE`.

## Acknowledgments

- **DeMoD LLC**: Original DCF/StreamDB design.
- **xAI**: Contributions via Grok (AI assistance).
- **Open-Source Community**: `libnds-rs`, `rust-psp`, `heapless`, and other libraries.

**Built for retro gaming enthusiasts with ❤️ in Rust**
