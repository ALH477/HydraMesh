# DeMoD 17-Byte Communication Framework (DCF)

**Reference Implementation — Minecraft Redstone**

**Version**: 3.0
**License**: LGPL-3.0 (see LICENSE)
**Implementation Medium**: Minecraft Java Edition Redstone
**Tested On**: Java 1.21.4 (Singleplayer Creative, Superflat)

---

## 1. Purpose & Scope

The **DeMoD 17-Byte Communication Framework (DCF)** is a **deterministic, time-framed analog communication protocol** with a **physical reference implementation** built entirely from Minecraft redstone mechanics.

This repository provides:

* A **formal protocol description**
* A **complete step-by-step build guide**
* A **reference implementation** enforcing protocol correctness via world physics

The build intentionally favors **clarity, determinism, and fault enforcement** over compactness.

---

## 2. What This Is (and Is Not)

### This *is*

* A serialized analog protocol
* A fixed-length framed transmission
* Physically enforced validation
* Jitter-tolerant replay buffering
* A hardware-style reference build

### This is *not*

* Cryptography
* Compression
* Asynchronous signaling
* Computational universality

All terminology (e.g. *authentication*, *demodulation*) is used **by analogy** to real-world systems.

---

## 3. Protocol Specification

### 3.1 Payload

* **Length**: 17 bytes (fixed)
* **Encoding**: High nibble first, then low nibble
* **Total symbols**: 34 nibbles

### 3.2 Header Value (Hex)

```
0x5254444346010000000000000000000001
```

### 3.3 Nibble Sequence

```
[5,2, 5,4, 4,4, 4,3, 4,6, 0,1,
 0,0, 0,0, 0,0, 0,0, 0,0,
 0,0, 0,0, 0,0, 0,0,
 0,1]
```

---

## 4. Analog Encoding Model

Each nibble is encoded as a **redstone signal strength** using a **barrel + comparator** pair.

### 4.1 Comparator Formula (Java Edition)

```
signal = 0                          if empty
signal = min(14, floor(14 × items / 576))
```

* Barrels: 9 slots × 64 items = 576 max
* Signal strength **15 is never used**
* Protocol values ≤ 6 for safety margin

### 4.2 Exact Item Counts (Stone)

| Signal | Items |
| -----: | ----: |
|      0 |     0 |
|      1 |    42 |
|      2 |    83 |
|      3 |   124 |
|      4 |   165 |
|      5 |   206 |
|      6 |   247 |

> **Important**:
> Use *exactly* these minimums. Overfilling can cause rounding drift.

---

## 5. World Setup (Required)

1. **Create World**

   * Superflat → Default
   * Preset:

     ```
     minecraft:bedrock,2*dirt,grass_block;1;village
     ```

2. **Game Rules**

   ```
   /gamemode creative
   /gamerule randomTickSpeed 1
   /gamerule doDaylightCycle false
   /gamerule doMobSpawning false
   /gamerule mobGriefing false
   /gamerule keepInventory true
   ```

3. **Prepare Build Area**

   ```
   /tp @s 0 100 0
   /fill -50 90 -50 50 110 300 air
   ```

---

## 6. Bill of Materials

```
barrel ×68
comparator ×170
repeater ×320
observer ×90
redstone_dust ×64
redstone_torch ×16
sticky_piston ×35
gravel ×35
hopper ×8
dropper ×4
lever ×3
target ×10
glowstone ×3
tnt ×1
stone ×1000+
```

---

## 7. System Architecture

```
Clock → TX (34) → Auth (34) → Buffer → Decoder → Payload
```

Each section is physically isolated and unidirectional.

---

## 8. Phase 1 — Master Clock (BORE Scheduler)

**Location**: Z = 0 → 10
**Purpose**: Provide a stable 3-tick global clock

### Build Steps

1. Place a repeater at `(0,4,0)` set to **3 ticks**
2. Dust at `(1,4,0)`
3. Redstone torch at `(1,3,-1)`
4. Observer at `(2,4,0)` facing **-Z**
5. Repeater at `(3,4,0)` set to **1 tick**
6. Dust at `(4,4,0)` and `(5,4,0)`
7. Loop dust back to repeater input
8. Lever at `(0,4,-1)`

**Output line**: Dust at `(5,5,0)`

Expected: steady pulse train, 2 ticks high / 1 tick low.

---

## 9. Phase 2 — Transmitter (TX)

**Location**: Z = 10 → 110
**Stages**: 34 (one per nibble)

### Stage Layout

For stage `i`:

```
Zb = 10 + 3*i
```

#### Per-Stage Build

1. Barrel at `(1,4,Zb)` → fill per table
2. Comparator at `(2,4,Zb)` facing +Z
3. Torch at `(2,3,Zb)` (locks output)
4. Repeater at `(3,4,Zb)` set to **2 ticks**
5. Observer at `(0,4,Zb)` facing +Z
6. Repeater at `(1,4,Zb+1)` set to **1 tick**
7. Dust at `(3,4,Zb+1)`
8. Dust up to `(3,5,Zb+1)` → SSS bus
9. Advance dust to next stage

Stage 0 is triggered by a button instead of advance input.

---

## 10. Phase 3 — Authentication Gate

**Location**: Z = 125 → 225
**Purpose**: Reject any incorrect symbol physically

### Per-Stage Layout

```
SSS → subtract comparator → torch → piston → gravel
```

#### Steps

1. Reference barrel `(2,4,Zb)` filled identically to TX
2. Comparator `(3,4,Zb)` facing +Z
3. Tap repeater `(1,5,Zb)`
4. Subtractor comparator `(2,5,Zb)`
5. Torch `(3,5,Zb+1)`
6. Sticky piston `(4,5,Zb+1)`
7. Gravel `(5,5,Zb+1)` under bus

Any mismatch breaks the line permanently until reset.

---

## 11. Phase 4 — Adaptive Jitter Buffer

**Location**: Z = 230 → 245
**Purpose**: Absorb lag and replay clean timing

### Components

* Observer edge detector
* Dropper + hopper FIFO
* Independent 6-tick release clock

Output is a regenerated, stable SSS bus.

---

## 12. Phase 5 — Decoder & Payload

**Location**: Z = 245 → 260

### Thresholds

* **Signal 14** → Reset
* **Signal 10** → TNT (demo payload)
* **Signal 6** → Heartbeat indicator

All payloads are downstream of authentication and buffering.

---

## 13. Testing & Verification

1. Start clock
2. Observe 34 pulses over ~10 seconds
3. Confirm:

   * Auth passes
   * Buffer replays
   * Payload triggers

### Tamper Test

* Remove **1 stone** from any barrel
* Result: auth breaks, no payload

---

## 14. Known Limitations

* Fixed frame length
* Fixed buffer depth
* Requires careful barrel filling
* Chunk borders require `/forceload` in SMP

---

## 15. License Notes (LGPL)

This implementation is licensed under **LGPL-3.0**.

You may:

* Study
* Modify
* Redistribute

Provided:

* Changes are documented
* The license remains intact
* Attribution is preserved

---

## 16. Final Notes

This project demonstrates that **Minecraft redstone can enforce protocol rules physically**, not just logically.

It is a **reference implementation**, intentionally explicit and verbose, designed to be read like a real systems framework—not a trick build.

**17 bytes. One clock. No forgiveness.**
