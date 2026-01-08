import os
from math import floor, ceil

# ===============================
# 1. CONFIGURATION
# ===============================
DATAPACK_NAME = "demod_suite_v2"
PACK_DESCRIPTION = "DeMoD v2.1: 17-Byte DCF Protocol (PHY + APP + UI)"

# Structure Dimensions
TOWER_WIDTH = 15
TOWER_LENGTH = 15
TOWER_HEIGHT = 280
FLOOR_INTERVAL = 8
DECK_INTERVAL = 8
STAGE_HEIGHT = 6 
SCOPE_INTERVAL = 5

# Protocol Definition (17 bytes = 34 nibbles)
# Standard Header (Valid)
TX_NIBBLES = [5,2,5,4,4,4,4,3,4,6,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
AUTH_NIBBLES = TX_NIBBLES

# ===============================
# 2. UTILITIES & NBT MATH
# ===============================
def mkdir(path):
    os.makedirs(path, exist_ok=True)

def write_file(path, content):
    with open(path, "w", encoding="utf-8") as f:
        f.write(content)

def get_items_needed(signal_strength, container_slots=27):
    """Calculates items needed for exact Comparator Signal Strength."""
    if signal_strength == 0: return 0
    if signal_strength == 1: return 1
    # Formula: strength = floor(1 + (items / (slots * 64)) * 14)
    total_slots_capacity = container_slots * 64
    return int(ceil(((signal_strength - 1) / 14) * total_slots_capacity))

def get_barrel_nbt(signal_strength):
    """Generates NBT for a Barrel with exact item counts."""
    if signal_strength == 0: return ""
    total_items = get_items_needed(signal_strength)
    items_nbt = []
    current_slot = 0
    while total_items > 0:
        stack_size = min(64, total_items)
        items_nbt.append(f'{{Slot:{current_slot}b,id:"minecraft:redstone",Count:{stack_size}b}}')
        total_items -= stack_size
        current_slot += 1
    return f'{{Items:[{",".join(items_nbt)}]}}'

def get_cb_nbt(command, type="impulse", auto=False, conditional=False):
    """Generates NBT for Command Blocks."""
    nbt = f'{{Command:"{command}"'
    if auto: nbt += ',auto:1b'
    if conditional: nbt += ',conditional:1b'
    nbt += '}'
    return nbt

# ===============================
# 3. DATAPACK SCAFFOLDING
# ===============================
base_path = DATAPACK_NAME
data_path = os.path.join(base_path, "data")
demod_path = os.path.join(data_path, "demod/functions")
tags_path = os.path.join(data_path, "minecraft/tags/functions")

mkdir(demod_path)
mkdir(os.path.dirname(tags_path))

# pack.mcmeta
write_file(os.path.join(base_path, "pack.mcmeta"),
           f'{{"pack": {{"pack_format": 10, "description": "{PACK_DESCRIPTION}"}}}}')

# load.json
write_file(os.path.join(tags_path, "load.json"),
           '{"values": ["demod:load"]}')

# load.mcfunction (Initialization)
init_cmds = [
    'scoreboard objectives add demood dummy {"text":"DeMoD Protocol"}',
    'gamerule commandBlockOutput false',
    'tellraw @a {"text":"[DeMoD] Suite Loaded. Run /function demod:install_all","color":"green"}'
]
write_file(os.path.join(demod_path, "load.mcfunction"), "\n".join(init_cmds))

# ===============================
# 4. MODULE: INSTALL TOWER (PHY)
# ===============================
def generate_tower():
    lines = []
    lines.append("# DeMoD PHY LAYER: TOWER INSTALLER")
    
    # Structure
    lines.append(f"fill ~0 ~0 ~0 ~{TOWER_WIDTH-1} ~{TOWER_HEIGHT-1} ~{TOWER_LENGTH-1} minecraft:deepslate_bricks")
    lines.append(f"fill ~1 ~1 ~1 ~{TOWER_WIDTH-2} ~{TOWER_HEIGHT-2} ~{TOWER_LENGTH-2} minecraft:air")
    lines.append(f"fill ~1 ~1 ~1 ~1 ~{TOWER_HEIGHT-2} ~1 minecraft:ladder[facing=east]") # Service Ladder

    # Interior Floors
    for y in range(FLOOR_INTERVAL, TOWER_HEIGHT, FLOOR_INTERVAL):
        lines.append(f"fill ~1 ~{y} ~1 ~{TOWER_WIDTH-2} ~{y} ~{TOWER_LENGTH-2} minecraft:smooth_stone_slab[type=bottom]")
        lines.append(f"setblock ~1 ~{y} ~1 minecraft:ladder[facing=east]")

    # Glass Decks
    for y in range(DECK_INTERVAL, TOWER_HEIGHT, DECK_INTERVAL):
        lines.append(f"fill ~5 ~{y} ~5 ~9 ~{y} ~9 minecraft:glass")

    # The 34 Stages (Nibbles)
    for idx in range(len(TX_NIBBLES)):
        y = 2 + idx * STAGE_HEIGHT
        tx_nbt = get_barrel_nbt(TX_NIBBLES[idx])
        auth_nbt = get_barrel_nbt(AUTH_NIBBLES[idx])

        # TX Side
        lines.append(f'setblock ~6 ~{y} ~6 minecraft:barrel[facing=east]{tx_nbt}')
        lines.append(f"setblock ~7 ~{y} ~6 minecraft:comparator[facing=east,mode=compare]")
        lines.append(f"setblock ~7 ~{y+1} ~6 minecraft:observer[facing=up]")
        lines.append(f"setblock ~7 ~{y+2} ~6 minecraft:repeater[delay=2,facing=south]")
        lines.append(f"setblock ~7 ~{y+3} ~6 minecraft:redstone_wire")

        # Auth Side
        lines.append(f'setblock ~8 ~{y} ~6 minecraft:barrel[facing=west]{auth_nbt}')
        lines.append(f"setblock ~9 ~{y} ~6 minecraft:comparator[facing=west,mode=compare]")
        lines.append(f"setblock ~9 ~{y+1} ~6 minecraft:observer[facing=up]")
        lines.append(f"setblock ~9 ~{y+2} ~6 minecraft:repeater[delay=2,facing=south]")
        lines.append(f"setblock ~9 ~{y+3} ~6 minecraft:redstone_wire")

        # Create Pulse Reset Functions (called by Schedule)
        # Note: We don't schedule them during install, the running machine does that.
        reset_tx_path = os.path.join(demod_path, f"reset_tx_{idx}.mcfunction")
        reset_auth_path = os.path.join(demod_path, f"reset_auth_{idx}.mcfunction")
        write_file(reset_tx_path, f"setblock ~7 ~{y} ~6 minecraft:air") # Breaks signal
        write_file(reset_auth_path, f"setblock ~9 ~{y} ~6 minecraft:air")

        # Debug Visualization
        if idx % SCOPE_INTERVAL == 0:
            scope_y = y + 4
            lines.append(f"setblock ~5 ~{scope_y} ~6 minecraft:shroomlight")
            lines.append(f"setblock ~10 ~{scope_y} ~6 minecraft:shroomlight")

    # BORE Clock Input (Base)
    lines.append(f"setblock ~5 ~1 ~1 minecraft:repeater[delay=3,facing=south]")
    lines.append(f"setblock ~5 ~2 ~1 minecraft:redstone_wire")

    # Jitter Buffer (Top)
    lines.append(f"setblock ~5 ~230 ~5 minecraft:dropper[facing=up]")
    lines.append(f"setblock ~5 ~229 ~5 minecraft:hopper[facing=down]") # Loop

    write_file(os.path.join(demod_path, "install_tower.mcfunction"), "\n".join(lines))

# ===============================
# 5. MODULE: SMART DECODER (APP)
# ===============================
def generate_decoder():
    lines = []
    lines.append("# DeMoD APP LAYER: DECODER LOGIC")
    base_y = 245
    
    # Platform Clearing
    lines.append(f"fill ~5 ~{base_y} ~5 ~10 ~{base_y+10} ~10 minecraft:air")
    lines.append(f"fill ~5 ~{base_y-1} ~5 ~10 ~{base_y-1} ~10 minecraft:iron_block")

    # Input Feed
    lines.append(f"setblock ~7 ~{base_y} ~6 minecraft:redstone_wire")

    # 1. RESET Logic (Signal 15) -> West
    lines.append(f"setblock ~6 ~{base_y} ~6 minecraft:comparator[facing=west,mode=compare]")
    lines.append(f'setblock ~5 ~{base_y} ~6 minecraft:command_block[facing=west]{get_cb_nbt("scoreboard players reset @a demood")}')
    lines.append(f'setblock ~4 ~{base_y} ~6 minecraft:chain_command_block[facing=west]{get_cb_nbt("tellraw @a {\\"text\\":\\"[DeMoD] Ready for Packet\\",\\"color\\":\\"gray\\"}", "chain", True)}')

    # 2. PAYLOAD Logic (Signal 10) -> South (Decay via wire length)
    lines.append(f"setblock ~7 ~{base_y} ~7 minecraft:redstone_wire")
    lines.append(f"setblock ~7 ~{base_y} ~8 minecraft:redstone_wire")
    lines.append(f"setblock ~7 ~{base_y} ~9 minecraft:redstone_wire")
    lines.append(f"setblock ~7 ~{base_y} ~10 minecraft:comparator[facing=east,mode=compare]")
    # Reward
    lines.append(f'setblock ~8 ~{base_y} ~10 minecraft:command_block[facing=east]{get_cb_nbt("give @p diamond 1")}')
    # FX
    lines.append(f'setblock ~9 ~{base_y} ~10 minecraft:chain_command_block[facing=east]{get_cb_nbt("playsound block.note_block.chime master @a ~ ~ ~ 2 2", "chain", True)}')

    # 3. HEARTBEAT Logic (Signal 1) -> East
    lines.append(f"setblock ~8 ~{base_y} ~6 minecraft:comparator[facing=east,mode=compare]")
    # Needs Redstone (Purple) so it stops when signal dies
    lines.append(f'setblock ~9 ~{base_y} ~6 minecraft:repeating_command_block[facing=east]{{Command:"title @a actionbar {\\"text\\":\\"<DeMoD> Signal Live\\",\\"color\\":\\"green\\"}"}}')

    write_file(os.path.join(demod_path, "install_decoder.mcfunction"), "\n".join(lines))

# ===============================
# 6. MODULE: CONTROL PANEL (UI)
# ===============================
def generate_control_panel():
    lines = []
    lines.append("# DeMoD UI LAYER: CONTROL PANEL")
    
    # Build a small booth at ~3 ~0 ~-3 (Front Left of tower)
    lines.append("fill ~2 ~0 ~-5 ~8 ~0 ~-2 minecraft:polished_blackstone")
    lines.append("fill ~2 ~1 ~-5 ~8 ~3 ~-2 minecraft:tinted_glass")
    lines.append("fill ~3 ~1 ~-4 ~7 ~2 ~-3 minecraft:air") # Inside space

    # 1. Start Button (Connects to Tower Input at ~5 ~1 ~1)
    lines.append("setblock ~5 ~2 ~-2 minecraft:warped_button[facing=north]")
    lines.append("setblock ~5 ~2 ~-1 minecraft:redstone_lamp")
    # Wiring from button to tower BORE input
    lines.append("fill ~5 ~1 ~-1 ~5 ~1 ~0 minecraft:redstone_wire")
    
    # 2. Signs
    lines.append('setblock ~4 ~2 ~-2 minecraft:oak_wall_sign[facing=north]{messages:["{\\"text\\":\\"START\\",\\"bold\\":true}","{\\"text\\":\\"TRANSMISSION\\"}","{\\"text\\":\\"-----------T\\"}","{\\"text\\":\\"DeMoD v2\\"}"]}')
    lines.append('setblock ~6 ~2 ~-2 minecraft:oak_wall_sign[facing=north]{messages:["{\\"text\\":\\"STATUS\\",\\"bold\\":true}","{\\"text\\":\\"Check Actionbar\\"}","{\\"text\\":\\"for Signal\\"}","{\\"text\\":\\"Integrity\\"}"]}')

    # 3. Teleport user to panel
    lines.append("tp @s ~5 ~1 ~-6 180 0")
    lines.append('tellraw @a {"text":"[DeMoD] Control Panel Installed. Press Button to Transmit.","color":"aqua"}')

    write_file(os.path.join(demod_path, "install_control_panel.mcfunction"), "\n".join(lines))

# ===============================
# 7. MASTER INSTALLER
# ===============================
def generate_master_install():
    lines = []
    lines.append("function demod:install_tower")
    lines.append("function demod:install_decoder")
    lines.append("function demod:install_control_panel")
    lines.append('tellraw @a {"text":"=================================","color":"dark_green"}')
    lines.append('tellraw @a {"text":"      DeMoD FULL SUITE INSTALLED ","color":"green","bold":true}')
    lines.append('tellraw @a {"text":"=================================","color":"dark_green"}')
    
    write_file(os.path.join(demod_path, "install_all.mcfunction"), "\n".join(lines))

# ===============================
# EXECUTE GENERATION
# ===============================
generate_tower()
generate_decoder()
generate_control_panel()
generate_master_install()

print(f"✅ Production Datapack '{DATAPACK_NAME}' generated successfully.")
print(f"📂 Location: {os.path.abspath(base_path)}")
