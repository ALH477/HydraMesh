import os

# ===============================
# CONFIGURATION
# ===============================
DATAPACK_NAME = "demod_dcf_receiver"
# The 'Smart Decoder' is installed relative to the base of the tower.
# It targets the top section where the raw signals emerge (Y=245+)
TOWER_HEIGHT = 280 

# ===============================
# UTILITIES
# ===============================
def mkdir(path):
    os.makedirs(path, exist_ok=True)

def write_file(path, content):
    with open(path, "w") as f:
        f.write(content)

def get_cb_nbt(command, type="impulse", auto=False, conditional=False):
    """
    Helper to generate NBT for Command Blocks.
    types: impulse (orange), chain (green), repeating (purple)
    """
    nbt = f'{{Command:"{command}"'
    if auto: nbt += ',auto:1b'  # "Always Active"
    if conditional: nbt += ',conditional:1b'
    nbt += '}'
    return nbt

# ===============================
# DATAPACK GENERATION
# ===============================
base_path = DATAPACK_NAME
data_path = os.path.join(base_path, "data")
demod_path = os.path.join(data_path, "demod/functions")
minecraft_tags_path = os.path.join(data_path, "minecraft/tags/functions")

mkdir(demod_path)
mkdir(os.path.dirname(minecraft_tags_path))

# ---- pack.mcmeta ----
write_file(os.path.join(base_path, "pack.mcmeta"),
           '{"pack": {"pack_format": 10, "description": "DeMoD Smart Receiver & Command Integration"}}')

# ---- load.json ----
write_file(os.path.join(minecraft_tags_path, "load.json"),
           '{"values": ["demod:init_receiver"]}')

# ---- init_receiver.mcfunction (Scoreboard Setup) ----
init_cmds = [
    'scoreboard objectives add demood dummy {"text":"DeMoD Protocol"}',
    'tellraw @a {"text":"[DeMoD] Receiver loaded. Run /function demod:install_smart_decoder at the TOWER BASE.","color":"green"}'
]
write_file(os.path.join(demod_path, "init_receiver.mcfunction"), "\n".join(init_cmds))

# ===============================
# BUILD install_smart_decoder.mcfunction
# ===============================
# This function is designed to be run at the BASE of the tower (same spot as install_full_tower).
# It calculates the relative Y offsets to reach the top deck (approx ~245).

lines = []
lines.append("# DeMoD SMART DECODER RETROFIT")
lines.append("# Installs Command Block Logic at the top of the tower")

# Offset to the decoder platform (Matches the previous script's output location)
base_y = 245 

# 1. Clean up the old 'Analog' Decoder (TNT/Lamps)
lines.append(f"fill ~5 ~{base_y} ~5 ~10 ~{base_y+10} ~10 minecraft:air")
lines.append(f"fill ~5 ~{base_y-1} ~5 ~10 ~{base_y-1} ~10 minecraft:quartz_block") # Clean floor

# 2. Re-establish the Comparator Inputs
# We assume the Jitter Buffer output is at ~7 ~244 ~6 feeding UP or SIDEWAYS
# We will pull from the wire/container at ~7 ~{base_y-1} ~6
lines.append(f"setblock ~7 ~{base_y} ~6 minecraft:redstone_wire")

# ==========================================================
# LOGIC CHAIN A: RESET (Signal 15)
# ==========================================================
# Comparator reading Signal 15
lines.append(f"setblock ~6 ~{base_y} ~6 minecraft:comparator[facing=west,mode=compare]")
# Impulse CB (Reset Obj) - Triggered by Pulse
cmd_reset = "scoreboard players reset @a demood"
lines.append(f'setblock ~5 ~{base_y} ~6 minecraft:command_block[facing=west]{get_cb_nbt(cmd_reset)}')
# Chain CB (Log)
cmd_log = 'tellraw @a {"text":"[DeMoD] Signal Reset.","color":"gray"}'
lines.append(f'setblock ~4 ~{base_y} ~6 minecraft:chain_command_block[facing=west]{get_cb_nbt(cmd_log, type="chain", auto=True)}')

# ==========================================================
# LOGIC CHAIN B: PAYLOAD (Signal 10)
# ==========================================================
# Comparator reading Signal 10 (needs distance to decay signal 15->10, or explicit subtractor)
# For simplicity in this generated script, we use a calibrated subtractor block.
# We place a weighted pressure plate or side-input to define the threshold.
# HERE: We use distance. Wire length 5 drops signal 15->10.

lines.append(f"setblock ~7 ~{base_y} ~7 minecraft:redstone_wire")
lines.append(f"setblock ~7 ~{base_y} ~8 minecraft:redstone_wire")
lines.append(f"setblock ~7 ~{base_y} ~9 minecraft:redstone_wire") # Signal has decayed
lines.append(f"setblock ~7 ~{base_y} ~10 minecraft:comparator[facing=east,mode=compare]")

# Impulse CB (Give Reward)
cmd_payload = "give @p diamond 1 {display:{Name:'{\"text\":\"DeMoD Packet\"}'}}"
lines.append(f'setblock ~8 ~{base_y} ~10 minecraft:command_block[facing=east]{get_cb_nbt(cmd_payload)}')

# Chain CB (Visual FX)
cmd_fx = "particle firework ~ ~2 ~ 0.5 0.5 0.5 0.1 10"
lines.append(f'setblock ~9 ~{base_y} ~10 minecraft:chain_command_block[facing=east]{get_cb_nbt(cmd_fx, type="chain", auto=True)}')

# Chain CB (Audio)
cmd_sfx = "playsound block.note_block.chime master @a ~ ~ ~ 1 2"
lines.append(f'setblock ~10 ~{base_y} ~10 minecraft:chain_command_block[facing=east]{get_cb_nbt(cmd_sfx, type="chain", auto=True)}')

# ==========================================================
# LOGIC CHAIN C: HEARTBEAT (Signal 1)
# ==========================================================
# We tap the wire immediately.
lines.append(f"setblock ~8 ~{base_y} ~6 minecraft:comparator[facing=east,mode=compare]")

# Repeating CB (Actionbar status)
# Note: Needs to be "Needs Redstone" so it only runs when signal is present
cmd_hb = 'title @a actionbar {"text":"<DeMoD> Carrier Signal Detected","color":"green"}'
lines.append(f'setblock ~9 ~{base_y} ~6 minecraft:repeating_command_block[facing=east]{{Command:"{cmd_hb}"}}') 

# -------------------------------
# Completion
# -------------------------------
lines.append('tellraw @a {"text":"[DeMoD] Smart Decoder & Logic Chains Installed.","color":"gold"}')

# Write main function
write_file(os.path.join(demod_path, "install_smart_decoder.mcfunction"), "\n".join(lines))

print(f"Datapack '{DATAPACK_NAME}' generated!")
print("1. Install alongside the Tower datapack.")
print("2. Run /reload")
print("3. Stand at the TOWER BASE (same spot you built it) and run:")
print("   /function demod:install_smart_decoder")
