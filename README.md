# Gameboy Emulator(WIP)

Basic Gameboy emulator written in rust.

# Running the emulator
- Open the folder and run ``cargo build --release``
- Run ``./target/release/gameboy.exe [path to gameboy file]``
    - Example with tetris: ``./target/release/gameboy.exe "./tetris.gb"``

# Work in progress:
- Implement Channel 3 and 4 for audio system
- Implement MBCs