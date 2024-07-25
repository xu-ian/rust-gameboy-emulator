# Gameboy Emulator(WIP)

Basic Gameboy emulator written in rust with egui for graphics.

# Running the emulator
- Open the folder and run ``cargo build --release``
- Run ``./target/release/gameboy.exe [path to gameboy file]``
    - Using tetris: ``./target/release/gameboy.exe "./tetris.gb"``

# Work in progress:
- Implement audio system
- Implement Timer
- Ability to load gameboy files on runtime
- Improve running performance