# Pac-Man Implementation in Haskell

A Pac-Man game implementation using Haskell and the Gloss graphics library.

## Overview

This project is a functional implementation of the classic Pac-Man arcade game, featuring core game mechanics and AI behavior for ghosts.

## Features

- Classic Pac-Man gameplay mechanics
- Ghost AI with different behavior patterns for each ghost type:
  - Blinky: Pursues Pac-Man directly
  - Pinky: Attempts to ambush Pac-Man
  - Inky: Complex behavior based on Pac-Man's position
  - Clyde: Alternates between chasing and scattering
- Power pellets and frightened ghost behavior
- Score tracking system
- Save/Load game functionality
- Pause feature

## Technical Details

### Dependencies

- Base >= 4.7 && < 5
- Gloss (for graphics rendering)
- Aeson (for JSON serialization)
- Text
- ByteString
- Containers
- Random

### Project Structure

The project is organized into several main modules:

- `Controller/`: Game loop, input handling, and I/O operations
- `Model/`: Game state, physics, AI, and type definitions
- `View/`: Rendering and asset management
- `Init`: Game initialization

### Building and Running

This project uses Stack as its build tool. To build and run:
Do `stack build` then `stack run`