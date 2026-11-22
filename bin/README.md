# Shaders of OCaml Executable Example (`bin`)

## Overview
The `bin` directory contains the demo executable application for `shaders_of_ocaml`. It demonstrates writing shaders with the PPX eDSL, compiling them to GLSL at startup with `Compile.pair`, linking them into an OpenGL 3.3 pipeline, and rendering a rotating colored 3D geometry using SDL2 (`tsdl`) and OpenGL bindings (`tgls`).

## Interface
- Executable: `shaders_of_ocaml` (build target: `bin/main.exe`).
- Invocation: `dune exec shaders_of_ocaml` or `dune exec bin/main.exe`.

## Invariants
- Runtime environment must provide access to an OpenGL 3.3 Core profile context and SDL2 library dependencies.
- Shader pipelines must successfully compile through `Compile.pair ~backend:GLSL` without interface mismatches before OpenGL state initialization.

## Side Effects
- Initializes an SDL2 video window and OpenGL context.
- Interacts with system graphics drivers, allocating GPU shader objects and vertex buffer objects.
- Executes an interactive event polling loop, handles user window input, and renders frames to the screen.

## Verification
- Run: `dune exec bin/main.exe`
