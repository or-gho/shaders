# Shaders of OCaml Core Library (`shaders_of_ocaml.core`)

## Overview
The `shaders_of_ocaml.core` library provides the main public-facing typed eDSL API and compiler backends. It exposes modules for building vertex and fragment shaders as typed OCaml values, checking stage interfaces, and transpiling shader pipelines into GLSL or WGSL target source code.

## Interface
- `Ty`: Runtime type descriptors (`Ty.float`, `Ty.int`, `Ty.uint`, `Ty.bool`, `Ty.vec2`..`Ty.vec4`, `Ty.ivec2`..`Ty.ivec4`, `Ty.uvec2`..`Ty.uvec4`, `Ty.bvec2`..`Ty.bvec4`, `Ty.mat2`..`Ty.mat4`, `Ty.sampler2d`..`Ty.usampler2d`).
- `E`: Typed expression builder functions for constants, arithmetic, comparisons, boolean logic, vector constructors, swizzling, matrix operations, math builtins, and texture sampling.
- `S`: Statement constructors (`S.assign`, `S.read`, `S.let_`, `S.if_`, `S.while_`, `S.for_`, `S.break_`, `S.continue_`, `S.discard`, `S.eval`).
- `Vertex`: Vertex shader builder workflow (`Vertex.create`, `Vertex.attribute`, `Vertex.uniform`, `Vertex.varying`, `Vertex.body`).
- `Fragment`: Fragment shader builder workflow (`Fragment.create`, `Fragment.uniform`, `Fragment.varying`, `Fragment.output`, `Fragment.body`).
- `Compile.pair`: Validates stage compatibility and transpiles a vertex and fragment shader pair into a `program` containing target `vertex` and `fragment` source strings (`?backend:GLSL | WGSL`).
- `Transpile.Glsl`: Backend emitter for GLSL shader stages.
- `Transpile.Wgsl`: Backend emitter for WGSL shader stages with structure and binding layout synthesizers.
- `Transpile.compile_pair`: Validates stage interfaces and transpiles both stages.
- Child Subdirectories:
  - [Utilities (`./utils/README.md`)](./utils/README.md) - Internal typed AST, GADT definitions, statement primitives, and stage builder state.

## Invariants
- `Compile.pair` enforces stage interface matching:
  - All varyings emitted by the vertex shader must correspond in name and type to varyings received by the fragment shader.
  - Uniforms declared across both stages with the same identifier must have identical types.
  - Interface mismatches return `Error (Interface_mismatch message)`.
- Backend code generation maintains mathematical operator precedence without altering semantics.

## Side Effects
- None. All transpilation and interface verification logic operates purely in-memory.

## Verification
- Run: `dune runtest`
