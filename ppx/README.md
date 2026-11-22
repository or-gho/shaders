# Shaders of OCaml PPX (`shaders_of_ocaml.ppx`)

## Overview
The `shaders_of_ocaml.ppx` package implements a PPX rewriter that allows developers to write GLSL-like shader specifications inside OCaml string literals. It parses the syntax at build time, checks declarations, types, and identifier constraints, and expands the code into type-safe `Shaders_of_ocaml` builder expressions.

## Interface
- `[%shader.vertex {| ... |}]`: Syntactic extension point that parses vertex shader declarations (attributes, uniforms, varyings) and body statements, emitting code that constructs a `vertex_stage shader`.
- `[%shader.fragment {| ... |}]`: Syntactic extension point that parses fragment shader declarations (uniforms, varyings, outputs) and body statements, emitting code that constructs a `fragment_stage shader`.
- `Shaders_ppx.expand`: Internal entrypoint expanding payload strings to OCaml AST.

## Invariants
- Shader declarations (`attribute`, `uniform`, `varying`, `output`) must precede the executable `void { ... }` block.
- Identifiers must adhere to naming rules and must not collide with GLSL reserved keywords; collisions raise compile-time PPX errors.
- Read-only bindings cannot be mutated: assignments to uniforms, vertex attributes, or fragment varyings trigger compile errors.
- Vertex shaders are required to assign a value to `gl_Position`.

## Side Effects
- None at runtime. During the build phase, macro expansion generates OCaml AST expressions or raises `Location.raise_errorf` compile-time exceptions on syntax or semantic errors.

## Verification
- Run: `dune runtest`
