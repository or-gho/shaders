# Shaders of OCaml Utilities (`shaders_of_ocaml.utils`)

## Overview
The `shaders_of_ocaml.utils` package provides the internal typed abstract syntax tree (AST), type descriptors, statement constructs, and staged builder infrastructure for shader construction. It defines the core phantom types, GADTs, and validation rules used by the public eDSL facade and transpiler.

## Interface
- `Types.t`: GADT indexing shader types (`Float`, `Int`, `UInt`, `Bool`, `Vec2`..`Vec4`, `IVec2`..`IVec4`, `UVec2`..`UVec4`, `BVec2`..`BVec4`, `Mat2`..`Mat4`, `Sampler2D`..`USampler2D`).
- `Types.glsl_name`: Mapping from type GADT to standard GLSL type identifier.
- `Types.wgsl_name`: Mapping from type GADT to WGSL type descriptor syntax.
- `Expr.t`: GADT representation for typed shader expressions (constants, variables, arithmetic, comparisons, boolean logic, vector constructors, swizzles, matrix math, and builtins).
- `Stmt.stmt`: GADT representation for statements (`Assign`, `Let`, `Decl_var`, `Expr_stmt`, `If`, `For`, `While`, `DoWhile`, `Switch`, `Break`, `Continue`, `Discard`, `Block`, `Return`).
- `Stmt.ref_`: Typed variable reference pairs `('a Types.t * string)`.
- `Shader.create_vertex`: Initializes an empty vertex stage builder.
- `Shader.create_fragment`: Initializes an empty fragment stage builder.
- `Shader.with_attribute`: Registers a vertex attribute binding and returns an updated builder with reference handle.
- `Shader.with_uniform`: Registers a uniform binding and returns an updated builder with reference handle.
- `Shader.with_varying`: Registers a varying interface binding between stages.
- `Shader.with_output`: Registers a fragment stage output binding.
- `Shader.vertex_body`: Finalizes vertex shader statements and binds the `gl_Position` output handle.
- `Shader.fragment_body`: Finalizes fragment shader statements.

## Invariants
- Variable and stage interface names must pass `Shader.is_ident` (must begin with an ASCII letter or underscore, followed by alphanumeric characters or underscores).
- Names must not match GLSL reserved keywords enumerated in `Shader.glsl_keywords` (`attribute`, `const`, `uniform`, `varying`, `buffer`, `shared`, etc.); violations raise `Invalid_argument`.
- Attributes can only be declared on `vertex_stage` builders; outputs can only be declared on `fragment_stage` builders.
- Type constraints are statically enforced at compile time via phantom types and GADTs.

## Side Effects
- None. All AST generation and builder functions are pure in-memory transformations.

## Verification
- Run: `dune runtest`
