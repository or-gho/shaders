# Shaders of OCaml

A typed eDSL for writing OpenGL and WebGPU shaders in OCaml.

Instead of keeping GLSL or WGSL as strings that only fail at runtime, you
describe a shader with OCaml values. The OCaml compiler checks the types and
names, and `Compile.pair` turns the description into GLSL or WGSL source.

You can write shaders in two ways:

1. **Builder eDSL.** Call functions such as `Vertex.attribute`,
   `Fragment.output`, and `S.assign`. Everything is an ordinary OCaml value.
2. **PPX.** Write GLSL-like text inside `[%shader.vertex {| ... |}]` or
   `[%shader.fragment {| ... |}]`. The PPX parses it at build time, so syntax
   errors are reported before the program runs.

Both produce the same output. A test in this repository checks that the PPX
and the builder generate identical GLSL.

## Subdirectories & System Boundaries

- **[`lib`](./lib/README.md)**: Core eDSL library defining type descriptors (`Ty`), expression constructors (`E`), statement builders (`S`), stage builders (`Vertex`, `Fragment`), and compilation backends (`Compile.pair`, `Transpile.Glsl`, `Transpile.Wgsl`).
- **[`lib/utils`](./lib/utils/README.md)**: Internal typed AST representations, GADTs, statement primitives, and stage builder state management.
- **[`ppx`](./ppx/README.md)**: PPX rewriter providing the `[%shader.vertex]` and `[%shader.fragment]` extensions for embedding GLSL-like syntax directly in OCaml.
- **[`bin`](./bin/README.md)**: Interactive OpenGL 3.3 executable application demonstrating end-to-end rendering using SDL2 (`tsdl`) and OpenGL (`tgls`).

## Global Invariants

- **Static Type Safety**: Expressions and statements are indexed by phantom type tags (`float_s`, `vec3`, `mat4`, etc.) statically checked by the OCaml compiler.
- **Stage Interface Consistency**: `Compile.pair` verifies that all varyings emitted by the vertex stage are matched in name and type by the fragment stage, and that uniform types agree across stages.
- **Identifier Safety**: All variable, attribute, uniform, varying, and output identifiers must satisfy identifier syntax and must not collide with GLSL reserved keywords.

## Global Side Effects

- The core library (`lib`) and PPX (`ppx`) perform purely functional, in-memory transformations with zero runtime side effects.
- The example application (`bin`) opens an OS window via SDL2, initializes an OpenGL context, allocates GPU resources, and processes window events.

## System Verification

- Run: `dune runtest`

---

## Example

```ocaml
open Shaders_of_ocaml

let vertex_shader =
  let vb = Vertex.create () in
  let vb, position = Vertex.attribute "position" Ty.vec3 vb in
  let vb, color    = Vertex.attribute "color" Ty.vec3 vb in
  let vb, mvp      = Vertex.uniform "mvp" Ty.mat4 vb in
  let vb, v_color  = Vertex.varying "v_color" Ty.vec3 vb in
  Vertex.body vb (fun gl_position ->
    [ S.assign v_color (S.read color)
    ; S.assign gl_position
        (E.mat4_mul_vec4 (S.read mvp)
           (E.vec4_of_vec3 (S.read position) (E.float 1.0)))
    ])

let fragment_shader =
  let fb = Fragment.create () in
  let fb, v_color   = Fragment.varying "v_color" Ty.vec3 fb in
  let fb, frag_color = Fragment.output "frag_color" Ty.vec4 fb in
  Fragment.body fb
    [ S.assign frag_color (E.vec4_of_vec3 (S.read v_color) (E.float 1.0)) ]

let glsl =
  match Compile.pair ~backend:GLSL vertex_shader fragment_shader with
  | Ok program -> program
  | Error err -> failwith (string_of_error err)

let vertex_src = glsl.vertex
let fragment_src = glsl.fragment
```

The same shaders in PPX form:

```ocaml
let vertex_shader =
  [%shader.vertex
    {| attribute position : vec3;
       attribute color    : vec3;
       uniform   mvp      : mat4;
       varying   v_color  : vec3;
       void {
         v_color = color;
         gl_Position = mvp * vec4(position, 1.0);
       } |}]

let fragment_shader =
  [%shader.fragment
    {| varying v_color    : vec3;
       output  frag_color : vec4;
       void {
         frag_color = vec4(v_color, 1.0);
       } |}]
```

To target WebGPU, change only the backend: `Compile.pair ~backend:WGSL ...`.

## What is checked, and when

| Error | Where it is caught |
|---|---|
| Wrong type in an expression, unknown OCaml name | OCaml compile time |
| Syntax error inside `{| ... |}` | build time, by the PPX |
| Varying names that differ between stages | `Compile.pair`, returns `Interface_mismatch` |
| Invalid GLSL or WGSL produced by the library | not checked by this library; test with a validator such as `naga` |

Varying names are plain strings, so OCaml cannot compare them across the two
stages. Bind them to shared values if you want the compiler to catch typos:

```ocaml
let v_color_name = "v_color"
```

## Requirements

- OCaml 4.14 or newer (5.x works)
- opam
- Nix, only if you use the flake (see below)

### Packages

Install the library and the PPX tooling with opam:

```sh
opam install dune ppxlib tsdl tgls
```

| Package | Why it is needed |
|---|---|
| `dune` | build system |
| `ppxlib` | needed to build the `[%shader.vertex]` / `[%shader.fragment]` PPX |
| `tsdl` | SDL2 bindings, used by the example program to open a window |
| `tgls` | OpenGL bindings (`Tgl3`), used by the example program |

For the example program you also need the system libraries, as in Debian and
Ubuntu:

```sh
sudo apt install build-essential pkg-config libsdl2-dev libgl-dev libgl1-mesa-dri mesa-utils
```

On macOS: `brew install sdl2 pkg-config`.

Load the opam environment in your shell:

- bash/zsh: `eval $(opam env)`
- fish: `eval (opam env)`

## Building

```sh
dune build
dune test
dune exec shaders_of_ocaml
```

The example executable needs OpenGL linked explicitly. In `bin/dune`:

```
(executable
 (name main)
 (public_name shaders_of_ocaml)
 (libraries shaders_of_ocaml tsdl tgls.tgl3)
 (link_flags (-cclib -Wl,--no-as-needed -cclib -lGL)))
```

Without the `link_flags` line the program fails with
`undefined symbol: glCreateShader`. On macOS, use `-framework OpenGL` instead
of `-lGL`.

To use the PPX in your own project, add it to the `preprocess` field:

```
(executable
 (name main)
 (libraries shaders_of_ocaml)
 (preprocess (pps shaders_of_ocaml.ppx)))
```

## Optional: validating WGSL

The test suite runs `naga` on the generated WGSL when it is installed. Install
it from https://github.com/gfx-rs/wgpu (the `naga-cli` crate) if you want
that check. Without it the test still runs and skips validation.


Then build and run as usual.

## Layout

```
dune-project
lib/          the eDSL and the GLSL / WGSL backends
lib/utils/    internal typed AST, GADTs, and builders
ppx/          the [%shader.vertex] and [%shader.fragment] PPX
bin/          example program (OpenGL 3.3 triangle)
test/         tests (builder vs PPX output, interface check, WGSL validation)
```
