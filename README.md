# OCaml triangle (baseline for Shaders_of_ocaml)

A coloured triangle rendered with OpenGL 3.3 core from OCaml. The GLSL shaders
are plain strings on purpose: this is the "before" picture that a typed
shader library will later replace.

Known to work on WSL2 (Ubuntu) with the fish shell, using the setup below.

## 1. System packages

These are installed with the OS package manager, not with opam.

### Debian / Ubuntu / WSL2 (Ubuntu)

```sh
sudo apt update
sudo apt install \
  build-essential pkg-config \
  libsdl2-dev \
  libgl-dev libgl1-mesa-dri \
  mesa-utils
```

| Package | Why it is needed |
|---|---|
| `build-essential` | C compiler and linker, used when building the OCaml bindings |
| `pkg-config` | lets the OCaml SDL binding find SDL2 |
| `libsdl2-dev` | SDL2 headers and library: window and OpenGL context |
| `libgl-dev` | provides the `libGL.so` symlink needed by the `-lGL` link flag |
| `libgl1-mesa-dri` | Mesa OpenGL drivers (software and D3D12 on WSL2) |
| `mesa-utils` | provides `glxinfo`, used to check which OpenGL version you get |

### macOS

```sh
brew install sdl2 pkg-config
```

(On macOS, see the note at the end about the forward-compatible flag, and use
`-framework OpenGL` instead of `-lGL`; the link flags below are Linux-specific.)

## 2. OCaml / opam packages

You need OCaml 4.14 or newer (5.x is fine), plus opam.

```sh
opam install dune tsdl tgls
```

| Package | Why it is needed |
|---|---|
| `dune` | the build system |
| `tsdl` | OCaml bindings to SDL2 (window, context, events) |
| `tgls` | OCaml bindings to OpenGL (`Tgl3` module) |

`tgls` also pulls in `ctypes` and `ctypes-foreign` automatically.

Make sure your shell has the opam environment loaded:

- bash/zsh: `eval $(opam env)`
- fish: `eval (opam env)`

## 3. Project layout

```
shaders/
  dune-project
  bin/
    dune
    main.ml
```

### `dune-project`

```
(lang dune 3.24)

(package
 (name shaders))
```

The `package` stanza is needed because `bin/dune` uses `public_name`.
If your dune is older than 3.0, change the version to match `dune --version`.

### `bin/dune`

```
(executable
 (name main)
 (public_name shaders)
 (libraries tsdl tgls.tgl3)
 (link_flags (-cclib -Wl,--no-as-needed -cclib -lGL)))
```

What each line does:

| Line | Meaning |
|---|---|
| `(name main)` | builds `bin/main.ml` into `main.exe` |
| `(public_name shaders)` | lets you run it as `dune exec shaders` |
| `(libraries tsdl tgls.tgl3)` | links SDL2 and the OpenGL 3.x bindings |
| `(link_flags ...)` | links the system OpenGL library into the executable |

**The `link_flags` line is required.** `tgls` looks up OpenGL functions
(like `glCreateShader`) inside the running program. Without this line the
program starts, opens a window, then crashes with
`undefined symbol: glCreateShader`. `--no-as-needed` stops the Linux linker
from dropping `libGL` as "unused", and it must come before `-lGL`.

If you do not want `public_name`, remove that line (and the `package`
stanza) and run `dune exec ./bin/main.exe` instead.

## 4. Two changes needed in `main.ml` on WSL2

WSL2 graphics drivers can refuse vsync, which makes the program exit with
"That operation is not supported". Make the swap-interval call non-fatal:

```ocaml
(* before *)
ok (Sdl.gl_set_swap_interval 1);

(* after *)
ignore (Sdl.gl_set_swap_interval 1);
```

## 5. Build and run

```sh
cd shaders
dune build
dune exec shaders
```

Press Esc or close the window to quit.

To see the exit code in fish: `dune exec shaders; echo "exit code: $status"`
(in bash/zsh it is `$?`).

## 6. Troubleshooting

| Symptom | Likely cause and fix |
|---|---|
| `dune: library "tsdl" not found` | run `opam install tsdl tgls`, and load the opam env |
| Package `sdl2` not found during install | install `libsdl2-dev` and `pkg-config` |
| `cannot find -lGL` at link time | `sudo apt install libgl-dev` |
| `undefined symbol: glCreateShader` | the `link_flags` line is missing from `bin/dune` |
| "That operation is not supported" | vsync refused: use `ignore` as in section 4 |
| Window opens and closes instantly | read the error printed in the terminal; often a GLSL compile error or OpenGL below 3.3 (see below) |
| No window at all | check `echo $DISPLAY` is not empty (WSLg), run `wsl --update` in PowerShell, then `wsl --shutdown` |

Checking your OpenGL version:

```sh
glxinfo -B | grep -i version
```

"OpenGL core profile version string" must be 3.3 or higher. Useful overrides
for testing (fish 3.1+ syntax; older fish: prefix with `env`):

```sh
LIBGL_ALWAYS_SOFTWARE=1 dune exec shaders
MESA_GL_VERSION_OVERRIDE=3.3 MESA_GLSL_VERSION_OVERRIDE=330 dune exec shaders
SDL_VIDEODRIVER=x11 dune exec shaders
```

## 7. Other notes

- **Project location on WSL2:** keeping the project under `/mnt/c/...` works
  but builds slowly. Moving it into the Linux filesystem (for example
  `~/shaders`) is faster.
- **macOS:** Apple only provides a 3.3 core context with the
  forward-compatible flag. If context creation fails, add
  `ok (Sdl.gl_set_attribute Sdl.Gl.context_flags Sdl.Gl.context_forward_compatible_flag);`
  before creating the window.
- **HiDPI displays:** if the triangle looks small or off-centre, set the
  viewport from `Sdl.gl_get_drawable_size`.

## 8. The problem this baseline demonstrates

Change `vec4(v_color, 1.0)` in `fragment_src` to `vec4(v_color, 1)`, or
misspell `v_color`. The OCaml build still succeeds and the error only
appears at runtime as a GLSL compile log. A typed shader library should
turn that into an OCaml compile error.
