## lib/utils/README.md

No drift detected.

## lib/README.md

- Symbol naming divergence in `module S`: `lib/README.md` documents `S.local`, `S.if_then_else`, `S.while_loop`, `S.for_loop`, and `S.return_void`. The codebase implementation in `lib/shaders_of_ocaml.mli` actually defines `S.let_`, `S.if_`, `S.while_`, `S.for_`, `S.break_`, `S.continue_`, `S.discard`, and `S.eval` (no `return_void` exists in `S`).
- Internal module API reference: `lib/README.md` refers to `Transpile.validate_pair`; the actual exported function in `lib/transpile.ml` is `Transpile.compile_pair`.

## ppx/README.md

No drift detected.

## bin/README.md

No drift detected.

## README.md

No drift detected.
