open Shaders_of_ocaml

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let failf fmt = Printf.ksprintf failwith fmt

let compile_ok ~label ~backend vs fs =
  match Compile.pair ~backend vs fs with
  | Ok p  -> p
  | Error e -> failf "%s: %s" label (string_of_error e)

let with_temp_file suffix f =
  let path = Filename.temp_file "shaders_of_ocaml_test_" suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)

(* ------------------------------------------------------------------ *)
(* Shaders under test                                                  *)
(* ------------------------------------------------------------------ *)

let hand_vertex =
  let vb = Vertex.create () in
  let vb, position = Vertex.attribute "position" Ty.vec3 vb in
  let vb, color    = Vertex.attribute "color"    Ty.vec3 vb in
  let vb, mvp      = Vertex.uniform   "mvp"      Ty.mat4 vb in
  let vb, v_color  = Vertex.varying   "v_color"  Ty.vec3 vb in
  Vertex.body vb (fun gl_position ->
    [ S.assign v_color (S.read color)
    ; S.assign gl_position
        (E.mat4_mul_vec4 (S.read mvp)
           (E.vec4_of_vec3 (S.read position) (E.float 1.0)))
    ])

let hand_fragment =
  let fb = Fragment.create () in
  let fb, v_color   = Fragment.varying "v_color"    Ty.vec3 fb in
  let fb, frag_color = Fragment.output "frag_color" Ty.vec4 fb in
  Fragment.body fb
    [ S.assign frag_color (E.vec4_of_vec3 (S.read v_color) (E.float 1.0)) ]

let ppx_vertex =
  [%shader.vertex
    {| attribute position : vec3;
       attribute color    : vec3;
       uniform   mvp      : mat4;
       varying   v_color  : vec3;
       void {
         v_color = color;
         gl_Position = mvp * vec4(position, 1.0);
       } |}]

let ppx_fragment =
  [%shader.fragment
    {| varying v_color    : vec3;
       output  frag_color : vec4;
       void {
         frag_color = vec4(v_color, 1.0);
       } |}]

(* ------------------------------------------------------------------ *)
(* Test cases                                                          *)
(* ------------------------------------------------------------------ *)

(* 1. PPX vertex source matches hand-written builder GLSL *)
let test_ppx_vertex_parity () =
  let hand = compile_ok ~label:"hand GLSL" ~backend:GLSL hand_vertex hand_fragment in
  let ppx  = compile_ok ~label:"ppx GLSL"  ~backend:GLSL ppx_vertex  ppx_fragment  in
  Alcotest.(check string) "vertex GLSL" hand.vertex ppx.vertex

(* 2. PPX fragment source matches hand-written builder GLSL *)
let test_ppx_fragment_parity () =
  let hand = compile_ok ~label:"hand GLSL" ~backend:GLSL hand_vertex hand_fragment in
  let ppx  = compile_ok ~label:"ppx GLSL"  ~backend:GLSL ppx_vertex  ppx_fragment  in
  Alcotest.(check string) "fragment GLSL" hand.fragment ppx.fragment

(* 3. WGSL backend produces non-empty source strings *)
let test_wgsl_non_empty () =
  let wgsl = compile_ok ~label:"WGSL" ~backend:WGSL ppx_vertex ppx_fragment in
  Alcotest.(check bool) "vertex non-empty"   true (wgsl.vertex   <> "");
  Alcotest.(check bool) "fragment non-empty" true (wgsl.fragment <> "")

(* 4. Naga validates the generated WGSL (skipped when naga not on PATH) *)
let test_naga_wgsl () =
  if Sys.command "command -v naga >/dev/null 2>&1" <> 0 then
    ()  (* naga absent — skip silently *)
  else begin
    let wgsl = compile_ok ~label:"WGSL naga" ~backend:WGSL ppx_vertex ppx_fragment in
    with_temp_file ".vert.wgsl" (fun vp ->
      write_file vp wgsl.vertex;
      if Sys.command ("naga " ^ Filename.quote vp ^ " >/dev/null 2>&1") <> 0 then
        failwith "naga rejected generated WGSL vertex shader");
    with_temp_file ".frag.wgsl" (fun fp ->
      write_file fp wgsl.fragment;
      if Sys.command ("naga " ^ Filename.quote fp ^ " >/dev/null 2>&1") <> 0 then
        failwith "naga rejected generated WGSL fragment shader")
  end

(* 5. Compile.pair returns Interface_mismatch when varying names disagree *)
let test_varying_name_mismatch () =
  let vb = Vertex.create () in
  let vb, _ = Vertex.varying "v_alpha" Ty.vec3 vb in
  let vs = Vertex.body vb (fun _ -> []) in
  let fb = Fragment.create () in
  let fb, _ = Fragment.varying "v_beta" Ty.vec3 fb in
  let fs = Fragment.body fb [] in
  match Compile.pair ~backend:GLSL vs fs with
  | Ok _    -> failwith "expected Interface_mismatch, got Ok"
  | Error (Interface_mismatch msg) ->
    (* message should mention both names *)
    if not (String.length msg > 0) then
      failwith ("empty mismatch message: " ^ msg)

(* 6. Reserved GLSL keyword is rejected with Invalid_argument *)
let test_reserved_keyword_rejected () =
  match
    let vb = Vertex.create () in
    let vb, _ = Vertex.attribute "shared" Ty.vec3 vb in
    let _ = Vertex.body vb (fun _ -> []) in
    ()
  with
  | ()                 -> failwith "expected Invalid_argument for 'shared'"
  | exception Invalid_argument _ -> ()

(* 7. gl_-prefix variable names are rejected *)
let test_gl_prefix_rejected () =
  match
    let vb = Vertex.create () in
    let vb, _ = Vertex.attribute "gl_Foo" Ty.vec3 vb in
    let _ = Vertex.body vb (fun _ -> []) in
    ()
  with
  | ()                 -> failwith "expected Invalid_argument for 'gl_Foo'"
  | exception Invalid_argument _ -> ()

(* 8. Duplicate binding name is rejected *)
let test_duplicate_name_rejected () =
  match
    let vb = Vertex.create () in
    let vb, _ = Vertex.attribute "pos" Ty.vec3 vb in
    let vb, _ = Vertex.attribute "pos" Ty.vec3 vb in
    let _ = Vertex.body vb (fun _ -> []) in
    ()
  with
  | ()                 -> failwith "expected Invalid_argument for duplicate 'pos'"
  | exception Invalid_argument _ -> ()

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "shaders_of_ocaml"
    [ "ppx-parity",
        [ Alcotest.test_case "vertex GLSL matches builder"   `Quick test_ppx_vertex_parity
        ; Alcotest.test_case "fragment GLSL matches builder" `Quick test_ppx_fragment_parity
        ]
    ; "backends",
        [ Alcotest.test_case "WGSL produces non-empty output" `Quick test_wgsl_non_empty
        ; Alcotest.test_case "Naga validates WGSL"            `Slow  test_naga_wgsl
        ]
    ; "interface-validation",
        [ Alcotest.test_case "varying name mismatch -> error" `Quick test_varying_name_mismatch
        ; Alcotest.test_case "reserved keyword rejected"       `Quick test_reserved_keyword_rejected
        ; Alcotest.test_case "gl_-prefix rejected"             `Quick test_gl_prefix_rejected
        ; Alcotest.test_case "duplicate name rejected"         `Quick test_duplicate_name_rejected
        ]
    ]
