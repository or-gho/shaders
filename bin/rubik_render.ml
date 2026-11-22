(* bin/rubik_render.ml
   Renders a Rubik's cube using shaders_of_ocaml (builder API) + tgls + tsdl.

   Each of the 26 visible cubies is drawn as a slightly scaled unit cube
   (scale 0.95) so there are visible gaps between cubies.

   Face colouring follows the standard Western colour scheme:
     U=white  D=yellow  R=red  L=orange  F=green  B=blue

   The face colour of each visible quad is determined by which world-space
   normal direction it faces (after applying the cubie's grid position and
   orientation).
*)

open Tgl3
open Shaders_of_ocaml

(* ------------------------------------------------------------------ *)
(* Small OpenGL helpers (mirrors the pattern in bin/main.ml)           *)
(* ------------------------------------------------------------------ *)

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f -> f a; Int32.to_int a.{0}

let gen_buffer     () = get_int (Gl.gen_buffers       1)
let gen_vao        () = get_int (Gl.gen_vertex_arrays  1)

let compile_gl_shader typ src =
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_int (Gl.get_shaderiv sid Gl.compile_status) = Gl.true_
  then sid
  else begin
    let len = get_int (Gl.get_shaderiv sid Gl.info_log_length) in
    let log = bigarray_create Bigarray.char len in
    Gl.get_shader_info_log sid len None log;
    failwith ("shader compile error:\n" ^ Gl.string_of_bigarray log)
  end

let link_gl_program vs_src fs_src =
  let vs  = compile_gl_shader Gl.vertex_shader   vs_src in
  let fs  = compile_gl_shader Gl.fragment_shader fs_src in
  let pid = Gl.create_program () in
  Gl.attach_shader pid vs;
  Gl.attach_shader pid fs;
  Gl.link_program pid;
  Gl.delete_shader vs;
  Gl.delete_shader fs;
  if get_int (Gl.get_programiv pid Gl.link_status) = Gl.true_
  then pid
  else begin
    let len = get_int (Gl.get_programiv pid Gl.info_log_length) in
    let log = bigarray_create Bigarray.char len in
    Gl.get_program_info_log pid len None log;
    failwith ("program link error:\n" ^ Gl.string_of_bigarray log)
  end

(* ------------------------------------------------------------------ *)
(* Shader construction via shaders_of_ocaml builder API               *)
(* ------------------------------------------------------------------ *)

let make_vertex_shader () =
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

let make_fragment_shader () =
  let fb = Fragment.create () in
  let fb, v_color    = Fragment.varying "v_color"    Ty.vec3 fb in
  let fb, frag_color = Fragment.output  "frag_color" Ty.vec4 fb in
  Fragment.body fb
    [ S.assign frag_color (E.vec4_of_vec3 (S.read v_color) (E.float 1.0)) ]

(* ------------------------------------------------------------------ *)
(* Face colours (r, g, b)                                              *)
(* ------------------------------------------------------------------ *)

let color_u = (1.0, 1.0, 1.0)   (* white  *)
let color_d = (1.0, 0.9, 0.0)   (* yellow *)
let color_r = (0.9, 0.1, 0.1)   (* red    *)
let color_l = (1.0, 0.5, 0.0)   (* orange *)
let color_f = (0.1, 0.8, 0.1)   (* green  *)
let color_b = (0.1, 0.1, 0.9)   (* blue   *)
let color_internal = (0.07, 0.07, 0.07)  (* near-black gap *)

(* Pick the sticker colour for a face whose outward normal (in cubie
   local space) points in direction (nx, ny, nz), given the cubie's
   grid position (cx, cy, cz).  Sticker is visible only if the face
   normal and the position agree on being at the cube boundary. *)
let sticker_color (cx, cy, cz) (nx, ny, nz) =
  let on_boundary coord normal =
    (coord = 1 && normal > 0) || (coord = -1 && normal < 0)
  in
  if      on_boundary cx nx then (if cx > 0 then color_r else color_l)
  else if on_boundary cy ny then (if cy > 0 then color_u else color_d)
  else if on_boundary cz nz then (if cz > 0 then color_f else color_b)
  else color_internal

(* ------------------------------------------------------------------ *)
(* Geometry: a unit cube, 6 faces, each face = 2 triangles            *)
(*                                                                     *)
(* Vertex layout: position (3 floats) + color (3 floats) = 6 floats   *)
(* ------------------------------------------------------------------ *)

(* The 6 faces of a unit cube, each defined by 4 corner offsets from   *)
(* the cubie centre, plus the outward integer normal.                   *)
(* Corners are listed counter-clockwise when viewed from outside.       *)
let cube_face_defs =
  (* (corner0, corner1, corner2, corner3, normal) — corners in CCW order *)
  [ ((-1,-1, 1),( 1,-1, 1),( 1, 1, 1),(-1, 1, 1), ( 0, 0, 1))   (* +Z front *)
  ; (( 1,-1,-1),(-1,-1,-1),(-1, 1,-1),( 1, 1,-1), ( 0, 0,-1))   (* -Z back  *)
  ; (( 1,-1, 1),( 1,-1,-1),( 1, 1,-1),( 1, 1, 1), ( 1, 0, 0))   (* +X right *)
  ; ((-1,-1,-1),(-1,-1, 1),(-1, 1, 1),(-1, 1,-1), (-1, 0, 0))   (* -X left  *)
  ; ((-1, 1, 1),( 1, 1, 1),( 1, 1,-1),(-1, 1,-1), ( 0, 1, 0))   (* +Y top   *)
  ; ((-1,-1,-1),( 1,-1,-1),( 1,-1, 1),(-1,-1, 1), ( 0,-1, 0))   (* -Y bot   *)
  ]

(* Generate the flat float buffer for one cubie. *)
let cubie_floats (c : Rubik_state.cubie) : float list =
  let (hx, hy, hz) = c.home in
  let orient = c.orient in
  (* Cubie geometry is axis-aligned: the quad drawn for face normal n sits
     at world direction n.  The sticker facing world direction n is the
     cubie's *local* face l = orient^T * n (orient maps local -> world).
     Sticker colours never change, so they are looked up relative to the
     cubie's home position via sticker_color home l. *)
  let local_dir (x, y, z) =
    let fx = float_of_int x
    and fy = float_of_int y
    and fz = float_of_int z in
    (* (orient^T * w)_i = dot of column i of orient with w *)
    ( int_of_float (Float.round
         (orient.(0) *. fx +. orient.(1) *. fy +. orient.(2) *. fz))
    , int_of_float (Float.round
         (orient.(3) *. fx +. orient.(4) *. fy +. orient.(5) *. fz))
    , int_of_float (Float.round
         (orient.(6) *. fx +. orient.(7) *. fy +. orient.(8) *. fz)) )
  in
  let scale = 0.475 in   (* half-side of one cubie; gap = 1 - 2*0.475 = 0.05 *)
  let (cx, cy, cz) = c.pos in
  let cx_f = float_of_int cx
  and cy_f = float_of_int cy
  and cz_f = float_of_int cz in
  List.concat_map (fun (c0, c1, c2, c3, normal) ->
    (* Find which local face's sticker faces this world direction. *)
    let l = local_dir normal in
    let (r, g, b) = sticker_color (hx, hy, hz) l in
    (* Each face = 2 triangles: (c0,c1,c2) and (c0,c2,c3). *)
    let vtx (dx, dy, dz) =
      let px = cx_f +. float_of_int dx *. scale in
      let py = cy_f +. float_of_int dy *. scale in
      let pz = cz_f +. float_of_int dz *. scale in
      [px; py; pz; r; g; b]
    in
    vtx c0 @ vtx c1 @ vtx c2 @
    vtx c0 @ vtx c2 @ vtx c3
  ) cube_face_defs

(* ------------------------------------------------------------------ *)
(* Matrix math (4×4 col-major)                                         *)
(* ------------------------------------------------------------------ *)

let mat4_mul a b =
  Array.init 16 (fun i ->
    let col = i / 4 and row = i mod 4 in
    let s = ref 0. in
    for k = 0 to 3 do s := !s +. a.(k*4+row) *. b.(col*4+k) done;
    !s)

let rotate_y t =
  let c = cos t and s = sin t in
  [| c; 0.;-.s; 0.;  0.; 1.; 0.; 0.;  s; 0.; c; 0.;  0.; 0.; 0.; 1. |]

let rotate_x t =
  let c = cos t and s = sin t in
  [| 1.; 0.; 0.; 0.;  0.; c; s; 0.;  0.;-.s; c; 0.;  0.; 0.; 0.; 1. |]

let translate_z tz =
  [| 1.; 0.; 0.; 0.;  0.; 1.; 0.; 0.;  0.; 0.; 1.; 0.;  0.; 0.; tz; 1. |]

let perspective fovy aspect near far =
  let f = 1.0 /. tan (fovy /. 2.0) in
  [| f /. aspect; 0.; 0.; 0.;
     0.; f; 0.; 0.;
     0.; 0.; (far +. near) /. (near -. far); -1.;
     0.; 0.; 2.0 *. far *. near /. (near -. far); 0. |]

(* ------------------------------------------------------------------ *)
(* Renderer state                                                      *)
(* ------------------------------------------------------------------ *)

type t =
  { program    : int
  ; mvp_loc    : int
  ; vao        : int
  ; vbo        : int
  ; floats_per_vertex : int
  ; float_size : int
  }

let create () : t =
  let vs = make_vertex_shader   () in
  let fs = make_fragment_shader () in
  let glsl =
    match Compile.pair ~backend:GLSL vs fs with
    | Ok p  -> p
    | Error e -> failwith ("Rubik shader compile: " ^ string_of_error e)
  in
  let program = link_gl_program glsl.vertex glsl.fragment in
  let mvp_loc = Gl.get_uniform_location program "mvp" in
  let vao = gen_vao    () in
  let vbo = gen_buffer () in
  let float_size = 4 in
  let floats_per_vertex = 6 in
  let stride = floats_per_vertex * float_size in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer vbo;
  (* Allocate an empty buffer — we will fill it every frame via sub_data. *)
  Gl.buffer_data Gl.array_buffer 0 None Gl.dynamic_draw;
  (* position: location 0, 3 floats *)
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  (* color: location 1, 3 floats *)
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (3 * float_size));
  Gl.enable_vertex_attrib_array 1;
  Gl.bind_vertex_array 0;
  { program; mvp_loc; vao; vbo; floats_per_vertex; float_size }

(* ------------------------------------------------------------------ *)
(* Draw                                                                *)
(* ------------------------------------------------------------------ *)

let draw (r : t) ~(cube : Rubik_state.t) ~(cam_yaw : float) ~(cam_pitch : float) =
  (* Build the full vertex buffer for all 26 cubies. *)
  let all_floats =
    Array.to_list cube
    |> List.concat_map (fun (c : Rubik_state.cubie) -> cubie_floats c)
  in
  let n = List.length all_floats in
  let buf = Bigarray.(Array1.of_array float32 c_layout (Array.of_list all_floats)) in
  let vertex_count = n / r.floats_per_vertex in

  (* Camera / projection — orbit controlled by yaw and pitch *)
  let model = mat4_mul (rotate_y cam_yaw) (rotate_x cam_pitch) in
  let view  = translate_z (-6.0) in
  let proj  = perspective 0.7 (1600.0 /. 1200.0) 0.1 100.0 in
  let mvp   = mat4_mul proj (mat4_mul view model) in
  let mvp_buf = Bigarray.(Array1.of_array float32 c_layout mvp) in

  Gl.use_program r.program;
  Gl.uniform_matrix4fv r.mvp_loc 1 false mvp_buf;

  Gl.bind_vertex_array r.vao;
  Gl.bind_buffer Gl.array_buffer r.vbo;
  Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size buf) (Some buf) Gl.dynamic_draw;
  Gl.draw_arrays Gl.triangles 0 vertex_count;
  Gl.bind_vertex_array 0
