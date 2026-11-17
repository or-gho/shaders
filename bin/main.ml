(* Baseline: a coloured triangle with OpenGL 3.3 core, using tsdl (window +
   GL context) and tgls (OpenGL bindings).

   The shaders below are plain strings. OCaml cannot check them: a typo in
   the GLSL only shows up when the driver compiles it at runtime. That is
   exactly the problem Shaders_of_ocaml is meant to fix, so this file is the
   "before" picture we will improve on. *)

open Tsdl
open Tgl3

(* Bring the eDSL into scope. *)
open Shaders_of_ocaml

let vertex_shader =
  let open Dsl in
  let open Shader in
  let open Types in
  let position = attribute Vec3 "position" in
  let color = attribute Vec3 "color" in
  let mvp = uniform Mat4 "mvp" in
  let v_color = Types.Vec3, "v_color" in
  { vs_attributes = [ Vec3, "position"; Vec3, "color" ]
  ; vs_uniforms = [ Mat4, "mvp" ]
  ; vs_varyings = [ v_color ]
  ; vs_body =
      [ assign v_color color
      ; assign (Vec4, "gl_Position") (mat4_mul_vec4 mvp (vec4_of_vec3 position (f 1.0)))
      ]
  }
;;

let fragment_shader =
  let open Dsl in
  let open Shader in
  let open Types in
  let v_color = Vec3, "v_color" in
  let v_color_in = attribute Vec3 "v_color" in
  { fs_uniforms = []
  ; fs_varyings = [ v_color ]
  ; fs_output = Vec4, "frag_color"
  ; fs_body = [ assign (Vec4, "frag_color") (vec4_of_vec3 v_color_in (f 1.0)) ]
  }
;;

(* Transpile to GLSL strings. The [compile_pair] call enforces that the
   vertex shader's varyings match the fragment shader's varyings. *)
let vertex_src, fragment_src =
  Shaders_of_ocaml_glsl.Transpile.compile_pair vertex_shader fragment_shader
;;

(* ---------- The shaders, as strings (the part we want to replace) ---------- *)

(* let vertex_src =
  {| #version 330 core
    layout(location = 0) in vec3 position;
    layout(location = 1) in vec3 color;
    uniform mat4 mvp;
    out vec3 v_color;
    void main() {
      v_color = color;
      gl_Position = mvp * vec4(position, 1.0);
    } |}
;;

let fragment_src =
  {|#version 330 core
in vec3 v_color;
out vec4 frag_color;
void main() {
  frag_color = vec4(v_color, 1.0);
}
|}
;; *)

(* ---------- Small helpers ---------- *)

let ok = function
  | Ok v -> v
  | Error (`Msg e) ->
    prerr_endline e;
    exit 1
;;

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a;
    Int32.to_int a.{0}
;;

let gen_vertex_array () = get_int (Gl.gen_vertex_arrays 1)
let gen_buffer () = get_int (Gl.gen_buffers 1)

(* ---------- Shader / program compilation ---------- *)

let compile_shader typ src =
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_int (Gl.get_shaderiv sid Gl.compile_status) = Gl.true_
  then Ok sid
  else (
    let len = get_int (Gl.get_shaderiv sid Gl.info_log_length) in
    let log = bigarray_create Bigarray.char len in
    Gl.get_shader_info_log sid len None log;
    Error (`Msg ("shader compile error:\n" ^ Gl.string_of_bigarray log)))
;;

let link_program vs_src fs_src =
  let vs = ok (compile_shader Gl.vertex_shader vs_src) in
  let fs = ok (compile_shader Gl.fragment_shader fs_src) in
  let pid = Gl.create_program () in
  Gl.attach_shader pid vs;
  Gl.attach_shader pid fs;
  Gl.link_program pid;
  Gl.delete_shader vs;
  Gl.delete_shader fs;
  if get_int (Gl.get_programiv pid Gl.link_status) = Gl.true_
  then Ok pid
  else (
    let len = get_int (Gl.get_programiv pid Gl.info_log_length) in
    let log = bigarray_create Bigarray.char len in
    Gl.get_program_info_log pid len None log;
    Error (`Msg ("program link error:\n" ^ Gl.string_of_bigarray log)))
;;

(* Choose which shape to draw. *)
type shape =
  | Triangle
  | Rectangle
  | Cube

let selected_shape = Cube

type vertex =
  { x : float
  ; y : float
  ; z : float
  ; r : float
  ; g : float
  ; b : float
  }

let triangle_vertices =
  [ { x = 0.0; y = 0.6; z = 0.0; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = -0.6; y = -0.5; z = 0.0; r = 0.0; g = 1.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; z = 0.0; r = 0.0; g = 0.0; b = 1.0 }
  ]
;;

let rectangle_vertices =
  [ { x = -0.6; y = 0.5; z = 0.0; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = 0.6; y = 0.5; z = 0.0; r = 0.0; g = 1.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; z = 0.0; r = 0.0; g = 0.0; b = 1.0 }
  ; { x = -0.6; y = 0.5; z = 0.0; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; z = 0.0; r = 0.0; g = 0.0; b = 1.0 }
  ; { x = -0.6; y = -0.5; z = 0.0; r = 1.0; g = 1.0; b = 0.0 }
  ]
;;

let cube_corners =
  [| -0.5, -0.5, 0.5
   ; 0.5, -0.5, 0.5
   ; 0.5, 0.5, 0.5
   ; -0.5, 0.5, 0.5
   ; -0.5, -0.5, -0.5
   ; 0.5, -0.5, -0.5
   ; 0.5, 0.5, -0.5
   ; -0.5, 0.5, -0.5
  |]
;;

let cube_faces =
  [ (0, 1, 2, 3), (1.0, 0.2, 0.2) (* front  red     *)
  ; (5, 4, 7, 6), (0.2, 1.0, 0.2) (* back   green   *)
  ; (1, 5, 6, 2), (0.2, 0.2, 1.0) (* right  blue    *)
  ; (4, 0, 3, 7), (1.0, 1.0, 0.2) (* left   yellow  *)
  ; (3, 2, 6, 7), (1.0, 0.2, 1.0) (* top    magenta *)
  ; (4, 5, 1, 0), (0.2, 1.0, 1.0)
  ]
;;

(* bottom cyan    *)

(* Each quad becomes two triangles: (a,b,c) and (a,c,d). *)
let cube_vertices =
  cube_faces
  |> List.concat_map (fun ((i0, i1, i2, i3), (r, g, b)) ->
    let v i =
      let x, y, z = cube_corners.(i) in
      { x; y; z; r; g; b }
    in
    [ v i0; v i1; v i2; v i0; v i2; v i3 ])
;;

let selected_vertices =
  match selected_shape with
  | Triangle -> triangle_vertices
  | Rectangle -> rectangle_vertices
  | Cube -> cube_vertices
;;

let vertex_count = List.length selected_vertices

let vertices =
  selected_vertices
  |> List.concat_map (fun { x; y; z; r; g; b } -> [ x; y; z; r; g; b ])
  |> Array.of_list
  |> Bigarray.(Array1.of_array float32 c_layout)
;;

let float_size = 4
let floats_per_vertex = 6
let stride = floats_per_vertex * float_size

let mat_mul a b =
  Array.init 16 (fun i ->
    let col = i / 4
    and row = i mod 4 in
    let s = ref 0.0 in
    for k = 0 to 3 do
      s := !s +. (a.((k * 4) + row) *. b.((col * 4) + k))
    done;
    !s)
;;

let rotate_y t =
  let c = cos t
  and s = sin t in
  [| c; 0.; -.s; 0.; 0.; 1.; 0.; 0.; s; 0.; c; 0.; 0.; 0.; 0.; 1. |]
;;

let rotate_x t =
  let c = cos t
  and s = sin t in
  [| 1.; 0.; 0.; 0.; 0.; c; s; 0.; 0.; -.s; c; 0.; 0.; 0.; 0.; 1. |]
;;

let translate_z tz = [| 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; tz; 1. |]

let perspective fovy aspect near far =
  let f = 1.0 /. tan (fovy /. 2.0) in
  [| f /. aspect
   ; 0.
   ; 0.
   ; 0.
   ; 0.
   ; f
   ; 0.
   ; 0.
   ; 0.
   ; 0.
   ; (far +. near) /. (near -. far)
   ; -1.
   ; 0.
   ; 0.
   ; 2.0 *. far *. near /. (near -. far)
   ; 0.
  |]
;;

(* ---------- Main ---------- *)

let () =
  ok (Sdl.init Sdl.Init.video);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core);
  ok (Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1);
  ok (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);
  let win =
    ok (Sdl.create_window ~w:800 ~h:600 "OCaml triangle" Sdl.Window.(opengl + shown))
  in
  let ctx = ok (Sdl.gl_create_context win) in
  ok (Sdl.gl_make_current win ctx);
  (* ok (Sdl.gl_set_swap_interval 1); *)
  ignore (Sdl.gl_set_swap_interval 1);
  Gl.enable Gl.depth_test;
  let program = ok (link_program vertex_src fragment_src) in
  let mvp_loc = Gl.get_uniform_location program "mvp" in
  (* Vertex array + buffer describing the triangle *)
  let vao = gen_vertex_array () in
  let vbo = gen_buffer () in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.buffer_data
    Gl.array_buffer
    (Gl.bigarray_byte_size vertices)
    (Some vertices)
    Gl.static_draw;
  (* location 0: position (vec2) *)
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  (* location 1: color (vec3) *)
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (3 * float_size));
  Gl.enable_vertex_attrib_array 1;
  let event = Sdl.Event.create () in
  let running = ref true in
  while !running do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> running := false
      | `Key_down when Sdl.Event.(get event keyboard_keycode) = Sdl.K.escape ->
        running := false
      | _ -> ()
    done;
    (* NEW: build the transform matrix for this frame *)
    let t = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let model = mat_mul (rotate_y t) (rotate_x (t *. 0.5)) in
    let view = translate_z (-3.0) in
    let proj = perspective 0.8 (800.0 /. 600.0) 0.1 100.0 in
    let mvp = mat_mul proj (mat_mul view model) in
    let mvp_buf = Bigarray.(Array1.of_array float32 c_layout mvp) in
    Gl.clear_color 0.08 0.08 0.1 1.0;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    Gl.use_program program;
    Gl.uniform_matrix4fv mvp_loc 1 false mvp_buf;
    Gl.bind_vertex_array vao;
    Gl.draw_arrays Gl.triangles 0 vertex_count;
    Sdl.gl_swap_window win
  done;
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Sdl.quit ()
;;
