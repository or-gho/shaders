(* Baseline: a coloured triangle with OpenGL 3.3 core, using tsdl (window +
   GL context) and tgls (OpenGL bindings).

   The shaders below are plain strings. OCaml cannot check them: a typo in
   the GLSL only shows up when the driver compiles it at runtime. That is
   exactly the problem Shaders_of_ocaml is meant to fix, so this file is the
   "before" picture we will improve on. *)

open Tsdl
open Tgl3

(* ---------- The shaders, as strings (the part we want to replace) ---------- *)

let vertex_src =
  {|#version 330 core
layout(location = 0) in vec2 position;
layout(location = 1) in vec3 color;
out vec3 v_color;
void main() {
  v_color = color;
  gl_Position = vec4(position, 0.0, 1.0);
}
|}
;;

let fragment_src =
  {|#version 330 core
in vec3 v_color;
out vec4 frag_color;
void main() {
  frag_color = vec4(v_color, 1.0);
}
|}
;;

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

let selected_shape = Rectangle

type vertex =
  { x : float
  ; y : float
  ; r : float
  ; g : float
  ; b : float
  }

let triangle_vertices =
  [ { x = 0.0; y = 0.6; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = -0.6; y = -0.5; r = 0.0; g = 1.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; r = 0.0; g = 0.0; b = 1.0 }
  ]
;;

let rectangle_vertices =
  [ { x = -0.6; y = 0.5; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = 0.6; y = 0.5; r = 0.0; g = 1.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; r = 0.0; g = 0.0; b = 1.0 }
  ; { x = -0.6; y = 0.5; r = 1.0; g = 0.0; b = 0.0 }
  ; { x = 0.6; y = -0.5; r = 0.0; g = 0.0; b = 1.0 }
  ; { x = -0.6; y = -0.5; r = 1.0; g = 1.0; b = 0.0 }
  ]
;;

let selected_vertices =
  match selected_shape with
  | Triangle -> triangle_vertices
  | Rectangle -> rectangle_vertices
;;

let vertex_count = List.length selected_vertices

let vertices =
  selected_vertices
  |> List.concat_map (fun { x; y; r; g; b } -> [ x; y; r; g; b ])
  |> Array.of_list
  |> Bigarray.(Array1.of_array float32 c_layout)
;;

let float_size = 4
let stride = 5 * float_size

(* ---------- Main ---------- *)

let () =
  ok (Sdl.init Sdl.Init.video);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core);
  ok (Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1);
  let win =
    ok (Sdl.create_window ~w:800 ~h:600 "OCaml triangle" Sdl.Window.(opengl + shown))
  in
  let ctx = ok (Sdl.gl_create_context win) in
  ok (Sdl.gl_make_current win ctx);
  (* ok (Sdl.gl_set_swap_interval 1); *)
  ignore (Sdl.gl_set_swap_interval 1);
  let program = ok (link_program vertex_src fragment_src) in
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
  Gl.vertex_attrib_pointer 0 2 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  (* location 1: color (vec3) *)
  Gl.vertex_attrib_pointer 1 3 Gl.float false stride (`Offset (2 * float_size));
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
    Gl.clear_color 0.08 0.08 0.1 1.0;
    Gl.clear Gl.color_buffer_bit;
    Gl.use_program program;
    Gl.bind_vertex_array vao;
    Gl.draw_arrays Gl.triangles 0 vertex_count;
    Sdl.gl_swap_window win
  done;
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Sdl.quit ()
;;
