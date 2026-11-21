

open Shaders_of_ocaml

let vertex_shader =
  let vb = Vertex.create () in
  let vb, position = Vertex.attribute "position" Ty.vec3 vb in
  let vb, color = Vertex.attribute "color" Ty.vec3 vb in
  let vb, mvp = Vertex.uniform "mvp" Ty.mat4 vb in
  let vb, v_color = Vertex.varying "v_color" Ty.vec3 vb in
  Vertex.body vb (fun gl_position ->
    [ S.assign v_color (S.read color)
    ; S.assign
        gl_position
        (E.mat4_mul_vec4
           (S.read mvp)
           (E.vec4_of_vec3 (S.read position) (E.float 1.0)))
    ])
;;

let fragment_shader =
  let fb = Fragment.create () in
  let fb, v_color = Fragment.varying "v_color" Ty.vec3 fb in
  let fb, frag_color = Fragment.output "frag_color" Ty.vec4 fb in
  Fragment.body fb
    [ S.assign
        frag_color
        (E.vec4_of_vec3 (S.read v_color) (E.float 1.0))
    ]
;;

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)
;;

let () =
  match Compile.pair ~backend:WGSL vertex_shader fragment_shader with
  | Ok program ->
      write_file "vertex.wgsl" program.vertex;
      write_file "fragment.wgsl" program.fragment;

      print_endline "WGSL generation successful";

      let validate path =
        let command = "naga " ^ Filename.quote path in
        let status = Sys.command command in
        if status <> 0 then
          failwith ("WGSL validation failed: " ^ path)
      in

      validate "vertex.wgsl";
      validate "fragment.wgsl";

      print_endline "Both WGSL shaders passed Naga validation!"

  | Error err ->
      failwith (string_of_error err)
