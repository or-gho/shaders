  open Shaders_of_ocaml

  let failf fmt = Printf.ksprintf failwith fmt

  let assert_equal label expected got =
    if not (String.equal expected got) then
      failf "%s mismatch:\n--- expected ---\n%s\n--- got ---\n%s"
        label expected got

  let hand_vertex =
    let vb = Vertex.create () in
    let vb, position = Vertex.attribute "position" Ty.vec3 vb in
    let vb, color = Vertex.attribute "color" Ty.vec3 vb in
    let vb, mvp = Vertex.uniform "mvp" Ty.mat4 vb in
    let vb, v_color = Vertex.varying "v_color" Ty.vec3 vb in
    Vertex.body vb
      (fun gl_position ->
         [ S.assign v_color (S.read color)
         ; S.assign
             gl_position
             (E.mat4_mul_vec4
                (S.read mvp)
                (E.vec4_of_vec3 (S.read position) (E.float 1.0)))
         ])

  let hand_fragment =
    let fb = Fragment.create () in
    let fb, v_color = Fragment.varying "v_color" Ty.vec3 fb in
    let fb, frag_color = Fragment.output "frag_color" Ty.vec4 fb in
    Fragment.body fb
      [ S.assign
          frag_color
          (E.vec4_of_vec3 (S.read v_color) (E.float 1.0))
      ]

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

  let compile ~label ~backend vs fs =
    match Compile.pair ~backend vs fs with
    | Ok program -> program
    | Error error -> failf "%s: %s" label (string_of_error error)

  let write_file path content =
    let channel = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel content)

  let remove_file path =
    if Sys.file_exists path then Sys.remove path

  let with_temp_file suffix write_validate =
    let path = Filename.temp_file "shaders_of_ocaml_" suffix in
    Fun.protect
      ~finally:(fun () -> remove_file path)
      (fun () -> write_validate path)

  let validate_wgsl path =
    if Sys.command ("naga " ^ Filename.quote path ^ " >/dev/null 2>&1") <> 0 then
      failf "naga rejected %s" path

  let naga_available () =
    Sys.command "command -v naga >/dev/null 2>&1" = 0

  let test_interface_mismatch () =
    let vb = Vertex.create () in
    let vb, _ = Vertex.varying "shared" Ty.vec3 vb in
    let vs = Vertex.body vb (fun _ -> []) in
    let fb = Fragment.create () in
    let fb, _ = Fragment.varying "different" Ty.vec3 fb in
    let fs = Fragment.body fb [] in
    match Compile.pair ~backend:GLSL vs fs with
    | Ok _ -> failwith "varying name mismatch should fail compilation"
    | Error (Interface_mismatch message) ->
      if not (String.contains message 'v' && String.contains message 'd') then
        failwith ("unexpected interface mismatch: " ^ message)

  let () =
    let hand = compile ~label:"hand-written GLSL" ~backend:GLSL
        hand_vertex hand_fragment in
    let ppx = compile ~label:"PPX GLSL" ~backend:GLSL
        ppx_vertex ppx_fragment in

    assert_equal "PPX vertex source" hand.vertex ppx.vertex;
    assert_equal "PPX fragment source" hand.fragment ppx.fragment;

    let wgsl = compile ~label:"WGSL" ~backend:WGSL
        ppx_vertex ppx_fragment in
    if wgsl.vertex = "" || wgsl.fragment = "" then
      failwith "WGSL backend produced empty shader source";

    with_temp_file ".vert.wgsl" (fun vertex_path ->
      write_file vertex_path wgsl.vertex;
      with_temp_file ".frag.wgsl" (fun fragment_path ->
        write_file fragment_path wgsl.fragment;
        if naga_available () then begin
          validate_wgsl vertex_path;
          validate_wgsl fragment_path
        end));

    test_interface_mismatch ();
    print_endline "shader tests passed"
