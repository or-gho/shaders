(* bin/main.ml
   Window setup, event loop, and input dispatch for the Rubik's cube showcase.
   All geometry and rendering logic lives in Rubik_state and Rubik_render;
   this file is restricted to SDL2 lifecycle and keyboard handling.

   Controls:
     U D L R F B       — Rubik face moves (clockwise)
     Shift + U/D/L/R/F/B — inverse (counter-clockwise)
     Left / Right arrow — orbit camera horizontally
     Up / Down arrow    — orbit camera vertically
     Left mouse drag    — orbit camera
     ESC               — quit
*)

open Tsdl
open Tgl3

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let ok = function
  | Ok v           -> v
  | Error (`Msg e) -> prerr_endline e; exit 1

(* ------------------------------------------------------------------ *)
(* Puzzle move dispatch                                                *)
(* ------------------------------------------------------------------ *)

let apply_move cube ev =
  let k    = Sdl.Event.(get ev keyboard_keycode) in
  let mods = Sdl.Event.(get ev keyboard_keymod) in
  let shift = (mods land Sdl.Kmod.lshift) <> 0
           || (mods land Sdl.Kmod.rshift) <> 0 in
  let pick cw ccw = if shift then ccw else cw in
  if      k = Sdl.K.u then pick Rubik_state.move_u  Rubik_state.move_u' cube
  else if k = Sdl.K.d then pick Rubik_state.move_d  Rubik_state.move_d' cube
  else if k = Sdl.K.l then pick Rubik_state.move_l  Rubik_state.move_l' cube
  else if k = Sdl.K.r then pick Rubik_state.move_r  Rubik_state.move_r' cube
  else if k = Sdl.K.f then pick Rubik_state.move_f  Rubik_state.move_f' cube
  else if k = Sdl.K.b then pick Rubik_state.move_b  Rubik_state.move_b' cube
  else cube

(* ------------------------------------------------------------------ *)
(* Main                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  ok (Sdl.init Sdl.Init.video);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  ok (Sdl.gl_set_attribute Sdl.Gl.context_profile_mask
        Sdl.Gl.context_profile_core);
  ok (Sdl.gl_set_attribute Sdl.Gl.doublebuffer 1);
  ok (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);
  ok (Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1);
  ok (Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 4);

  let win =
    ok (Sdl.create_window
          ~w:800 ~h:600
          "Shaders of OCaml — Rubik's cube"
          Sdl.Window.(opengl + shown))
  in
  let ctx = ok (Sdl.gl_create_context win) in
  ok (Sdl.gl_make_current win ctx);
  ignore (Sdl.gl_set_swap_interval 1);

  Gl.enable Gl.depth_test;

  let renderer = Rubik_render.create () in
  let cube     = ref (Rubik_state.create ()) in

  (* Camera angles controlled by arrow keys — no auto-spin. *)
  let cam_yaw   = ref 0.4 in   (* horizontal orbit, radians *)
  let cam_pitch = ref 0.3 in   (* vertical tilt, radians *)
  let cam_step  = 0.08 in      (* radians per key press *)
  let dragging  = ref false in
  let mouse_x   = ref 0 in
  let mouse_y   = ref 0 in
  let mouse_sensitivity = 0.01 in
  let event   = Sdl.Event.create () in
  let running = ref true in

  while !running do
    (* --- Events --- *)
    while Sdl.poll_event (Some event) do
      (match Sdl.Event.(enum (get event typ)) with
       | `Quit    -> running := false
       | `Mouse_button_down ->
         if Sdl.Event.(get event mouse_button_button) = Sdl.Button.left then begin
           dragging := true;
           mouse_x := Sdl.Event.(get event mouse_button_x);
           mouse_y := Sdl.Event.(get event mouse_button_y)
         end
       | `Mouse_button_up ->
         if Sdl.Event.(get event mouse_button_button) = Sdl.Button.left then
           dragging := false
       | `Mouse_motion when !dragging ->
         let x = Sdl.Event.(get event mouse_motion_x) in
         let y = Sdl.Event.(get event mouse_motion_y) in
         let dx = x - !mouse_x and dy = y - !mouse_y in
         cam_yaw := !cam_yaw +. float_of_int dx *. mouse_sensitivity;
         cam_pitch := max (-1.5) (min 1.5
           (!cam_pitch +. float_of_int dy *. mouse_sensitivity));
         mouse_x := x;
         mouse_y := y
       | `Key_down ->
         let k = Sdl.Event.(get event keyboard_keycode) in
         if      k = Sdl.K.escape then running := false
         else if k = Sdl.K.left   then cam_yaw   := !cam_yaw   -. cam_step
         else if k = Sdl.K.right  then cam_yaw   := !cam_yaw   +. cam_step
         else if k = Sdl.K.up     then cam_pitch := !cam_pitch  -. cam_step
         else if k = Sdl.K.down   then cam_pitch := !cam_pitch  +. cam_step
         else cube := apply_move !cube event
       | _ -> ())
    done;

    (* --- Render --- *)
    Gl.clear_color 0.08 0.08 0.1 1.0;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);

    Rubik_render.draw renderer
      ~cube:!cube
      ~cam_yaw:!cam_yaw
      ~cam_pitch:!cam_pitch;

    Sdl.gl_swap_window win
  done;

  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  Sdl.quit ()
