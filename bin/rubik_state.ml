(* bin/rubik_state.ml
   Pure OCaml Rubik's cube state machine — no OpenGL dependencies.

   The 3×3 Rubik's cube has 26 visible cubies.  Each cubie lives at a grid
   position (x, y, z) where every coordinate is in {-1, 0, 1} (the centre
   piece (0,0,0) is excluded because it is never visible).

   Orientation is tracked as a 3×3 column-major rotation matrix stored as a
   flat float array of length 9:
     [m0 m3 m6]
     [m1 m4 m7]
     [m2 m5 m8]
   i.e. column j is at indices 3j, 3j+1, 3j+2.

   A move rotates the slice at a given coordinate by 90° and updates both the
   position and the orientation of every cubie in that slice.
*)

(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

type cubie =
  { pos    : int * int * int   (* current grid position, coords in {-1,0,1} *)
  ; home   : int * int * int   (* position at creation; sticker colours are
                                  fixed relative to this home position       *)
  ; orient : float array        (* 3×3 col-major rotation matrix, len=9;
                                  maps cubie-local directions to world       *)
  }

type t = cubie array

(* ------------------------------------------------------------------ *)
(* Matrix helpers (3×3 col-major)                                      *)
(* ------------------------------------------------------------------ *)

let identity3 () : float array =
  [| 1.; 0.; 0.;   0.; 1.; 0.;   0.; 0.; 1. |]

(* Multiply two 3×3 col-major matrices: result = a * b *)
let mat3_mul (a : float array) (b : float array) : float array =
  let r = Array.make 9 0. in
  for col = 0 to 2 do
    for row = 0 to 2 do
      let s = ref 0. in
      for k = 0 to 2 do
        s := !s +. a.(k * 3 + row) *. b.(col * 3 + k)
      done;
      r.(col * 3 + row) <- !s
    done
  done;
  r

(* Apply a 3×3 col-major matrix to an integer grid position, rounding to
   nearest integer (positions are always on the {-1,0,1} lattice). *)
let mat3_apply_pos (m : float array) (x, y, z) : int * int * int =
  let fx = float_of_int x
  and fy = float_of_int y
  and fz = float_of_int z in
  let rx = m.(0) *. fx +. m.(3) *. fy +. m.(6) *. fz in
  let ry = m.(1) *. fx +. m.(4) *. fy +. m.(7) *. fz in
  let rz = m.(2) *. fx +. m.(5) *. fy +. m.(8) *. fz in
  (int_of_float (Float.round rx),
   int_of_float (Float.round ry),
   int_of_float (Float.round rz))

(* ------------------------------------------------------------------ *)
(* 90° rotation matrices.                                              *)
(*                                                                     *)
(* Each is a proper rotation (det = +1) about the positive axis by     *)
(* +90° (_pos, right-hand rule) or -90° (_neg).  Written out as the    *)
(* image of (x,y,z):                                                   *)
(*                                                                     *)
(*   rot_y_pos : (x,y,z) -> ( z, y, -x)   i.e. R_y(+90)                 *)
(*   rot_y_neg : (x,y,z) -> (-z, y,  x)   i.e. R_y(-90)                 *)
(*   rot_x_pos : (x,y,z) -> ( x,-z,  y)   i.e. R_x(+90)                 *)
(*   rot_x_neg : (x,y,z) -> ( x, z, -y)   i.e. R_x(-90)                 *)
(*   rot_z_pos : (x,y,z) -> (-y, x,  z)   i.e. R_z(+90)                 *)
(*   rot_z_neg : (x,y,z) -> ( y,-x,  z)   i.e. R_z(-90)                 *)
(*                                                                     *)
(* A clockwise turn of a face, viewed from outside that face, is always *)
(* a -90° rotation about the outward-pointing axis.                     *)
(* ------------------------------------------------------------------ *)

let rot_y_pos () =
  [|  0.; 0.;-1.;
      0.; 1.; 0.;
      1.; 0.; 0. |]

let rot_y_neg () =
  [|  0.; 0.; 1.;
      0.; 1.; 0.;
     -1.; 0.; 0. |]

let rot_x_pos () =
  [| 1.; 0.; 0.;
     0.; 0.; 1.;
     0.;-1.; 0. |]

let rot_x_neg () =
  [| 1.; 0.; 0.;
     0.; 0.;-1.;
     0.; 1.; 0. |]

let rot_z_pos () =
  [|  0.; 1.; 0.;
     -1.; 0.; 0.;
      0.; 0.; 1. |]

let rot_z_neg () =
  [|  0.;-1.; 0.;
      1.; 0.; 0.;
      0.; 0.; 1. |]

(* ------------------------------------------------------------------ *)
(* Cube construction                                                   *)
(* ------------------------------------------------------------------ *)

let all_positions : (int * int * int) list =
  let coords = [-1; 0; 1] in
  let positions = ref [] in
  List.iter (fun x ->
    List.iter (fun y ->
      List.iter (fun z ->
        if not (x = 0 && y = 0 && z = 0) then
          positions := (x, y, z) :: !positions
      ) coords
    ) coords
  ) coords;
  !positions

let create () : t =
  all_positions
  |> List.map (fun pos -> { pos; home = pos; orient = identity3 () })
  |> Array.of_list

(* ------------------------------------------------------------------ *)
(* Generic move: select slice, rotate positions and orientations       *)
(* ------------------------------------------------------------------ *)

let apply_move (rot : float array) (axis : [`X | `Y | `Z]) (coord : int) (cube : t) : t =
  Array.map (fun cubie ->
    let (x, y, z) = cubie.pos in
    let on_slice =
      match axis with
      | `X -> x = coord
      | `Y -> y = coord
      | `Z -> z = coord
    in
    if not on_slice then cubie
    else
      { pos    = mat3_apply_pos rot cubie.pos
      ; home   = cubie.home
      ; orient = mat3_mul rot cubie.orient
      }
  ) cube

(* ------------------------------------------------------------------ *)
(* The 6 standard face moves and their inverses                        *)
(* ------------------------------------------------------------------ *)

(* U: top slice (y=+1), clockwise when viewed from above (+Y) *)
let move_u  cube = apply_move (rot_y_neg ()) `Y   1  cube
let move_u' cube = apply_move (rot_y_pos ()) `Y   1  cube

(* D: bottom slice (y=-1), clockwise when viewed from below (-Y);
   outward axis is -Y so clockwise = +90° about +Y *)
let move_d  cube = apply_move (rot_y_pos ()) `Y (-1) cube
let move_d' cube = apply_move (rot_y_neg ()) `Y (-1) cube

(* R: right slice (x=+1), clockwise when viewed from the right (+X) *)
let move_r  cube = apply_move (rot_x_neg ()) `X   1  cube
let move_r' cube = apply_move (rot_x_pos ()) `X   1  cube

(* L: left slice (x=-1), clockwise when viewed from the left (-X);
   outward axis is -X so clockwise = +90° about +X *)
let move_l  cube = apply_move (rot_x_pos ()) `X (-1) cube
let move_l' cube = apply_move (rot_x_neg ()) `X (-1) cube

(* F: front slice (z=+1), clockwise when viewed from the front (+Z) *)
let move_f  cube = apply_move (rot_z_neg ()) `Z   1  cube
let move_f' cube = apply_move (rot_z_pos ()) `Z   1  cube

(* B: back slice (z=-1), clockwise when viewed from behind (-Z);
   outward axis is -Z so clockwise = +90° about +Z *)
let move_b  cube = apply_move (rot_z_pos ()) `Z (-1) cube
let move_b' cube = apply_move (rot_z_neg ()) `Z (-1) cube
