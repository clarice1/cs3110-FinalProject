(**A state consists of the lower left corner of the window, 
   the upper right corner of the window, 
   the complex number in the lower left corner,
   the complex number in the upper right corner,
   where the current drawing started from,
   and where the line drawn ends as a complex number*)
type state = {
  ll_c : Complex.t;
  ur_c : Complex.t;
  started_drawing : int * int;
  last_point : Complex.t option;
  im : Graphics.image;
  width : int;
  height : int;
  fb : Complex.t -> Complex.t -> Complex.t option;
  f : Complex.t -> Complex.t -> Complex.t;
  color : (int option * Complex.t) -> Color.rgb;
  iter : int;
  col : Graphics.color
}

let recompute s = 
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
  Graphics.set_color s.col;
  let s' = {s with width = Graphics.size_x (); height = Graphics.size_y ()} in
  let beginning_matrix = 
    Matrix.cx_init s.ll_c s.ur_c s'.height s'.width in
  let final_matrix = Matrix.iterate_with_stop_2 s'.fb s.iter beginning_matrix in
  let im = ToImage.colorize s.color final_matrix |> Graphic_image.of_image in
  {s' with im = im}


(**[is_in_window s z] is [true] if and only if [z] is in bounds for the image*)
let is_in_window {ll_c; ur_c} ({re; im} : Complex.t) = 
  re > ll_c.re && re < ur_c.re && im > ll_c.im && im < ur_c.im

(**[cx_of_coord s p] is the complex number corresponding to the point [p]*)
let cx_of_coord {width; height; ll_c; ur_c} 
    (x, y) = 
  Complex.add ll_c 
    {re = (ur_c.re -. ll_c.re)/. 
          (float_of_int (width)) *. (float_of_int x);
     im = (ur_c.im -. ll_c.im)/. 
          (float_of_int (height)) *. (float_of_int y)}

let string_of_coord s p = 
  let z = cx_of_coord s p in 
  string_of_float z.re ^ " + " ^ string_of_float z.im ^ "i"

let redraw s col = 
  Graphics.draw_image s.im 0 0; 
  let (mp1, mp2) as mp = Graphics.mouse_pos () in
  let str = string_of_coord s mp in 
  let (w, h) = Graphics.text_size (str) in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 w h;
  Graphics.moveto 0 0;
  Graphics.set_color Graphics.black;
  Graphics.draw_string str;
  Graphics.set_color col;
  Graphics.moveto mp1 mp2

(**[coord_of_cx s z] is the coordinate corrsponding to [z]*)
let coord_of_cx 
    {width; height; ll_c; ur_c} 
    ({re; im} : Complex.t) = 
  let dx = float_of_int (width) /. (ur_c.re -. ll_c.re) *. (re -. ll_c.re) in 
  let dy = float_of_int (height) /. (ur_c.im -. ll_c.im) *. (im -. ll_c.im) in
  (int_of_float dx, int_of_float dy)

let rec go s color click button = 
  if Graphics.key_pressed () then button (Graphics.read_key ()) else
  if Graphics.size_x () <> s.width || Graphics.size_y () <> s.height then
    let s' = recompute s in 
    redraw s' color;
    go s' color click button
  else
    let (mp1, mp2) as mp = Graphics.mouse_pos () in
    if Graphics.button_down () then click (cx_of_coord s mp);
    if mp = s.started_drawing then (*User has not moved mouse *)
      match s.last_point with 
      | None -> 
        Graphics.moveto mp1 mp2;
        go {s with last_point = Some (cx_of_coord s mp)}  color click button
      | Some x -> 
        let fx = s.f (cx_of_coord s mp) x in
        if is_in_window s fx then
          let (x1, x2) = coord_of_cx s fx in 
          Graphics.lineto x1 x2; 
          Unix.sleepf 0.2;
        else ();
        go {s with last_point = Some fx}  color click button
    else (
      redraw s color;
      go {s with last_point = None; started_drawing = mp} color click button)

let start_with_bonus ll_c ur_c col fb color f iter click button im = 
  Graphics.set_color col;
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in
  let started_drawing = (Graphics.mouse_pos ()) in
  let s = {ll_c; ur_c; started_drawing; 
           last_point = None; im; width;
           color; f; 
           fb; iter; height; col} in
  redraw s col;
  go s col click button

let start ll_cx ur_cx c fb fc f iter im = 
  start_with_bonus ll_cx ur_cx c (fun x y -> fb y) fc (fun x y -> f y) iter 
    (fun x -> ()) (fun x -> ()) im
