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

exception Q

exception Z

let rec_im f col iter ll ur color = 
  Graphics.set_color Graphics.white;
  let (width, height) = Graphics.size_x (), Graphics.size_y () in 
  Graphics.fill_rect 0 0 width height;
  Graphics.set_color col;
  let beginning_matrix = 
    Matrix.cx_init ll ur height width in
  let final_matrix = Matrix.iterate_with_stop_2 f iter beginning_matrix in
  ToImage.colorize color final_matrix |> Graphic_image.of_image 

let recompute s = 
  let im = rec_im s.fb s.col s.iter s.ll_c s.ur_c s.color in 
  {s with width = Graphics.size_x (); height = Graphics.size_y (); im = im}


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

let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + " ^ string_of_float z.im ^ "i"

let string_of_coord s p = 
  cx_of_coord s p |> string_of_complex

let redraw s = 
  Graphics.draw_image s.im 0 0; 
  let (mp1, mp2) as mp = Graphics.mouse_pos () in
  let str = string_of_coord s mp in 
  let (w, h) = Graphics.text_size (str) in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 w h;
  Graphics.moveto 0 0;
  Graphics.set_color Graphics.black;
  Graphics.draw_string str;
  Graphics.set_color s.col;
  Graphics.moveto mp1 mp2

(**[coord_of_cx s z] is the coordinate corrsponding to [z]*)
let coord_of_cx 
    {width; height; ll_c; ur_c} 
    ({re; im} : Complex.t) = 
  let dx = float_of_int (width) /. (ur_c.re -. ll_c.re) *. (re -. ll_c.re) in 
  let dy = float_of_int (height) /. (ur_c.im -. ll_c.im) *. (im -. ll_c.im) in
  (int_of_float dx, int_of_float dy)

let clicker click s mp = 
  try if Graphics.button_down () then click (cx_of_coord s mp)
  with 
  | Z -> ()

let rec go s (click : Complex.t -> unit) button = 
  key_reader s click button;
  if Graphics.size_x () <> s.width || Graphics.size_y () <> s.height then
    resize s click button 
  else
    let (mp1, mp2) as mp = Graphics.mouse_pos () in
    clicker click s mp;
    if mp = s.started_drawing then (*User has not moved mouse *)
      match s.last_point with 
      | None -> 
        Graphics.moveto mp1 mp2;
        go {s with last_point = Some (cx_of_coord s mp)} click button
      | Some x -> 
        let fx = s.f (cx_of_coord s mp) x in
        if is_in_window s fx then
          let (x1, x2) = coord_of_cx s fx in 
          Graphics.lineto x1 x2; 
          Unix.sleepf 0.2;
        else ();
        go {s with last_point = Some fx} click button
    else (
      redraw s;
      go {s with last_point = None; started_drawing = mp} click button)

and resize s click button = 
  let s' = recompute s in 
  redraw s';
  go s' click button

and zoom factor s = 
  let center = Graphics.mouse_pos () |> cx_of_coord s in 
  let x_step = (s.ur_c.re -. s.ll_c.re) /. (2. *. factor) in
  let y_step = (s.ur_c.im -. s.ll_c.im) /. (2. *. factor) in 
  {s with ll_c = {re = center.re -. x_step; im = center.im -. y_step};
          ur_c = {re = center.re +. x_step; im = center.im +. y_step}} 
  |> recompute 

and key_reader s click button = 
  if Graphics.key_pressed () then
    begin
      match Graphics.read_key () with 
      | 'q' -> raise Q
      | 'z' -> raise Z
      | 'c' -> start_with_bonus_state (zoom 2. s) click button
      | 'e' -> start_with_bonus_state 
                 (recompute {s with iter = s.iter * 2}) click button
      | x -> let str = Char.escaped x in 
        begin
          match float_of_string_opt str with 
          | Some factor when factor > 0. -> 
            start_with_bonus_state (zoom factor s) click button
          | _ -> button x
        end
    end

and start_with_bonus_state s click button = 
  try 
    redraw s;
    go s click button
  with | Z -> ()

let start_with_bonus_aux ll_c ur_c col fb color f iter click button im = 
  Graphics.set_color col;
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in
  let started_drawing = (Graphics.mouse_pos ()) in
  let s = {ll_c; ur_c; started_drawing; 
           last_point = None; im; width;
           color; f; 
           fb; iter; height; col} in
  start_with_bonus_state s click button

let start_with_bonus ll_c ur_c col fb color f iter click button im = 
  try start_with_bonus_aux ll_c ur_c col fb color f iter click button im with
  | Q | Z -> ()


let start ll_cx ur_cx c fb fc f iter im = 
  start_with_bonus ll_cx ur_cx c (fun x y -> fb y) fc (fun x y -> f y) iter 
    (fun x -> ()) (fun x -> ()) im
