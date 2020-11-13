(**A state consists of the complex number in the lower left corner,
   the complex number in the upper right corner,
   where the current drawing started from,
   where the current line drawn ends as a complex number,
   the image currently displayed in the graphics window,
   the width and height of the image, 
   the function to pass into [Matrix.iterate_with_stop_2] 
   when recomputing is necessary (eg when zooming),
   the function that computes where the line should next go,
   the function that colors points corresponding to the iterations and
   final value,
   the number of iterations in the current image,
   the drawing color,
   the function activated when a point on the image is clicked, 
   the function giving additional key bindings,
   and the state to return to if the user wishes to redo*)
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
  color : int -> (int option * Complex.t) -> Color.rgb;
  iter : int;
  col : Graphics.color;
  click : Complex.t -> unit;
  button : char -> unit;
  mutable redo : state option
}

(**The exception raised when ['q'] is pressed on the keyboard*)
exception Q

(**The exception raised when ['z'] is pressed on the keyboard containing the 
   state when the exception was raised*)
exception Z of state

(**[rec_im] recomutes the image corresponding to the inputted functions*)
let rec_im f col iter ll ur color = 
  Graphics.set_color Graphics.white;
  let (width, height) = Graphics.size_x (), Graphics.size_y () in 
  Graphics.fill_rect 0 0 width height;
  Graphics.set_color col;
  let beginning_matrix = 
    Matrix.cx_init ll ur height width in
  let final_matrix = Matrix.iterate_with_stop_2 f iter beginning_matrix in
  ToImage.colorize (color iter) final_matrix |> Graphic_image.of_image 

(**[recompute s] is the state with image recomputed according to the other
   parameters in [s]*)
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

(**[string_of_complex z] is the string representation of [z]*)
let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + " ^ string_of_float z.im ^ "i"

(**[string_of_coord s p] represents the complex value at coordinate [p]*)
let string_of_coord s p = 
  cx_of_coord s p |> string_of_complex

(**[redraw s] redraws the image stored in state [s], also updating the
   complex value in the bottom left corner*)
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

let catch_z s e1 = 
  try e1 () with 
  | Z s' -> 
    s.redo <- Some s';
    redraw s

(**[clicker] reacts to a user clicking on the mouse corresponding to 
   the complex value clicked and [s.click]*)
let clicker (s : state) (mp : int * int) : unit = 
  (fun () -> if Graphics.button_down () then s.click (cx_of_coord s mp)) 
  |> catch_z s

(**[go] is the main operation unit of [lineDrawer]*)
let rec go s  = 
  key_reader s;
  if Graphics.size_x () <> s.width || Graphics.size_y () <> s.height then
    resize s
  else
    let (mp1, mp2) as mp = Graphics.mouse_pos () in
    clicker s mp;
    if mp = s.started_drawing then (*User has not moved mouse *)
      match s.last_point with 
      | None -> 
        Graphics.moveto mp1 mp2;
        go {s with last_point = Some (cx_of_coord s mp)}
      | Some x -> 
        let fx = s.f (cx_of_coord s mp) x in
        if is_in_window s fx then
          let (x1, x2) = coord_of_cx s fx in 
          Graphics.lineto x1 x2; 
          Unix.sleepf 0.2;
        else ();
        go {s with last_point = Some fx}
    else (
      redraw s;
      go {s with last_point = None; started_drawing = mp})

(**[resize s] starts the graphic with the new window size*)
and resize s= 
  let s' = recompute s in 
  redraw s';
  go s'

(**[zoom factor s] is the state corresponding to zooming in by a factor of 
   [factor] centered at the current mouse position*)
and zoom factor s = 
  let center = Graphics.mouse_pos () |> cx_of_coord s in 
  let x_step = (s.ur_c.re -. s.ll_c.re) /. (2. *. factor) in
  let y_step = (s.ur_c.im -. s.ll_c.im) /. (2. *. factor) in 
  {s with ll_c = {re = center.re -. x_step; im = center.im -. y_step};
          ur_c = {re = center.re +. x_step; im = center.im +. y_step};
          last_point = None} 
  |> recompute 

(**[e s] is the function that activates when ['e'] is clicked in state [s].
   Currently increases iterations by a factor of 2.*)
and e s = 
  (fun () -> recompute {s with iter = s.iter * 2; last_point = None} |> 
             start_with_bonus_state) |> (catch_z s)

(**[c s] is the function that activates when ['c'] is clicked in state [s].
   Currently zooms by a factor of 20 centered at current mouse position*)
and c s = (fun () -> zoom 20. s |> start_with_bonus_state) |> catch_z s

(**[y s] is the function that activates when ['y'] is clicked in state [s].
   Currently undoes most recent click of ['z']*)
and y s = 
  match s.redo with 
  | None -> () 
  | Some s' -> (fun () -> start_with_bonus_state_with_redo s') |> catch_z s

(**[int_press s x] is the functions that activates when a key corresponding
   to a digit '1' - '9' is pressed. Currently zooms in by that amount,
   centered on the current mouse position*)
and int_press s x = 
  (fun () -> 
     let factor = float_of_int (int_of_char x - int_of_char '0') in 
     start_with_bonus_state (zoom factor s))
  |> catch_z s

(**[key_reader] reads user input from the keyboard and reacts according
   to which key was pressed.*)
and key_reader s = 
  if Graphics.key_pressed () then
    begin
      match Graphics.read_key () with 
      | 'q' -> raise Q
      | 'z' -> raise (Z s)
      | 'c' -> c s
      | 'e' -> e s
      | 'y' -> y s
      | x when '1' <= x && x <= '9' -> int_press s x
      | x -> s.button x
    end

and start_with_bonus_state_with_redo (s : state) : unit =
  redraw s;
  go {s with last_point = None}

and start_with_bonus_state (s : state) : unit = 
  redraw s;
  go {s with last_point = None; redo = None}

let start_with_bonus_aux ll_c ur_c col fb color f iter click button im = 
  Graphics.set_color col;
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in
  let started_drawing = (Graphics.mouse_pos ()) in
  let s = {ll_c; ur_c; started_drawing; 
           last_point = None; im; width;
           color; f; 
           fb; iter; height; col; click; button; redo = None} in
  start_with_bonus_state s

let start_with_bonus ll_c ur_c col fb color f iter click button im = 
  try start_with_bonus_aux ll_c ur_c col fb color f iter click button im with
  | Q | Z _ -> ()


let start ll_cx ur_cx c fb fc f iter im = 
  start_with_bonus ll_cx ur_cx c (fun x y -> fb y) fc (fun x y -> f y) iter 
    (fun x -> ()) (fun x -> ()) im