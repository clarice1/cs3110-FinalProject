(**A state consists of the lower left corner of the window, 
   the upper right corner of the window, 
   the complex number in the lower left corner,
   the complex number in the upper right corner,
   where the current drawing started from,
   and where the line drawn ends as a complex number*)
type state = {
  ll : int * int;
  ur : int * int;
  ll_c : Complex.t;
  ur_c : Complex.t;
  started_drawing : int * int;
  last_point : Complex.t option
}

(**[is_in_window s z] is [true] if and only if [z] is in bounds for the image*)
let is_in_window {ll_c; ur_c} ({re; im} : Complex.t) = 
  re > ll_c.re && re < ur_c.re && im > ll_c.im && im < ur_c.im

(**[cx_of_coord s p] is the complex number corresponding to the point [p]*)
let cx_of_coord {ll = (l1, l2); ur = (u1, u2); ll_c; ur_c} 
    (x, y) = 
  Complex.add ll_c 
    {re = (ur_c.re -. ll_c.re)/. 
          (float_of_int (u1 - l1)) *. (float_of_int x -. (float_of_int l1));
     im = (ur_c.im -. ll_c.im)/. 
          (float_of_int (u2 - l2)) *. (float_of_int y -. (float_of_int l2))}

(**[coord_of_cx s z] is the coordinate corrsponding to [z]*)
let coord_of_cx 
    {ll = (l1, l2); ur = (u1, u2); ll_c; ur_c} 
    ({re; im} : Complex.t) = 
  let dx = float_of_int (u1 - l1) /. (ur_c.re -. ll_c.re) *. (re -. ll_c.re) in 
  let dy = float_of_int (u2 - l2) /. (ur_c.im -. ll_c.im) *. (im -. ll_c.im) in
  (int_of_float dx + l1, int_of_float dy + l2)

let rec go s f im click button = 
  if Graphics.key_pressed () then button (Graphics.read_key ()) else
    let (mp1, mp2) as mp = Graphics.mouse_pos () in
    if Graphics.button_down () then click (cx_of_coord s mp);
    if mp = s.started_drawing then (*User has not moved mouse *)
      match s.last_point with 
      | None -> 
        Graphics.moveto mp1 mp2;
        go {s with last_point = Some (cx_of_coord s mp)} f im click button
      | Some x -> 
        let fx = f (cx_of_coord s mp) x in
        if is_in_window s fx then
          let (x1, x2) = coord_of_cx s fx in 
          Graphics.lineto x1 x2; 
          Unix.sleepf 0.2;
        else ();
        go {s with last_point = Some fx} f im click button
    else (
      Graphics.draw_image im 0 0; 
      go {s with last_point = None; started_drawing = mp} f im) click button

let start_with_bonus ll ur ll_c ur_c = 
  Graphics.set_color Graphics.red;
  go {ll; ur; ll_c; ur_c; started_drawing = (Graphics.mouse_pos ()); 
      last_point = None}

let start ll ur ll_c ur_c f im = 
  start_with_bonus ll ur ll_c ur_c (fun x y -> f y) im 
    (fun x -> ()) (fun x -> ())
