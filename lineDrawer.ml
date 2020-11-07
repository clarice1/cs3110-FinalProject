type state = {
  ll : int * int;
  ur : int * int;
  ll_c : Complex.t;
  ur_c : Complex.t;
  started_drawing : int * int;
  last_point : Complex.t option
}

let is_in_window {ll_c; ur_c} ({re; im} : Complex.t) = 
  re > ll_c.re && re < ur_c.re && im > ll_c.im && im < ur_c.im

let cx_of_coord {ll = (l1, l2); ur = (u1, u2); ll_c; ur_c} 
    (x, y) = 
  Complex.add ll_c 
    {re = (ur_c.re -. ll_c.re)/. 
          (float_of_int (u1 - l1)) *. (float_of_int x -. (float_of_int l1));
     im = (ur_c.im -. ll_c.im)/. 
          (float_of_int (u2 - l2)) *. (float_of_int y -. (float_of_int l2))}

let coord_of_cx 
    {ll = (l1, l2); ur = (u1, u2); ll_c; ur_c} 
    ({re; im} : Complex.t) = 
  let dx = float_of_int (u1 - l1) /. (ur_c.re -. ll_c.re) *. (re -. ll_c.re) in 
  let dy = float_of_int (u2 - l2) /. (ur_c.im -. ll_c.im) *. (im -. ll_c.im) in
  (int_of_float dx + l1, int_of_float dy + l2)

let rec go s f = 
  if Graphics.key_pressed () then () else
  if Graphics.button_down () then
    let (mp1, mp2) as mp = Graphics.mouse_pos () in
    if mp = s.started_drawing then
      match s.last_point with 
      | None -> 
        Graphics.moveto mp1 mp2;
        go {s with last_point = Some (cx_of_coord s (Graphics.mouse_pos ()))} f
      | Some x -> 
        let fx = f x in
        if is_in_window s fx then
          let (x1, x2) = coord_of_cx s fx in 
          Graphics.lineto x1 x2; else ();
        go {s with last_point = Some fx} f
    else go {s with last_point = None; started_drawing = mp} f
  else go s f

let start ll ur ll_c ur_c = 
  go {ll; ur; ll_c; ur_c; started_drawing = (Graphics.mouse_pos ()); 
      last_point = None}