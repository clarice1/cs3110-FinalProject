open Graphics


type t = {
  mutable bcol : Graphics.color;
  mutable fcol : Graphics.color;
  mutable font : string;
  mutable font_size : int;
  mutable lw : int;
  mutable x : int;
  mutable y : int;
}

let make_default_context () = {
  bcol = Graphics.white;
  fcol = Graphics.black;
  font = "fixed";
  font_size = 12;
  lw = 1;
  x = 0;
  y = 0;
}
let get_bcol g = g.bcol
let get_fcol g = g.fcol
let get_font g = g.font
let get_font_size g = g.font_size
let get_lw g = g.lw
let get_curr g = (g.x , g.y)

let set_bcol g c = g.bcol <- c 
let set_fcol g c = g.fcol <- c
let set_font g f = g.font <- f 
let set_font_size g s = g.font_size <- s
let set_lw g l = g.lw <- l
let set_curr g p = g.x <- fst p; g.y <- snd p

let use_gui g = 
  Graphics.set_color (get_fcol g);
  Graphics.set_font (get_font g);
  Graphics.set_text_size (get_font_size g);
  Graphics.set_line_width (get_lw g);
  let p = get_curr g in 
  Graphics.moveto (fst p) (snd p)

