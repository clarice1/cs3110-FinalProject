open Graphics

(******************************************************************************)
(*Graphics Context*)
(******************************************************************************)
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


(******************************************************************************)
(*Events*)
(******************************************************************************)
type rich_event = 
  | MouseDown | MouseUp | MouseDrag | MouseMove
  | MouseEnter | MouseExit | Exposure 
  | GotFocus | LostFocus | KeyPress | KeyRelease


(******************************************************************************)
(*Options*)
(******************************************************************************)

type opt_val =
  | Copt of Graphics.color 
  | Sopt of string 
  | Iopt of int 
  | Bopt of bool

type lopt = (string * opt_val) list

exception OptErr

let get_color lo name default = 
  try
    match List.assoc name lo with 
    | Copt c -> c 
    | _  -> raise OptErr
  with Not_found -> default

let get_string lo name default = 
  try
    match List.assoc name lo with 
    | Sopt s -> s 
    | _  -> raise OptErr
  with Not_found -> default

let get_int lo name default = 
  try 
    match List.assoc name lo with 
    | Iopt i -> i 
    | _  -> raise OptErr
  with Not_found -> default

let get_bool lo name default = 
  try
    match List.assoc name lo with 
    | Bopt b -> b 
    | _  -> raise OptErr
  with Not_found -> default
(* Can we combine these into one function? Would we even want to (LOC) *)

let set_gc gc lst_opt = 
  set_bcol gc (get_color lst_opt "Background" (get_bcol gc));
  set_fcol gc (get_color lst_opt "Foreground" (get_fcol gc));
  set_font gc (get_string lst_opt "Font" (get_font gc));
  set_font_size gc (get_int lst_opt "FontSize" (get_font_size gc));
  set_lw gc (get_int lst_opt "LineWidth" (get_lw gc))

let make_dc = make_default_context ()


(******************************************************************************)
(*Construction of Components*)
(******************************************************************************)


(******************************************************************************)
(*Child Components*)
(******************************************************************************)


(******************************************************************************)
(*Event Handling*)
(******************************************************************************)


(******************************************************************************)
(*Defining Components*)
(******************************************************************************)


(******************************************************************************)
(*Enriched Components*)
(******************************************************************************)
