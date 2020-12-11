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

type component = 
  { mutable info : string;  
    mutable x : int; mutable y : int; 
    mutable w : int; mutable h : int;
    mutable gc : t;
    mutable container : bool;
    mutable parent : component list;
    mutable children : component list;
    mutable layout_options : lopt;
    mutable layout : component -> lopt -> unit; 
    mutable display : unit -> unit;
    mutable mem : int * int -> bool;
    mutable listener : rich_status -> bool}
and rich_status = 
  {re : rich_event; 
   stat : Graphics.status; 
   mutable key_focus : component; 
   mutable gen_focus : component;
   mutable last : component}

let get_gc c = c.gc

let is_container c = c.container

let in_rect c (xp,yp) = 
  (xp >= c.x) && (xp < c.x + c.w) && (yp >= c.y) && (yp < c.y + c.h)

let display_rect c ()   =
  let gc = get_gc c in 
  Graphics.set_color (get_bcol gc); 
  Graphics.fill_rect c.x c.y c.w c.h

let direct_layout c c1 lopt =
  let px = get_int lopt "PosX" 0
  and py = get_int lopt "PosY" 0 in  
  c1.x <- c.x + px; c1.y <- c.y + py

let create_component iw ih  = 
  let dc = 
    {info="Anonymous";  
     x=0; y=0; w=iw; h=ih; 
     gc = make_default_context() ;  
     container = false;
     parent = []; children = []; 
     layout_options = [];
     layout = (fun a b -> ());
     display = (fun () -> ());
     mem = (fun s -> false); 
     listener = (fun s  -> false);} in 
  dc.layout <- direct_layout dc;
  dc.mem <- in_rect dc;
  dc.display <- display_rect dc;
  dc

let empty_component = create_component 0 0


(******************************************************************************)
(*Child Components*)
(******************************************************************************)

let rec change_coord c (dx, dy) = 
  c.x <- c.x + dx; c.y <- c.y + dy;
  List.iter (fun s -> change_coord s (dx,dy) ) c.children

let add_component c c1 lopt =
  if c1.parent <> [] then failwith "add_component: already a parent"
  else
  if not (is_container c ) then failwith "add_component: not a container"
  else
  if (c1.x + c1.w > c.w) || (c1.y + c1.h > c.h) 
  then failwith "add_component: bad position"
  else
    c.layout c1 lopt;   
  c1.layout_options <- lopt;
  List.iter (fun s -> change_coord s (c1.x, c1.y)) c1.children;
  c.children <- c1::c.children;
  c1.parent <-  [c]

let remove_component c c1 = 
  c.children <- List.filter ((!=) c1) c.children;
  c1.parent <- List.filter ((!=) c) c1.parent;
  List.iter (fun s -> change_coord s (- c1.x, - c1.y)) c1.children;
  c1.x <- 0; c1.y <- 0

let set_layout f c = 
  if c.children = [] then c.layout <- f
  else 
    let ls = c.children in 
    List.iter (remove_component c) ls;
    c.layout <- f;
    List.iter (fun s -> add_component c s s.layout_options) ls

let rec display c = 
  c.display ();
  List.iter (fun cx -> display cx ) c.children


(******************************************************************************)
(*Event Handling*)
(******************************************************************************)

let get_event e = e.re
let get_mouse_x e = e.stat.Graphics.mouse_x
let get_mouse_y e = e.stat.Graphics.mouse_y
let get_key e = e.stat.Graphics.key

let has_key_focus e c = e.key_focus == c
let take_key_focus e c = e.key_focus <- c
let lose_key_focus e c = e.key_focus <- empty_component
let has_gen_focus e c = e.gen_focus == c
let take_gen_focus e c = e.gen_focus <- c
let lose_gen_focus e c = e.gen_focus <- empty_component

let rec send_event rs c = 
  match get_event rs with 
    MouseDown | MouseUp | MouseDrag | MouseMove -> 
    if c.mem(get_mouse_x rs, get_mouse_y rs) then 
      if  List.exists (fun sun -> send_event rs sun) c.children then true
      else ( if c.listener rs then (rs.last <-c; true) else false )
    else false
  | KeyPress | KeyRelease -> 
    if has_key_focus rs c then 
      ( if c.listener rs then (rs.last<-c; true ) 
        else false )
    else List.exists (fun sun -> send_event rs sun) c.children
  | _  -> c.listener rs
(*val send_event : rich_status -> component -> bool = <fun>*)

let compute_rich_event s0 s1  = 
  if s0.Graphics.button <> s1.Graphics.button then            
    begin
      if s0.Graphics.button then MouseDown else MouseUp 
    end
  else if s1.Graphics.keypressed then KeyPress                   
  else if (s0.Graphics.mouse_x <> s1.Graphics.mouse_x  ) ||  
          (s0.Graphics.mouse_y <> s1.Graphics.mouse_y  ) then
    begin
      if s1.Graphics.button then MouseDrag else MouseMove
    end
  else raise Not_found
(*val compute_rich_event : Graphics.status -> Graphics.status -> rich_event =
  <fun>*)


let send_new_events res0 res1 = 
  if res0.key_focus <> res1.key_focus then 
    begin
      ignore(send_event  {res1 with re = LostFocus} res0.key_focus); 
      ignore(send_event  {res1 with re = GotFocus} res1.key_focus) 
    end;
  if (res0.last <> res1.last) && 
     (( res1.re = MouseMove) || (res1.re = MouseDrag)) then
    begin
      ignore(send_event  {res1 with re = MouseExit} res0.last); 
      ignore(send_event  {res1 with re = MouseEnter} res1.last )
    end
(*val send_new_events : rich_status -> rich_status -> unit = <fun>*)

let initial_re = 
  { re = Exposure; 
    stat = { Graphics.mouse_x=0; Graphics.mouse_y=0;
             Graphics.key = ' ';
             Graphics.button = false;
             Graphics.keypressed = false };
    key_focus = empty_component; 
    gen_focus = empty_component;
    last = empty_component }

let loop b_disp b_motion c = 
  let res0 = ref initial_re in
  try 
    display c;
    while true do 
      let lev = [Graphics.Button_down; Graphics.Button_up; 
                 Graphics.Key_pressed] in 
      let flev = if b_motion then (Graphics.Mouse_motion) :: lev 
        else lev in 
      let s = Graphics.wait_next_event flev
      in 
      let res1 = {!res0 with stat = s} in 
      try 
        let res2 = {res1 with 
                    re = compute_rich_event !res0.stat res1.stat} in
        ignore(send_event res2 c);
        send_new_events !res0 res2;
        res0 := res2;      
        if b_disp then display c
      with Not_found -> ()
    done
  with e -> raise e;;
(*val loop : bool -> bool -> component -> unit = <fun>*)

let make_click e x y = 
  {re = e;
   stat = {Graphics.mouse_x=x; Graphics.mouse_y=y;
           Graphics.key = ' '; Graphics.button = false;
           Graphics.keypressed = false};
   key_focus = empty_component;
   gen_focus = empty_component;
   last = empty_component}
(*val make_click : rich_event -> int -> int -> rich_status = <fun>*)


let make_key e ch c = 
  {re = e;
   stat = {Graphics.mouse_x=0; Graphics.mouse_y=0;
           Graphics.key = c; Graphics.button = false;
           Graphics.keypressed = true};
   key_focus = empty_component;
   gen_focus = empty_component;
   last = empty_component};;
(*val make_key : rich_event -> 'a -> char -> rich_status = <fun>*)







(******************************************************************************)
(*Defining Components*)
(******************************************************************************)


(******************************************************************************)
(*Enriched Components*)
(******************************************************************************)
