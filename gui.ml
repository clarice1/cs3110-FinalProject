(** Implementation largely from the book 

    Chailloux, Emmanuel, Manoury, Pascal, Pagano, Bruno (2000) 
    Chapter 13 Constructing a Graphical Interface, 
    Développement d’applications avec Objective Caml

    a translation of which can be found at 
    https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora124.html
*)


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

let set_gc gc lst_opt = 
  set_bcol gc (get_color lst_opt "Background" (get_bcol gc));
  set_fcol gc (get_color lst_opt "Foreground" (get_fcol gc));
  set_font gc (get_string lst_opt "Font" (get_font gc));
  set_font_size gc (get_int lst_opt "FontSize" (get_font_size gc));
  set_lw gc (get_int lst_opt "LineWidth" (get_lw gc))

let make_dc () = make_default_context ()


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
  List.iter (fun s -> change_coord s (dx,dy)) c.children

let add_component c c1 lopt =
  if c1.parent <> [] then failwith "add_component: already a parent"
  else
  if not (is_container c) then failwith "add_component: not a container"
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
      if c.listener rs then (rs.last<-c; true) 
      else false
    else List.exists (fun sun -> send_event rs sun) c.children
  | _  -> c.listener rs

let compute_rich_event s0 s1  = 
  if s0.Graphics.button <> s1.Graphics.button then            
    begin
      if s1.Graphics.button then MouseDown else MouseUp 
    end
  else if s1.Graphics.keypressed then KeyPress                
  else if (s0.Graphics.mouse_x <> s1.Graphics.mouse_x  ) ||  
          (s0.Graphics.mouse_y <> s1.Graphics.mouse_y  ) then
    begin
      if s1.Graphics.button then MouseDrag else MouseMove
    end
  else raise Not_found

let send_new_events res0 res1 = 
  if res0.key_focus != res1.key_focus  then 
    begin
      ignore(send_event  {res1 with re = LostFocus} res0.key_focus); 
      ignore(send_event  {res1 with re = GotFocus} res1.key_focus) 
    end;
  if res0.last != res1.last && (( res1.re = MouseMove) || (res1.re = MouseDrag))
  then
    begin
      ignore (send_event  {res1 with re = MouseExit} res0.last); 
      ignore (send_event  {res1 with re = MouseEnter} res1.last)
    end

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

let make_click e x y = 
  {re = e;
   stat = {Graphics.mouse_x=x; Graphics.mouse_y=y;
           Graphics.key = ' '; Graphics.button = false;
           Graphics.keypressed = false};
   key_focus = empty_component;
   gen_focus = empty_component;
   last = empty_component}


let make_key e ch c = 
  {re = e;
   stat = {Graphics.mouse_x=0; Graphics.mouse_y=0;
           Graphics.key = c; Graphics.button = false;
           Graphics.keypressed = true};
   key_focus = empty_component;
   gen_focus = empty_component;
   last = empty_component}


(******************************************************************************)
(*Defining Components*)
(******************************************************************************)

let display_init comp = 
  Graphics.set_color (get_bcol (get_gc comp)); display_rect  comp ();
  let gui= get_gc comp in 
  use_gui gui;
  let (a,b) = get_curr gui in 
  Graphics.moveto (comp.x+a) (comp.y+b)


let display_label lab comp () = 
  display_init comp; Graphics.draw_string lab

let create_label s lopt =
  let gui = make_default_context () in   set_gc gui lopt; use_gui gui;
  let (w, h) = Graphics.text_size s in 
  let u = create_component w h  in 
  u.mem <- (fun x -> false);  u.display <- display_label s u;
  u.info <- "Label"; u.gc <- gui;
  u

let change_label_text u s = u.display <- display_label s u 

let create_panel b w h lopt =
  let u = create_component w h   in 
  u.container <- b;
  u.info <- if b then "Panel container" else "Panel";
  let gc = make_default_context () in set_gc gc lopt; u.gc <- gc;
  u

let center_layout comp comp1 lopt = 
  comp1.x <- comp.x + ((comp.w -comp1.w) /2); 
  comp1.y <- comp.y + ((comp.h -comp1.h) /2)

let grid_layout (a, b)  c c1 lopt = 
  let px = get_int lopt "Col" 0
  and py = get_int lopt "Row" 0 in 
  if (px >= 0) && (px < a) && ( py >=0) && (py < b) then 
    let lw = c.w /a 
    and lh = c.h /b in 
    if (c1.w > lw) || (c1.h > lh) then 
      failwith "grid_placement: too big component"
    else 
      c1.x <- c.x + px * lw + (lw - c1.w)/2;
    c1.y <- c.y + py * lh + (lh - c1.h)/2;
  else  failwith "grid_placement: bad position"

let open_main_window w h = 
  Graphics.close_graph();
  Graphics.open_graph (" "^ string_of_int w ^"x"^ string_of_int h);
  let u = create_component  w h in
  u.container <- true; 
  u.info <- "Main Window"; 
  u

type button_state = 
  { txt : string; 
    mutable action :  button_state -> unit }

let create_bs st = {txt = st; action = fun x -> ()}

let set_bs_action bs f = bs.action <- f

let get_bs_text bs = bs.txt

let display_button  c bs  () =  
  display_init c; Graphics.draw_string (get_bs_text bs)

let listener_button c bs  e = match get_event e with 
    MouseDown -> bs.action bs; c.display (); true
  | _ -> false

let create_button s lopt     =
  let bs = create_bs s in  
  let gc = make_default_context () in 
  set_gc gc lopt; use_gui gc; 
  let w,h = Graphics.text_size (get_bs_text bs) in 
  let u = create_component w h   in
  u.display <- display_button u bs;
  u.listener <- listener_button u bs;
  u.info <- "Button";
  u.gc <- gc;
  u,bs

(** The index ind shows which string is to be highlighted in the list of values. 
    The sep and height fields describe in pixels the distance between 
    two choices and the height of a choice. The action function takes an 
    argument of type choice_state as an input and does its job using the index.
*)
type choice_state = 
  { mutable ind : int; values : string array; mutable sep : int; 
    mutable height : int; mutable action : choice_state -> unit }

let create_cs sa = 
  {ind = 0; values = sa; sep = 2; 
   height = 1; action = fun x -> ()}

let set_cs_action cs f = cs.action <- f
let get_cs_text cs = cs.values.(cs.ind)

let display_choice comp cs  () = 
  display_init comp;
  let (x,y) = Graphics.current_point()  
  and nb = Array.length cs.values in 
  for i = 0 to nb-1 do 
    Graphics.moveto x (y + i*(cs.height + cs.sep));
    Graphics.draw_string cs.values.(i)
  done;
  Graphics.set_color (get_fcol (get_gc comp));
  Graphics.fill_rect x (y + cs.ind * (cs.height + cs.sep)) comp.w cs.height;
  Graphics.set_color (get_bcol (get_gc comp));      
  Graphics.moveto x  (y + cs.ind * (cs.height + cs.sep));
  Graphics.draw_string cs.values.(cs.ind) 

let listener_choice c cs e = match e.re with 
    MouseUp -> 
    let y = e.stat.Graphics.mouse_y in 
    let cy = c.y in 
    let i = (y - cy) / ( cs.height + cs.sep) in
    cs.ind <- i; c.display ();
    cs.action cs; true
  |   _  -> false 

let create_choice lc lopt  =
  let sa =  (Array.of_list (List.rev lc)) in 
  let cs = create_cs sa in 
  let gc = make_default_context () in 
  set_gc gc lopt;  use_gui gc;
  let awh = Array.map (Graphics.text_size) cs.values in 
  let w = Array.fold_right (fun (x,y) -> max x)  awh 0 
  and h = Array.fold_right (fun (x,y) -> max y)  awh 0 in
  let h1 = (h + cs.sep) * (Array.length sa) + cs.sep  in  
  cs.height <- h;
  let u = create_component w h1   in
  u.display <- display_choice u cs;
  u.listener <- listener_choice u cs ;
  u.info <- "Choice " ^ (string_of_int (Array.length cs.values));
  u.gc <- gc;
  (u, cs)

type textfield_state = 
  { mutable txt : string; 
    dir : bool; mutable ind1 : int; mutable ind2 : int; len : int;
    mutable visible_cursor : bool; mutable cursor : char; 
    mutable visible_echo : bool; mutable echo : char; 
    mutable action : textfield_state -> unit } 

let create_tfs txt size dir  = 
  let l = String.length txt in
  if size < l then failwith "create_tfs";
  let ind1 = if dir then 0 else size-1-l 
  and ind2 = if dir then l else size-1 in 
  let n_txt = (if dir then (txt^(String.make (size-l) ' '))
               else ((String.make (size-l) ' ')^txt )) in
  {txt = n_txt; dir=dir; ind1 = ind1; ind2 = ind2; len=size;
   visible_cursor  = false;  cursor = ' '; visible_echo =  true; echo = ' ';
   action= fun x -> ()}

let set_tfs_action tfs f = tfs.action <- f

let set_tfs_cursor b c tfs =  tfs.visible_cursor <- b; tfs.cursor <- c  

let set_tfs_echo b c tfs =  tfs.visible_echo <- b; tfs.echo <- c  

let get_tfs_text tfs = 
  if tfs.dir then String.sub tfs.txt tfs.ind1 (tfs.ind2 - tfs.ind1)
  else String.sub tfs.txt (tfs.ind1 + 1) (tfs.ind2 - tfs.ind1)

let set_tfs_text tf tfs txt = 
  let l = String.length txt in 
  if l > tfs.len then failwith "set_tfs_text";
  let b = Bytes.of_string tfs.txt in
  String.blit (String.make tfs.len ' ') 0 b 0 tfs.len;
  tfs.txt <- Bytes.to_string b;
  if tfs.dir then 
    (
      let b = Bytes.of_string tfs.txt in
      String.blit txt 0 b 0 l;
      tfs.txt <- Bytes.to_string b;
      tfs.ind2 <- l )
  else   ( 
    let b = Bytes.of_string tfs.txt in
    String.blit txt 0 b (tfs.len -l) l;
    tfs.txt <- Bytes.to_string b; 
    tfs.ind1 <- tfs.len-l-1 );
  tf.display ()

let display_cursor c tfs = 
  if tfs.visible_cursor then 
    ( use_gui (get_gc c);
      let (x,y) = Graphics.current_point() in 
      let (a,b) = Graphics.text_size " " in 
      let shift =  a * (if tfs.dir then max (min (tfs.len-1) tfs.ind2)  0 
                        else tfs.ind2) in  
      Graphics.moveto (c.x + x + shift) (c.y + y);
      Graphics.draw_char tfs.cursor)

let display_textfield c tfs  () = 
  display_init c;
  let s = String.make tfs.len ' ' 
  and txt = get_tfs_text tfs in 
  let nl = String.length txt in 
  if (tfs.ind1 >= 0) && (not tfs.dir) then 
    Graphics.draw_string (String.sub s 0 (tfs.ind1+1) );
  if tfs.visible_echo  then (Graphics.draw_string (get_tfs_text tfs))
  else Graphics.draw_string (String.make (String.length txt) tfs.echo);
  if (nl > tfs.ind2) && (tfs.dir) 
  then Graphics.draw_string (String.sub s tfs.ind2 (nl-tfs.ind2));
  display_cursor c tfs

let bt_set tfs e = 
  let b = Bytes.of_string tfs.txt in 
  Bytes.set b tfs.ind2 (get_key e); Bytes.to_string b

let blt tfs ind = 
  let b = Bytes.of_string tfs.txt in 
  String.blit tfs.txt 1 b 0 ind; Bytes.to_string b

let g32 tfs e = if tfs.dir then 
    ( ( ( if tfs.ind2 >= tfs.len then (
          tfs.txt <- blt tfs (tfs.ind2 - 1); 
          tfs.ind2 <- tfs.ind2 - 1));
          tfs.txt <- bt_set tfs e;
          tfs.ind2 <- tfs.ind2 + 1))
  else 
    ( tfs.txt <- blt tfs tfs.ind2; 
      tfs.txt <- bt_set tfs e;
      if tfs.ind1 >= 0 then tfs.ind1 <- tfs.ind1 - 1)

let listener_text_field u tfs e = 
  match e.re with 
    MouseDown -> take_key_focus e u ; true 
  | KeyPress -> 
    ( if Char.code (get_key e) >= 32 then 
        g32 tfs e
      else ( 
        ( match Char.code (get_key e) with 
            13 -> tfs.action tfs
          |  9 -> lose_key_focus e u
          |  8 -> if (tfs.dir && (tfs.ind2 > 0)) 
            then tfs.ind2 <- tfs.ind2 - 1
            else if (not tfs.dir) && (tfs.ind1 < tfs.len - 1) 
            then tfs.ind1 <- tfs.ind1 + 1                   
          | _ -> ()
        ))); u.display(); true
  | _ -> false

let create_text_field  txt size dir lopt  = 
  let tfs = create_tfs txt size dir in
  let gc = make_default_context () in 
  set_gc gc lopt; use_gui gc;
  let (w,h) = Graphics.text_size (tfs.txt) in 
  let u = create_component w h   in
  u.display <- display_textfield u tfs;
  u.listener <-  listener_text_field u tfs ;
  u.info <- "TextField"; u.gc <- gc;
  u,tfs

type border_state = 
  {mutable relief : string; mutable line : bool;
   mutable bg2 : Graphics.color; mutable size : int}

let create_border_state lopt = 
  {relief = get_string lopt "Relief" "Flat";
   line = get_bool lopt "Outlined" false;
   bg2 = get_color lopt "Background2" Graphics.black;
   size = get_int lopt "Border_size" 2}

let display_border bs c1 c () = 
  let x1 = c.x  and y1 = c.y in
  let x2 = x1+c.w-1 and y2 = y1+c.h-1 in 
  let ix1 = c1.x and iy1 =  c1.y in
  let ix2 = ix1+c1.w-1 and iy2 = iy1+c1.h-1 in 
  let border1 g = Graphics.set_color g;
    Graphics.fill_poly [| (x1,y1);(ix1,iy1);(ix2,iy1);(x2,y1) |] ;
    Graphics.fill_poly [| (x2,y1);(ix2,iy1);(ix2,iy2);(x2,y2) |] 
  in
  let border2 g =  Graphics.set_color g;
    Graphics.fill_poly [| (x1,y2);(ix1,iy2);(ix2,iy2);(x2,y2) |] ;
    Graphics.fill_poly [| (x1,y1);(ix1,iy1);(ix1,iy2);(x1,y2) |] 
  in
  display_rect c ();
  if bs.line then (Graphics.set_color (get_fcol (get_gc c));
                   draw_rect x1 y1 c.w c.h);
  let b1_col = get_bcol ( get_gc c)  
  and b2_col = bs.bg2 in 
  match bs.relief with 
    "Top" ->  (border1 b1_col; border2 b2_col)
  |  "Bot" -> (border1 b2_col; border2 b1_col) 
  |  "Flat" ->  (border1 b1_col; border2 b1_col)
  |  s -> failwith ("display_border: unknown relief: "^s) 

let create_border c lopt = 
  let bs = create_border_state lopt in  
  let p = create_panel true (c.w + 2 * bs.size) 
      (c.h + 2 * bs.size) lopt in 
  set_layout (center_layout p) p;
  p.display <- display_border bs c p;
  add_component p c []; p