open Complex 
open Polynomial
open Matrix
open ToImage
open Graphics
open Gui

type state = 
  {mutable col : Color.rgb}


let st = {col = {b = 241; r = 205; g = 132;}} 

type success = {color : Color.rgb; coeffs : Complex.t list; ll : Complex.t;
                ur : Complex.t; iter : int; dim : string; name : string}

exception Succeeded of success

type newton_success = 
  {color : Color.rgb; coeffs : Complex.t list; ll : Complex.t;
   ur : Complex.t; iter : int; dim : string; name : string; tol : float}

exception NewtonSucceeded of newton_success

type succmandelbrot = {color : Color.rgb; dim : string; name : string}

exception Succmandelbrot of succmandelbrot

type succ_from_image = 
  {name : string; degree : int; s : float; dim : string; iter_compute : int; 
   iter_draw : int; roots : string; coeff : string; save : string; 
   color : Color.rgb}

exception Succ_from_im of succ_from_image

let action_dir s cs = 
  s.col <- match get_cs_text cs with 
    |"R" -> {b = 39; r = 234; g = 32;}
    | "O" -> {b = 34; r = 230; g = 126;}
    | "Y" -> {b = 15; r = 241; g = 196;}
    | "G" -> {b = 50; r = 0; g = 148;}
    | "B" -> {b = 221; r = 6; g = 82;}
    | "I" -> {b = 100; r = 27; g = 20;}
    | "V" -> {b = 241; r = 205; g = 132;}
    | _ -> failwith "action_dir"

let err = "Could not parse this input"

(**[valid_dim s] is [s] if [s] corresponds to a valid dimension. Otherwise,
   raises [Failure]*)
let valid_dim s = 
  match String.split_on_char 'x' s with 
  | [width; height] -> ignore (int_of_string width + int_of_string height); s
  | _ -> failwith "bad"

(**[wipe_labels lst] changes all labels in [lst] to [""]*)
let wipe_labels = List.iter (fun x -> change_label_text x "") 

(**[get_input f field comp] is [f (get_tfs_text field)] if this is valid, 
   changing the label text of [comp] if not. *)
let get_input f field comp = 
  try f (get_tfs_text field) 
  with | Failure _ -> change_label_text comp err; failwith "bad input"

let action_go s tfs_coeffs tfs_ll tfs_ur tfs_iter tfs_dim tfs_name 
    coeffs_err ll_err ur_err iter_err dim_err ex x = 
  try wipe_labels [coeffs_err; dim_err; ll_err; ur_err; iter_err];
    raise 
      (ex 
         {color = s.col; 
          coeffs = get_input Parse.lst_cx tfs_coeffs coeffs_err;
          dim = get_input valid_dim tfs_dim dim_err;
          ll = get_input Parse.complex_of_string tfs_ll ll_err;
          ur = get_input Parse.complex_of_string tfs_ur ur_err;
          iter = get_input int_of_string  tfs_iter iter_err;
          name = get_tfs_text tfs_name
         })
  with | Failure _ -> ()

let action_go_newton s tfs_coeffs tfs_ll tfs_ur tfs_iter tfs_dim tfs_name 
    tfs_tol coeffs_err ll_err ur_err iter_err dim_err tol_err x = 
  try wipe_labels [coeffs_err; dim_err; ll_err; ur_err; iter_err; tol_err];
    raise 
      (NewtonSucceeded 
         {color = s.col; 
          coeffs = get_input Parse.lst_cx tfs_coeffs coeffs_err;
          dim = get_input valid_dim tfs_dim dim_err;
          ll = get_input Parse.complex_of_string tfs_ll ll_err;
          ur = get_input Parse.complex_of_string tfs_ur ur_err;
          iter = get_input int_of_string  tfs_iter iter_err;
          name = get_tfs_text tfs_name;
          tol = get_input float_of_string tfs_tol tol_err
         })
  with | Failure _ -> ()

let action_go_mandelbrot s tfs_dim tfs_name 
    dim_err x= 
  try wipe_labels [dim_err;];
    raise 
      (Succmandelbrot 
         {color = s.col; 
          dim = get_input valid_dim tfs_dim dim_err;
          name = get_tfs_text tfs_name
         })
  with | Failure _ -> ()

let is_valid s = 
  try ignore(Bmp.load s []); s 
  with | _ -> failwith "not a bmp"

let action_go_from_im s tfs_name tfs_deg tfs_s tfs_dim tfs_itercomp tfs_iterdraw 
    tfs_roots tfs_coeff tfs_save deg_err s_err itercomp_err iterdraw_err 
    file_err dim_err x = 
  try wipe_labels [deg_err; s_err; itercomp_err; iterdraw_err; file_err];
    raise (Succ_from_im {
        name = get_input is_valid tfs_name file_err;
        degree = get_input int_of_string tfs_deg deg_err;
        s = get_input float_of_string tfs_s s_err;
        dim = get_input valid_dim tfs_dim dim_err;
        iter_compute = get_input int_of_string tfs_itercomp itercomp_err;
        iter_draw = get_input int_of_string tfs_iterdraw iterdraw_err;
        roots = get_tfs_text tfs_roots;
        coeff = get_tfs_text tfs_coeff;
        save = get_tfs_text tfs_save;
        color = s.col
      })
  with | Failure _ -> ()


let gray1 = (Graphics.rgb 120 120 120)

let set_col comp = set_bcol (get_gc comp) gray1

let rbox_opts = ["Relief", Sopt "Top"; 
                 "Background", Copt Graphics.red; 
                 "Border_size", Iopt 4]

let add_3 el1 el2 el3 dim1 dim2 = 
  let panel = create_panel true dim1 dim2 [] in
  set_layout (grid_layout (1, 3) panel) panel;
  add_component panel (create_border el1 []) ["Row", Iopt 2];
  add_component panel (create_border el2 rbox_opts) ["Row", Iopt 1];
  add_component panel el3 ["Row", Iopt 0];
  set_col panel;
  set_col el3; panel

let dpan pan1 pan2 pan3 = 
  set_layout (grid_layout (1, 2) pan1) pan1;
  add_component pan1 pan2 ["Row", Iopt 0];
  add_component pan1 pan3 ["Row", Iopt 1];
  set_col pan1

let create_err () = 
  let l = create_label err [] in 
  change_label_text l ""; l

let l_bg = ["Background", Copt gray1]

let create_input w h s = 
  let m = open_main_window w h
  and l_color = create_label "color:" l_bg
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and l_coeffs = create_label "coefficients:" l_bg
  and tf_coeffs, tfs_coeffs = create_text_field "1, 0 + 0i, 0.26" 70 true []
  and coeff_err = create_err ()
  and l_ll = create_label "lower left coordinate:" l_bg
  and tf_ll, tfs_ll = create_text_field "-2 + -2i" 20 true []
  and ll_err = create_err ()
  and l_ur = create_label "upper right coordinate:" l_bg
  and tf_ur, tfs_ur = create_text_field "2 + 2i" 20 true []
  and ur_err = create_err ()
  and l_iter = create_label "number of iterations:" l_bg
  and tf_iter, tfs_iter = create_text_field "100" 20 true []
  and iter_err = create_err ()
  and l_dim = create_label "dimensions of window:" l_bg
  and tf_dim, tfs_dim = create_text_field "500x500" 20 true []
  and dim_err = create_err ()
  and l_name = create_label "name for saving images:" l_bg 
  and tf_name, tfs_name = create_text_field "fractal" 20 true []
  and b, bs = create_button " Go " []
  in 

  let name_panel = create_panel true 180 100 [] in
  set_layout (grid_layout (1, 3) name_panel) name_panel;
  add_component name_panel (create_border l_name []) ["Row", Iopt 2];
  add_component name_panel (create_border tf_name rbox_opts) ["Row", Iopt 1];
  set_col name_panel;

  let ll_panel = add_3 l_ll tf_ll ll_err 180 100 in

  let iter_panel = add_3 l_iter tf_iter iter_err 180 100 in 

  let ur_panel = add_3 l_ur tf_ur ur_err 180 100 in 

  let color_panel = create_panel true 50 250 [] in
  set_layout (grid_layout (1, 2) color_panel) color_panel;
  add_component color_panel (create_border l_color []) ["Row", Iopt 1];
  add_component color_panel (create_border c []) ["Row", Iopt 0];
  set_col color_panel;

  let big_pan = create_panel true 600 250 [] in
  set_layout (grid_layout (3, 1) big_pan) big_pan;
  add_component big_pan color_panel ["Col", Iopt 2];
  set_col big_pan;

  let name_iter_panel = create_panel true 180 200 [] in 
  dpan name_iter_panel name_panel iter_panel;
  add_component big_pan name_iter_panel [];

  let ll_ur_panel = create_panel true 180 200 [] in 
  dpan ll_ur_panel ll_panel ur_panel;
  add_component big_pan ll_ur_panel ["Col", Iopt 1];

  let coeff_panel = add_3 l_coeffs tf_coeffs coeff_err 450 100 in

  let dim_panel = add_3 l_dim tf_dim dim_err 200 100 in 

  set_bcol (get_gc coeff_panel) gray1;
  set_bcol (get_gc coeff_err) gray1;

  let big_pan_2 = create_panel true 600 300 [] in
  set_layout (grid_layout (1, 3) big_pan_2) big_pan_2;
  add_component big_pan_2 dim_panel ["Row", Iopt 2];
  add_component big_pan_2 coeff_panel ["Row", Iopt 1];
  add_component big_pan_2 b ["Row", Iopt 0];
  set_col big_pan_2;

  set_cs_action cs (action_dir s);
  set_bs_action bs 
    (action_go s tfs_coeffs tfs_ll tfs_ur tfs_iter tfs_dim tfs_name 
       coeff_err ll_err ur_err iter_err dim_err (fun s -> Succeeded s));

  set_layout (grid_layout (1,2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 ["Row", Iopt 0];

  set_bcol (get_gc m) gray1;
  m

let create_input_mandelbrot w h s = 
  let m = open_main_window w h
  and l_color = create_label "color:" l_bg
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and l_dim = create_label "dimensions of window:" l_bg
  and tf_dim, tfs_dim = create_text_field "500x500" 20 true []
  and dim_err = create_err ()
  and l_name = create_label "name for saving images:" l_bg 
  and tf_name, tfs_name = create_text_field "mandelbrot" 20 true []
  and b, bs = create_button " Go " []
  in 
  change_label_text dim_err "";
  let rbox_opts = ["Relief", Sopt "Top"; 
                   "Background", Copt Graphics.red; 
                   "Border_size", Iopt 4] in

  let add_3 panel el1 el2 el3 = 
    set_layout (grid_layout (1, 3) panel) panel;
    add_component panel (create_border el1 []) ["Row", Iopt 2];
    add_component panel (create_border el2 rbox_opts) ["Row", Iopt 1];
    add_component panel el3 ["Row", Iopt 0];
    set_col panel;
    set_col el3 in

  let name_panel = create_panel true 180 100 [] in
  set_layout (grid_layout (1, 3) name_panel) name_panel;
  add_component name_panel (create_border l_name []) ["Row", Iopt 2];
  add_component name_panel (create_border tf_name rbox_opts) ["Row", Iopt 1];
  set_col name_panel;

  let color_panel = create_panel true 50 250 [] in
  set_layout (grid_layout (1, 2) color_panel) color_panel;
  add_component color_panel (create_border l_color []) ["Row", Iopt 1];
  add_component color_panel (create_border c []) ["Row", Iopt 0];
  set_col color_panel;

  let big_pan = create_panel true 600 250 [] in
  set_layout (grid_layout (2, 1) big_pan) big_pan;
  add_component big_pan name_panel [];
  add_component big_pan color_panel ["Col", Iopt 1];
  set_col big_pan;

  let dim_panel = create_panel true 200 100 [] in 
  add_3 dim_panel l_dim tf_dim dim_err;

  let big_pan_2 = create_panel true 600 300 [] in
  set_layout (grid_layout (1, 3) big_pan_2) big_pan_2;
  add_component big_pan_2 dim_panel ["Row", Iopt 2];
  add_component big_pan_2 b ["Row", Iopt 0];
  set_col big_pan_2;

  set_cs_action cs (action_dir s);
  set_bs_action bs 
    (action_go_mandelbrot s tfs_dim tfs_name dim_err);

  set_layout (grid_layout (1,2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 ["Row", Iopt 0];

  set_bcol (get_gc m) gray1;
  m

let create_input_newton w h s = 
  let m = open_main_window w h
  and l_coeffs = create_label "roots:" l_bg
  and tf_coeffs, tfs_coeffs = create_text_field "1, -1, i, -i" 70 true []
  and coeff_err = create_err ()
  and l_ll = create_label "lower left coordinate:" l_bg
  and tf_ll, tfs_ll = create_text_field "-2 + -2i" 20 true []
  and ll_err = create_err ()
  and l_ur = create_label "upper right coordinate:" l_bg
  and tf_ur, tfs_ur = create_text_field "2 + 2i" 20 true []
  and ur_err = create_err ()
  and l_iter = create_label "number of iterations:" l_bg
  and tf_iter, tfs_iter = create_text_field "100" 20 true []
  and iter_err = create_err ()
  and l_dim = create_label "dimensions of window:" l_bg
  and tf_dim, tfs_dim = create_text_field "500x500" 20 true []
  and dim_err = create_err ()
  and l_name = create_label "name for saving images:" l_bg 
  and tf_name, tfs_name = create_text_field "fractal" 20 true []
  and l_tolerance = create_label "tolerance:" l_bg
  and tf_tolerance, tfs_tolerance = create_text_field "0.01" 20 true []
  and tol_err = create_err ()
  and b, bs = create_button " Go " []
  in 

  let name_panel = create_panel true 180 100 [] in
  set_layout (grid_layout (1, 3) name_panel) name_panel;
  add_component name_panel (create_border l_name []) ["Row", Iopt 2];
  add_component name_panel (create_border tf_name rbox_opts) ["Row", Iopt 1];
  set_col name_panel;

  let ll_panel = add_3 l_ll tf_ll ll_err 180 100 in

  let iter_panel = add_3 l_iter tf_iter iter_err 180 100 in 

  let ur_panel = add_3 l_ur tf_ur ur_err 180 100  in 

  let big_pan = create_panel true 600 250 [] in
  set_layout (grid_layout (2, 1) big_pan) big_pan;
  set_col big_pan;

  let name_iter_panel = create_panel true 180 200 [] in 
  dpan name_iter_panel name_panel iter_panel;
  add_component big_pan name_iter_panel [];

  let ll_ur_panel = create_panel true 180 200 [] in 
  dpan ll_ur_panel ll_panel ur_panel;
  add_component big_pan ll_ur_panel ["Col", Iopt 1];

  let coeff_panel = add_3 l_coeffs tf_coeffs coeff_err 450 100 in

  let dim_panel = add_3 l_dim tf_dim dim_err 180 100 in 

  let tol_panel = add_3 l_tolerance tf_tolerance tol_err 180 100 in 

  let dim_tol_panel = create_panel true 450 100 [] in 
  set_layout (grid_layout (2, 1) dim_tol_panel) dim_tol_panel;
  add_component dim_tol_panel dim_panel ["Row", Iopt 0];
  add_component dim_tol_panel tol_panel ["Col", Iopt 1];
  set_col dim_tol_panel;

  set_bcol (get_gc coeff_panel) gray1;
  set_bcol (get_gc coeff_err) gray1;

  let big_pan_2 = create_panel true 600 300 [] in
  set_layout (grid_layout (1, 3) big_pan_2) big_pan_2;
  add_component big_pan_2 dim_tol_panel ["Row", Iopt 2];
  add_component big_pan_2 coeff_panel ["Row", Iopt 1];
  add_component big_pan_2 b ["Row", Iopt 0];
  set_col big_pan_2;

  set_bs_action bs 
    (action_go_newton s tfs_coeffs tfs_ll tfs_ur tfs_iter tfs_dim tfs_name 
       tfs_tolerance coeff_err ll_err ur_err iter_err dim_err tol_err);

  set_layout (grid_layout (1,2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 ["Row", Iopt 0];

  set_bcol (get_gc m) gray1;
  m

let create_input_from_image w h s= 
  let m = open_main_window w h 
  and lfile = create_label "file to load:" l_bg 
  and tf_file, tfs_file = create_text_field "bmp examples/cat1.bmp" 21 true [] 
  and rf_err = create_err ()
  and l_coeff = create_label "file for storing coefficients:" l_bg 
  and tf_coeff, tfs_coeff = create_text_field 
      "cat/coefficients.txt" 20 true [] 
  and l_roots = create_label "file for storing roots:" l_bg 
  and tf_roots, tfs_roots = create_text_field "cat/roots.txt" 20 true [] 
  and l_dim = create_label "dimensions of window:" l_bg
  and tf_dim, tfs_dim = create_text_field "500x500" 20 true []
  and dim_err = create_err () 
  and l_name = create_label "name for saving images:" l_bg
  and tf_name, tfs_name = create_text_field "cat/kitty" 20 true []
  and name_err = create_err () 
  and l_deg = create_label "degree of polynomial:" l_bg
  and tf_deg, tfs_deg = create_text_field "400" 20 true []
  and deg_err = create_err () 
  and l_iter = create_label "computing iterations:" l_bg
  and tf_iter, tfs_iter = create_text_field "10000" 20 true []
  and iter_err = create_err ()
  and l_it_draw = create_label "drawing iterations:" l_bg
  and tf_it_draw, tfs_it_draw = create_text_field "30" 20 true []
  and it_draw_err = create_err ()
  and l_s = create_label "s value:" l_bg 
  and tf_s, tfs_s = create_text_field "0.0025" 20 true []
  and s_err = create_err () 
  and l_color = create_label "color:" l_bg
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and b, bs = create_button " Go " [] in

  let file_panel = add_3 lfile tf_file rf_err 220 100 
  and deg_panel = add_3 l_deg tf_deg deg_err 220 100 
  and dim_panel = add_3 l_dim tf_dim dim_err 220 100 
  and s_panel = add_3 l_s tf_s s_err 220 100 
  and iter_pan_1 = add_3 l_iter tf_iter iter_err 220 100 
  and iter_pan_2 = add_3 l_it_draw tf_it_draw it_draw_err 220 100
  and coeff_panel = add_3 l_coeff tf_coeff (create_err ()) 300 100
  and roots_panel = add_3 l_roots tf_roots (create_err ()) 300 100
  and name_panel = add_3 l_name tf_name name_err 300 100

  in 
  let file_s_panel = add_3 file_panel s_panel iter_pan_1 230 330
  and deg_dim_panel = add_3 deg_panel dim_panel iter_pan_2 230 330 in

  let color_panel = create_panel true 50 250 [] in
  set_layout (grid_layout (1, 2) color_panel) color_panel;
  add_component color_panel (create_border l_color []) ["Row", Iopt 1];
  add_component color_panel (create_border c []) ["Row", Iopt 0];
  set_col color_panel;

  let big_pan = create_panel true 700 350 [] in 
  set_layout (grid_layout (3, 1) big_pan) big_pan;
  add_component big_pan file_s_panel [];
  add_component big_pan deg_dim_panel ["Col", Iopt 1];
  add_component big_pan color_panel ["Col", Iopt 2];
  set_col big_pan;

  let big_pan_2 = create_panel true 700 350 [] in
  set_layout (grid_layout (2, 2) big_pan_2) big_pan_2;
  add_component big_pan_2 coeff_panel [];
  add_component big_pan_2 roots_panel ["Row", Iopt 1];
  add_component big_pan_2 name_panel ["Row", Iopt 1; "Col", Iopt 1];
  add_component big_pan_2 b ["Col", Iopt 1];

  set_col big_pan_2;

  set_layout (grid_layout (1, 2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 [];

  set_cs_action cs (action_dir s);
  set_bs_action bs (action_go_from_im s tfs_file tfs_deg tfs_s tfs_dim tfs_iter 
                      tfs_it_draw 
                      tfs_roots tfs_coeff tfs_name deg_err s_err iter_err 
                      it_draw_err 
                      rf_err dim_err);

  set_col m;

  m


let main_drawer () = create_input 700 700 st

let main_drawer_mandelbrot () = create_input_mandelbrot 700 700 st

let main_drawer_newton () = create_input_newton 700 700 st

let mainb _ = loop true false (main_drawer ())

let mandelbrot _ = loop true false (main_drawer_mandelbrot ())

let newton _ = loop true false (main_drawer_newton ())

let create_control w h = 
  let m = open_main_window w h in
  let main_b, main_bs = create_button " Main " [] 
  and newton_b, newton_bs = create_button " Newton " []
  and mandelbrot_b, mandelbrot_bs = create_button " Mandelbrot " []
  and from_image_b, from_image_bs = create_button " From Image " [] in 
  set_layout (grid_layout (2, 2) m) m; 
  add_component m (create_border main_b []) [];
  add_component m (create_border newton_b []) ["Col", Iopt 1];
  add_component m (create_border mandelbrot_b []) ["Row", Iopt 1];
  add_component m (create_border from_image_b []) 
    ["Row", Iopt 1; "Col", Iopt 1];
  set_bs_action main_bs mainb;
  set_bs_action mandelbrot_bs mandelbrot;
  set_bs_action newton_bs newton;
  set_bs_action from_image_bs 
    (fun _ -> loop true false (create_input_from_image 700 700 st));
  set_col m;
  m

let landing = create_control 700 700

let succ (s  : success) = 
  Graphics.close_graph ();
  Graphics.open_graph (" " ^ s.dim);
  let poly = Polynomial.from_list s.coeffs in
  LineDrawer.start s.ll s.ur 
    Graphics.red
    (Polynomial.bounded poly)
    (fun iter -> julia_color iter s.color)
    (Polynomial.eval poly)
    s.iter 
    s.name

let succ_fi (s : succ_from_image) = 
  Graphics.close_graph ();
  let ll : Complex.t = {re = -1.; im = -1.} in 
  let ur : Complex.t = {re = 1.; im = 1.} in 
  Random.self_init ();
  let poly = FromImage.from_file s.name
      ll ur s.degree s.iter_compute s.s
      s.coeff
      s.roots in 
  let bpoly = RootPolynomial.bound poly in
  Graphics.open_graph (" " ^ s.dim) ;
  LineDrawer.start ll ur Graphics.red (RootPolynomial.bbounded bpoly)
    (fun i -> ToImage.julia_color i s.color)
    (RootPolynomial.eval poly)
    s.iter_draw
    s.save

let () = try loop false false landing with 
  | Graphic_failure _ -> ()
  | Succeeded s -> succ s
  | NewtonSucceeded s -> 
    Graphics.close_graph ();
    Graphics.open_graph (" " ^ s.dim);
    Newton.full_newton s.ll s.ur s.iter s.coeffs s.tol
  | Succmandelbrot s -> Mandelbrot.run (" " ^ s.dim) s.color s.name
  | Succ_from_im s -> succ_fi s
