open Complex 
open Polynomial
open Matrix
open ToImage
open Graphics
open Gui
open Mandelbrot

(**exception Bad_input

   (** [complex_of_float f] is the complex representation of float [f], with [re]
    as [f] and [im] as 0. *)
   let complex_of_float f =
   {re = f; im = 0.}

   (** [lst_of_complex_floats str] is the list of floats from string [str]
    Requires: [str] contains only chars representing floats, separated by spaces
    Raises: [Invalid_argument str] if str is not formatted as specified above*)
   let lst_of_complex_floats str = 
   str
   |> String.trim
   |> String.split_on_char ' '
   |> List.filter (fun x -> x <> " ")
   |> List.map float_of_string
   |> List.map complex_of_float

   (** [string_of_rgb str] returns the rgb value of color [str]. 
    Requires: str is a capital letter representing one of ROYGBIV*)
   let string_of_rgb str = match str with
   | "R" -> let my_col : Color.rgb = {b = 39; r = 234; g = 32;} in my_col
   | "O" -> {b = 34; r = 230; g = 126;}
   | "Y" -> {b = 15; r = 241; g = 196;}
   | "G" -> {b = 50; r = 0; g = 148;}
   | "B" -> {b = 221; r = 6; g = 82;}
   | "I" -> {b = 100; r = 27; g = 20;}
   | "V" -> {b = 241; r = 205; g = 132;}
   | _ -> raise Bad_input

   (** [int_type_checker input] is the corresponding int value of [input]
    Raises: [Bad_input] if [input] is not well-formatted *)
   let int_type_checker input = 
   try int_of_string input
   with 
   | Failure s -> raise Bad_input

   (** [float_type_checker input] is the corresponding float value of [input]
    Raises: [Bad_input] if [input] is not well-formatted *)
   let float_type_checker input =
   try float_of_string input
   with 
   | Failure s -> raise Bad_input

   (** [polynomial_input_type_checker input] is the corresponding list of complex 
    numers (representing a polynomial) of [input]
    Raises: [Bad_input] if [input] is not well-formatted *)
   let polynomial_input_type_checker input = 
   try input |> lst_of_complex_floats |> from_list
   with
   | Invalid_argument s -> raise Bad_input
   | Failure s -> raise Bad_input

   (** [get_good_input ask type_checker error_message] is the input converted to
    its desired type. We prompt the user to enter an input via the string [ask],
    and if the input is not well-formatted, we give the user the string 
    [error_message], asking them to try again. [type_checker] is the function
    that ensures the user's input is well-formatted. *)
   let rec get_good_input ask type_checker error_message =
   print_endline ask;
   print_string "> ";
   let input = read_line()
   in
   try type_checker input 
   with
   | Bad_input -> begin
      print_endline error_message;
      get_good_input ask type_checker error_message
    end

   (** [make_image lst] produces a window with an image  of the Julia Set taken
    by repeatedly applying the polynomial represented by [seq] *)
   let make_image polynomial name =
   let col = get_good_input 
      "Please enter the ROYGBIV color of the image"
      string_of_rgb
      "You did not enter the ROYGBIV color correctly, please try again"
   in 
   let width = get_good_input 
      "Please enter the width of the image"
      int_type_checker
      "You did not enter the width correctly (it is an int), please try again"
   in
   let length = get_good_input 
      "Please enter the length of the image"
      int_type_checker
      "You did not enter the length correctly (it is an int), please try again"
   in
   let iter = get_good_input 
      "Please enter the number (int) of iterations you would like to check"
      int_type_checker
      "You did not enter the number of iterations correctly (it is an int), please try again"
   in 
   let llre = get_good_input 
      "Please enter the lower left coordinate real value (preferrably a negative float)"
      float_type_checker
      "You did not enter the coordinate correctly (it is a float), please try again"
   in
   let llim = get_good_input 
      "Please enter the lower left coordinate imaginary value (preferrably a negative float)"
      float_type_checker
      "You did not enter the coordinate correctly (it is a float), please try again"
   in
   let urre = get_good_input 
      "Please enter the upper right coordinate real value (preferrably a positive float)"
      float_type_checker
      "You did not enter the coordinate correctly (it is a float), please try again"
   in
   let urim = get_good_input 
      "Please enter the upper right coordinate imaginary value (preferrably a positive float)"
      float_type_checker
      "You did not enter the coordinate correctly (it is a float), please try again"
   in                                                     
   let str = " " ^ (string_of_int width) ^ "x" ^ (string_of_int length) 
   in
   Graphics.open_graph str;
   LineDrawer.start {re = llre; im = llim}
    {re = urre; im = urim} Graphics.red 
    (bounded polynomial)
    (fun (iter : int) -> (julia_color iter col))
    (eval polynomial) iter name

   (** [main ()] prompts for the client to input a sequence of numbers, then tells
    the client where to find the outputted .bmp image *)
   let main () =
   ANSITerminal.(print_string [red]
                  "\n\nCreate Your Own Fractal!\n");

   print_endline "What do you want to name your image? (one word, no spaces)";
   print_string "> ";
   let name = read_line ()
   (*Not used for now; we want to incorporate the name later *)     
   in
   let polynomial = get_good_input
      "please enter a sequences of floats (perferably between 0. and 1. separated
    by spaces only. \n
    (e.g. 0.11 0.03 0.2020)\n"
      polynomial_input_type_checker
      "You did not enter the floats correctly. Please try again"
   in 
   make_image polynomial name

   (* Execute the user interface *)
   let () = main () *)
let m = open_main_window 420 150

type state = 
  {mutable col : Color.rgb}


let st = {col = {b = 241; r = 205; g = 132;}} 

type success = {color : Color.rgb; coeffs : Complex.t list; ll : Complex.t;
                ur : Complex.t; iter : int; dim : string; name : string}

exception Succeeded of success

type succmandelbrot = {color : Color.rgb; dim : string; name : string}

exception Succmandelbrot of succmandelbrot

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
    coeffs_err ll_err ur_err iter_err dim_err x= 
  try wipe_labels [coeffs_err; dim_err; ll_err; ur_err; iter_err];
    raise 
      (Succeeded 
         {color = s.col; 
          coeffs = get_input Parse.lst_cx tfs_coeffs coeffs_err;
          dim = get_input valid_dim tfs_dim dim_err;
          ll = get_input Parse.complex_of_string tfs_ll ll_err;
          ur = get_input Parse.complex_of_string tfs_ur ur_err;
          iter = get_input int_of_string  tfs_iter iter_err;
          name = get_tfs_text tfs_name
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

let gray1 = (Graphics.rgb 120 120 120)

let set_col comp = set_bcol (get_gc comp) gray1

let create_input w h s = 
  let m = open_main_window w h
  and l_color = create_label "color:" ["Background", Copt gray1]
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and l_coeffs = create_label "coefficients:" ["Background", Copt gray1]
  and tf_coeffs, tfs_coeffs = create_text_field "1, 0 + 0i, 0.25" 70 false []
  and coeff_err = create_label err []
  and l_ll = create_label "lower left coordinate:" ["Background", Copt gray1]
  and tf_ll, tfs_ll = create_text_field "-2 + -2i" 20 false []
  and ll_err = create_label err []
  and l_ur = create_label "upper right coordinate:" ["Background", Copt gray1]
  and tf_ur, tfs_ur = create_text_field "2 + 2i" 20 false []
  and ur_err = create_label err []
  and l_iter = create_label "number of iterations:" ["Background", Copt gray1]
  and tf_iter, tfs_iter = create_text_field "100" 20 false []
  and iter_err = create_label err []
  and l_dim = create_label "dimensions of window:" ["Background", Copt gray1]
  and tf_dim, tfs_dim = create_text_field "500x500" 20 false []
  and dim_err = create_label err []
  and l_name = create_label "name for saving images:" ["Background", Copt gray1] 
  and tf_name, tfs_name = create_text_field "fractal" 20 false []
  and b, bs = create_button " Go " []
  in 
  change_label_text coeff_err "";
  change_label_text ll_err "";
  change_label_text ur_err "";
  change_label_text iter_err "";
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

  let ll_panel = create_panel true 180 100 [] in
  add_3 ll_panel l_ll tf_ll ll_err;


  let iter_panel = create_panel true 180 100 [] in 
  add_3 iter_panel l_iter tf_iter iter_err;

  let ur_panel = create_panel true 180 100 [] in 
  add_3 ur_panel l_ur tf_ur ur_err;

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
  add_component big_pan name_iter_panel [];

  let ll_ur_panel = create_panel true 180 200 [] in 
  add_component big_pan ll_ur_panel ["Col", Iopt 1];

  let coeff_panel = create_panel true 450 100 [] in
  add_3 coeff_panel l_coeffs tf_coeffs coeff_err;

  let dim_panel = create_panel true 200 100 [] in 
  add_3 dim_panel l_dim tf_dim dim_err;

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
       coeff_err ll_err ur_err iter_err dim_err);

  set_layout (grid_layout (1,2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 ["Row", Iopt 0];

  set_bcol (get_gc m) gray1;
  m

let create_input_mandelbrot w h s = 
  let m = open_main_window w h
  and l_color = create_label "color:" ["Background", Copt gray1]
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and l_dim = create_label "dimensions of window:" ["Background", Copt gray1]
  and tf_dim, tfs_dim = create_text_field "500x500" 20 false []
  and dim_err = create_label err []
  and l_name = create_label "name for saving images:" ["Background", Copt gray1] 
  and tf_name, tfs_name = create_text_field "mandelbrot" 20 false []
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

  let dpan pan1 pan2 pan3 = 
    set_layout (grid_layout (1, 2) pan1) pan1;
    add_component pan1 pan2 ["Row", Iopt 0];
    add_component pan1 pan3 ["Row", Iopt 1];
    set_col pan1 in

  let color_panel = create_panel true 50 250 [] in
  set_layout (grid_layout (1, 2) color_panel) color_panel;
  add_component color_panel (create_border l_color []) ["Row", Iopt 1];
  add_component color_panel (create_border c []) ["Row", Iopt 0];
  set_col color_panel;

  let big_pan = create_panel true 600 250 [] in
  set_layout (grid_layout (3, 1) big_pan) big_pan;
  add_component big_pan color_panel ["Col", Iopt 2];
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
  and l_color = create_label "color:" ["Background", Copt gray1]
  and c, cs = create_choice ["R"; "O"; "Y"; "G"; "B"; "I"; "V"] []
  and l_coeffs = create_label "coefficients:" ["Background", Copt gray1]
  and tf_coeffs, tfs_coeffs = create_text_field "1, 0 + 0i, 0.25" 70 false []
  and coeff_err = create_label err []
  and l_ll = create_label "lower left coordinate:" ["Background", Copt gray1]
  and tf_ll, tfs_ll = create_text_field "-2 + -2i" 20 false []
  and ll_err = create_label err []
  and l_ur = create_label "upper right coordinate:" ["Background", Copt gray1]
  and tf_ur, tfs_ur = create_text_field "2 + 2i" 20 false []
  and ur_err = create_label err []
  and l_iter = create_label "number of iterations:" ["Background", Copt gray1]
  and tf_iter, tfs_iter = create_text_field "100" 20 false []
  and iter_err = create_label err []
  and l_dim = create_label "dimensions of window:" ["Background", Copt gray1]
  and tf_dim, tfs_dim = create_text_field "500x500" 20 false []
  and dim_err = create_label err []
  and l_name = create_label "name for saving images:" ["Background", Copt gray1] 
  and tf_name, tfs_name = create_text_field "fractal" 20 false []
  and b, bs = create_button " Go " []
  in 
  change_label_text coeff_err "";
  change_label_text ll_err "";
  change_label_text ur_err "";
  change_label_text iter_err "";
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

  let ll_panel = create_panel true 180 100 [] in
  add_3 ll_panel l_ll tf_ll ll_err;

  let iter_panel = create_panel true 180 100 [] in 
  add_3 iter_panel l_iter tf_iter iter_err;

  let ur_panel = create_panel true 180 100 [] in 
  add_3 ur_panel l_ur tf_ur ur_err;

  let big_pan = create_panel true 600 250 [] in
  set_layout (grid_layout (3, 1) big_pan) big_pan;
  set_col big_pan;

  let name_iter_panel = create_panel true 180 200 [] in 
  add_component big_pan name_iter_panel [];

  let ll_ur_panel = create_panel true 180 200 [] in 
  add_component big_pan ll_ur_panel ["Col", Iopt 1];

  let coeff_panel = create_panel true 450 100 [] in
  add_3 coeff_panel l_coeffs tf_coeffs coeff_err;

  let dim_panel = create_panel true 200 100 [] in 
  add_3 dim_panel l_dim tf_dim dim_err;

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
       coeff_err ll_err ur_err iter_err dim_err);

  set_layout (grid_layout (1,2) m) m;
  add_component m big_pan ["Row", Iopt 1];
  add_component m big_pan_2 ["Row", Iopt 0];

  set_bcol (get_gc m) gray1;
  m


let main_drawer = create_input 700 700 st

let main_drawer_mandelbrot = create_input_mandelbrot 700 700 st

let main_drawer_newton = create_input_newton 700 700 st

let mainb _ = try loop true false main_drawer
  with 
  | Succeeded s -> 
    Graphics.close_graph ();
    Graphics.open_graph (" " ^ s.dim);
    let poly = Polynomial.from_list s.coeffs in
    LineDrawer.start s.ll s.ur 
      Graphics.red
      (Polynomial.bounded poly)
      (fun iter -> julia_color iter s.color)
      (Polynomial.eval poly)
      s.iter 
      s.name;
    raise (Graphic_failure "Linedrawer completed")

let mandelbrot _ = try loop true false main_drawer_mandelbrot
  with 
  | Succeeded s -> 
    Graphics.close_graph ();
    Graphics.open_graph (" " ^ s.dim);
    LineDrawer.start_with_bonus {re = -2.; im = -2.}
      {re = 2.; im = 2.} 
      Graphics.red
      (fun c -> Polynomial.bounded (poly c))
      (fun i -> ToImage.julia_color i s.color)
      (fun c z -> Complex.add (Complex.mul z z) c)
      100
      color_z2pc
      (fun x -> ())
      s.name

let newton _ = try loop true false main_drawer_newton
  with 
  | Succeeded s -> 
    Graphics.close_graph ();
    Graphics.open_graph (" " ^ s.dim);
    LineDrawer.start_with_bonus {re = -2.; im = -2.}
      {re = 2.; im = 2.} 
      Graphics.red
      (fun c -> Polynomial.bounded (poly c))
      (fun i -> ToImage.julia_color i s.color)
      (fun c z -> Complex.add (Complex.mul z z) c)
      100
      color_z2pc
      (fun x -> ())
      s.name


let create_control w h = 
  let m = open_main_window w h in
  let main_b, main_bs = create_button " Main " [] 
  and newton_b, newton_bs = create_button " Newton " []
  and mandelbrot_b, mandelbrot_bs = create_button " Mandelbrot " [] in 
  set_layout (grid_layout (3, 1) m) m; 
  add_component m main_b [];
  add_component m newton_b ["Col", Iopt 1];
  add_component m mandelbrot_b ["Col", Iopt 2];
  set_bs_action main_bs mainb;
  set_bs_action mandelbrot_bs mandelbrot;
  set_bs_action newton_bs newton;
  set_col m;
  m

let landing = (create_control 700 700)

let () = try loop false false landing with | Graphic_failure _ -> ()


(*let create_conv w h fe = 
  and  l1 = create_label "Francs" [
  "Background", Copt gray1]
  and l2 = create_label "Euros" [
  "Background", Copt gray1]
  and c,cs = create_choice ["->"; "<-"] []
  and tf1,tfs1 = create_text_field  "0" 10 false []
  and tf2,tfs2 = create_text_field "0" 10 false []
  and b,bs = create_button " Go " []
  in 
  let gc = get_gc m in
  set_bcol gc gray1;
  set_layout (grid_layout (3,2) m ) m;
  let tb1 = create_border tf1  []
  and tb2 = create_border tf2  []  
  and bc = create_border c  [] 
  and bb = 
  create_border  b  
  ["Border_size", Iopt 4; "Relief", Sopt "Bot";
   "Background", Copt gray1; "Background2", Copt Graphics.black] 
  in
  set_cs_action cs (action_dir fe);
  set_bs_action bs (action_go fe tf1 tf2 tfs1 tfs2);
  add_component m l1 ["Col",Iopt 0;"Row",Iopt 1];
  add_component m l2 ["Col",Iopt 2;"Row",Iopt 1];
  add_component m bc ["Col",Iopt 1;"Row",Iopt 1];
  add_component m tb1 ["Col",Iopt 0;"Row",Iopt 0];
  add_component m tb2 ["Col",Iopt 2;"Row",Iopt 0];
  add_component m bb  ["Col",Iopt 1;"Row",Iopt 0];
  m,bs,tf1,tf2

  let (m,c,t1,t2) = create_conv 420 150 fe 

  let () = loop false false m *)


