open Complex 
open Polynomial
open Matrix
open ToImage
open Graphics
open Gui

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

type state_conv = 
  { mutable a:float; mutable b:float; mutable dir : bool;
    fa : float; fb : float } ;;
let e =  6.55957074
let fe = { a =0.0; b=0.0; dir = true; fa = e; fb = 1./. e};;
let calculate fe = 
  if fe.dir then fe.b <- fe.a /. fe.fa else fe.a <- fe.b /. fe.fb
let action_dir fe cs = match get_cs_text cs with 
    "->" -> fe.dir <- true
  | "<-" -> fe.dir <- false
  | _ -> failwith "action_dir";;
let action_go fe tf_fr tf_eu tfs_fr tfs_eu  x = 
  if fe.dir then 
    let r = float_of_string (get_tfs_text tfs_fr) in 
    fe.a <- r; calculate fe; 
    let sr = Printf.sprintf "%.2f" fe.b in 
    set_tfs_text tf_eu tfs_eu  sr
  else 
    let r = float_of_string (get_tfs_text tfs_eu) in
    fe.b <- r; calculate fe;
    let sr = Printf.sprintf "%.2f" fe.a in 
    set_tfs_text tf_fr tfs_fr sr;;

let create_conv w h fe = 
  let gray1 = (Graphics.rgb 120 120 120) in 
  let m = open_main_window w h
  and  l1 = create_label "Francs" ["Font", courier_bold_24;
                                   "Background", Copt gray1]
  and l2 = create_label "Euros" ["Font", courier_bold_24;
                                 "Background", Copt gray1]
  and c,cs = create_choice ["->"; "<-"] ["Font", courier_bold_18]
  and tf1,tfs1 = create_text_field  "0" 10 false ["Font", courier_bold_18]
  and tf2,tfs2 = create_text_field "0" 10 false ["Font", courier_bold_18]
  and b,bs = create_button " Go " ["Font", courier_bold_24]
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
  m,bs,tf1,tf2;;

let (m,c,t1,t2) = create_conv 420 150 fe ;; 

let () = loop false false m


