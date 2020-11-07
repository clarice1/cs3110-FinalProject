open Complex 
open Polynomial
open Matrix
open ToImage
open Graphics

(** [complex_of_float f] is the complex representation of float [f], with [re]
    as [f] and [im] as 0. *)
let complex_of_float f =
  {re = f; im = 0.}

(** [lst_of_ints str] is the list of ints from string [str]
    Requires: [str] contains only chars representing ints, separated by spaces
    Raises: [Invalid_argument str] if str is not formatted as specified above*)
let lst_of_complex_floats str = 
  str
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> " ")
  |> List.map float_of_string
  |> List.map complex_of_float

(** [string_of_rgb str] returns the rgb value of color [str]. *)
let string_of_rgb str = match str with
  | "red" -> let my_col : Color.rgb = {b = 39; r = 234; g = 32;} in my_col
  | "orange" -> {b = 34; r = 230; g = 126;}
  | "yellow" -> {b = 15; r = 241; g = 196;}
  | "green" -> {b = 50; r = 0; g = 148;}
  | "blue" -> {b = 221; r = 6; g = 82;}
  | "indigo" -> {b = 100; r = 27; g = 20;}
  | _ -> {b = 241; r = 205; g = 132;}



(** [make_image lst] produces .bmp image representation of the Julia Set taken
    by repeatedly applying the polynomial represented by [seq] *)
let make_image seq =
  print_endline "please enter the ROYGBIV color of the image";
  print_string "> ";
  let col = string_of_rgb (read_line ())
  in 
  print_endline "please enter the width of the image";
  print_string "> ";
  let width = int_of_string (read_line ())
  in
  print_endline "please enter the length of the image";
  print_string "> "; 
  let length = int_of_string (read_line ())
  in
  print_endline "please enter the number (int) of iterations you would like to check";
  print_string "> ";
  let iter = int_of_string (read_line ())
  in 
  print_endline "lower left coordinate real value? (preferrably a negative float)";
  print_string "> ";
  let llre = float_of_string (read_line ())
  in
  print_endline "lower left coordinate imaginary value? (preferrably a negative float)";
  print_string "> ";
  let llim = float_of_string (read_line ())
  in
  print_endline "upper right coordinate real value? (preferrably a negative float)";
  print_string "> ";
  let urre = float_of_string (read_line ())
  in
  print_endline "upper right coordinate imaginary value? (preferrably a negative float)";
  print_string "> ";
  let urim = float_of_string (read_line ())
  in                                                     
  let matrix = cx_init {re = llre; im = llim} 
      {re = urre; im = urim} 
      length 
      width
  in
  let polynomial = from_list (lst_of_complex_floats seq) 
  in                                          (*Hardcoding color Blue for now *) 
  colorize 
    (julia_color iter col) 
    (iterate_with_stop (bounded polynomial) iter matrix)

(** [main ()] prompts for the client to input a sequence of numbers, then tells
    the client where to find the outputted .bmp image *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nCreate Your Own Fractal!\n");

  print_endline "What do you want to name your image? (one word, no spaces)";
  print_string "> ";
  let name = read_line ()
  in
  print_endline "Please enter a sequence of floats between 0. and 1. separated 
                  by spaces only.\n";
  print_endline "(e.g. 0.11 0.03 0.2020)\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | seq -> Bmp.save (name ^ ".bmp") [] (make_image seq); 

    print_endline "you can find the .bmp file here. "

(* Execute the user interface *)
let () = main ()