open Complex 
open Polynomial
open Matrix
open ToImage

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

(** [make_image lst] produces .bmp image representation of the Julia Set taken
    by repeatedly applying the polynomial represented by [seq] *)
let make_image seq =
  print_endline "please enter the width of the image";
  print_string "> ";
  let width = int_of_string (read_line ())
  in
  print_endline "please enter the length of the image";
  print_string "> "; 
  let length = int_of_string (read_line ())
  in                                                    
  let matrix = cx_init {re = -10.; im = -10.} 
      {re = 10.; im = 10.} 
      length 
      width
  in
  let polynomial = from_list (lst_of_complex_floats seq) 
  in                                            (*Hardcoding color Blue for now *) 
  colorize (julia_color 100 R) 
    (iterate_with_stop (bounded polynomial) 100 matrix)


(*ToImage.colorize (ToImage.julia_color 0) matrix*)

(** [main ()] prompts for the client to input a sequence of numbers, then tells
    the client where to find the outputted .bmp image *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nCreate Your Own Fractal!\n");
  print_endline "Please enter a sequence of floats between 0. and 1. separated 
                  by spaces only.\n";
  print_endline "(e.g. 0.11 0.03 0.2020)\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()                                             (*QUESTION: do we need this exception check?*)
  | seq -> Bmp.save "test.bmp" [] (make_image seq); 

    print_endline "you can find the .bmp file here. "

(* Execute the user interface *)
let () = main ()