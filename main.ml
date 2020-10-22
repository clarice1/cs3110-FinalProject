(** [lst_of_ints str] is the list of ints from string [str]
    Requires: [str] contains only chars representing ints, separated by spaces
    Raises: [Invalid_argument str] if str is not formatted as specified above*)
let lst_of_ints str = 
  str
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> " ")
  |> List.map int_of_string

(** [make_image lst] produces .bmp image representation of the Julia Set taken
    by repeatedly applying the polynomial represented by [seq] *)
let make_image seq =
  let lst = lst_of_ints seq 
  in
  failwith "Unimplemented"                                                (*Still not completely sure on how the process works, *)

(** [main ()] prompts for the client to input a sequence of numbers, then tells
    the client where to find the outputted .bmp image *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nCreate Your Own Fractal!\n");
  print_endline "Please enter a sequence of numbers separated by spaces only.\n";
  print_endline "(e.g. 11 03 2020)\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()                                             (*QUESTION: do we need this exception check?*)
  | seq -> make_image seq

(* Execute the user interface *)
let () = main ()