let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + "^ string_of_float z.im ^ "i"

(**[cx_of_singletons str] is the complex number representing [str]. 
   Raises: [Failure] if [str] is of the form [f] or [f ^ "i"], where [f] is a 
   valid input to [float_of_string]*)
let cx_of_singleton str : Complex.t = 
  match String.split_on_char 'i' (String.trim str) with 
  | [f] -> {re = float_of_string f; im = 0.}
  | [""; ""] -> Complex.i
  | [f; ""] -> {re = 0.; im = float_of_string f}
  | _ -> failwith "invalid"

let complex_of_string str = 
  str 
  |> String.split_on_char '+'
  |> List.map cx_of_singleton
  |> List.fold_left Complex.add Complex.zero

let lst_cx str = 
  str 
  |> String.split_on_char ','
  |> List.filter ((<>) "")
  |> List.map complex_of_string