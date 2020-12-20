let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + "^ string_of_float z.im ^ "i"

(**[cx_of_singletons str] is the complex number representing [str]. 
   Raises: [Failure] if [str] is of the form [f] or [f ^ "i"], where [f] is a 
   valid input to [float_of_string] or empty*)
let cx_of_singleton str : Complex.t = 
  match String.split_on_char 'i' (String.trim str) with 
  | [f] -> {re = float_of_string f; im = 0.}
  | [""; ""] -> Complex.i
  | ["-"; ""] -> {re = 0.; im = -1.}
  | [f; ""] -> {re = 0.; im = float_of_string f}
  | _ -> failwith "invalid"

let split_minus str = 
  match String.split_on_char '-' (String.trim str) with 
  | h :: [] -> [h]
  | "" :: h2 :: [] -> ["-" ^ h2]
  | "" :: h2 :: h3 :: [] -> ["-" ^ h2; "-" ^ h3]
  | _ -> failwith "invalid" 

let split_plus str = 
  let con h = String.contains h 'i' in 
  match String.split_on_char '+' str with 
  | h :: [] -> split_minus h
  | h1 :: h2 :: [] -> 
    if con h1 && con h2 then failwith "invalid" else 
    if con h1 || con h2 then split_minus h1 @ split_minus h2 
    else failwith "invalid"
  | _ -> failwith "invalid"

let complex_of_string str = 
  str 
  |> split_plus
  |> List.map cx_of_singleton
  |> List.fold_left Complex.add Complex.zero

let lst_cx str = 
  str 
  |> String.split_on_char ','
  |> List.filter ((<>) "")
  |> List.map complex_of_string