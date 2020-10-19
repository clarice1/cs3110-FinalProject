(** AF: The array [|a, b, ..., f, g|] represents the polynomial 
    (ax^k + bx^(k-1) + ... + fx + g).
    RI: If the array is non-empty, a is not 0. Which I immediately break in 
    our zero polynomial...
*)
type t = Complex.t array

let zero = [|0|]

let from_list lst = 
  Array.of_list lst

let eval p z =
  Array.length p - 1
(* not done*)

let get_bound p = failwith "Unimplemented"

let bounded p z = failwith "Unimplemented"