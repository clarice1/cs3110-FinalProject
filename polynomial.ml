(** AF: The list [a, b, ..., f, g] represents the polynomial 
    (ax^k + bx^(k-1) + ... + fx + g).
    RI: If the list is non-empty, a is not 0.
*)
type t = Complex.t list

let zero = []

let from_list lst = lst

let eval p z =
  let f acc c = Complex.add (Complex.mul acc z) c
  in 
  List.fold_left f Complex.zero p

(** [bound_deg_l2 size p] returns infinity if [p] is of degree 1 or less. 
    It returns infinity if [p] does not diverge and it returns 0. if [p] 
    diverges immediately. *)
let bound_deg_l2 size p =
  let norm h = Complex.norm h in
  if size < 1 then infinity 
  else 
    match p with 
    | a :: b :: [] -> 
      if norm a < 1. then infinity else
      if norm a > 1. then 0. else (* note the case b/(1-a)*)
      if a <> Complex.one || norm b = 0. then infinity else 0.
    | _ -> failwith "broke if statement" 

(** [peek lst] returns the first element of [lst].
    Returns: failwith "Empty" if [lst] is the empty list. *)
let peek = function
  | [] -> failwith "Empty"
  | x :: _ -> Complex.norm x

(** [pop lst] returns [lst] without its first element *)
let pop = function
  | [] -> []
  | _ :: xs -> xs

let get_bound p = 
  let size = List.length p - 1 in
  let f acc c = 
    acc +. Complex.norm c
  in
  if size >= 2 then List.fold_left f 0. (pop p) /. peek p |> Float.max 1.
  else bound_deg_l2 size p

let bounded p z =
  if Complex.norm z > get_bound p then None else Some (eval p z)