(** AF: The list [a, b, ..., f, g] represents the polynomial 
    (ax^k + bx^(k-1) + ... + fx + g).
    RI: If the list is non-empty, a is not 0.
*)
type t = Complex.t list

let zero = []

let from_list lst = lst

(** [poly_calc t z size] returns the value of term of degree [size]. *)
let poly_calc t z size = 
  Complex.pow z size |> Complex.mul t

let eval p z = 
  let (one : Complex.t) = {re = 1.; im = 0.} in
  let size_p = float_of_int (List.length p - 1) in
  let rec term p z size acc =
    match p with 
    | [] -> acc
    | h :: t -> 
      term t z (Complex.sub one size) (Complex.add acc (poly_calc h z size))
  in term p z {re = size_p; im = 0.} {re = 0.; im = 0.}

(** [bound_deg_l2 size p] returns infinity if [p] is of degree 1 or less. 
    It returns infinity if [p] does not diverge and it returns 0. if [p] 
    diverges immediately. *)
let bound_deg_l2 size (p : Complex.t list)=
  let norm h = Complex.norm h in
  let not_1 (a : Complex.t) = 
    a.re = -1. || a.im = 1. || a.im = -1. in 
  if size < 1 then infinity 
  else 
    match p with 
    | a :: b :: [] -> 
      if norm a < 1. then infinity else
      if norm a > 1. then 0. else (* note the case b/(1-a)*)
      if not_1 a || norm b = 0. then infinity else 0.
    | _ -> failwith "broke if statement" 

let get_bound (p : Complex.t list) = 
  let size = List.length p - 1 in 
  let rec form acc = function
    | [] -> acc
    | h :: [] -> acc /. (Complex.norm h)
    | h :: t -> form (acc +. (Complex.norm h)) t
  in 
  if size >= 2 then Float.max 1. (form 1. (List.rev p)) else bound_deg_l2 size p

let bounded p z =
  if get_bound p = infinity then Some (eval p z) else None