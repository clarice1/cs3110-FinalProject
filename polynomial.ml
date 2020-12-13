(** AF: The list [a, b, ..., f, g] represents the polynomial 
    (ax^k + bx^(k-1) + ... + fx + g).
    RI: If the list is non-empty, a is not 0.
*)
type t = Complex.t list

type bt = {
  poly : t;
  bound : float
}

let zero = []

let from_list lst = lst

let to_list lst = lst

let eval p z =
  let f acc c = Complex.add (Complex.mul acc z) c
  in 
  List.fold_left f Complex.zero p

(**[de_zero p] is [p] without leading 0s*)
let rec de_zero = function 
  | hd :: tl as s -> if hd = Complex.zero then de_zero tl else s
  | x -> x

let rec diff_with_degree deg = function
  | [] | [_] -> []
  | hd :: tl -> (Complex.mul {re = float_of_int deg; im = 0.} hd) :: 
                diff_with_degree (deg - 1) tl

let diff p = diff_with_degree (List.length p - 1) p |> de_zero

(**[of_degree n p] is [p] with [n - degree(p)] many 0s added*)
let rec of_degree n p = if List.length p - 1 <= n 
  then Complex.zero :: of_degree (n - 1) p
  else p

let sum p q = 
  let size = (max (List.length p) (List.length q)) in 
  let p = of_degree size p in 
  let q = of_degree size q in 
  List.map2 (fun a b -> Complex.add a b) p q |> de_zero

let rec mul p = function 
  | hd :: tl -> 
    sum (mul p tl) 
      (List.map (Complex.mul hd) (p @ (List.init (List.length tl) 
                                         (fun x -> Complex.zero))))
  | [] -> []

let from_roots = 
  List.fold_left (fun acc c -> mul (from_list [Complex.one; Complex.neg c]) acc) 
    [Complex.one]


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

let get_bound p = 
  match p with
  | [] | [_] | [_; _] -> bound_deg_l2 (List.length p - 1) p
  | hd :: tl -> 
    let f acc c = 
      acc +. Complex.norm c
    in List.fold_left f 1. tl /. (Complex.norm hd) |> Float.max 1.

let bound p = {
  poly = p;
  bound = get_bound p
}

let forget p = p.poly

let bounded p z =
  if Complex.norm z > get_bound p then None else Some (eval p z)

let bbounded p z = 
  if Complex.norm z > p.bound then None else Some (eval p.poly z)