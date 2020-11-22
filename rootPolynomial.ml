type t = Complex.t * Complex.t list

type bt = {
  poly : t;
  bound : float
}

let from_roots a lst = (a, lst)

let to_poly ((a, roots) : t) : Polynomial.t = 
  Polynomial.mul (Polynomial.from_list [a]) (Polynomial.from_roots roots)

let bdcomp p = 
  let b = Polynomial.get_bound (to_poly p) in b

let bound p = {
  poly = p;
  bound = bdcomp p
}

let eval (a, roots) z = 
  List.fold_left (fun acc c -> Complex.add acc (Complex.log (Complex.sub z c)))
    (Complex.log a) roots |> Complex.exp

let is_infinite z = match Float.classify_float z with 
  | FP_normal | FP_subnormal | FP_zero -> false
  | FP_infinite | FP_nan -> true

let bbounded p z = 
  let normz = Complex.norm z in 
  if normz > p.bound || is_infinite normz
  then None else Some (eval p.poly z)