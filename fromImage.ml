(**[an z [c1,...,cn]] is [|z-c1||z-c2|...|z-cn|]*)
let an z lst = 
  List.fold_left (fun acc zj -> (Float.log Complex.(norm (sub z zj))) +. acc) 
    0. lst |> Float.exp

(**[anp1 [hd,a1,...,an]] is [an hd [a1,...,an]]*)
let anp1 = function 
  | hd :: tl -> an hd tl
  | [] -> 0.

(**[choose_n n vals] is [min n (Array.length vals - 1)] many random values 
   from [vals]. The order of elements in [vals] is changed. *)
let choose_n n vals = 
  let num = min (n - 1) (Array.length vals - 2) in 
  for i = 0 to num do 
    let vali = vals.(i) in 
    let j = Random.int ((Array.length vals) - i - 1) in 
    vals.(i) <- vals.(j + i);
    vals.(j + i) <- vali
  done;
  Array.sub vals 0 num

(**[optimize acc vals iterations] randomly picks [iterations] many values from
   [vals], giving the value that optimizes [an z acc] and the value of [an] at
   that maximum*)
let optimize acc vals iterations = 
  let possibilities = choose_n iterations vals in 
  Array.fold_left (fun (current_val, current_max) z -> 
      let zan = an z acc in 
      if zan >= current_max then (z, zan) else (current_val, current_max))
    (Complex.zero, an Complex.zero acc)
    possibilities

(**[compute acc vals iterations n] finds [n] points which, together with those
   in [acc], optimize the function defining leja points. *)
let rec compute acc vals iterations n = 
  if n <= 0 then acc else 
    let (argmax, _) = optimize acc vals iterations in 
    compute (argmax :: acc) vals iterations (n - 1)

(**[poly_vals vals n an s] is the polynomial with roots of zero and those 
   elements in [vals], with a coefficient as specified in the paper*)
let poly_vals vals n an s = 
  let nf = float_of_int n in 
  RootPolynomial.from_roots {re = exp(-. nf *. s /. 2.) /. an; im = 0.}
    (Complex.zero :: vals)

(**[poly values n iterations s] computes the polynomial corresponding to the
   leja points of the values in [values], as described in the paper. *)
let poly values n iterations s = 
  let leja = compute [] values iterations n in 
  let (dn_args, an) = (leja, anp1 leja) in 
  poly_vals dn_args n an s

(**[black_check (coord,c)] is true if and only if [c] is black or [coord] is
   sufficiently close to the origin. *)
let black_check ((coord, c) : Complex.t * Color.rgb) = 
  c = {r = 0; g = 0; b = 0} || Complex.norm coord <= 0.001

(**[is_black_try i j m] is [black_check (Matrix.get i j m)] if [i j] is in 
   bounds for [m] and false otherwise*)
let rec is_black_try i j m = 
  try 
    black_check (Matrix.get i j m)
  with | Invalid_argument _ -> false

(**[is_black i j m] is true if and only if [Matrix.get i j m] is black and 
   one of its neighbors is not (that is, this point is on the boundary). Points
   of norm at most 0.001 are automatically considered in the set. *)
let is_black i j m = 
  is_black_try i j m && 
  not (is_black_try (i - 1) j m && is_black_try (i + 1) j m
       && is_black_try i (j - 1) m && is_black_try i (j + 1) m)

let black_vals m = 
  let w = Matrix.columns m in 
  let h = Matrix.rows m in 
  let vals = ref [] in 
  for i = 0 to h - 1 do 
    for j = 0 to w - 1 do 
      let (coord, col) =  Matrix.get i j m in 
      if is_black i j m then
        vals := coord :: !vals
    done 
  done; 
  Array.of_list !vals

let color_matrix (image : Rgb24.t) ll ur = 
  let w = image.width in 
  let h = image.height in 
  let m = Matrix.cx_init ll ur h w in 
  Matrix.mapi (fun i j z -> (z, Rgb24.get image j i)) m

(**[get_image file] is the [Rgb24.t] image contained in the file with name [f],
   resized to be appropriate for computing leja points*)
let get_image file = 
  (match Bmp.load file [] with 
   | Rgb24 f -> f 
   | Index8 f -> Index8.to_rgb24 f
   | Index16 f -> Index16.to_rgb24 f
   | Rgba32 f -> failwith "Rgba32 used"
   | Cmyk32 f -> failwith "cmyk used")
  |> fun im -> Rgb24.resize None im 6000 6000

let rec print_nums oc = function 
  | [] -> ()
  | (hd : Complex.t) :: tl -> 
    Printf.fprintf oc "%f + %fi \n" hd.re hd.im;
    print_nums oc tl

let print_lst file nums = 
  let oc = open_out file in 
  print_nums oc nums;
  close_out oc

let from_file file ll ur n iterations s coeff roots = 
  let blk = color_matrix (get_image file) ll ur |> black_vals in 
  let p = poly blk n iterations s in 
  print_lst coeff (RootPolynomial.to_poly p |> Polynomial.to_list);
  print_lst roots (RootPolynomial.roots p);
  p
