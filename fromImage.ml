let an z lst = 
  List.fold_left (fun acc zj -> (Float.log Complex.(norm (sub z zj))) +. acc) 
    0. lst |> Float.exp

let anp1 = function 
  | hd :: tl -> an hd tl
  | [] -> 0.

(**[choose_n n vals] is [min n (Array.length vals - 1)] many random values 
   from [vals]. The order of elements in [vals] is changed. *)
let choose_n n vals = 
  let num = min (n - 1) (Array.length vals - 2) in 
  for i = 0 to min (n - 1) num do 
    let vali = vals.(i) in 
    let j = Random.int ((Array.length vals) - i - 1) in 
    vals.(i) <- vals.(j + i);
    vals.(j + i) <- vali
  done;
  Array.sub vals 0 num

let rec optimize acc vals iterations = 
  let possibilities = choose_n iterations vals in 
  Array.fold_left (fun (current_val, current_max) z -> 
      let zan = an z acc in 
      if zan >= current_max then (z, zan) else (current_val, current_max))
    (Complex.zero, an Complex.zero acc)
    possibilities

let rec compute acc vals iterations n = 
  if n <= 0 then acc else 
    let (argmax, _) = optimize acc vals iterations in 
    compute (argmax :: acc) vals iterations (n - 1)


let poly_vals vals n an s = 
  let nf = float_of_int n in 
  RootPolynomial.from_roots {re = exp(-. nf *. s /. 2.) /. an; im = 0.}
    (Complex.zero :: vals)

let poly values n iterations s = 
  let leja = compute [] values iterations n in 
  let (dn_args, an) = (leja, anp1 leja) in 
  poly_vals dn_args n an s

let rec is_black_try i j m = 
  try 
    black_check (Matrix.get i j m)
  with | Invalid_argument _ -> false

and black_check ((coord, c) : Complex.t * Color.rgb) = 
  c = {r = 0; g = 0; b = 0} || Complex.norm coord <= 0.001

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

let get_image file = 
  (match Bmp.load file [] with 
   | Rgb24 f -> f 
   | Index8 f -> Index8.to_rgb24 f
   | Index16 f -> Index16.to_rgb24 f
   | Rgba32 f -> failwith "Rgba32 used"
   | Cmyk32 f -> failwith "cmyk used")
  |> fun im -> Rgb24.resize None im 6000 6000

let from_file file ll ur n iterations s = 
  let blk = color_matrix (get_image file) ll ur |> black_vals in 
  poly blk n iterations s
