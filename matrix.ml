(**AF: The array [|[|a11,...,a1m|], ..., [|an1,...,anm|]|] represents the matrix
   a11 ... a1m
    . .     .
    .   .   .
    .     . .
   an1 ... anm
   RI: each array has the same number of elements. The array is nonempty, 
   as are each of its elements. 
*)
type 'a t = 'a array array

type cx_t = Complex.t t

let to_lst m = m |> Array.map Array.to_list |> Array.to_list

let rows = Array.length

let columns m = Array.length m.(0)

let init rows columns f = Array.init rows (fun i -> Array.init columns (f i))

(**[from_2flt_int z1 z2 x] is [(z1 - z2) / (x - 1)]*)
let from_2flt_int z1 z2 x =
  (z1 -. z2) /. (float_of_int (x - 1))

(**[cx_init_fun ll x_step y_step m n] is the complex numer with real part
   [l_re + n x_step] and imaginary part [t_im - m y_step]*)
let cx_init_fun l_re t_im x_step y_step m n: Complex.t=
  let fn = float_of_int n in 
  let fm = float_of_int m in 
  {re = l_re +. fn *. x_step; im = t_im -. fm *. y_step}

let cx_init (ll : Complex.t) (ur : Complex.t) height width = 
  let x_step = from_2flt_int ur.re ll.re width in
  let y_step = from_2flt_int ur.im ll.im height in
  init height width (cx_init_fun ll.re ur.im x_step y_step)

let get i j m = 
  try (
    m.(i).(j))
  with 
  | Invalid_argument _ -> 
    raise 
      (Invalid_argument ("(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"))

(**[map m f] is the matrix obtained by applying [f] to each entry of [m]*)
let map f = Array.map (Array.map f)

(**[iterate_fun f n x] is [f iter_num (f (iternum + 1) (...f iternum + n x)...)] 
   where [f] is applied [n] times
   Requires: [n >= 0]*)
let rec iterate_fun f iter_num n x = 
  if n < 1 then x else iterate_fun f (iter_num + 1) (n - 1) (f iter_num x)

let iterate f n m = 
  map (iterate_fun (fun x y -> f y) 0 n) m

let p f x = Option.is_none (Option.bind (Some x) f)

(**[checker p n x] is [(Some n, x)] if [p x] otherwise [(None, x)] *)
let checker f n x = 
  if p f x then (Some n, x) else (None, x)

let checker_opt f n = function
  |(None, y) -> if p f y then (Some n, y) else (None, y)
  | y -> y

(**[val_to_tuple f p n x] is 
   -[(Some n, y)] if [x = (None, y)] and [f y = None]
   -[(Some z, y)] if [x = (None, y)] and [f y = None]
   -[x] otherwise*)
let val_to_tuple f n = function
  | (None, y) -> 
    begin
      match f y with 
      | None -> (Some n, y)
      | Some z -> (None, z)
    end
  | y -> y

let iterate_with_stop f n m = 
  m |> map (fun x -> (None, x))
  |> map (iterate_fun (val_to_tuple f) 0 n)
  |> map (checker_opt f n)

let map2 f = Array.map2 (Array.map2 f)

let iterate_with_stop_2 f n m =
  m |> map (fun x -> (None, x))
  |> map2 (fun x y -> (iterate_fun (val_to_tuple (f x)) 0 n) y) m
  |> map2 (fun x y -> checker_opt (f x) n y) m