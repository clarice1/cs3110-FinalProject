(**AF: The list [[a11,...,a1m], ..., [an1,...,anm]] represents the matrix
   a11 ... a1m
    . .     .
    .   .   .
    .     . .
   an1 ... anm
   RI: each list has the same number of elements.
*)
type 'a t = 'a list list

let to_lst m = m

let init rows columns f = 
  List.init rows (fun i -> (List.init columns (f i)))

let get i j m = 
  try (
    List.nth (List.nth m i) j) 
  with 
  |Failure _| Invalid_argument _ -> 
    raise 
      (Invalid_argument ("(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ")"))

(**[map m f] is the matrix obtained by applying [f] to each entry of [m]*)
let map f m = List.map (List.map f) m

(**[iterate_fun f n x] is [f iter_num (f (iternum + 1) (...f iternum + n x)...)] 
   where [f] is applied [n] times
   Requires: [n >= 0]*)
let rec iterate_fun f iter_num n x = 
  if n < 1 then x else iterate_fun f (iter_num + 1) (n - 1) (f iter_num x)

let iterate f n m = 
  map (iterate_fun (fun x y -> f y) 0 n) m

(**[checker p n x] is [(Some n, x)] if [p x] otherwise [(None, x)] *)
let checker p n x = 
  if p x then (Some n, x) else (None, x)

(**[val_to_tuple f p n x] is [(Some (n + 1), f y)] if [x = (None, y)]
   and [p (f y)], [(None, f y)] if [x = (None, y)] and not [p (f y)], 
   and [x] otherwise*)
let val_to_tuple f p n = function
  | (None, y) -> 
    checker p (n + 1) (f y)
  | y -> y

let iterate_with_stop f n p m = 
  m |> map (checker p 0)
  |> map (iterate_fun (val_to_tuple f p) 0 n)