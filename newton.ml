let newton_fun_no_stop f f' z = 
  Complex.(sub z (div (f z) (f' z)))

let newton_fun f f' roots tolerance z =
  if List.exists (fun c -> Complex.(norm (sub c z) < tolerance)) roots 
  then None
  else Some (newton_fun_no_stop f f' z)

let default_colors : Color.rgb list = [
  {r = 0; g = 0; b = 255};
  {r = 0; g = 255; b = 0};
  {r = 255; g = 0; b = 0};
  {r = 0; g = 255; b = 255};
  {r = 255; g = 0; b = 255};
  {r = 255; g = 255; b = 0};
  {r = 142; g = 68; b = 173};
  {r = 243; g = 156; b = 18}
]

let rec default_n lst n = 
  if n = 0 then [] else match lst with 
    | hd :: tl -> hd :: default_n tl (n - 1)
    | [] -> failwith "too long"

let newton_color colors roots tolerance (_, z) = 
  List.fold_left2 
    (fun acc c color -> 
       if Complex.(norm (sub c z)) < tolerance then color else acc) 
    ({r = 0; g = 0; b = 0} : Color.rgb) roots colors

let newton_with_defs roots = 
  newton_color (default_n default_colors (List.length roots)) roots

let full_newton ll ur iter width height roots tolerance = 
  let poly = Polynomial.from_roots roots in 
  let polyfun = poly |> Polynomial.eval in
  let diff = Polynomial.diff poly |> Polynomial.eval in 
  let f = newton_fun polyfun diff roots tolerance in 
  let color_fun = newton_with_defs roots tolerance in
  Graphics.open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  LineDrawer.start ll ur
    Graphics.black 
    f
    (fun i -> color_fun)
    (newton_fun_no_stop polyfun diff) 
    iter
    "newton"
