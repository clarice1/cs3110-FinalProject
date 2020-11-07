let newton_fun f f' roots tolerance z =
  if List.exists (fun c -> Complex.(norm (sub c z) < tolerance)) roots 
  then None
  else Some Complex.(sub z (div (f z) (f' z)))

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
