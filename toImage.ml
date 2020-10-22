open Matrix
open Images
let julia_color coordinate iter = 
  failwith "unimplemented"

let colorize f m =
  let rows = rows m
  and columns = columns m in
  let orig_image = Rgb24.make rows columns {r = 0; g=0; b=0; } in 
  for i = 0 to rows-1 do
    for j = 0 to columns - 1 do
      let coord_iter = get i j m in
      let julia = julia_color (fst coord_iter) (snd coord_iter) in 
      Rgb24.set orig_image i j julia;
    done;
  done;
  let fin_image = Images.Rgb24 orig_image
  in fin_image

