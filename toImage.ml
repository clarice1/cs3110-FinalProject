open Matrix
open Images
let julia_color iter coordinate = 
  failwith "unimplemented"

let colorize f m =
  let rows = rows m
  and columns = columns m in
  let orig_image = Rgb24.make columns rows {r = 0; g = 0; b = 0; } in 
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      Rgb24.set orig_image j i (f (get i j m));
    done;
  done;
  Images.Rgb24 orig_image

