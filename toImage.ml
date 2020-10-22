open Matrix
open Images
let julia_color coordinate iter = 
  failwith "unimplemented"

let colorize f m =
  let color_mat = iterate f 1 m in
  let rows = rows color_mat
  and columns = columns color_mat in
  let orig_image = Rgb24.make rows columns {r = 0; g=0; b=0; } in 
  for i = 0 to rows-1 do
    for j = 0 to columns - 1 do
      Rgb24.set orig_image i j (get i j color_mat);
    done;
  done;
  let fin_image = Images.Rgb24 orig_image
  in fin_image

