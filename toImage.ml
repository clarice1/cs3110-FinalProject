open Matrix
open Images

(*type col = R | B | G *)

let julia_color iter of_color coordinate = 
  let black : Color.rgb = {r = 0; g = 0; b = 0} in
  if fst coordinate = None then black
  else 
    begin
      let coord_iter = match fst coordinate with 
        | None -> 0
        | Some x -> x in 
      let col_decr = ((coord_iter) * 255 / iter) in
      match of_color with
      | {b = blue; r = red; g = green;} -> 
        let my_col : Color.rgb = 
          {b = blue - col_decr; r = red - col_decr; g = green - col_decr} in my_col
    end

let colorize (f : ((int option * 'a) -> Color.rgb)) m =
  let rows = rows m
  and columns = columns m in
  let orig_image = Rgb24.make columns rows {r = 0; g = 0; b = 0; } in 
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      let coord = get i j m in
      Rgb24.set orig_image j i (f coord);
    done;
  done;
  Images.Rgb24 orig_image

