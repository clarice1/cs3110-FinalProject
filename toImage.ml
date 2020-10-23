open Matrix
open Images

type col = R | B | G

let julia_color iter col coordinate = 
  let black : Color.rgb = {r = 0; g = 0; b = 0} in
  if fst coordinate = None then black
  else begin
    let coord_iter = match fst coordinate with 
      | None -> 0
      | Some x -> x in 
    match col with
    | R -> let red_col : Color.rgb = 
             {b = 0; r = 255 - ((coord_iter) * 255 / iter); g = 0;} in red_col
    | B -> let blue_col : Color.rgb = 
             {b = 255 - ((coord_iter) * 255 / iter); r = 0; g = 0;} in blue_col
    | G -> let green_col : Color.rgb = 
             {b = 0; r = 0; g = 255 - ((coord_iter) * 255 / iter);} in green_col
  end



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

