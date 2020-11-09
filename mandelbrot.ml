let color_z2pc c = 
  let width = Graphics.size_x () in 
  let height = Graphics.size_y () in 
  let beginning_matrix = Matrix.cx_init {re = -2.; im = -2.} {re = 2.; im = 2.} 
      height width in 
  let poly = Polynomial.from_list [Complex.one; Complex.zero; c] in
  let final_matrix = Matrix.iterate_with_stop 
      (Polynomial.bounded poly) 
      100 beginning_matrix in 
  let image = ToImage.colorize 
      (ToImage.julia_color 100 {r = 0; b = 255; g = 0}) final_matrix |> 
              Graphic_image.of_image in 
  Graphics.draw_image image 0 0; 
  LineDrawer.start {re = -2.; im = -2.}
    {re = 2.; im = 2.} Graphics.red (Polynomial.bounded poly) 
    (ToImage.julia_color 100 {r = 0; b = 255; g = 0})
    (Polynomial.eval poly) 100 image


let poly c = (Polynomial.from_list [Complex.one; Complex.zero; c])

let () = 
  let beginning_matrix = 
    Matrix.cx_init {re = -2.; im = -2.} {re = 2.; im = 2.} 750 750 in
  let final_matrix = Matrix.iterate_with_stop_2
      (fun c -> Polynomial.bounded (poly c))
      100
      beginning_matrix in 
  Graphics.open_graph " 750x750";
  let image = ToImage.colorize 
      (ToImage.julia_color 100 {r = 0; b = 255; g = 0}) final_matrix 
              |> Graphic_image.of_image in
  LineDrawer.start_with_bonus {re = -2.; im = -2.}
    {re = 2.; im = 2.} 
    Graphics.red
    (fun c -> Polynomial.bounded (poly c))
    (ToImage.julia_color 100 {r = 0; b = 255; g = 0})
    (fun c z -> Complex.add (Complex.mul z z) c)
    100
    color_z2pc
    (fun x -> ())
    image


