let color_z2pc c = 
  let beginning_matrix = Matrix.cx_init {re = -2.; im = -2.} {re = 2.; im = 2.} 
      750 750 in 
  let poly = Polynomial.from_list [Complex.one; Complex.zero; c] in
  let final_matrix = Matrix.iterate_with_stop 
      (Polynomial.bounded poly) 
      100 beginning_matrix in 
  let image = ToImage.colorize 
      (ToImage.julia_color 100 ToImage.B) final_matrix |> 
              Graphic_image.of_image in 
  Graphics.draw_image image 0 0; 
  LineDrawer.start (0, 0) (750, 750) {re = -2.; im = -2.}
    {re = 2.; im = 2.} Graphics.red  (Polynomial.eval poly) image

let () = 
  let beginning_matrix = 
    Matrix.cx_init {re = -2.; im = -2.} {re = 2.; im = 2.} 750 750 in
  let final_matrix = Matrix.iterate_with_stop_2
      (fun c -> Polynomial.bounded 
          (Polynomial.from_list [Complex.one; Complex.zero; c]))
      100
      beginning_matrix in 
  Graphics.open_graph " 750x750";
  let image = ToImage.colorize 
      (ToImage.julia_color 100 ToImage.B) final_matrix 
              |> Graphic_image.of_image in
  LineDrawer.start_with_bonus (0, 0) (750, 750) {re = -2.; im = -2.}
    {re = 2.; im = 2.} 
    Graphics.red
    (fun c z -> Complex.add (Complex.mul z z) c)
    image
    color_z2pc
    (fun x -> ())

