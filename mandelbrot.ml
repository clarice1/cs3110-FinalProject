(**[string_of_complex z] is the string representation of [z]*)
let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + " ^ string_of_float z.im ^ "i"

let poly c = Polynomial.from_list [Complex.one; Complex.zero; c]

let color_z2pc c = 
  let poly = poly c in
  LineDrawer.start_ex {re = -2.; im = -2.}
    {re = 2.; im = 2.} Graphics.red (Polynomial.bounded poly) 
    (fun i -> ToImage.julia_color i {r = 0; b = 255; g = 0})
    (Polynomial.eval poly) 100 (string_of_complex c)

let () = 
  Graphics.open_graph " 750x750";
  LineDrawer.start_with_bonus {re = -2.; im = -2.}
    {re = 2.; im = 2.} 
    Graphics.red
    (fun c -> Polynomial.bounded (poly c))
    (fun i -> ToImage.julia_color i {r = 0; b = 255; g = 0})
    (fun c z -> Complex.add (Complex.mul z z) c)
    100
    color_z2pc
    (fun x -> ())
    "mandelbrot"