(**[string_of_complex z] is the string representation of [z]*)
let string_of_complex = Parse.string_of_complex

let poly c = Polynomial.from_list [Complex.one; Complex.zero; c]

let color_z2pc col c = 
  let poly = poly c in
  LineDrawer.start_ex {re = -2.; im = -2.}
    {re = 2.; im = 2.} Graphics.red (Polynomial.bounded poly) 
    (fun i -> ToImage.julia_color i col)
    (Polynomial.eval poly) 100 (string_of_complex c)

let run dim col name = 
  Graphics.open_graph dim;
  LineDrawer.start_with_bonus {re = -2.; im = -2.}
    {re = 2.; im = 2.} 
    Graphics.red
    (fun c -> Polynomial.bounded (poly c))
    (fun i -> ToImage.julia_color i col)
    (fun c z -> Complex.add (Complex.mul z z) c)
    100
    (color_z2pc col)
    (fun x -> ())
    name