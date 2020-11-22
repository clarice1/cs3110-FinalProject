let () = 
  let ll : Complex.t = {re = -1.; im = -1.} in 
  let ur : Complex.t = {re = 1.; im = 1.} in 
  Random.self_init ();
  let poly = FromImage.from_file "cat1.bmp" ll ur 400 10000 (1. /. 400.) in 
  let bpoly = RootPolynomial.bound poly in
  Graphics.open_graph " 500x500";
  LineDrawer.start ll ur Graphics.red (RootPolynomial.bbounded bpoly)
    (fun i -> ToImage.julia_color i {r = 0; b = 255; g = 0})
    (RootPolynomial.eval poly)
    30 
    "kitty"