let () = 
  let ll : Complex.t = {re = -1.; im = -1.} in 
  let ur : Complex.t = {re = 1.; im = 1.} in 
  Random.self_init ();
  Unix.mkdir "cat" 0o777;
  let poly = FromImage.from_file "bmp examples/cat1.bmp" 
      ll ur 400 10000 (1. /. 400.)
      "cat/cat1 coefficients.txt"
      "cat/cat1 roots.txt" in 
  let bpoly = RootPolynomial.bound poly in
  Graphics.open_graph " 500x500";
  LineDrawer.start ll ur Graphics.red (RootPolynomial.bbounded bpoly)
    (fun i -> ToImage.julia_color i {r = 0; b = 255; g = 0})
    (RootPolynomial.eval poly)
    30 
    "cat/kitty"