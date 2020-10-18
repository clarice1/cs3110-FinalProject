(** An image.*)
open Images
(** [julia_color v iter] takes a tuple with the rate of divergence and the final 
    value and takes  *)
val julia_color : (int option * Complex.t) -> int -> Color.rgb
(** [ colorize f m ] takes a function f that returns a *)
val colorize : ((int option * Complex.t) -> Color.rgb) -> 
  (int option * Complex.t) Matrix.t -> Images.t



