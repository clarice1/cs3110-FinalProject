(** An image.*)
open Images
(** [ colorize f m ] takes a function f that  *)

val colorize: ((int option * Complex.t) -> Color.rgb) -> (int option * Complex.t) Matrix.t -> Images.Rgb24