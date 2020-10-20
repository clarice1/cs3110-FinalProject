(** 
   Representation of a Julia Set in a .bmp image.

   This module draws the Julia Set from a particular function           (*QUESTION: Do we need any other inputs here?*)
*)


(** An image.*)
open Images                                                             (*we should import Images into our _tags file, like in A2 *)

(*QUESTION: I couldn't figure out each input was supposed to represent; we need to revisit this entire mli *)
(** [julia_color d iter] is the color of a particular complex coordinate in the matrix*)
val julia_color : (int option * Complex.t) -> int -> Color.rgb

(** [ colorize f m ] takes a function f that returns a *)
val colorize : ((int option * Complex.t) -> Color.rgb) -> 
  (int option * Complex.t) Matrix.t -> Images.t



