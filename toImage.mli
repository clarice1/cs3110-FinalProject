(** 
   Representation of a matrix in a .bmp image.

   This module maps a matrix onto a graphic .bmp image, with each value in the
   matrix determining the particular color in the corresponding region on .bmp.
   It further handles the colorization of a Julia Set.
*)
open Images

(** [col] is the type invariant that represents the color desired by the 
    client for the fractal. If the col were to be R, then the fractal would be 
    colored with shades of red. This is also true for B (Blue) and G (Green). *)
(*type col = R | B | G*)

(** [julia_color iter of_color coordinate] is the color of a particular complex
    [coordinate] in the matrix, having applied the function for this particular 
    Julia Set [iter] times. N.B. [coordinate] is a pair containing the final 
    value option after applying the function [iter] times (should it converge)
    as well as the complex coordinate *)
val julia_color : int -> Color.rgb -> (int option * Complex.t) -> Color.rgb

(** [colorize f m ] is the .bmp image of matrix [m] with function [f] applied to
    each complex coordinate to determine the color of that corresponding region
    of the image *)
val colorize : ((int option * 'a) -> Color.rgb) -> 
  (int option * 'a) Matrix.t -> Images.t



