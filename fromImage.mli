(**
   Functions to convert images to a polynomial so filled Julia sets resemble 
   input images.
*)

(**[poly values n iterations s] computes an approximation to the polynomial
   on page 8506 of the paper. As [iterations] increases, a better result is
   obtained. [delta_n] is used as the logarithmic capacity. *)
val poly : Complex.t array -> int -> int -> float -> RootPolynomial.t

(**[black_vals m] is the first argument in any pair where the second argument
   is black*)
val black_vals : (Complex.t * Color.rgb) Matrix.t -> Complex.t array

(**[color_matrix image ll ur] is the matrix of pairs of the complex coordinate
   at the point and the color at the point. [ll] is the lower left corner
   and [ur] is the upper right corner
   Requires: the real part and complex part of [ll] are both smaller than the 
   corresponding part of [ur]*)
val color_matrix : Rgb24.t -> Complex.t -> Complex.t -> 
  (Complex.t * Color.rgb) Matrix.t

(**[from_file file ll ur n iterations s coeff roots] completes the entire 
   process starting with the image with name [file]. [file] should be the name 
   of a .bmp file. The coefficients of the polynomial are printed to the file
   [coeff] and the roots are printer to [roots]. *)
val from_file : string -> Complex.t -> Complex.t -> int -> int -> float -> 
  string -> string -> RootPolynomial.t