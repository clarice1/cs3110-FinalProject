

(**[newton_fun f f' roots tolerance z] is [None] if [z] is within
   [tolerance] of a value in [roots] and is [Some z-f(z)/f'(z)] otherwise*)
val newton_fun : (Complex.t -> Complex.t) -> (Complex.t -> Complex.t) 
  -> Complex.t list -> float -> Complex.t -> Complex.t option

(**[newton_fun_no_stop] is like [newton_fun] but does not stop if a value
   is near a root.*)
val newton_fun_no_stop : (Complex.t -> Complex.t) -> (Complex.t -> Complex.t) 
  -> Complex.t -> Complex.t

(**[newton_color colors roots tolerance (n, z)] is a color
   in [colors] satisfying [|z - c < tolerance|], where [c] is the
   position at the corresponding location in [roots], or black if no such 
   root exists*)
val newton_color : Color.rgb list -> Complex.t list -> float ->
  (int option * Complex.t) -> Color.rgb

(**[newton_with_defs] is like [newton_color] but with a built-in default list of
   colors. Requires: at most 8 roots.*)
val newton_with_defs : Complex.t list -> float ->
  (int option * Complex.t) -> Color.rgb

(**[full_newton ll ur iter width height roots] manages all of Newton's method
   for an image of width [width], height [height], [iter] many iterations on
   a polynomial with roots in [roots]. There should be at most 8 roots.*)
val full_newton : Complex.t -> Complex.t -> int -> int -> int ->
  Complex.t list -> unit