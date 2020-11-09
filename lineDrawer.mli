(**[start ll_cx ur_cx c fb fc f iter] reads user input on the location of the 
   mouse and starts drawing a lines corresponding to where the point is mapped 
   by repeatedly applying [f]. [ll_cx] and [ur_cx] are
   the complex values corresponding to the lower left
   and upper right corners respectively. The graphics window should be
   already initialized to the desired ratio, but this function will draw
   the image and resize if the window is resized.*)
val start : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t option) -> ((int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t) -> int -> Graphics.image -> unit

(**[start_with_bonus] is like [start], but the user may also specify the 
   drawing color, what happens when the mouse is clicked, 
   depending on the complex value clicked, as well as what happens on keyboard 
   input depending on the char inputted. 
   The function may also depend on the initial value.
   Customize the drawing color.*)
val start_with_bonus : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t -> Complex.t option) -> 
  ((int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t -> Complex.t)-> int ->
  (Complex.t -> unit) -> (char -> unit)  -> Graphics.image -> unit