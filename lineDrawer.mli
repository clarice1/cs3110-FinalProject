(**[start ll ur ll_cx ur_cx c f] reads user input on the location of the mouse
   and starts drawing a lines corresponding to where the point is mapped by 
   repeatedly applying [f]. [ll] and [ur] are the coordinates of the lower
   left and upper right corners of the window and [ll_cx] and [ur_cx] are
   the corresponding complex values. [c] should bbe the desired drawing
   color for lines and [im] is the image to be redrawn when the mouse moves.
   The graphics window should already be open.*)
val start : int * int -> int * int -> Complex.t -> Complex.t -> 
  Graphics.color -> (Complex.t -> Complex.t) -> Graphics.image -> unit

(**[start_with_bonus] is like [start], but the user may also specify the 
   drawing color, what happens when the mouse is clicked, 
   depending on the complex value clicked, as well as what happens on keyboard 
   input depending on the char inputted. 
   The function may also depend on the initial value.
   Customize the drawing color.*)
val start_with_bonus : int * int -> int * int -> Complex.t -> Complex.t -> 
  Graphics.color -> (Complex.t -> Complex.t -> Complex.t) -> 
  Graphics.image -> (Complex.t -> unit) -> (char -> unit)  -> unit