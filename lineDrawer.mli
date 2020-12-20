(** 
   Image manipulator to draw lines.

   This module edits Graphics images to draw lines from one application
   of a polynomial function to the next. That is, when we consider the repeated
   application of a particular polynomial function, LineDrawer traces each 
   output (in order) to visualize whether the function diverges or converges at
   that particular point. 
*)

(**[start ll_cx ur_cx c fb fc f iter] reads user input on the location of the 
   mouse and starts drawing a lines corresponding to where the point is mapped 
   by repeatedly applying [f]. [ll_cx] and [ur_cx] are
   the complex values corresponding to the lower left
   and upper right corners respectively. The graphics window should be
   already initialized to the desired ratio, but this function will compute and 
   draw the image and resize if the window is resized. [c] should be the 
   desired color for lines. When the key ['s'] is clicked, an image named 
   [name] ^ the number of images saved previously ^ [.bmp].*)
val start : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t option) -> 
  (int -> (int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t) -> int -> string -> unit

(**[start_ex] is like [start] but an exception is raised when the user
   presses ['z'] or ['q'] if there is no way to return. If inside another
   lineDrawer program, the exception for ['z'] can be caught and used to
   redo using the ['y'] key. *)
val start_ex : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t option) -> 
  (int -> (int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t) -> int -> string -> unit

(**[start_with_bonus] is like [start], but the user may also specify the 
   drawing color, what happens when the mouse is clicked, 
   depending on the complex value clicked, and add additional key bindings.
   The function may also depend on the initial value.*)
val start_with_bonus : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t -> Complex.t option) -> 
  (int -> (int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t -> Complex.t)-> int ->
  (Complex.t -> unit) -> (char -> unit)  -> string -> unit

(**[start_bonus_ex] is like [start_with_bonus] but an exception is raised when 
   the user presses ['z'] or ['q'] if there is no way to return. If inside another
   lineDrawer program, the exception for ['z'] can be caught and used to
   redo using the ['y'] key. *)
val start_bonus_ex : Complex.t -> Complex.t -> Graphics.color -> 
  (Complex.t -> Complex.t -> Complex.t option) -> 
  (int -> (int option * Complex.t) -> Color.rgb) ->
  (Complex.t -> Complex.t -> Complex.t)-> int ->
  (Complex.t -> unit) -> (char -> unit)  -> string -> unit