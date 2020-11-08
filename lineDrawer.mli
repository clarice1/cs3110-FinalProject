(**[start ll ur ll_cx ur_cx f] reads user input on the location of the mouse
   and starts drawing a lines corresponding to where the point is mapped by 
   repeatedly applying [f]. The graphics window should already be open.*)
val start : int * int -> int * int -> Complex.t -> Complex.t -> 
  (Complex.t -> Complex.t) -> Graphics.image -> unit

(**[start_with_bonus] is like [start], but the user may also specify what 
   happens when the mouse is clicked, depending on the complex value clicked,
   as well as what happens on keyboard input depending on the char inputted. 
   The funciton may also depend on the initial value.*)
val start_with_bonus : int * int -> int * int -> Complex.t -> Complex.t -> 
  (Complex.t -> Complex.t -> Complex.t) -> Graphics.image -> 
  (Complex.t -> unit) -> (char -> unit) -> unit