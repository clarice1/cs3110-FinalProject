(**[start ll ur ll_cx ur_cx f] reads user input on the location of the mouse
   and starts drawing a lines corresponding to where the point is mapped by 
   repeatedly applying [f]. The graphics window should already be open.*)
val start : int * int -> int * int -> Complex.t -> Complex.t -> 
  (Complex.t -> Complex.t) -> unit