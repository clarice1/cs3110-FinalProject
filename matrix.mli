type 'a t

(**[init rows columns f] is the matrix with [rows] many rows
   [columns] many columns, and the value and row i, column j is [f i j]*)
val init : int -> int -> (int -> int -> 'a) -> 'a t

(**[get i j m] is the value of [m] at row [i] column [j]. Raises
   [index_out_of_bounds] if there is no value for that index.*)
val get : int -> int -> 'a t -> 'a

(**[iterate m f n] is the matrix with the same number of rows and columns as [m]
   whose value at row [i] column [j] is [f] applied to [get i j m] [n] times.*)
val iterate : 'a t -> ('a -> 'a) -> int -> 'a t

(**[iterate_with_stop m f n p] is the matrix with the same number of rows and
   columns as [m] whose value at row [i] column [j] is [(some k, z)] if 
   [0 <= k <= n] is minimal such that [p (f(f(...f(get i j m))) ) = true], 
   where [k] is the number of times [f] is applyed and [z] is the result of 
   applying [f] to [get i j m] [k] times, or [(none, z)] if there is no
   such [k] and [z] is the result of applying [f] to [get i j m] [n] times.*)
val iterate_with_stop : 'a t -> ('a -> 'a) -> 
  int -> ('a -> bool) -> (int option) * 'a t