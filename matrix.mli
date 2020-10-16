(**The type of a matrix. Indices start at 0*)
type 'a t

(**[to_lst m] is the rows of [m]*)
val to_lst : 'a t -> 'a list list

(**[init rows columns f] is the matrix with [rows] many rows
   [columns] many columns, and the value and row i, column j is [f i j]*)
val init : int -> int -> (int -> int -> 'a) -> 'a t

(**[get i j m] is the value of [m] at row [i] column [j]. Raises
   [Invalid_argument "(i, j)"] if there is no value for that index.*)
val get : int -> int -> 'a t -> 'a

(**[iterate f n m] is the matrix with the same number of rows and columns as [m]
   whose value at row [i] column [j] is [f] applied to [get i j m] [n] times.
   Requires: [n >= 0]*)
val iterate : ('a -> 'a) -> int -> 'a t -> 'a t

(**[iterate_with_stop m f n p] is the matrix with the same number of rows and
   columns as [m] whose value at row [i] column [j] is [(some k, z)] if 
   [1 <= k <= n] is minimal such that [p (f(f(...f(get i j m))) ) = true], 
   where [k] is the number of times [f] is applyed and [z] is the result of 
   applying [f] to [get i j m] [k] times, or [(none, z)] if there is no
   such [k] and [z] is the result of applying [f] to [get i j m] [n] times.
   Requires: [n >= 0]*)
val iterate_with_stop : ('a -> 'a) -> int -> ('a -> bool) -> 
  'a t -> (int option * 'a) t