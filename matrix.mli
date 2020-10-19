(**Matrix operations. 
   Throughout, m(i, j) is the value of matrix m at row i, column j 
   Throughout, f^k is the k-fold composition of f with itself*)

(**The type of a matrix. Indices start at 0*)
type 'a t

type cx_t = Complex.t t

(**[to_lst m] is the rows of [m]*)
val to_lst : 'a t -> 'a list list

(**[rows m] is the number of rows in [m]*)
val rows : 'a t -> int

(**[columns m] is the number of columns in [m]*)
val columns : 'a t -> int

(**[init rows columns f] is the matrix with [rows] many rows
   [columns] many columns, and the value and row i, column j is [f i j]
   Requires: [rows, columns > 0]*)
val init : int -> int -> (int -> int -> 'a) -> 'a t

(**[complex_init ll ur height width] is the complex matrix with [width] many 
   rows and [height] many columns whose values are a grid of evenly spaced 
   complex numbers with value in the lower left corner ll and value in 
   upper right ur
   Requires: 
   [ll.re < ur.re]
   [ll.im < ur.im]
   [width, height > 1]*)
val cx_init : Complex.t -> Complex.t -> int -> int -> cx_t

(**[get i j m] is [m(i, j)]. Raises
   [Invalid_argument "(i, j)"] if there is no value for that index.*)
val get : int -> int -> 'a t -> 'a

(**[iterate f n m] is the matrix with the same number of rows and columns as [m]
   whose value at row [i] column [j] is [f^n m(i, j)].
   Requires: [n >= 0]*)
val iterate : ('a -> 'a) -> int -> 'a t -> 'a t

(**[iterate_with_stop f n m] is the matrix with the same number of rows and 
   columns as [m] and whose value at i, j is (Some k, z) if 0 <= k <= n is 
   such that 
   -[(z -> Option.bind z f)^k (Some m(i, j)) = Some z] and 
   -[(z -> Option.bind z f)^(k+1) (Some m(i,j)) = None], 
   or [(None, f^n(z))] if no such [k] exists.*)
val iterate_with_stop : ('a -> 'a option) -> int -> 'a t -> (int option * 'a) t

(**Like iterate_with_stop, but the function map also depend on its initial
   value. The input function should have as its first argument the initial
   value and its second the most recent value. *)
val iterate_with_stop_2 : ('a -> 'a -> 'a option) -> int -> 'a t -> 
  (int option * 'a) t