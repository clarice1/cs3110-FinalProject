(** 
   Representation of a matrix.

   This module represents a matrix and the data stored within it, including 
   complex matrices.
*)

(**Matrix operations. 
   Throughout, m(i, j) is the value of matrix m at row i, column j 
   Throughout, f^k is the k-fold composition of f with itself*)

(** ['a t] is the type of a matrix containing values of type ['a] *)
type 'a t

(** [cx_t] is the type representing complex numbers *)
type cx_t = Complex.t t                                              (*QUESTION: Why do we need this? Isn't Complex.t t enough? *)

(**[to_lst m] is a list of the rows of matrix [m], each represented as a list *)
val to_lst : 'a t -> 'a list list

(**[init rows columns f] is a matrix with [rows] many rows and [columns] many 
   columns. Further, the value at the the [i]th row and [j]th column of this 
   matrix is [f i j] *)
val init : int -> int -> (int -> int -> 'a) -> 'a t

(**[complex_init ll ur rows columns] is the complex matrix with [rows] many 
   rows and [columns] many columns whose values are a grid of evenly spaced 
   complex numbers with the value [ll] in its lower left corner and the value 
   [ur] in its upper right corner
   Requires: the real number in [ll] is less than the real number in [ur] and
             the imaginary number in [ll] is less than the imaginary number in
             [ur] and
             [rows] and [columns] are both greater than 0 *)
val cx_init : Complex.t -> Complex.t -> int -> int -> cx_t

(**[get i j m] is the value in the [i]th row and [j]th column of matrix [m] 
   Raises: [Invalid_argument "(i, j)"] if there is no value for that index *)
val get : int -> int -> 'a t -> 'a

(**[iterate f n m] is the matrix with the same number of rows and columns as [m]
   but whose value at row [i] column [j] is [f^n m(i, j)], i.e. the n-fold 
   composition of f with itself
   Requires: [n] is greater than 0 *)
val iterate : ('a -> 'a) -> int -> 'a t -> 'a t

(**[iterate_with_stop f n m] is [iterate f n m] except each value in the output
   matrix is an ['a] option, with [Some f^n m(i, j)] representing the n-fold 
   composition of f with itself and [None] the value stored if [f^n m(i, j)]
   diverges sufficiently. 


                                                                        (*QUESTION: Is this a correct way to describe iterate_with_stop?*)
   if 0 <= k <= n is 
   such that 
   -[(z -> Option.bind z f)^k (Some m(i, j)) = Some z] and 
   -[(z -> Option.bind z f)^(k+1) (Some m(i,j)) = None], 
   or [(None, f^n(z))] if no such [k] exists.*)
val iterate_with_stop : ('a -> 'a option) -> int -> 'a t -> (int option * 'a) t