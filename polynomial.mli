(** 
   Representation of a polynomial.

   This module represents a polynomial of any degree. It also determines where 
   on the complex plane will the polynomial diverge, and where it will converge.
*)


(** [t] is the type of polynomial *)
type t

(** [zero] is the zero polynomial *)
val zero : t

(** [eval p z] is the result of applying polynomial [p] to input [z] *)
val eval : t -> Complex.t -> Complex.t

(**[diff p] is the derivative of [p]*)
val diff : t -> t

(**[sum p q] is the polynomial [p + q] *)
val sum : t -> t -> t

(**[mul p q] is the polynomial [pq] *)
val mul : t -> t -> t

(**[from roots lst] is the monic polynomial of degree [List.length lst] whose
   roots are the entries in [lst]*)
val from_roots : Complex.t list -> t

(** [bounded p input] is [None] if iterating polynomial [p] beginning at [input]
    is guaranteed to diverge and [Some (eval p input)] if [p] is not guaranteed 
    to diverge *)
val bounded : t -> Complex.t -> Complex.t option

(** [from_list lst] is the polynomial with coefficients in [lst]. The 
    first element of [lst] is the coefficient of the highest degree of the 
    polynomial, continuing in decreasing order of degree
    Requires: if [lst] is non-empty, the first element of [lst] is not 0 *)
val from_list : Complex.t list -> t

(** [get_bound p] returns a radius outside of which iterating polynomial [p] is 
    guaranteed to diverge *)
val get_bound : t -> float