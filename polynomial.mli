(** A polynomial. *)
type t

(** [zero] is the zero polynomial.*)
val zero : t

(** [eval p z] is the result of applying [p] to [z]. *)
val eval : t -> Complex.t -> Complex.t

(** [bounded p input] returns [None] if iterating [p] starting at [input] is 
    guaranteed to diverge; returns [Some eval p z] if [p] is not guaranteed to 
    diverge after [input]. *)
val bounded : t -> Complex.t -> Complex.t option

(** [from_list lst] returns the polynomial with coefficients in [lst]. The 
    first element of [lst] is the coefficient of the highest degree of our 
    polynomial, continuing in decreasing order of degree. 
    Require: if [lst] is non-empty, the first element of [lst] is not 0. *)
val from_list : Complex.t list -> t

(** [get_bound p] returns a radius above which iterating [p] is guaranteed to 
    diverge. *)
val get_bound : t -> float