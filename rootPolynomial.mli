(**The type of a polynomial based off of roots *)
type t

(**The type of a polynomial with a cached upper bound after which 
   divergence is guaranteed *)
type bt

(**[from_roots a [z1,...,zn]] is [a(z-z1)...(z-zn)]*)
val from_roots : Complex.t -> Complex.t list -> t

(**[to_poly p] converts a root polynomial to the coefficient-based polynomial*)
val to_poly : t -> Polynomial.t

(**[eval p z] is [p(z)], treating polynomials as functions in the usual manner*)
val eval : t -> Complex.t -> Complex.t

(**[bound p] caches a bound after which divergence is guaranteed*)
val bound : t -> bt

(**[bounded p z] is [None] if divergence starting at [z] is guaranteed. 
   Otherwise, values is [Some (eval p z)]*)
val bbounded : bt -> Complex.t -> Complex.t option