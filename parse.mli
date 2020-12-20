(**
   Functions to convert between strings and complex numbers.
*)

(**[string_of_complex z] is the string corresponding to [z]*)
val string_of_complex : Complex.t -> string

(**[lst_cx str] is the list of complex numbers represented by the string [str]
   of comma-separated values in [str]. Raises [Failure] if this is not
   possible*)
val lst_cx : string -> Complex.t list

(**[complex_of_string s] is the complex number represented by the string [s].
   Raises: [Failure] if [s] does not correspond to a complex number*)
val complex_of_string : string -> Complex.t