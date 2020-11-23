let string_of_complex (z : Complex.t) = 
  string_of_float z.re ^ " + "^ string_of_float z.im ^ "i"