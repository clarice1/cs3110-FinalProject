let () = Newton.full_newton {re = -2.; im = -2.} {re = 2.; im = 2.} 100 500
    500 
    [Complex.one; Complex.(neg one); Complex.i; Complex.(neg i)]
    0.0001