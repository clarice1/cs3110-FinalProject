let () = Newton.full_newton {re = -2.; im = -2.} {re = 2.; im = 2.} 100 500
    500 
    [Complex.zero; Complex.one; Complex.(neg one)]
    0.0001