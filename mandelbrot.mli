(**
   Function to draw the Mandelbrot set and related fractals.
*)


(**[run dim col name] starts linedrawer with the mandelbrot set. The
   dimensions are started with [dim] and the color will be [col]. Images
   are saved with [name]*)
val run : string -> Color.rgb -> string -> unit