# 3110-FinalProject
This is our private repo for our CS3110 Final Project.

# Terminal Fractal
To make a julia fractal using the terminal, run 'make terminalfractal' in 
terminal and follow the prompts.

# GUI Fractal
To run the GUI run 'make fractal' in terminal. There are four types of fractals.
1) Mandelbrot
2) Newton
3) Main (which is a julia fractal)
4) FromImage

Each of the 4 fractals are initialized with default coefficients or roots which 
may be changed through the user interface. Hovering over an image displays the 
results of iteratively applying the function defining the fractal starting at
the point where the mouse is.

At the moment, the following key bindings are implemented:
   q: quit the application. If this is used while looking at a Julia set from 
      make mandelbrot, pressing q once will bring you back to the mandelbrot set 
      and pressing q again will quit. 
   z: undo last action. If there are no actions to undo, quit the application
   y: redo last press of z. The action of leaving the window for a polynomial 
      during make mandelbrot cannot be redone 
   s: saves the image with the name provided
   e: double the number of iterations computed
   c: zoom in by a factor of 20, centered on current mouse position
   digits 1-9: zoom in by that factor, centered on current mouse position

1) Mandelbrot builds and starts a system that draws the mandelbrot set given a 
window size and color. Clicking on a point will draw the associated Julia set. 

2) Newton gives an example of a fractal created through Newton's method. You can
change window size, number of iterations, roots, and coordinates of the fractal.

3) Main builds a filled julia set given coefficients, color, window size, 
coordinates, and the number of iterations.

4) FromImage takes in an image– black and other, in form .bmp. The function 
works best if the image has a connected complement. There are some examples of 
images in the folder 'bmp examples'.

Congratulations! You have generated your own fractal.