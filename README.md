# Canvas Fractals
Simple fractal drawing program in [blank-canvas](https://github.com/ku-fpg/blank-canvas)


To see the results, run the program (e.g. `runghc fractal.hs`), then point a browser at `localhost:3000`.


Fractals are represented here as lazy infinite trees.  The primary means of interacting with a fractal are the functions `step`, which returns the next level of the tree as a list and `approximate` which returns a `Canvas ()` that draws an approximation to the 'rest of' the fractal at the current level - this is needed in order to bottom-out the recursion when drawing.


The `mkFractal` function generates `Fractal` data structures given a function that converts from a single line to the next level of the fractal.  It applies this function recursively to every line in the fractal.  The union constructor, `(:+)`, combines two Fractals into a single `Fractal` object.  It would be fairly simple to add a constructor based on arbitrary Canvas objects to allow fractals with components other than lines.


The `drawLeaves` and `drawBranches` functions both draw a representation of a given `Fractal` to a given depth, the first under the assumption that each level replaces the one before (as in the Koch curve), and the second under the assumption that it is added in instead (as in tree fractals where every level is drawn).
