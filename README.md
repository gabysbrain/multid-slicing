
**Please use [MdSlicing.jl](https://github.com/gabysbrain/MdSlicing.jl) now.**
 
# Multi-dimensional Slicing

A library for computing 1D and 2D slices through multi-dimensional datasets. 

## Installation

Installation is via `devtools`

```
install_github("gabysbrain/multid-slicing")
```

## 1D slice example

1. If your data is a set of samples in a table, rather than a function then 
   build a regression model on your dataset so you have a function to 
   visualize. Divide your data into `X` which is a table of all independent
   variables and `Y` which is a vector of a single dependent variable. For this
   example, the regression is done using a Gaussian process model via
   the [`mlegp` library](https://cran.r-project.org/web/packages/mlegp/index.html).
   ```
   library(mlegp)

   m = mlegp(X, Y)
   f = function(x) {predict(m, x)}
   ```
2. You will also need to create a `problemSpec` instance to keep track of 
   the names and limits of each dimension.
   ```
   library(multidslicing)
   lims = createProblemSpec(x1=c(-1, 1), x2=c(-1, 1), x3=c(-1, 1))
   ```
3. Create a set of slices from the function you have. Here, we create 50 
   slices of the function for each dimension.
   ```
   slices = sliceplorer(f, lims, 50)
   ```
4. Now plot the slices
   ```
   plot(slices)
   ```

## License

This project is licensed under a BSD License --- see the 
[LICENSE.md](LICENSE.md) file for details.

