# foodwebr

<!-- badges: start -->
<!-- badges: end -->

foodwebr makes it easy to visualise the dependency graph of a set of functions (i.e. who calls who).
This can be useful for exploring an unfamiliar codebase,or reminding yourself what you wrote ten
minutes ago

## Installation

You can install foodwebr from GitHub:

``` r
devtools::install_github("lewinfox/foodwebr")
```

## Example

We have a bunch of functions in the global environment, some of which call each other:

``` r
library(foodwebr)

f = function() 1
g = function() f()
h = function() { f(); g() }
i = function() { f(); g(); h() }
j = function() j()
```

We compute the "function matrix". The matrix is 1 if the function on the y-axis calls the function
on the x-axis, and 0 otherwise. `function_matrix()` looks at functions in the global environment by
default, but you can specify another environment using the `env` argument.

Note that self-calls are ignored (`funmat["j", "j"]` is zero even though `j()` calls itself).

``` r
funmat <- function_matrix()

funmat
#>       CALLEE
#> CALLER f g h i j
#>      f 0 0 0 0 0
#>      g 1 0 0 0 0
#>      h 1 1 0 0 0
#>      i 1 1 1 0 0
#>      j 0 0 0 0 0

```

The `graph_spec_from_matrix()` function translates the function matrix into a
[graphviz](https://graphviz.org/) specification:

``` r
graphvis_spec <- graph_spec_from_matrix(funmat)

graphvis_spec
#> digraph g {
#> g -> { f }
#> h -> { f, g }
#> i -> { f, g, h }
#> }
```

We can visualise the graph using `Diagrammer::grViz()`.

``` r
DiagrammeR::grViz(graphvis_spec)
```
![Function dependency graph](man/figures/graph.png)
