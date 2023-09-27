
<!-- README.md is generated from README.Rmd. Please edit that file -->

# foodwebr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lewinfox/foodwebr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lewinfox/foodwebr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`foodwebr` makes it easy to visualise the dependency graph of a set of
functions (i.e. who calls who). This can be useful for exploring an
unfamiliar codebase, or reminding yourself what you wrote ten minutes
ago

## Installation

You can install foodwebr from GitHub:

``` r
devtools::install_github("lewinfox/foodwebr")
```

## Basic usage

Say we have a bunch of functions in the global environment, some of
which call each other:

``` r
library(foodwebr)

f <- function() 1
g <- function() f()
h <- function() { f(); g() }
i <- function() { f(); g(); h() }
j <- function() j()
```

A call to `foodweb()` will calculate a graph of the dependencies.

``` r
fw <- foodweb()
```

Printing the object will show the [graphviz](https://graphviz.org/)
representation:

``` r
fw
#> # A `foodweb`: 5 vertices and 7 edges 
#> digraph 'foodweb' {
#>   f()
#>   g() -> { f() }
#>   h() -> { f(), g() }
#>   i() -> { f(), g(), h() }
#>   j() -> { j() }
#> }
```

Plotting will draw the graph.

``` r
plot(fw)
```

<div class="grViz html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-bebf81d879e077792911" style="width:100%;height:192px;"></div>
<script type="application/json" data-for="htmlwidget-bebf81d879e077792911">{"x":{"diagram":"digraph \"foodweb\" {\n  \"f()\"\n  \"g()\" -> { \"f()\" }\n  \"h()\" -> { \"f()\", \"g()\" }\n  \"i()\" -> { \"f()\", \"g()\", \"h()\" }\n  \"j()\" -> { \"j()\" }\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

`foodweb()` looks at its calling environment by default. If you want to
look at another environment you can either pass a function to the `FUN`
argument of `foodweb()` or pass an environment to the `env` argument. If
`FUN` is provided then the value of `env` is ignored, and the
environment of `FUN` will be used.

### Filtering

If a specific function is passed to `FUN`, the default behaviour is to
remove functions that are not descendants or antecedents of that
function.

``` r
# `j()` will not be included
foodweb(FUN = g)
#> # A `foodweb`: 4 vertices and 6 edges 
#> digraph 'foodweb' {
#>   g() -> { f() }
#>   h() -> { g(), f() }
#>   i() -> { g(), h(), f() }
#>   f()
#> }

# Force inclusion of unconnected functions by using `filter = FALSE`
foodweb(FUN = g, filter = FALSE)
#> # A `foodweb`: 5 vertices and 7 edges 
#> digraph 'foodweb' {
#>   f()
#>   g() -> { f() }
#>   h() -> { f(), g() }
#>   i() -> { f(), g(), h() }
#>   j() -> { j() }
#> }
```

You can use this feature when exploring code in other packages: calling
`foodweb()` on a function in another package will show you how functions
in that package relate to each other. I’m using `cowsay` here as it’s
small enough that the output is readable.

By default when calling `foodweb()` on a specific function we only see
functions that are in the direct line of descendants or antecendents of
the specified function.

``` r
if (requireNamespace("cowsay", quietly = TRUE)) {
  plot(foodweb(cowsay::say))
}
```

<div class="grViz html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-404eca409fce9f872862" style="width:100%;height:192px;"></div>
<script type="application/json" data-for="htmlwidget-404eca409fce9f872862">{"x":{"diagram":"digraph \"foodweb\" {\n  \"say()\" -> { \"check_color()\", \"check4pkg()\", \"get_who()\" }\n  \"check_color()\" -> { \"check4pkg()\" }\n  \"check4pkg()\"\n  \"get_who()\" -> { \"make_longcat()\" }\n  \"make_longcat()\"\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

If we want to include *all* functions in the package, we can pass
`filter = FALSE`:

``` r
if (requireNamespace("cowsay", quietly = TRUE)) {
  plot(foodweb(cowsay::say, filter = FALSE))
}
```

<div class="grViz html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-2f7491812a89da2d9ba6" style="width:100%;height:192px;"></div>
<script type="application/json" data-for="htmlwidget-2f7491812a89da2d9ba6">{"x":{"diagram":"digraph \"foodweb\" {\n  \"check_color()\" -> { \"check4pkg()\" }\n  \"check4pkg()\"\n  \"endless_horse()\" -> { \"get_who()\" }\n  \"get_who()\" -> { \"make_longcat()\" }\n  \"make_longcat()\"\n  \"say()\" -> { \"check_color()\", \"check4pkg()\", \"get_who()\" }\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

### `graphviz` as text

In case you want to do something with the
[graphviz](https://graphviz.org/) output (make it prettier, for
example), use `as.text = TRUE`. This returns the graphviz specification
as a character vector.

``` r
foodweb(as.text = TRUE)
#> digraph 'foodweb' {
#>   "f()"
#>   "g()" -> { "f()" }
#>   "h()" -> { "f()", "g()" }
#>   "i()" -> { "f()", "g()", "h()" }
#>   "j()" -> { "j()" }
#> }
```

Calling `as.character()` on a `foodweb` object will have the same
effect.

## Using `tidygraph`

The [`tidygraph`](https://tidygraph.data-imaginist.com/) package
provides tools for graph analysis. A `foodweb` object can be converted
into a tidy graph object using `tidygraph::as_tbl_graph()` to allow more
sophisticated analysis and visualisation.

``` r
if (requireNamespace("tidygraph", quietly = TRUE)) {
  tg <- tidygraph::as_tbl_graph(foodweb())
  tg
}
#> # A tbl_graph: 5 nodes and 7 edges
#> #
#> # A directed multigraph with 2 components
#> #
#> # A tibble: 5 × 1
#>   name 
#>   <chr>
#> 1 f    
#> 2 g    
#> 3 h    
#> 4 i    
#> 5 j    
#> #
#> # A tibble: 7 × 2
#>    from    to
#>   <int> <int>
#> 1     2     1
#> 2     3     1
#> 3     3     2
#> # ℹ 4 more rows
```

## How does it work?

Understanding the algorithm is important as there are some key
limitations to be aware of. To identify the relationships between
functions, `foodwebr`:

- Lists all the functions in an environment.
- Tokenises the `body()` of each function.
- Compares each token against the list of function names.
- If a token matches a function name, (i.e. the name of function B
  appears in the body of function A), records a link from A to B.

This last point leads to the possibility of name masking, where a
function contains an internal variable that matches the name of another
function in the environment. This will lead to a false link.

For example:

``` r
f1 <- function() {
  1
}

f2 <- function() {
  f1 <- 10 # This variable `f1` will be confused with the function `f1()`
  2
}

# The foodweb mistakenly believes that function `f2()` calls function `f1()`
foodweb()
#> # A `foodweb`: 2 vertices and 1 edge 
#> digraph 'foodweb' {
#>   f1()
#>   f2() -> { f1() }
#> }
```

If you know how to fix this please leave a comment in
[\#2](https://github.com/lewinfox/foodwebr/issues/2).

## See also

`foodwebr` is similar to these functions/packages:

- [`mvbutils::foodweb()`](): The OG of function dependency graphs in R,
  and the inspiration for foodwebr. Less user-friendly output, in my
  opinion.
- [`DependenciesGraphs`](https://github.com/datastorm-open/DependenciesGraphs):
  Provides much nicer visualisations but does not appear to be actively
  maintained.
