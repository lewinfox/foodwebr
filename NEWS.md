# foodwebr 1.1.0

* `foodweb()` now detects functions passed to other functions rather than
  called directly (#5). This covers `do.call()`, the `lapply()` family
  (`sapply()`, `Map()`, `Reduce()`, `apply()`, etc.) and purrr (`map()` and
  friends, `reduce()`, `keep()`, `safely()`, etc.), e.g. `do.call("foo", args)`,
  `lapply(x, foo)`, `purrr::map(x, ~ foo(.x))` or `x %>% map(foo)`. (#5) (@lewinfox)

# foodwebr 1.0.0

* `plot.foodweb()` now passes ellipsis arguments to `DiagrammeR::grViz()` (@SigurdJanson)
* Improve the core algorithm to correctly differentiate between functions and
  variables in function body (@lewinfox)


# foodwebr 0.1.1

* First release
