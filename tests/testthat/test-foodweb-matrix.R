e <- new.env()
e$foo <- function() bar()
e$bar <- function() {}
e$foobar <- function() {
  foo()
  bar()
}
e$recurse <- function() recurse()
e$func_arg <- function(x = foo()) bar()
e$fn_with_shadowed_names <- function() {
  foo <- 1
  bar()
}
funmat <- foodweb_matrix(e)

test_that("`foodweb_matrix()` finds basic relationships", {
  expect_equal(funmat["foo", "bar"], 1)
  expect_equal(funmat["foobar", "foo"], 1)
  expect_equal(funmat["foobar", "bar"], 1)
  expect_equal(funmat["bar", "foo"], 0)
  expect_equal(funmat["recurse", "recurse"], 1)
})

test_that("`foodweb_matrix()` finds functions in default arguments", {
  expect_equal(funmat["func_arg", "foo"], 1)
})

test_that("`foodweb_matrix()` identifies recursive calls", {
  expect_equal(funmat["recurse", "recurse"], 1)
})

test_that("`foodweb_matrix()` ignores variables with the same name as functions (regression for #2)", {
  expect_equal(funmat["fn_with_shadowed_names", "foo"], 0)
  expect_equal(funmat["fn_with_shadowed_names", "bar"], 1)
})

test_that("`foodweb_matrix()` walks up to the parent namespace (regression for cowsay::say)", {
  # The branch in foodweb_matrix() that detects a namespace parent env is
  # exactly this shape: a local env whose parent is a namespace.
  skip_if_not_installed("glue")
  local_env <- new.env(parent = asNamespace("glue"))
  expect_s3_class(foodweb_matrix(local_env), "foodweb_matrix")
})

test_that("`foodweb_matrix()` detects functions passed to `do.call()` (#5)", {
  e <- new.env()
  e$target <- function() 1
  e$dc_string <- function() do.call("target", list())
  e$dc_symbol <- function() do.call(target, list())
  e$dc_named <- function() do.call(args = list(), what = target)
  e$dc_namespaced <- function() base::do.call(target, list())
  fm <- foodweb_matrix(e)
  expect_equal(fm["dc_string", "target"], 1)
  expect_equal(fm["dc_symbol", "target"], 1)
  expect_equal(fm["dc_named", "target"], 1)
  expect_equal(fm["dc_namespaced", "target"], 1)
})

test_that("`foodweb_matrix()` detects functions passed to the `lapply()` family", {
  e <- new.env()
  e$target <- function(...) 1
  e$use_lapply <- function(x) lapply(x, target)
  e$use_sapply <- function(x) sapply(x, FUN = "target")
  e$use_vapply <- function(x) vapply(x, target, numeric(1))
  e$use_mapply <- function(x) mapply(target, x)
  e$use_map <- function(x) Map(target, x)
  e$use_reduce <- function(x) Reduce(target, x)
  e$use_filter <- function(x) Filter(target, x)
  e$use_apply <- function(x) apply(x, 1, target)
  e$in_default <- function(x, y = lapply(x, target)) y
  e$in_nested_fn <- function(x) function(y) lapply(y, target)
  fm <- foodweb_matrix(e)
  callers <- setdiff(names(e), "target")
  for (caller in callers) {
    expect_equal(fm[caller, "target"], 1, label = caller)
  }
})

test_that("`foodweb_matrix()` detects functions passed to purrr", {
  # purrr doesn't need to be installed: the functions are only inspected, never run
  e <- new.env()
  e$target <- function(...) 1
  e$use_map <- function(x) map(x, target)
  e$use_map_chr <- function(x) map_chr(x, target)
  e$use_named <- function(x) map(.f = target, x)
  e$use_map2 <- function(x, y) map2(x, y, target)
  e$use_pmap <- function(l) pmap(l, target)
  e$use_map_if <- function(x) map_if(x, is.null, target)
  e$use_keep <- function(x) keep(x, target)
  e$use_reduce <- function(x) reduce(x, target)
  e$use_safely <- function() safely(target)
  e$use_exec <- function() rlang::exec("target")
  e$use_formula <- function(x) map(x, ~ target(.x))
  e$use_lambda <- function(x) map(x, function(i) target(i))
  fm <- foodweb_matrix(e)
  callers <- setdiff(names(e), "target")
  for (caller in callers) {
    expect_equal(fm[caller, "target"], 1, label = caller)
  }
})

test_that("`foodweb_matrix()` ignores non-function arguments and local names in higher-order calls", {
  e <- new.env()
  e$target <- function(...) 1
  e$data_arg <- function() lapply(target, identity)
  e$local_fn <- function() {
    target <- function(...) 2
    lapply(1:3, target)
  }
  e$param_fn <- function(target) lapply(1:3, target)
  e$dots <- function(...) lapply(...)
  e$chained <- function(x) x$f(1, 2)(3)
  e$purrr_data_arg <- function() map(target, identity)
  e$purrr_formula <- function(x) map(x, ~ .x + 1)
  e$model_formula <- function(d) lm(y ~ target(x), data = d)
  e$caller <- function() target()
  fm <- foodweb_matrix(e)
  expect_equal(fm["data_arg", "target"], 0)
  expect_equal(fm["local_fn", "target"], 0)
  expect_equal(fm["param_fn", "target"], 0)
  expect_equal(fm["dots", "target"], 0)
  expect_equal(fm["purrr_data_arg", "target"], 0)
  expect_equal(fm["purrr_formula", "target"], 0)
  expect_equal(fm["model_formula", "target"], 0)
})

test_that("`foodweb_matrix()` handles tricky higher-order function calls", {
  e <- new.env()
  e$target <- function(...) 1
  e$f <- function(...) 1
  # Should find `target`
  e$magrittr_pipe <- function(x) x %>% lapply(target)
  e$magrittr_pipe_purrr <- function(x) x %>% map(target)
  e$magrittr_pipe_dot <- function(x) x %>% Reduce(target, .)
  e$native_pipe <- function(x) x |> lapply(target)
  e$args_reordered <- function(m) apply(MARGIN = 1, X = m, target)
  e$dots_then_named <- function(...) mapply(..., FUN = target)
  e$nested_formula <- function(x) map(x, ~ map(.x, ~ target(.x)))
  e$hof_in_hof <- function(x) Map(identity, lapply(x, target))
  e$local_in_other_fn <- function(x) {
    g <- function() target <- 1
    lapply(x, target)
  }
  # Should not find `target`, or `f`
  e$purrr_pluck <- function(x) map_chr(x, "target")
  e$inner_param <- function(x) lapply(x, function(f) do.call(f, list()))
  e$empty_arg <- function(x) lapply(x, )
  e$empty_first_arg <- function(x) Map(, x)
  e$string_var <- function() {
    target <- "a string"
    do.call(target, list())
  }
  fm <- foodweb_matrix(e)
  finds <- c(
    "magrittr_pipe", "magrittr_pipe_purrr", "magrittr_pipe_dot", "native_pipe",
    "args_reordered", "dots_then_named", "nested_formula", "hof_in_hof", "local_in_other_fn"
  )
  for (caller in finds) {
    expect_equal(fm[caller, "target"], 1, label = caller)
  }
  ignores <- c("purrr_pluck", "inner_param", "empty_arg", "empty_first_arg", "string_var")
  for (caller in ignores) {
    expect_equal(fm[caller, "target"], 0, label = caller)
    expect_equal(fm[caller, "f"], 0, label = caller)
  }
})

# Known gaps: these document cases we don't handle. Each is skipped with the reason.

test_that("`foodweb_matrix()` finds functions whose name is stored in a variable", {
  skip("Can't fix: the function name is only known at run time")
  e <- new.env()
  e$target <- function(...) 1
  e$caller <- function() {
    fn <- "target"
    do.call(fn, list())
  }
  expect_equal(foodweb_matrix(e)["caller", "target"], 1)
})

test_that("`foodweb_matrix()` handles partially matched argument names", {
  skip("Won't fix: partial matching like `sapply(x, FU = f)` is rare and discouraged")
  e <- new.env()
  e$target <- function(...) 1
  e$caller <- function(x) sapply(x, FU = target)
  expect_equal(foodweb_matrix(e)["caller", "target"], 1)
})

test_that("`foodweb_matrix()` finds condition handlers", {
  skip("Won't fix: only `do.call()`, the `lapply()` family and purrr are covered (#5)")
  e <- new.env()
  e$target <- function(...) 1
  e$caller <- function() tryCatch(1, error = target)
  expect_equal(foodweb_matrix(e)["caller", "target"], 1)
})

test_that("`foodweb_matrix()` respects a locally redefined higher-order function", {
  skip("Won't fix: redefining `lapply()` locally is rare")
  e <- new.env()
  e$target <- function(...) 1
  e$caller <- function(x) {
    lapply <- function(a, b) b
    lapply(x, target)
  }
  expect_equal(foodweb_matrix(e)["caller", "target"], 0)
})

test_that("`foodweb_matrix()` handles a higher-order function wrapped in brackets", {
  skip("Won't fix: `(lapply)(x, f)` is rare")
  e <- new.env()
  e$target <- function(...) 1
  e$caller <- function(x) (lapply)(x, target)
  expect_equal(foodweb_matrix(e)["caller", "target"], 1)
})

test_that("`filter_matrix()` keeps ancestors and descendants and drops unrelated functions", {
  names <- c("f", "g", "h", "i", "j")
  fm <- matrix(0, 5, 5, dimnames = list(names, names))
  fm["g", "f"] <- 1 # g -> f (f is a descendant of g)
  fm["h", "g"] <- 1 # h -> g (h is an ancestor of g)
  fm["i", "h"] <- 1 # i -> h (i is a transitive ancestor of g)
  # j is disconnected and should be dropped

  filtered <- filter_matrix("g", fm)
  expect_setequal(rownames(filtered), c("f", "g", "h", "i"))
  expect_false("j" %in% rownames(filtered))
})

test_that("`graphviz_spec_from_matrix()` renders callers, callees, and isolated nodes", {
  fm <- matrix(0, 3, 3, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  fm["b", "a"] <- 1
  fm["c", "a"] <- 1
  fm["c", "b"] <- 1

  spec <- graphviz_spec_from_matrix(fm)

  expect_type(spec, "character")
  expect_match(spec, "digraph 'foodweb'", fixed = TRUE)
  expect_match(spec, '"a()"', fixed = TRUE)
  expect_match(spec, '"b()" -> { "a()" }', fixed = TRUE)
  expect_match(spec, '"c()" -> { "a()", "b()" }', fixed = TRUE)
})
