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
