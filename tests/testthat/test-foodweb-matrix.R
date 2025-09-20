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

test_that("`foodweb_matrix()` ignores variables with the same name as functions", {
  expect_equal(funmat["fn_with_shadowed_names", "foo"], 0)
  expect_equal(funmat["fn_with_shadowed_names", "bar"], 1)
})
