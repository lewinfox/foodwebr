e <- new.env()
e$foo <- function() bar()
e$bar <- function() {}
e$foobar <- function() {foo(); bar()}
e$recurse <- function() recurse()  # Should return 0 for recursive calls
e$func_arg <- function(x = foo()) bar()
e$fn_with_shadowed_names <- function() {foo <- 1; bar()}
funmat <- function_matrix(e)

test_that("`function_matrix()` finds basic relationships", {
  expect_equal(funmat["foo", "bar"], 1)
  expect_equal(funmat["foobar", "foo"], 1)
  expect_equal(funmat["foobar", "bar"], 1)
  expect_equal(funmat["bar", "foo"], 0)
})

test_that("`function_matrix()` finds functions in default arguments", {
  expect_equal(funmat["func_arg", "foo"], 1)
})

test_that("`function_matrix()` ignores recursive calls", {
  expect_equal(funmat["recurse", "recurse"], 0)
})

test_that("`function_matrix()` ignores variables with the same name as functions", {
  skip("Still need to work this out...")
  expect_equal(funmat["fn_with_shadowed_names", "foo"], 0)
  expect_equal(funmat["fn_with_shadowed_names", "bar"], 1)
})