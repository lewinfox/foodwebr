a <- function() {
  x <- utils::getAnywhere("x")
  y <- exp(mean(1:10))
  z <- rlang::sym("x")
  zz <- get_called_functions(a)
  TRUE
}

test_that("`get_calls()` detects calls correctly", {
  cls <- get_calls(body(a))
  cls <- unlist(cls)
  expect_identical(cls[[2]], quote(utils::getAnywhere))
  expect_identical(cls[[3]], quote(exp))
  expect_identical(cls[[4]], quote(mean))
})

test_that("`get_called_functions()` captures the right functions", {
  expect_equal(
    get_called_functions(a),
    c("utils::getAnywhere", "rlang::sym", "foodwebr::get_called_functions")
  )
})