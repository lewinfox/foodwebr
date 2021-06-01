# Running everything in a `local()` block to keep the test env clean. Probably not necessary right
# now but seems like good practice
local({
  f <- function() 1
  g <- function() f()
  h <- function() { f(); g() }
  i <- function() { f(); g(); h() }
  j <- function() j()

  fw <- foodweb()
  fw_txt <- foodweb(as.text = TRUE)

  test_that("the result has the correct class", {
    expect_s3_class(fw, "foodweb")
    expect_s3_class(get_funmat(fw), "foodweb_matrix")
    expect_type(get_graphviz_spec(fw), "character")
    expect_type(as.character(fw), "character")
    expect_true(inherits(as.matrix(fw), "matrix"))
    expect_true(is.foodweb(fw))
  })

  test_that("the result has the correct dimensions", {
    expect_equal(nrow(get_funmat(fw)), 5)
    expect_equal(ncol(get_funmat(fw)), 5)
    expect_equal(sum(get_funmat(fw)), 7)
  })

  test_that("printing functions are giving expected output", {
    expect_output(print(fw), "5 vertices")
    expect_output(print(fw), "7 edges")
    expect_output(print(get_funmat(fw)), "foodweb matrix")
  })

  test_that("supplying a function works", {
    expect_s3_class(foodweb(g), "foodweb")
  })

  test_that("errors are raised appropriately", {
    # Calling on something that isn't an environments
    expect_error(foodweb(env = character()), class = "foodwebr_bad_environment")
    # Calling foodweb() on an orphan function
    expect_error(foodweb(j), class = "foodwebr_isolated_function")
    # Calling on an environment with no functions
    expect_error(foodweb(env = new.env()), class = "foodwebr_no_functions")
    # Calling on a function that doesn't call any others
    e <- new.env()
    e$f <- function() 1
    expect_error(foodweb(env = e), class = "foodwebr_no_web")
  })

  test_that("`as.character()` works", {
    expect_equal(fw_txt, as.character(fw))
  })

  test_that("`as_tbl_graph()` works", {
    expect_s3_class(tidygraph::as_tbl_graph(fw), "tbl_graph")
  })
})
