# ---- Foodweb matrix ----

#' Create a function caller/callee matrix
#'
#' Returns a matrix of 0s and 1s with a row and column for each function in an environment, such
#' that if the function on the x-axis calls the function on the y-axis, the element is 1, otherwise
#' 0.
#'
#' @param env Environment in which to search for functions.
#'
#' @return An n x n matrix where _n_ is the number of functions in `env`.
#'
#' @export
foodweb_matrix <- function(env = parent.frame()) {
  if (!is.environment(env)) {
    cli::cli_alert_danger("{.var {env}} must be an environment, not {typeof(env)}")
    rlang::abort("Unable to create foodweb matrix", "foodwebr_bad_environment")
  }

  # Check if we're in a function's local environment but the parent is a namespace
  # This happens with package functions that have local environments
  parent_env <- parent.env(env)
  if (!identical(parent_env, emptyenv()) && isNamespace(parent_env)) {
    # Use the namespace instead of the local function environment
    env <- parent_env
  }

  funs <- as.character(utils::lsf.str(envir = env))
  n <- length(funs)

  if (n == 0) {
    env_label <- glue::glue("<env: {rlang::env_label(env)}>")
    msg <- glue::glue("No functions found in {{.var {env_label}}}")
    cli::cli_alert_danger(msg)
    rlang::abort("No functions found", "foodwebr_no_functions")
  }

  funmat <- matrix(0, n, n, dimnames = list(CALLER = funs, CALLEE = funs))
  for (i in seq_along(funs)) {
    funmat[i, functions_called_by(funs[[i]], funs, env)] <- 1
  }

  if (sum(funmat) == 0) {
    rlang::abort("No inter-function calls detected", "foodwebr_no_web")
  }

  class(funmat) <- c("foodweb_matrix", class(funmat))
  funmat
}

#' Which functions does a function call?
#'
#' Given an input function `fn_name` and a list of candidate functions `funs_to_match`, return the
#' indices in `funs_to_match` of functions that `fn_name` calls.
#'
#' @param fn_name `<chr>` The name of the function of interest.
#' @param funs_to_match `<chr>` Only these functions will be considered as callees.
#' @param env `<env>` The environment in which `fn_name` lives.
#'
#' @return An integer vector of positions in `funs_to_match`.
#'
#' @keywords internal
functions_called_by <- function(fn_name, funs_to_match, env) {
  f <- get(fn_name, envir = env)
  calls <- codetools::findGlobals(f, merge = FALSE)$functions
  which(funs_to_match %in% calls)
}

#' Filter a function matrix
#'
#' @param fn_name String giving the name of the function we're interested in
#' @param fn_mat Matrix produced by [foodweb_matrix()]
#'
#' @return A filtered function matrix containing only functions that are direct descendants or
#'   antecedents of `fn_name`.
#'
#' @keywords internal
filter_matrix <- function(fn_name, fn_mat) {
  # We need to construct a list of column and row indexes checked / to keep
  fns_to_keep <- fn_name

  queue <- fn_name
  seen <- character(nrow(fn_mat))
  i <- 1

  # Look up the tree
  while (i <= length(queue)) {
    seen[[i]] <- queue[[i]]
    new_funs <- rownames(fn_mat)[fn_mat[, queue[[i]]] > 0]
    fns_to_keep <- union(fns_to_keep, new_funs)
    new_funs <- setdiff(new_funs, seen)
    queue <- c(queue, setdiff(new_funs, queue))
    i <- i + 1
  }

  queue <- fn_name
  seen <- character()
  i <- 1

  # Look down the tree
  while (i <= length(queue)) {
    seen[[i]] <- queue[[i]]
    new_funs <- colnames(fn_mat)[fn_mat[queue[[i]], ] > 0]
    fns_to_keep <- union(fns_to_keep, new_funs)
    new_funs <- setdiff(new_funs, seen)
    queue <- c(queue, setdiff(new_funs, queue))
    i <- i + 1
  }

  fn_mat[fns_to_keep, fns_to_keep]
}

# ---- S3 methods ----

#' Print a `foodweb_matrix`
#'
#' @param x A `foodweb_matrix`
#' @param ... Unused
#'
#' @return `x`, invisibly
#'
#' @export
print.foodweb_matrix <- function(x, ...) {
  cat(crayon::silver("# A foodweb matrix:", nrow(x), "functions and", sum(x), "links\n"))
  print(unclass(x))
  invisible(x)
}
