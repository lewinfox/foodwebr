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

  # Find functions using ls() instead of lsf.str() (which seems unreliable)
  all_objects <- ls(envir = env, all.names = TRUE)

  funs <- all_objects[sapply(all_objects, function(x) {
    tryCatch(is.function(get(x, envir = env)), error = function(e) FALSE)
  })]

  n <- length(funs)

  if (n == 0) {
    env_label <- glue::glue("<env: {rlang::env_label(env)}>")
    msg <- glue::glue("No functions found in {{.var {env_label}}}")
    cli::cli_alert_danger(msg)
    rlang::abort("No functions found", "foodwebr_no_functions")
  }

  # Rest of the function remains the same
  funmat <- matrix(0, n, n, dimnames = list(CALLER = funs, CALLEE = funs))
  CALLER.of <- lapply(funs, functions_called_by, funs_to_match = funs, where = env)
  n.CALLER <- unlist(lapply(CALLER.of, length))

  if (sum(n.CALLER) == 0) {
    rlang::abort("No inter-function calls detected", "foodwebr_no_web")
  }

  setup <- c(rep(1:length(funs), n.CALLER), unlist(CALLER.of))
  dim(setup) <- c(sum(n.CALLER), 2)
  funmat[setup] <- 1

  rownames(funmat) <- as.character(rownames(funmat))
  colnames(funmat) <- as.character(colnames(funmat))

  class(funmat) <- c("foodweb_matrix", class(funmat))

  return(funmat)
}

#' Which functions does a function call?
#'
#' Given an input function `fn_name` and a list of candidate functions `funs_to_match`, return a
#' list of all the functions in `funs_to_match` that appear in the definition of `fn_name`.
#'
#' @param fn_name `<chr>`\cr The name of the function of interest
#' @param funs_to_match `<chr>`\cr Only these functions will be considered as parents
#' @param where `<env>`\cr An environment, or text specifying an environment
#'
#' @return A character vector listing the functions in `funs_to_match` that call `fn_name`.
#'
#' @keywords internal
functions_called_by <- function(fn_name, funs_to_match, where) {
  # List of environments where we want to look for functions
  if (is.environment(where)) {
    where <- list(where)
  } else {
    where <- as.list(where)
  }

  # Which of our environments does `fn_name` exist in?
  found_in_envs <- unlist(lapply(where, exists, x = fn_name), use.names = FALSE)

  # Grab the function definition so we can analyse it
  if (!any(found_in_envs)) {
    # The function can't be found in the specified environments, so check for it elsewhere.
    f <- if (exists(fn_name)) get(fn_name) else list()
  } else {
    idx <- seq_along(found_in_envs)[found_in_envs]
    # Get it from the environment in which we found it
    f <- get(fn_name, pos = where[[idx[1]]])
  }

  # Use codetools to find function calls (not variable assignments)
  if (!is.function(f)) {
    return(numeric(0))
  }

  tryCatch({
    # findGlobals returns a list with $functions and $variables
    globals <- codetools::findGlobals(f, merge = FALSE)
    function_calls <- globals$functions

    # Find which of the called functions are in our funs_to_match list
    matched_indices <- match(function_calls, funs_to_match, nomatch = 0)
    matched_indices[matched_indices > 0]
  }, error = function(e) {
    # Fallback to empty result if findGlobals fails
    numeric(0)
  })
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
