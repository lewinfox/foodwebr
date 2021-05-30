# ---- Foodweb matrix ----

#' Create a function caller/callee matrix
#'
#' Returns a matrix of 0s and 1s with a row and column for each function in an environment, such
#' that if the function on the x-axis calls the function on th y-axis, the element is 1, otherwise
#' 0.
#'
#' @param env Environment in which to search for functions. Can either be an environment
#'   or a character string denoting an environment (e.g. `"package:emissions"`). Environment must
#'   be on the search path.
#'
#' @return An n x n matrix where _n_ is the number of functions in `env`.
#'
#' @export
foodweb_matrix <- function(env = parent.frame()) {
  if (!is.environment(env)) {
    cli::cli_alert_danger("{.var {env}} must be an environment, not {typeof(env)}")
    rlang::abort("Unable to create foodweb matrix", "foodwebr_bad_environment")
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
  # CALLER.of is a list of indices into `funs`, such that if CALLER.of[1] = [2 3 4] it means that
  # funs[1] calls funs[2], funs[3] and funs[4].
  CALLER.of <- lapply(funs, functions_called_by, funs_to_match = funs, where = env)

  # For each function, how many functions does it call?
  n.CALLER <- unlist(lapply(CALLER.of, length))

  if (sum(n.CALLER) == 0) {
    # TODO: Can we capture base or other package fns?
    rlang::abort("Function does not call any matched functions", "foodwebr_no_web")
  }

  # Construct the function caller/callee matrix
  setup <- c(rep(1:length(funs), n.CALLER), unlist(CALLER.of))
  dim(setup) <- c(sum(n.CALLER), 2)
  funmat[setup] <- 1

  # Convert dimnames to string
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
  which <- unlist(lapply(where, exists, x = fn_name), use.names = FALSE)

  # Grab the function definition so we can analyse it
  if (!any(which)) {
    # The function can't be found in the specified environments, so check for it elsewhere.
    f <- if (exists(fn_name)) get(fn_name) else list()
  } else {

    idx <- seq_along(which)[which]  # No idea why this is necessary!

    # Get it from the environment in which we found it
    f <- get(fn_name, pos = where[[idx[1]]])
  }

  # Tokenise the function body so we can scan it for other functions. The output `tokens` is a
  # character vector of the deparsed function body
  tokens <- tokenise_function(f)
  if (!length(tokens)) {
    return(numeric(0))
  }

  # We now want to ask "which of these tokens matches a name in `funs_to_match`?".
  #
  # TODO: What if we want to capture functions that exist in other environments? I.e. return the
  #       entire dependency tree? That could be useful, if a bit overwhelming. Would need to go
  #       back to filtering out generics etc.
  matched_tokens <- match(tokens, funs_to_match, nomatch = 0)

  # `matched_tokens` is now a vector such that the i'th element is zero if that element does not
  # match anything in `funs_to_match`. If a match _was_ found, then the i'th element contains the
  # numeric index of the token in `funs_to_match`:
  #
  # funs_to_match <- c("foo", "bar")
  # tokens <- c("{", "x", "foo", "2", "}")
  # matched_tokens <- match(tokens, funs_to_match, nomatch = 0)
  #
  # matched_tokens
  # ## [0 0 1 0 0]    (No match, no match, matched index 1 (foo), no match, no match)
  #
  res <- matched_tokens[matched_tokens > 0]

  return(res)
}

#' Convert a function body to text
#'
#' @param x A function
#'
#' @return A character string containing the tokenised function body
#'
#' @keywords internal
tokenise_function <- function(x) {
  # Given a function as input, break it down into tokens and return them as text for analysis

  # We need to break the input down into atomic language units
  listable <- is.list(x)
  if (!listable) {
    # Is an S4 object, extract the `.Data` component (if there is one)
    if (isS4(x) && ('.Data' %in% names(methods::getSlots(class(x))))) {
      x <- x@.Data
    }

    # Can we break it down further?
    listable <- !is.atomic(x) && !is.symbol(x)
    if (listable) {
      x <- as.list(x)
    }
  }

  if (listable) {
    # Recurse into the language object
    return(unlist(lapply(x, tokenise_function), use.names = FALSE))
  }

  # If we get this far we know we've reached the bottom of the AST and we can convert the language
  # objects to text and send them back up
  paste(deparse(x), collapse = "\n")
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
    new_funs <-  rownames(fn_mat)[fn_mat[, queue[[i]]] > 0]
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