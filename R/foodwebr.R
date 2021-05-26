# ---- Function analysis ----

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
function_matrix <- function(env = parent.frame()) {
  if (!is.environment(env)) {
    cli::cli_alert_danger("{.var {env}} must be an environment, not {typeof(env)}")
    rlang::abort("Unable to create function matrix", "foodwebr_bad_environment")
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
  diag(funmat) <- 0 # to drop self-references

  # Convert dimnames to string
  rownames(funmat) <- as.character(rownames(funmat))
  colnames(funmat) <- as.character(colnames(funmat))

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
#' @param fn_mat Matrix prpduced by [function_matrix()]
#'
#' @return A filtered function matrix containing only functions that are direct descendants or
#'   antecedants of `fn_name`.
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

# ---- Build a foodweb ----


#' Create a foodweb
#'
#' A `foodweb` object describes the relationship of functions in an environment. It has two
#' components: `funmat` (function matrix) which encodes the caller/callee relationships (i.e. which
#' functions call which) and `graphviz_spec` which is a text representation of the graph and is used
#' for the default plotting behaviour.
#'
#' `foodweb()` looks at the global environment by default. If you want to look at another
#' environment you can either pass a function to the `FUN` argument of `foodweb()` or pass an
#' environment to the `env` argument. If `FUN` is provided then the value of `env` is ignored, and
#' the environment of `FUN` will be used.
#'
#' @param FUN A function.
#' @param env An environment, `parent.frame()` by default. Ignored if `FUN` is not `NULL`.
#' @param filter Boolean. If `TRUE`, only functions that are direct descndants or antecedants of
#'   `FUN` will be shown.
#' @param as.text Boolean. If `TRUE`, rather than rendering the graph the intermediate graphviz
#'   specification is returned.
#'
#' @return If `as.text` is `TRUE`, a character vector. Otherwise, a `foodweb` object as described
#'   above.
#' @export
#'
#' @examples
#' # Create some functions to look at
#' f <- function() 1
#' g <- function() f()
#' h <- function() { f(); g() }
#' i <- function() { f(); g(); h() }
#' j <- function() j()
#'
#' x <- foodweb()
#' x
#'
#' # You can access the components directly or via getter functions
#' x$funmat
#' get_graphviz_spec(x)
#'
#' # Calculate the foodweb of a function in another package
#' foodweb(glue::glue)
foodweb <- function(FUN = NULL, env = parent.frame(), filter = !is.null(FUN), as.text = FALSE) {
  fn_name <- as.character(substitute(FUN))
  if (is.null(FUN) && filter) {
    cli::cli_alert_warning("{.var FUN} is {.val NULL} so {.code filter = TRUE} has no effect")
    filter <- FALSE
  }
  title <- paste0("<env: ", rlang::env_label(env), ">", collapse = "")
  if (!is.null(FUN)) {
    FUN <- match.fun(FUN)
    env <- environment(FUN)
    if (length(fn_name) > 1) {
      title <- paste0(paste(fn_name[c(2, 1, 3)], collapse = ""), "()", collapse = "")
    } else {
      title <- paste(fn_name, "()", collapse = "")
    }

  }
  fm <- function_matrix(env)
  if (filter) {
    fn_name <- fn_name[length(fn_name)]
    fm <- filter_matrix(fn_name, fm)
    if (!is.matrix(fm)) {
      cli::cli_alert_info("{.fn {fn_name}} does not call, and is not called by, any other functions")
      return(invisible(NULL))
    }
  }
  fw <- new_foodweb(funmat = fm, title = title)
  if (as.text) (
    return(as.character(fw))
  )
  fw
}

#' Create a new `foodweb` object
#'
#' A `foodweb` object describes the relationship of functions in an environment. It has two
#' components: a `funmat` (function matrix) which encodes the caller/callee relationships (i.e.
#' which functions call which) and a `grviz_spec` which is a text representation of the graph and
#' is used for the default plotting behaviour.
#'
#' This function should not be called directly, use [foodweb()] instead.
#'
#' @param funmat A function matrix created by [function_matrix()]
#' @param title Title to appear on the default graph visualisation
#'
#' @seealso foodweb
#'
#' @return A `foodweb`.
new_foodweb <- function(funmat, title) {
  # TODO: Input validation?
  gr_sp <- graphviz_spec_from_matrix(funmat, title)
  structure(list(funmat = funmat, graphviz_spec = gr_sp), class = "foodweb")
}

#' Is an object a `foodweb`?
#'
#' @param x The object to test
#'
#' @return Boolean
is.foodweb <- function(x) {
  inherits(x, "foodweb")
}

#' Extract the function matrix from a `foodweb` object.
#'
#' @param x A `foodweb`
#'
#' @return `x$funmat` - a numeric matrix.
#'
#' @export
get_funmat <- function(x) {
  if (!is.foodweb(x)) {
    stop(deparse(substitute(x)), " must be a `foodweb` object")
  }
  x$funmat
}

#' Extract the GraphViz specification from a `foodweb` object.
#'
#' @param x A `foodweb`
#'
#' @return `x$graphviz_spec` - a character scalar.
#'
#' @export
get_graphviz_spec <- function(x) {
  if (!is.foodweb(x)) {
    stop(deparse(substitute(x)), " must be a `foodweb` object")
  }
  x$graphviz_spec
}

# ---- Creating graphviz spec ----

#' Create a `grViz` specification from a function matrix
#'
#' Given a function matrix created by [function_matrix()], convert it into a text specification
#' that can be passed to [DiagrammeR::grViz()].
#'
#' @param funmat A function matrix generated by [function_matrix()].
#' @param title The name of the graph. Will be displayed as mouseover text.
#'
#' @return A text string.
#'
#' @export
graphviz_spec_from_matrix <- function(funmat, title = "foodweb") {
  template <- "digraph \"{title}\" {{{graph_data}\n}}"
  graph_data <- character()
  for (caller_name in rownames(funmat)) {
    called_fns <- colnames(funmat)[funmat[caller_name, ] > 0]
    called_fns <- glue::glue("\"{called_fns}()\"")
    # TODO: Do we want an option to include orphan functions in the output?
    if (length(called_fns) > 0) {
      spec <- glue::glue("  \"{caller_name}()\" -> {{ {paste(called_fns, collapse = ', ')} }}")
      graph_data <- paste(graph_data, spec, sep = "\n")
    }
  }
  glue::glue(template)
}


#---- S3 methods ----

#' Print a `foodweb` object
#'
#' Prints the `grvis_spec` member of a `foodweb` object.
#'
#' @param x A `foodweb` object.
#' @param ... Unused, only included for consistency with S3 generic.
#'
#' @export
print.foodweb <- function(x, ...) {
  cat("`foodweb` with", nrow(x$funmat), "nodes and", sum(x$funmat), "edges:\n\n")
  cat(x$graphviz_spec)
}

#' Plot a `foodweb` object
#'
#' Calls [DiagrammeR::grViz()] on the `graphvis_spec` element of the `foodweb`.
#'
#' @param x A `foodweb` object.
#' @param ... Unused, only included for consistency with S3 generic.
#'
#' @export
plot.foodweb <- function(x, ...) {
  DiagrammeR::grViz(x$graphviz_spec)
}

#' @export
str.foodweb <- function(object, ...) {
  cat("`foodweb` with", nrow(object$funmat), "nodes and", sum(object$funmat), "edges")
}

#' @export
summary.foodweb <- function(object, ...) {
  str.foodweb(object)
}

#' @export
as.character.foodweb <- function(x, ...) {
  x$graphviz_spec
}
