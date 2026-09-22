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
#' As well as direct calls like `foo()`, this detects functions passed to [do.call()], the
#' `lapply()` family and purrr, e.g. `do.call("foo", args)`, `lapply(x, foo)` or
#' `purrr::map(x, ~ foo(.x))`. See [functions_passed_by()].
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
  # Only walk the body again if `f` calls a higher-order function. `purrr::map()` is reported
  # by `findGlobals()` as a call to `::`, so check for that too.
  if (any(calls %in% c(names(higher_order_functions), "::", ":::"))) {
    calls <- c(calls, functions_passed_by(f))
  }
  which(funs_to_match %in% calls)
}

# Higher-order functions whose function argument we want to detect. Each entry lists the formal
# arguments of the higher-order function in order, up to and including the one that takes a
# function, which is always the last element. This lets us match the function argument whether it
# is supplied by name or by position, without needing the higher-order function to be installed.
hof_args <- function(args, fns) structure(rep(list(args), length(fns)), names = fns)

# These accept a function or the name of a function as a string
base_hofs <- c(
  hof_args("what", "do.call"),
  hof_args("FUN", c("match.fun", "mapply")),
  hof_args("f", c("Map", "Reduce", "Filter", "Find", "Position")),
  hof_args(c("X", "FUN"), c("lapply", "sapply", "vapply")),
  hof_args(c("env", "FUN"), "eapply"),
  hof_args(c("object", "f"), "rapply"),
  hof_args(c("X", "MARGIN", "FUN"), "apply"),
  hof_args(c("X", "INDEX", "FUN"), "tapply"),
  hof_args(c("X", "Y", "FUN"), "outer"),
  # `rlang::exec()` / `purrr::exec()`
  hof_args(".fn", "exec")
)

# In purrr, a string is not a function name: `map(x, "name")` extracts the element called "name"
purrr_hofs <- c(
  hof_args(".f", c(
    "as_mapper", "auto_browse", "in_parallel", "partial", "possibly", "quietly", "safely",
    "invoke", "invoke_map", "invoke_map_chr", "invoke_map_dbl", "invoke_map_df", "invoke_map_dfc",
    "invoke_map_dfr", "invoke_map_int", "invoke_map_lgl", "invoke_map_raw"
  )),
  hof_args("f", c("insistently", "slowly")),
  hof_args(".p", "negate"),
  hof_args(c(".x", ".f"), c(
    "map", "map_chr", "map_dbl", "map_df", "map_dfc", "map_dfr", "map_int", "map_lgl",
    "map_raw", "map_vec", "imap", "imap_chr", "imap_dbl", "imap_dfc", "imap_dfr", "imap_int",
    "imap_lgl", "imap_raw", "imap_vec", "walk", "iwalk", "modify", "imodify", "lmap",
    "reduce", "reduce_right", "accumulate", "accumulate_right", "detect", "detect_index"
  )),
  hof_args(c(".x", ".p"), c(
    "keep", "discard", "compact", "every", "some", "none", "head_while", "tail_while"
  )),
  hof_args(c(".x", ".y", ".f"), c(
    "map2", "map2_chr", "map2_dbl", "map2_df", "map2_dfc", "map2_dfr", "map2_int", "map2_lgl",
    "map2_raw", "map2_vec", "walk2", "modify2", "reduce2", "reduce2_right", "accumulate2"
  )),
  hof_args(c(".l", ".f"), c(
    "pmap", "pmap_chr", "pmap_dbl", "pmap_df", "pmap_dfc", "pmap_dfr", "pmap_int", "pmap_lgl",
    "pmap_raw", "pmap_vec", "pwalk"
  )),
  hof_args(c(".x", ".at", ".f"), c("map_at", "modify_at", "lmap_at")),
  hof_args(c(".x", ".p", ".f"), c("map_if", "modify_if", "lmap_if")),
  hof_args(c(".x", ".depth", ".f"), c("map_depth", "modify_depth", "at_depth")),
  hof_args(c(".x", ".where", ".f"), "modify_in")
)

higher_order_functions <- c(base_hofs, purrr_hofs)

# magrittr pipes, which pass the left-hand side as the first argument of the right-hand side
magrittr_pipes <- c("%>%", "%T>%", "%<>%", "%!>%")

#' Which functions are passed to higher-order functions?
#'
#' `codetools::findGlobals()` only reports functions that are called directly. A function passed
#' by name, as in `do.call("foo", args)` or `lapply(x, foo)`, shows up as a string or a variable
#' and is missed. This walks the body (and default arguments) of `f` looking for calls to the
#' functions in `higher_order_functions` and returns the names of any functions passed to them as
#' a bare name or a string. For purrr-style formula lambdas such as `map(x, ~ foo(.x))`, the
#' functions called inside the formula are returned.
#'
#' Names that are local to `f`, or to an anonymous function inside it, are ignored, as they refer
#' to the local object rather than a function in the environment.
#'
#' @param f `<fn>` The function to analyse.
#'
#' @return A character vector of function names.
#'
#' @keywords internal
functions_passed_by <- function(f) {
  found <- character()
  add <- function(names, locals) found <<- c(found, setdiff(names, locals))

  add_passed_function <- function(e, locals) {
    hof <- e[[1]]
    # Handle `purrr::map()` as well as `map()`
    if (is.call(hof) && length(hof) == 3 && is.symbol(hof[[1]]) &&
        as.character(hof[[1]]) %in% c("::", ":::")) {
      hof <- hof[[3]]
    }
    if (!is.symbol(hof) && !is.character(hof)) return()
    hof <- as.character(hof)
    if (!hof %in% names(higher_order_functions)) return()
    arg <- function_arg(e, higher_order_functions[[hof]])
    if (is.symbol(arg)) {
      add(as.character(arg), locals)
    } else if (is.character(arg) && length(arg) == 1 && hof %in% names(base_hofs)) {
      add(arg, locals)
    } else if (is.call(arg) && identical(arg[[1]], as.symbol("~"))) {
      lambda <- as.function(list(arg[[length(arg)]]))
      add(codetools::findGlobals(lambda, merge = FALSE)$functions, locals)
    }
  }

  walk <- function(e, locals) {
    if (!is.call(e)) return()
    head <- e[[1]]
    if (identical(head, as.symbol("function"))) {
      # An anonymous function: its arguments and assignments are local to its body
      fmls <- e[[2]]
      locals <- c(locals, names(fmls), codetools::findFuncLocals(fmls, e[[3]]))
      for (a in as.list(fmls)) if (!missing(a)) walk(a, locals)
      walk(e[[3]], locals)
      return()
    }
    if (is.symbol(head) && as.character(head) %in% magrittr_pipes && length(e) == 3 &&
        is.call(e[[3]]) && !any(vapply(as.list(e[[3]]), identical, logical(1), as.symbol(".")))) {
      # `x %>% f(y)` is `f(x, y)`, so insert the implicit first argument
      rhs <- e[[3]]
      e[[3]] <- as.call(c(rhs[[1]], as.symbol("."), as.list(rhs)[-1]))
    }
    add_passed_function(e, locals)
    for (a in as.list(e)) if (!missing(a)) walk(a, locals)
  }

  locals <- c(names(formals(f)), codetools::findFuncLocals(formals(f), body(f)))
  for (a in as.list(formals(f))) if (!missing(a)) walk(a, locals)
  walk(body(f), locals)
  unique(found)
}

#' Extract the function argument from a call to a higher-order function
#'
#' Mimics R's argument matching (exact names first, then position) for the leading formal
#' arguments of a higher-order function.
#'
#' @param call `<call>` A call to a higher-order function, e.g. `quote(lapply(x, foo))`.
#' @param formal_names `<chr>` The formal arguments of the higher-order function, in order, up to
#'   and including the one that takes a function (the last element).
#'
#' @return The expression passed as the function argument, or `NULL` if it can't be determined.
#'
#' @keywords internal
function_arg <- function(call, formal_names) {
  args <- as.list(call)[-1]
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  target <- formal_names[[length(formal_names)]]
  if (target %in% arg_names) {
    i <- match(target, arg_names)
  } else {
    positional <- which(arg_names == "")
    idx <- match(target, setdiff(formal_names, arg_names))
    if (idx > length(positional)) {
      return(NULL)
    }
    # Once we hit `...` we can't tell which position an argument is in
    if (any(vapply(args[positional[seq_len(idx)]], identical, logical(1), as.symbol("...")))) {
      return(NULL)
    }
    i <- positional[[idx]]
  }
  # An empty argument, as in `lapply(x, )`. This must be checked before it is assigned to a
  # variable, as R won't let you use a variable holding an empty argument.
  if (identical(args[[i]], quote(expr = ))) {
    return(NULL)
  }
  args[[i]]
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
