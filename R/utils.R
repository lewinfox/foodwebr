#' Extract all call objects from a function body
#'
#' Given an input function body, return all the (non-primitive) calls to other functions within it.
#' The returned data structure is a nested list reflecting the structure of the source code.
#'
#' @param expr The result of calling [body()] on a function.
#'
#' @return A nested list of function symbols found in `expr`.
#'
#' @keywords internal
get_calls <- function(expr) {
  # For any function calls we want the first element, which is the function being called
  if (is.call(expr)) {
    # We want to extract these namespace calls as a single object rather than separating them into
    # `::`, pkg and fn.
    if (identical(expr[[1L]], quote(`::`))) {
      return(expr)
    }
    # Assignment is a special case - we want to avoid mis-identifying the LHS as a function (for
    # example, `c <- some_func()` might look like a call to the function `c()`). However, sometimes
    # assignment to a function is OK, e.g, `colnames(foo) <- bar`.
    if (identical(expr[[1L]], quote(`<-`)) || identical(expr[[1L]], quote(`=`))) {
      if (is.call(expr[[2L]])) {
        return(lapply(expr[[2L:3L]], get_calls))
      } else {
        return(get_calls(expr[[3L]]))
      }
    }
    if (any(sapply(expr, is.call))) {
      return(lapply(expr, get_calls))
    }
    return(expr[[1L]])
  }
  if (rlang::is_syntactic_literal(expr) || rlang::is_missing(expr)) {
    return(NULL)
  }
  return(expr)
}

#' Return the names of all the functions called by another
#'
#' @param FUN A function
#'
#' @return A character vector containing names of all (non-primitive) functions called by `FUN`
#'
#' @keywords internal
get_called_functions <- function(FUN, env = environment(FUN)) {
  # Determine the namespace of FUN
  if (!is.function(FUN)) {
    FUN <- match.fun(FUN)
  }
  env_name <- rlang::env_label(env)
  b <- body(FUN)
  l <- get_calls(b)
  ul <- unlist(l)
  # Remove any missing values
  ul <- ul[!sapply(ul, rlang::is_missing)]
  ul <- ul[!sapply(ul, is.null)]
  fns <- unique(ul)
  # Differentiate between functions and other symbols (e.g. variables)
  fns <- fns[sapply(fns, expr_is_bare_function, env = env)]
  # Remove primitives
  funcs <- lapply(fns, eval, envir = env)
  is_primitive <- sapply(funcs, is.primitive)
  # Remove base functions
  namespaces <- sapply(funcs, rlang::ns_env_name)
  base_fns <- namespaces == "base"
  keep <- !base_fns & !is_primitive
  build_namespaced_function_name(as.character(fns[keep]), namespaces[keep])
}

#' Is an expression a function?
#'
#' E.g. `mean` or `cowsay::say` (not invoked)
#'
#' @param expr An expression
#' @param env The environment to look in
#'
#' @return Boolean
#'
#' @keywords internal
expr_is_bare_function <- function(expr, env = parent.frame()) {
  is.function(try(eval(expr, envir = env), silent = TRUE))
}

#' Standardise function names
#'
#' Given a function name that may or may not contain a namespace (`"foo::bar"` or `"bar"`) and the
#' namespace to which that function belongs (`"foo"`), return `"foo::bar"`.
#'
#' @param fn Character vector of function names
#' @param ns Character vector of namespaces
#'
#' @return Character vector of fully-qualified function names
#'
#' @keywords internal
build_namespaced_function_name <- function(fn, ns) {
  ifelse(grepl("::", fn), fn, paste(ns, fn, sep = "::"))
}

function_to_name <- function(FUN) {
  fn_name <- deparse(substitute(FUN))
  x <- rlang::enexpr(FUN)
  y <- rlang::enquo(FUN)
  if (is.primitive(FUN)) {
    env_name <- "base"
  } else {
    env_name <- sub("^namespace:", "", rlang::env_label(environment(FUN)))
  }
  res <- paste(env_name, fn_name, sep = "::")
  sub(paste0("(", env_name, "::){2}"), paste0(env_name, "::"), res)
}

ns_to_pkgname <- function(ns_env) {
  gsub("^namespace:", "", rlang::env_label(ns_env))
}