# ---- Foodweb ----

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
#' @param filter Boolean. If `TRUE`, only functions that are direct descendants or antecedents of
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
  if (!is.null(FUN)) {
    FUN <- match.fun(FUN)
    env <- environment(FUN)
  }
  fm <- foodweb_matrix(env)
  if (filter) {
    fn_name <- fn_name[length(fn_name)]
    fm <- filter_matrix(fn_name, fm)
    if (!is.matrix(fm)) {
      cli::cli_alert_info("{.fn {fn_name}} does not call, and isn't called by, any other functions")
      return(invisible(NULL))
    }
  }
  fw <- new_foodweb(funmat = fm)
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
#' @param funmat A function matrix created by [foodweb_matrix()]
#'
#' @seealso foodweb
#'
#' @return A `foodweb`.
#'
#' @keywords internal
new_foodweb <- function(funmat) {
  # TODO: Input validation?
  gr_sp <- graphviz_spec_from_matrix(funmat)
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

# ---- Getters ----

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


#---- S3 methods ----

#' Print a `foodweb` object
#'
#' Prints the `graphvis_spec` member of a `foodweb` object.
#'
#' @param x A `foodweb` object.
#' @param ... Unused, only included for consistency with S3 generic.
#'
#' @export
#'
#' @keywords internal
print.foodweb <- function(x, ...) {
  cat(crayon::silver("# A `foodweb`:", nrow(x$funmat), "nodes and", sum(x$funmat), "edges\n"))
  # TODO: This is a bit of a hack - we are relying on the fact that the graphviz string contains
  #       single quotes around the title (which we wish to retain) but double quotes around the
  #       vertex (function) names, which we wish to remove.
  cat(stringr::str_remove_all(x$graphviz_spec, "\""))
}

#' Plot a `foodweb` object
#'
#' Calls [DiagrammeR::grViz()] on the `graphvis_spec` element of the `foodweb`.
#'
#' @param x A `foodweb` object.
#' @param ... Unused, only included for consistency with S3 generic.
#'
#' @export
#'
#' @keywords internal
plot.foodweb <- function(x, ...) {
  DiagrammeR::grViz(x$graphviz_spec)
}

#' @export
#'
#' @keywords internal
str.foodweb <- function(object, ...) {
  cat("A `foodweb`: ", nrow(object$funmat), "nodes and", sum(object$funmat), "edges")
}

#' @export
#'
#' @keywords internal
summary.foodweb <- function(object, ...) {
  cat("A `foodweb`: ", nrow(object$funmat), "nodes and", sum(object$funmat), "edges")
}

#' @export
#'
#' @keywords internal
as.character.foodweb <- function(x, ...) {
  x$graphviz_spec
}

#' Convert a `foodweb` to a matrix
#'
#' This is equivalent to calling [get_funmat()] on `x`.
#'
#' @param x A `foodweb`
#' @param rownames.force,... Ignored, only included for compatibility with S3 generic
#'
#' @export
#'
#' @keywords internal
as.matrix.foodweb <- function(x, rownames.force = NA, ...) {
  if (!is.na(rownames.force)) {
    cli::cli_alert_info("{.var rownames_force} has no effect")
  }
  fm <- get_funmat(x)
  # Remove the `foodweb_matrix` class
  class(fm) <- setdiff(class(fm), "foodweb_matrix")
  fm
}

#' Convert a `foodweb` to a `tidygraph`
#'
#' This is an S3 method for the generic [tidygraph::as_tbl_graph()].
#'
#' @param x A `foodweb` object created by [foodweb()].
#'
#' @return A new [tidygraph::tbl_graph] object.
#'
#' @importFrom tidygraph as_tbl_graph
#' @export
#'
#' @keywords internal
as_tbl_graph.foodweb <- function(x, ...) {
  # TODO: Input validation
  nodes_df <- data.frame(name = rownames(x$funmat), stringsAsFactors = FALSE)
  n_edges <- sum(x$funmat)
  edges_df <- data.frame(from = integer(n_edges), to = integer(n_edges))
  row_idx <- 1
  for (i in 1:nrow(x$funmat)) {
    if (row_idx > n_edges) {
      break
    }
    targets <- which(x$funmat[i, ] > 0)
    n_new_edges <- length(targets)
    if (n_new_edges > 0) {
      edges_df$from[row_idx:(row_idx + n_new_edges - 1)] <- i
      edges_df$to[row_idx:(row_idx + n_new_edges - 1)] <- targets
      row_idx <- row_idx + n_new_edges
    }
  }
  tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df)
}
