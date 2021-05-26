#' Convert a foodweb to a `tidygraph`
#'
#' This is an S3 method for the generic [tidygraph::as_tbl_graph()].
#'
#' @param x A `foodweb` object created by [foodweb()].
#'
#' @return A new `tbl_graph` object.
#'
#' @export
as_tbl_graph.foodweb <- function(x) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package `tidygraph` is required but not installed")
  }
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
