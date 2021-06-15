#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Create RETRY version of GET
#'
#' This wrapper on httr retries the httr::GET function based on best-practice heuristics
#'
#' @param ... arguments passed to `httr::GET`
#'
#' @keywords internal
.retry_get <- function(...) {
  httr::RETRY("GET", ...)
}
