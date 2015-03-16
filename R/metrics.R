#' Intersection of two vectors at a given depth
#'
#' @param x A vector.
#' @param y A vector.
#' @param depth A number.
#' @return The intersection of \code{x} and \code{y} to \code{depth}.
intersection <- function(x, y, depth = min(length(x), length(y))) {
	intersect(head(x, depth), head(y, depth))
}

#' Overlap of two vectors at a given depth
#'
#' @param x A vector.
#' @param y A vector.
#' @param depth A number.
#' @return The overlap of \code{x} and \code{y} to \code{depth}.
overlap <- function(x, y, depth = min(length(x), length(y))) {
	length(intersection(x, y, depth))
}

#' Agreement of two vectors at a given depth
#'
#' @param x A vector.
#' @param y A vector.
#' @param depth A number.
#' @return The agreement of \code{x} and \code{y} to \code{depth}.
agreement <- function(x, y, depth = min(length(x), length(y))) {
	overlap(x, y, depth) / depth
}
