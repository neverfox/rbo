#' Extrapolated Rank-Biased Overlap on even-length vectors
#'
#' @param x A vector.
#' @param y A vector.
#' @param p A number, representing persistence.
rbo_ext_even <- function(x, y, p = 0.9) {
	assertthat::assert_that(length(x) == length(y))
	k <- length(x)
	seq.k <- seq_len(k)
	pow <- functional::Curry("^", x = p)
	a <- functional::Curry(agreement, x = x, y = y)
	a(k) * pow(k) + ((1 - p) / p) * sum(sapply(seq.k, a) * sapply(seq.k, pow))
}

#' Extrapolated Rank-Biased Overlap on uneven-length vectors
#'
#' @param l A vector.
#' @param s A vector, shorter than \code{l}.
#' @param p A number, representing persistence.
rbo_ext_uneven <- function(l, s, p = 0.9) {
	assertthat::assert_that(length(l) > length(s))
	len.l <- length(l)
	len.s <- length(s)
	seq.l <- seq_len(len.l)
	seq.tail <- seq.l[-seq_len(len.s)]
	pow <- functional::Curry("^", x = p)
	a <- functional::Curry(agreement, x = l, y = s)
	o <- functional::Curry(overlap, x = l, y = s)
	((1 - p) / p) *
	(sum(sapply(seq.l, a) * sapply(seq.l, pow)) +
	 sum(a(len.s) *
	 sapply(seq.tail, function(d) (d - len.s) / d) *
	 sapply(seq.tail, pow))) +
	((o(len.l) - o(len.s))/len.l + a(len.s)) * pow(len.l)
}

#' Extrapolated Rank-Biased Overlap
#'
#' @param x A vector.
#' @param y A vector.
#' @param p A number, representing persistence.
#' @export
rbo_ext <- function(x, y, p = 0.9) {
	assertthat::assert_that(
		length(x) > 0,
		length(y) > 0,
		is.vector(x),
		is.vector(y)
	)
	len.x <- length(x)
	len.y <- length(y)

	if (len.x > len.y) {
		rbo_ext_uneven(x, y, p)
	} else if (len.y > len.x) {
		rbo_ext_uneven(y, x, p)
	} else {
		rbo_ext_even(x, y, p)
	}
}
