#' Percent Change
#'
#' Calculates the percentage change from the previous value for each value in the input vector.
#' The first member of the output vector will be NA.
#' A 1.2% change will be expressed as the value \code{1.2}.
#'
#' @param input A vector of numerics
#' @return A vector containing the percentage change of the previous value.
#' @export
percent_change <- function(input) {
	ret <- c(NA)
	for (i in 2:length(input)) {
		ret[i] = (input[i] / input[i - 1]) * 100
	}
	ret
}
