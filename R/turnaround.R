#' Turnaround
#'
#' For each value in the input vector, returns TRUE if the value "turned around", meaning if it was increasing and then decreased, or was decreasing and then increased.
#' The first member of the output vector will be NA.
#'
#' @param input A vector of numerics
#' @return A vector of TRUE if the value turned around, false if not.
#' @export
turnaround <- function(input) {
	sign(input - lag(input)) != sign(lag(input) - lag(input, 2))
}
