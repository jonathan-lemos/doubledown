#' Cross
#'
#' For each value of the two input vectors, returns \code{TRUE} if their values "crossed" at a given point, and \code{FALSE} if not.
#' The first member of the output vector will be NA.
#'
#' @param input1 The first input vector of numerics
#' @param input2 The second input vector of numerics
#' @return A vector of \code{TRUE} if the values cross, and \code{FALSE} if not.
#' @export
cross <- function(input1, input2) {
	tmp <- input1 - input2
	sign(tmp) != sign(lag(tmp))
}
