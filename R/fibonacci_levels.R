#' Fibonacci Retracement Levels
#'
#' Calculates values between the minimum and maximum over a given period. For example, 25 would be 25% above the minimum, and 80 would be 20% below the maximum.
#'
#' @param min The minimum value to draw Fibonacci levels between.
#' @param max The maximum value to draw Fibonacci levels between.
#' @param levels The Fibonacci levels to draw. By default this draws the Fibonacci levels of 0%, 23.6068%, 38.1966%, 50%, 61.8034%, and 100%.
#' @export
fibonacci_levels <- function(min, max, levels = c(0, 23.6068, 38.1966, 50, 61.8034, 100)) {
	min + ((max - min) * levels)
}

