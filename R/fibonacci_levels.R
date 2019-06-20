#' Fibonacci Retracement Levels
#'
#' Calculates values between the minimum and maximum over a given period. For example, 25 would be 25% above the minimum, and 80 would be 20% below the maximum.
#'
#' @param input A vector of numerics
#' @param n The number of days starting from the latest to draw Fibonacci levels over. By default this is 30. Pass \code{Inf} to use the entire input vector.
#' @param levels The Fibonacci levels to draw. By default this draws the Fibonacci levels of 0%, 23.6068%, 38.1966%, 50%, 61.8034%, and 100%.
#' @export
fibonacci_levels <- function(input, n = 30, levels = c(0, 23.6068, 38.1966, 50, 61.8034, 100)) {
	selection <- input %>% tail(n)
	minimum <- selection %>% min
	maximum <- selection %>% max
	minimum + ((maximum - minimum) * levels)
}

