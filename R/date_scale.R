#' Date Scale
#'
#' Scales the x-axis of a ggplot properly to display stock dates.
#' The x-axis will ignore weekends and holidays.
#'
#' @param plot The ggplot.
#' @param data The data.
#' @param major_breaks The number of major breaks in the graph. By default this is 10.
#' @param minor_breaks The number of minor breaks in the graph. By default this is \code{major_breaks * 5}.
#' @return The same plot with the x-axis scaled properly.
#' @export
date_scale <- function(plot, data) {
	plot +
		scale_x_bd(business.dates = data$timestamp, labels = date_format("%b '%y"))
}
