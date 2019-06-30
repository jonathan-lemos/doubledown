#' Plot Scale Y
#'
#' Scales the y-axis of a ddplot. This will scale the y-axis to include the minimum and maximum values of the given input as well as any previous inputs.
#'
#' @param plot The ddplot.
#' @param ... The datasets the y-axis should be scaled on. These must be the same length as the ddplot's timestamp vector.
#' @param padding The padding to put on either side as a percentage of the difference between the minimum and maximum. The default is \{0.05} for a 5% padding.
#' @return The same plot with the y-axis scaled.
#' @export
plot_scale_y <- function(plot, ..., padding = 0.05) {
	vals <- list(...) %>%
			  lapply(function(x)
					 (data.frame("timestamp" = plot$data$timestamp, "x" = x) %>%
						 filter(timestamp >= plot$since, timestamp <= plot$until))$x) %>% unlist

	plot$gmin <- min(min(vals), plot$gmin)
	plot$gmax <- max(max(vals), plot$gmax)
	diff <- plot$gmax - plot$gmin
	plot +
		ylim(plot$gmin - diff * padding, plot$gmax + diff * padding)
}
