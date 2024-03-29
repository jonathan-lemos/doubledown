#' Plot Volume
#'
#' Adds a volume chart to a ddplot.
#'
#' @param plot The ddplot.
#' @return The ddplot with a volume component added.
#' @export
plot_volume <- function(plot) {
	plot %>%
		add_trace(y = ~volume,
				  type = "bar",
				  yaxis = "y",
				  marker = list(color = '#C9EFF9'),
				  name = "Volume")
}
