#' Double Down Plot
#'
#' Creates a ggplot() for use with doubledown's functions.
#'
#' @param data A data frame containing columns "close", "high", "low", "open", "timestamp", and "volume".
#'
ddplot <- function(data, since = Sys.Date() - months(1), until = Sys.Date(), num_breaks = 5) {
	tmp <- data %>% filter(timestamp >= since, timestamp <= until)
	diff = max(tmp$high) - min(tmp$low)

	ret <- ggplot(data,
				  aes(x = data$timestamp,
					  close = close,
					  high = high,
					  low = low,
					  open = open,
					  volume = volume)) +

			scale_x_bd(business.dates = data$timestamp,
				   labels = date_format("%b '%y"),
				   max.major.breaks = num_breaks,
				   limits = c(since, until)) +

			ylim(min(tmp$low) - diff * 0.05, max(tmp$high) + diff * 0.05) +
			xlab("date") +
			ylab("close")

		ret$since = since
		ret$until = until
		ret
}
