format_date <- function(plot, data) {
	plot +
		scale_x_date(breaks = data$timestamp) +
		theme(axis.text.x = element_text(angle = 45))
}

add_candlestick <- function(plot, data) {
	plot +
		geom_linerange(aes(x = timestamp, ymin = low, ymax = high), data = data) +
		geom_rect(aes(xmin = timestamp - 0.4,
					  xmax = timestamp + 0.4,
					  ymin = pmin(open, close),
					  ymax = pmax(open, close), data = data),
				  fill = ifelse(data$close >= data$open, "darkgreen", "darkred"))
}

results <- c("TSLA", "AAPL") %>% get_all
p1 <- results[["TSLA"]] %>% tail(30) %>%
	ggplot %>%
	add_candlestick(results[["TSLA"]] %>% tail(30))
p1 %>% plot
# p1 <- ggplot() + geom_line(aes(x = as.numeric(row.names(res)), y = close), data = res)
# plot(p1)
