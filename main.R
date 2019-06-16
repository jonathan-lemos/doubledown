source(file = "import.R")

import("readr")
import("dplyr")
import("ggplot2")

source(file = "api_key.R")
source(file = "indicators.R")

get_csv <- function(symbol) {
	link <- paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=", symbol, "&datatype=csv&outputsize=full&apikey=", api_key, sep = "")
	response <- link %>% read.csv
	return(response[nrow(response) : 1,])
}

res <- get_csv("TSLA")
res <- res %>%
	select(timestamp, adjusted_close) %>%
	rename(close = adjusted_close) %>%
	mutate(moving_avg = simple_moving_avg(close))
res %>% head %>% print
# p1 <- ggplot() + geom_line(aes(x = as.numeric(row.names(res)), y = close), data = res)
# plot(p1)
