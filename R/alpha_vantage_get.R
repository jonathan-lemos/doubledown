#' Alpha Vantage Get
#'
#' Downloads a CSV from Alpha Vantage and returns a data frame.
#' The dates in the data frame are ascending, meaning row 1 is the earliest date.
#' Historic data is split adjusted, but not dividend adjusted.
#' The columns retrieved are "close", "high", "low", "open", "timestamp" and "volume"
#'
#' @param symbol The stock ticker to retrieve. Example: \code{"TSLA"}.
#' @param api_key The Alpha Vantage API key to use. Get one at https://www.alphavantage.co/support/#api-key
#' @return The corresponding data frame.
#' @export
alpha_vantage_get <- function(symbol, api_key) {
	split_adjust <- function(input, split_coefficient) {
		if (length(input) != length(split_coefficient)) {
			stop("The length of the input does not equal the length of the split coefficient data.")
		}
		coef <- 1
		for (i in length(input):1) {
			input[i] <- input[i] / coef
			coef <- coef * split_coefficient[i]
		}
		input
	}

	link <- paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=", symbol, "&datatype=csv&outputsize=full&apikey=", api_key, sep = "")

	response <- link %>% read.csv
	response <- response[nrow(response) : 1,]
	response <- response %>% mutate(
			   timestamp = as.Date(timestamp),
			   open = open %>% split_adjust(split_coefficient),
			   high = high %>% split_adjust(split_coefficient),
			   low = low %>% split_adjust(split_coefficient),
			   close = close %>% split_adjust(split_coefficient)
			   ) %>%
		select(-adjusted_close, -dividend_amount, -split_coefficient)
}
