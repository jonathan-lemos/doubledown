#' Alpha Vantage Get All
#'
#' Retrieves info for all of the given stock tickers, returning a \code{hash()} of data frames.
#' The returned hashmap maps stock tickers to data frames returned from \code{alpha_vantage_get}.
#'
#' @param ... One or more stock tickers (strings) to retrieve.
#' @param api_key The Alpha Vantage API key to use. Get one at https://www.alphavantage.co/support/#api-key
#' @return The corresponding \code{hash()}.
#' @export
av_get_all <- function(..., api_key) {
	symbols <- list(...)
	hash(symbols, symbols %>% lapply(alpha_vantage_get, api_key))
}
