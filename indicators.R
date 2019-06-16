source(file = "import.R")
import("magrittr")
import("dequer")

sma <- function(input, n = 20) {
	acc <- 0
	ret <- c()
	for (i in 1:length(input)) {
		acc <- acc + input[i]
		if (i > n) {
			acc <- acc - input[i - n]
		}
		ret[i] <- acc / min(i, n)
	}
	return(ret)
}

ema <- function(input, n = 20) {
	multiplier <- 2 / (n + 1)
	acc <- input[1]
	ret <- c(input[1])
	for (i in 2:length(input)) {
		acc <- (acc * (1 - multiplier)) + (input[i] * multiplier)
		ret[i] <- acc
	}
	return(ret)
}

macd <- function(input, fast = 12, slow = 26) {
	return(input %>% ema(fast) - input %>% ema(slow))
}

macd_signal <- function(input, n = 9, fast = 12, slow = 26) {
	return input %>% macd(fast, slow) %>% ema(9)
}

pct_chg <- function(input) {
	ret <- c(NA)
	for (i in 2:length(input)) {
		ret[i] = (input[i] / input[i - 1]) * 100
	}
	return(ret)
}

rsi <- function(input, n = 14) {
	sample <- input[1:n] %>% pct_chg
	avg_gain <- sample[sample > 0] %>% mean
	avg_loss <- sample[sample < 0] %>% mean
	return(100 - (100 / (1 + (avg_gain / avg_loss))))
}

bb_moving_sd <- function(input, n = 20) {
	mavg <- input %>% sma(n)
	ma <- input[1]
	variance <- 0
	variance_queue <- queue()
	variance_queue %>% pushback(0)
	ret <- c(NA)

	for (i in 2:length(input)) {
		ma <- ma + input[i]
		variance <- variance + (input[i] - ma / min(i, n)) ^ 2
		variance_queue %>% pushback(variance)
		if (i > n) {
			ma <- ma - input[i - n]
			variance <- variance - variance_queue %>% pop
		}
		ret[i] <- sqrt(variance / (min(i, n) - 1))
	}
	return(ret)
}

bb_top <- function(input, mavg_days = 20, n_stddev = 2) {
	mavg <- input %>% sma(mavg_days)
	stddev <- bb_moving_sd(mavg_days) * n_stddev
	return(mavg + stddev)
}

bb_bot <- function(input, mavg_days = 20, n_stddev = 2) {
	mavg <- input %>% sma(mavg_days)
	stddev <- input %>% bb_moving_sd(mavg_days) * n_stddev
	return(mavg - stddev)
}

bb_div <- function(input, mavg_days = 20, n_stddev = 2) {
	return(input %>% bb_moving_sd(mavg_days) * n_stddev)
}
