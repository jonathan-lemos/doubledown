import <- function(name) {
	if (!require(name, character.only = TRUE)) {
		install.packages(name)
		library(name, character.only = TRUE)
	}
}
