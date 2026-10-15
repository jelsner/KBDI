# Inches of rain reaching soil, after 0.2 inches interception per wet spell.
netRainfall <- function(Rainfall24) {
  if (!is.numeric(Rainfall24) || any(!is.finite(Rainfall24)) || any(Rainfall24 < 0))
    stop("Rainfall must be finite, nonnegative inches.")
  result <- numeric(length(Rainfall24))
  cumulative <- 0
  for (i in seq_along(Rainfall24)) {
    if (Rainfall24[i] == 0) cumulative <- 0
    result[i] <- max(0, cumulative + Rainfall24[i] - .2) - max(0, cumulative - .2)
    cumulative <- cumulative + Rainfall24[i]
  }
  result
}
