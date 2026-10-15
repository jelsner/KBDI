# KBDI: hundredths of an inch, temperatures in F, annual mean rain R in inches.
# Subtract effective rain before evaluating today's drying increment.
droughtIndex <- function(Q, R, MaxTemp, NetR) {
  if (length(Q) != 1L || !is.finite(Q) || Q < 0 || Q > 800 ||
      length(R) != 1L || !is.finite(R) || R <= 0 ||
      length(MaxTemp) != length(NetR) || any(!is.finite(MaxTemp)) ||
      any(!is.finite(NetR)) || any(NetR < 0)) stop("Invalid KBDI inputs.")
  values <- numeric(length(NetR))
  for (i in seq_along(NetR)) {
    Q <- max(0, Q - 100 * NetR[i])
    drying <- (800 - Q) * max(0, .968 * exp(.0486 * MaxTemp[i]) - 8.3) /
      (1 + 10.88 * exp(-.0441 * R)) * .001
    Q <- min(800, Q + drying)
    values[i] <- Q
  }
  list(Ql = values, DroughtIndex = pmin(7L, floor(values / 100)))
}
