# Computes the Keetch-Byram Drought Index (KDBI) from net 24-hr rainfall in inches and daily high temperature in F. 
# No missing values are allowed
droughtIndex <- function(Q, R, MaxTemp, NetR) {
  Q <- 269  # starting value
  R <- 59.23 # annual rainfall amount at location of interest
  
  Ql <- numeric()
  DeltaQl <- numeric()
  for (i in 1:length(NetR)) {
    DeltaQ <-
      (800 - Q) * (.968 * exp(.0486 * MaxTemp[i]) - 8.3) / (1 + 10.88 * exp(-.0441 * R)) * .001
    Q <-
      ifelse(NetR[i] == 0,  Q + DeltaQ,  (Q + DeltaQ) - NetR[i] * 100)
    Q <- ifelse(Q < 0, 0, Q)
    Ql <- c(Ql, Q)
    DeltaQl <- c(DeltaQl, DeltaQ)
    DroughtIndex <- floor(Ql / 100)
  }
  output <- list(Ql = Ql,
                 DroughtIndex = DroughtIndex)
  return(output)
}
