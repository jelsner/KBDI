source("scripts/netRainfall.R")
source("scripts/droughtIndex.R")
stopifnot(isTRUE(all.equal(netRainfall(c(.1, .15, .1, 0, .3)), c(0, .05, .1, 0, .1))),
          length(netRainfall(numeric())) == 0,
          inherits(try(netRainfall(c(0, NA)), silent = TRUE), "try-error"))
q <- droughtIndex(400, 60, rep(100, 1000), rep(0, 1000))$Ql
stopifnot(all(diff(q) >= 0), all(q >= 0 & q <= 800),
          droughtIndex(400, 60, 20, 10)$Ql == 0,
          length(droughtIndex(400, 60, numeric(), numeric())$Ql) == 0)
cat("Core hydrology tests passed.\n")
