# Application checks: temporal trend baselines and uncertainty in paired skill.
stopifnot(requireNamespace("MASS", quietly = TRUE))
s <- read.csv("Data/processed/seasonal-fires.csv")
formulae <- list(Climatology = nFires ~ 1, Drought = nFires ~ DrynessCm,
                TimeTrend = nFires ~ YearDecade, DroughtAndTime = nFires ~ DrynessCm + YearDecade)
fit_checked <- function(formula, d) {
  fit <- MASS::glm.nb(formula, data = d, control = glm.control(maxit = 100))
  if (!fit$converged || !is.null(fit$th.warn)) stop("NB convergence failure in application checks")
  fit
}
checks <- list()
for (cause in c("All", "Human", "Natural")) {
  d <- subset(s, Cause == cause & eligible); d <- d[order(d$Year), ]
  d$YearDecade <- (d$Year - 2000) / 10
  for (label in names(formulae)) for (i in seq.int(16, nrow(d))) {
    fit <- fit_checked(formulae[[label]], d[seq_len(i - 1), ])
    mu <- as.numeric(predict(fit, newdata = d[i, ], type = "response"))
    checks[[length(checks) + 1L]] <- data.frame(Cause = cause, Model = label, Year = d$Year[i],
      Observed = d$nFires[i], Expected = mu, LogScore = -dnbinom(d$nFires[i], mu = mu, size = fit$theta, log = TRUE))
  }
}
checks <- do.call(rbind, checks)
metrics <- do.call(rbind, lapply(split(checks, interaction(checks$Cause, checks$Model)), function(d)
  data.frame(Cause = d$Cause[1], Model = d$Model[1], N = nrow(d), MAE = mean(abs(d$Observed - d$Expected)),
    RMSE = sqrt(mean((d$Observed - d$Expected)^2)), MeanNegLogScore = mean(d$LogScore))))
write.csv(checks, "outputs/forecast-checks.csv", row.names = FALSE)
write.csv(metrics, "outputs/forecast-check-metrics.csv", row.names = FALSE)
set.seed(20261015)
uncertainty <- list()
for (cause in c("All", "Human", "Natural")) {
  a <- subset(checks, Cause == cause & Model == "Climatology")
  b <- subset(checks, Cause == cause & Model == "Drought")
  stopifnot(identical(a$Year, b$Year))
  ea <- abs(a$Observed - a$Expected); eb <- abs(b$Observed - b$Expected); n <- length(ea)
  # Circular moving-block bootstrap over paired evaluation years (block length 3).
  draws <- replicate(2000, {
    starts <- sample.int(n, ceiling(n / 3), replace = TRUE)
    idx <- unlist(lapply(starts, function(j) ((j - 1 + 0:2) %% n) + 1))[seq_len(n)]
    c(Difference = mean(ea[idx] - eb[idx]), Percent = 100 * (1 - mean(eb[idx]) / mean(ea[idx])))
  })
  uncertainty[[cause]] <- data.frame(Cause = cause, N = n, BlockLength = 3,
    MAEImprovement = mean(ea - eb), Lower95 = quantile(draws[1, ], .025), Upper95 = quantile(draws[1, ], .975),
    PercentImprovement = 100 * (1 - mean(eb) / mean(ea)), PercentLower95 = quantile(draws[2, ], .025),
    PercentUpper95 = quantile(draws[2, ], .975))
}
write.csv(do.call(rbind, uncertainty), "outputs/forecast-skill-uncertainty.csv", row.names = FALSE)
# Preserve the original prior-burn-area question as a separate, aligned sensitivity.
fires <- read.csv("Data/processed/anf-fires.csv")
fires$Date <- as.Date(fires$Date)
for (i in seq_len(nrow(s))) {
  start <- as.Date(paste0(s$Year[i] - 1, "-05-01")); end <- as.Date(paste0(s$Year[i], "-05-01"))
  s$PriorBurnThousands[i] <- if (s$Year[i] == min(s$Year)) NA_real_ else
    sum(fires$fire_size[fires$Date >= start & fires$Date < end]) / 1000
}
write.csv(s, "Data/processed/seasonal-application-data.csv", row.names = FALSE)
burn_checks <- list()
for (cause in c("All", "Natural")) {
  d <- subset(s, Cause == cause & eligible & is.finite(PriorBurnThousands)); d <- d[order(d$Year), ]
  for (i in seq.int(16, nrow(d))) for (burn in c(FALSE, TRUE)) {
    formula <- if (burn) nFires ~ DrynessCm + PriorBurnThousands else nFires ~ DrynessCm
    fit <- fit_checked(formula, d[seq_len(i - 1), ])
    mu <- as.numeric(predict(fit, newdata = d[i, ], type = "response"))
    burn_checks[[length(burn_checks) + 1L]] <- data.frame(Cause = cause, Model = if (burn) "DroughtAndPriorBurn" else "Drought",
      Year = d$Year[i], Observed = d$nFires[i], Expected = mu)
  }
}
write.csv(do.call(rbind, burn_checks), "outputs/prior-burn-hindcasts.csv", row.names = FALSE)
print(metrics)
print(do.call(rbind, uncertainty))
