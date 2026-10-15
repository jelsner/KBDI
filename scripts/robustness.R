# Trend sensitivity to covariance lag, period, and original KBDI implementation.
if (!exists("hac_fit")) source("scripts/climate.R", local = TRUE)
annual <- read.csv("Data/processed/annual-climate.csv")
rows <- list()
for (start in c(1944, 1960, 1980)) for (lag in c(0, 1, 3, 5)) for (variable in c("MeanDeficit", "HighDays")) {
  d <- subset(annual, Year >= start & is.finite(annual[[variable]]))
  d$Value <- d[[variable]]
  m <- lm(Value ~ Year, data = d)
  se <- sqrt(diag(sandwich::NeweyWest(m, lag = lag, prewhite = FALSE, adjust = TRUE)))["Year"]
  b <- coef(m)["Year"]; z <- qt(.975, df.residual(m))
  rows[[length(rows) + 1L]] <- data.frame(Variable = variable, Start = start, HACLag = lag,
    N = nrow(d), Estimate = b, Lower95 = b - z * se, Upper95 = b + z * se)
}
write.csv(do.call(rbind, rows), "outputs/period-covariance-sensitivity.csv", row.names = FALSE)
# Reproduce the historical functions, including the dplyr lag used in the notebooks.
e <- new.env(); e$lag <- dplyr::lag
sys.source("archive/legacy/netRainfall.R", e); sys.source("archive/legacy/droughtIndex.R", e)
w <- read.csv("Data/processed/daily-weather.csv")
w$Date <- as.Date(w$Date)
w$LegacyQlm <- NA_real_
runs <- split(seq_len(nrow(w)), cumsum(c(TRUE, diff(w$complete) != 0)))
for (idx in runs) if (w$complete[idx[1]]) w$LegacyQlm[idx] <- e$droughtIndex(400, 60,
  w$TMAX[idx] * 1.8 + 32, e$netRainfall(w$PRCP[idx] / 25.4))$Ql * .254
eligible <- w$days_since_gap >= 365 & is.finite(w$Qlm) & is.finite(w$LegacyQlm)
write.csv(data.frame(Date = w$Date[eligible], CorrectedMm = w$Qlm[eligible],
  LegacyMm = w$LegacyQlm[eligible], DifferenceMm = w$Qlm[eligible] - w$LegacyQlm[eligible]),
  "outputs/legacy-algorithm-comparison.csv", row.names = FALSE)
# Annual summaries use identical eligible complete years for both algorithms.
annual_compare <- do.call(rbind, lapply(split(w, w$Year), function(d) {
  ok <- nrow(d) == expected_days(d$Year[1]) && all(d$days_since_gap >= 365) &&
    all(is.finite(d$Qlm)) && all(is.finite(d$LegacyQlm))
  if (!ok) return(NULL)
  data.frame(Year = d$Year[1], Corrected = mean(d$Qlm), Legacy = mean(d$LegacyQlm))
}))
trends <- do.call(rbind, lapply(c("Corrected", "Legacy"), function(v) {
  d <- annual_compare; d$Value <- d[[v]]
  cbind(Algorithm = v, subset(hac_fit(d), Term == "Year"))
}))
write.csv(trends, "outputs/algorithm-trend-sensitivity.csv", row.names = FALSE)
