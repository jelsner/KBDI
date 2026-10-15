# Integration checks after a successful pipeline run.
s <- read.csv("Data/processed/seasonal-fires.csv")
f <- read.csv("Data/processed/anf-fires.csv")
w <- read.csv("Data/processed/daily-weather.csv")
p <- read.csv("outputs/hindcasts.csv")
stopifnot(!anyDuplicated(f$fod_id), !anyDuplicated(s[c("Year", "Cause")]),
          all(diff(as.Date(w$Date)) == 1), all(w$KBDI >= 0 & w$KBDI <= 800, na.rm = TRUE),
          all(!s$eligible | s$days_since_gap >= 365),
          all(p$Expected > 0), all(p$Lower90 <= p$Upper90))
for (cause in unique(s$Cause)) {
  d <- subset(s, Cause == cause & eligible)
  a <- subset(p, Cause == cause & Model == "Drought")
  b <- subset(p, Cause == cause & Model == "Climatology")
  stopifnot(identical(a$Year, b$Year), identical(a$Observed, b$Observed),
            all(vapply(a$Year, function(y) sum(d$Year < y) >= 15, logical(1))))
}
for (year in unique(s$Year)) {
  d <- subset(s, Year == year)
  season <- subset(f, Year == year & Month %in% 5:7)
  stopifnot(d$nFires[d$Cause == "All"] == nrow(season),
            d$nFires[d$Cause == "All"] >= sum(d$nFires[d$Cause != "All"]))
}
cat("Pipeline integrity and hindcast checks passed.\n")
