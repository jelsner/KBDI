w <- read.csv("Data/processed/daily-weather.csv")
m <- read.csv("Data/processed/monthly-climate.csv")
a <- read.csv("Data/processed/annual-climate.csv")
t <- read.csv("outputs/monthly-trends.csv")
s <- read.csv("outputs/kbdi-sensitivity.csv")
p <- read.csv("outputs/prism-station-check.csv")
stopifnot(!anyDuplicated(m[c("Year", "Month")]), !anyDuplicated(a$Year),
          all(is.finite(m$MeanDeficit) == (m$KBDIDays == m$Days)),
          all(t$PAdjusted >= t$P - 1e-10),
          nrow(unique(s[c("AnnualRain", "InitialQ", "Burnin")])) == 27,
          abs(sum(p$AreaWeight) - 1) < 1e-8,
          all(p$RawR2 >= 0 & p$RawR2 <= 1), all(p$AnomalyR2 >= 0 & p$AnomalyR2 <= 1))
for (i in seq_len(nrow(a))) if (is.finite(a$MeanDeficit[i])) {
  d <- subset(m, Year == a$Year[i])
  stopifnot(nrow(d) == 12, all(is.finite(d$MeanDeficit)),
    isTRUE(all.equal(a$MeanDeficit[i], weighted.mean(d$MeanDeficit, d$Days))))
}
cat("Climate completeness, multiplicity, and spatial integrity checks passed.\n")
