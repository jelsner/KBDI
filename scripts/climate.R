# Long-term station drought analysis; run after pipeline.R, from repository root.
stopifnot(requireNamespace("sandwich", quietly = TRUE))
source("scripts/netRainfall.R"); source("scripts/droughtIndex.R")
library(ggplot2)
w <- read.csv("Data/processed/daily-weather.csv")
w$Date <- as.Date(w$Date)
# Full calendar years only. Partial latest year remains available for forecasts.
last_year <- as.integer(format(max(w$Date), "%Y"))
if (max(w$Date) < as.Date(paste0(last_year, "-12-31"))) last_year <- last_year - 1L
w <- subset(w, Year <= last_year)
expected_days <- function(year, month = NULL) {
  begin <- as.Date(sprintf("%d-%02d-01", year, if (is.null(month)) 1 else month))
  end <- if (is.null(month)) as.Date(sprintf("%d-01-01", year + 1)) else
    seq(begin, by = "month", length.out = 2)[2]
  as.integer(end - begin)
}
monthly_summary <- function(weather, burnin = 365) {
  do.call(rbind, lapply(split(weather, interaction(weather$Year, weather$Month)), function(d) {
    if (!nrow(d)) return(NULL)
    expected <- expected_days(d$Year[1], d$Month[1])
    kd <- is.finite(d$Qlm) & d$days_since_gap >= burnin
    complete_k <- nrow(d) == expected && all(kd)
    data.frame(Year = d$Year[1], Month = d$Month[1], Days = nrow(d),
      KBDIDays = sum(kd), MeanDeficit = if (complete_k) mean(d$Qlm) else NA_real_,
      HighDays = if (complete_k) sum(d$KBDI > 650) else NA_real_,
      RainMm = if (nrow(d) == expected && all(is.finite(d$PRCP))) sum(d$PRCP) else NA_real_,
      TmaxC = if (nrow(d) == expected && all(is.finite(d$TMAX))) mean(d$TMAX) else NA_real_,
      TminC = if (nrow(d) == expected && all(is.finite(d$TMIN))) mean(d$TMIN) else NA_real_)
  }))
}
hac_fit <- function(d, formula = Value ~ Year) {
  d <- d[complete.cases(d[all.vars(formula)]), ]; d <- d[order(d$Year), ]
  if (nrow(d) < 20) stop("Insufficient years for climate trend.")
  m <- lm(formula, data = d)
  # Lag-3 Newey-West covariance on annual observations, without prewhitening.
  se <- sqrt(diag(sandwich::NeweyWest(m, lag = 3, prewhite = FALSE, adjust = TRUE)))
  b <- coef(m); critical <- qt(.975, df.residual(m))
  data.frame(Term = names(b), Estimate = unname(b), SE = unname(se),
    Lower95 = unname(b - critical * se), Upper95 = unname(b + critical * se),
    P = unname(2 * pt(-abs(b / se), df.residual(m))), N = nrow(d),
    FirstYear = min(d$Year), LastYear = max(d$Year), row.names = NULL)
}
monthly <- monthly_summary(w)
monthly <- monthly[order(monthly$Year, monthly$Month), ]
write.csv(monthly, "Data/processed/monthly-climate.csv", row.names = FALSE)
# No scaling partially observed years or months to pretend they are complete.
annual <- do.call(rbind, lapply(split(monthly, monthly$Year), function(d) {
  row <- data.frame(Year = d$Year[1])
  for (v in c("MeanDeficit", "HighDays", "RainMm", "TmaxC", "TminC"))
    row[[v]] <- if (nrow(d) == 12 && all(is.finite(d[[v]]))) {
      if (v %in% c("HighDays", "RainMm")) sum(d[[v]]) else weighted.mean(d[[v]], d$Days)
    } else NA_real_
  row
}))
write.csv(annual, "Data/processed/annual-climate.csv", row.names = FALSE)
# Updated and original-endpoint comparisons use the same corrected computation.
monthly_trends <- list(); annual_trends <- list()
for (endpoint in unique(c(2020L, last_year))) for (variable in c("MeanDeficit", "RainMm", "TmaxC", "TminC")) {
  for (month in 1:12) {
    d <- subset(monthly, Year <= endpoint & Month == month)
    d$Value <- d[[variable]]
    t <- subset(hac_fit(d), Term == "Year")
    monthly_trends[[length(monthly_trends) + 1L]] <- cbind(Endpoint = endpoint, Variable = variable, Month = month, t)
  }
}
monthly_trends <- do.call(rbind, monthly_trends)
monthly_trends$PAdjusted <- ave(monthly_trends$P, interaction(monthly_trends$Endpoint, monthly_trends$Variable), FUN = function(p) p.adjust(p, "BH"))
write.csv(monthly_trends, "outputs/monthly-trends.csv", row.names = FALSE)
for (endpoint in unique(c(2020L, last_year))) for (variable in c("MeanDeficit", "HighDays", "RainMm", "TmaxC", "TminC")) {
  d <- subset(annual, Year <= endpoint); d$Value <- d[[variable]]
  t <- subset(hac_fit(d), Term == "Year")
  annual_trends[[length(annual_trends) + 1L]] <- cbind(Endpoint = endpoint, Variable = variable, t)
}
annual_trends <- do.call(rbind, annual_trends)
annual_trends$PAdjusted <- ave(annual_trends$P, annual_trends$Endpoint, FUN = function(p) p.adjust(p, "BH"))
write.csv(annual_trends, "outputs/annual-trends.csv", row.names = FALSE)
# Repaired ENSO source. Contemporary SOI adjustment is descriptive, not attribution.
soi_path <- "Data/raw/soi.data"
soi_url <- "https://psl.noaa.gov/data/correlation/soi.data"
if ("--refresh" %in% commandArgs(TRUE) || !file.exists(soi_path)) {
  download.file(soi_url, paste0(soi_path, ".download"), quiet = TRUE)
  stopifnot(file.rename(paste0(soi_path, ".download"), soi_path))
}
soi_lines <- readLines(soi_path)
soi_lines <- soi_lines[grepl("^\\s*[12][0-9]{3}\\s+[-0-9]", soi_lines)]
soi_lines <- soi_lines[lengths(strsplit(trimws(soi_lines), "[[:space:]]+")) == 13]
soi_matrix <- read.table(text = soi_lines, header = FALSE, fill = FALSE)
stopifnot(ncol(soi_matrix) == 13)
soi <- data.frame(Year = rep(soi_matrix[[1]], each = 12), Month = rep(1:12, nrow(soi_matrix)),
                  SOI = as.vector(t(as.matrix(soi_matrix[, -1]))))
soi$SOI[soi$SOI <= -99] <- NA_real_
joined <- merge(monthly, soi, by = c("Year", "Month"))
enso <- list()
for (month in 1:12) {
  d <- subset(joined, Month == month & Year >= 1951)
  d$Value <- d$MeanDeficit
  t <- hac_fit(d, Value ~ Year + SOI)
  enso[[month]] <- cbind(Month = month, t[t$Term != "(Intercept)", ])
}
enso <- do.call(rbind, enso)
enso$PAdjusted <- ave(enso$P, enso$Term, FUN = function(p) p.adjust(p, "BH"))
write.csv(enso, "outputs/enso-adjusted-trends.csv", row.names = FALSE)
# Sensitivity to rain climatology, starting deficit, and spin-up exclusion.
settings <- expand.grid(AnnualRain = c(55, 60, 65), InitialQ = c(0, 400, 800), Burnin = c(180, 365, 730))
sensitivity <- list()
for (i in seq_len(nrow(settings))) {
  ws <- w; ws$KBDI <- NA_real_
  runs <- split(seq_len(nrow(ws)), cumsum(c(TRUE, diff(ws$complete) != 0)))
  for (idx in runs) if (ws$complete[idx[1]]) ws$KBDI[idx] <- droughtIndex(settings$InitialQ[i], settings$AnnualRain[i],
    ws$TMAX[idx] * 1.8 + 32, netRainfall(ws$PRCP[idx] / 25.4))$Ql
  ws$Qlm <- ws$KBDI * .254
  ms <- monthly_summary(ws, settings$Burnin[i])
  for (month in c(4, 5, 6, 7)) {
    d <- subset(ms, Month == month); d$Value <- d$MeanDeficit
    t <- subset(hac_fit(d), Term == "Year")
    sensitivity[[length(sensitivity) + 1L]] <- cbind(settings[i, ], Month = month, t)
  }
}
sensitivity <- do.call(rbind, sensitivity)
write.csv(sensitivity, "outputs/kbdi-sensitivity.csv", row.names = FALSE)
# Full-record and detrended monthly station relationships are descriptive.
valid <- subset(w, days_since_gap >= 365 & is.finite(Qlm))
valid$CalendarDay <- as.Date(paste0("2000-", format(valid$Date, "%m-%d")))
p <- ggplot(valid, aes(CalendarDay, Year, fill = Qlm)) + geom_tile() +
  scale_fill_distiller(palette = "BrBG", direction = -1, name = "Deficit\n(mm)") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") + theme_minimal() +
  labs(x = NULL, y = NULL, title = "Station-derived KBDI across the daily record")
ggsave("outputs/drought-calendar.png", p, width = 10, height = 5, dpi = 180, bg = "white")
tr <- subset(monthly_trends, Endpoint == last_year & Variable == "MeanDeficit")
p <- ggplot(tr, aes(factor(Month, levels = 1:12, labels = month.abb), Estimate * 10)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_errorbar(aes(ymin = Lower95 * 10, ymax = Upper95 * 10), width = .15) +
  geom_point(aes(color = PAdjusted < .05), size = 2.5) +
  scale_color_manual(values = c("grey50", "#007A78"), labels = c("No", "Yes"), name = "BH-adjusted\np < 0.05") +
  theme_minimal() + labs(x = NULL, y = "Deficit trend (mm per decade)", title = "Seasonal differences in long-term drying",
    subtitle = "95% intervals use Newey–West annual covariance; complete months only")
ggsave("outputs/monthly-drought-trends.png", p, width = 9, height = 4.5, dpi = 180, bg = "white")
p <- ggplot(annual, aes(Year, HighDays)) + geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#007A78", na.rm = TRUE) +
  theme_minimal() + labs(x = NULL, y = "Days with KBDI > 650", title = "Annual exposure to high moisture deficit",
    subtitle = "Only fully observed years after spin-up are included; line is descriptive")
ggsave("outputs/high-deficit-days.png", p, width = 8, height = 4, dpi = 180, bg = "white")
climate_manifest <- list(run_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), first_year = min(w$Year),
  last_complete_year = last_year, station = "USW00093805", soi_url = soi_url,
  soi_md5 = unname(tools::md5sum(soi_path)), annual_hac_lag = 3,
  monthly_multiplicity = "BH adjustment within each variable and endpoint across 12 months",
  sensitivity = "27 combinations: annual rain 55/60/65 in; initial Q 0/400/800; spin-up 180/365/730 days")
jsonlite::write_json(climate_manifest, "outputs/climate-manifest.json", auto_unbox = TRUE, pretty = TRUE)
print(subset(monthly_trends, Endpoint == last_year & Variable == "MeanDeficit"))
print(subset(annual_trends, Endpoint == last_year))
# Two primary drought outcomes, with their own physical units.
a_plot <- rbind(data.frame(Year = annual$Year, Value = annual$MeanDeficit, Outcome = "Mean moisture deficit (mm)"),
                data.frame(Year = annual$Year, Value = annual$HighDays, Outcome = "Days with KBDI > 650"))
p <- ggplot(a_plot, aes(Year, Value)) + geom_point(na.rm = TRUE, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#007A78", na.rm = TRUE) +
  facet_wrap(~Outcome, ncol = 1, scales = "free_y") + theme_minimal() +
  labs(x = NULL, y = NULL, title = "Long-term drought change at Tallahassee airport",
       subtitle = "Complete eligible years only; trend uncertainty is reported separately")
ggsave("outputs/annual-drought-change.png", p, width = 9, height = 6, dpi = 180, bg = "white")
