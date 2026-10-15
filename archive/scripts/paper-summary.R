# Generate a results-linked revision plan and standalone LaTeX summary.
a <- read.csv("outputs/annual-trends.csv")
m <- read.csv("outputs/monthly-trends.csv")
u <- read.csv("outputs/forecast-skill-uncertainty.csv")
f <- read.csv("outputs/metrics.csv")
p <- jsonlite::fromJSON("outputs/prism-manifest.json")
endpoint <- max(a$Endpoint)
annual <- subset(a, Endpoint == endpoint & Variable == "MeanDeficit")
high <- subset(a, Endpoint == endpoint & Variable == "HighDays")
may <- subset(m, Endpoint == endpoint & Variable == "MeanDeficit" & Month == 5)
april <- subset(m, Endpoint == endpoint & Variable == "MeanDeficit" & Month == 4)
nu <- subset(u, Cause == "Natural")
values <- c(annual_slope=annual$Estimate * 10, annual_low=annual$Lower95 * 10, annual_high=annual$Upper95 * 10,
  annual_n=annual$N, annual_first=annual$FirstYear, annual_last=annual$LastYear,
  high_slope=high$Estimate * 10, high_low=high$Lower95 * 10, high_high=high$Upper95 * 10,
  may_slope=may$Estimate * 10, may_low=may$Lower95 * 10, may_high=may$Upper95 * 10,
  may_p=may$PAdjusted, april_slope=april$Estimate * 10, april_low=april$Lower95 * 10, april_high=april$Upper95 * 10,
  prism_r2=p$anomaly_r2_area_weighted,
  natural_base=subset(f, Cause=="Natural" & Model=="Climatology")$MAE,
  natural_drought=subset(f, Cause=="Natural" & Model=="Drought")$MAE,
  natural_skill_low=nu$Lower95, natural_skill_high=nu$Upper95)
strings <- vapply(values, function(x) formatC(x, digits=2, format="f"), character(1))
for (key in c("annual_n", "annual_first", "annual_last")) strings[key] <- as.character(values[key])
strings["may_p"] <- formatC(values["may_p"], digits=3, format="f")
strings["analysis_date"] <- format(Sys.time(), "%d %B %Y", tz="America/New_York")
for (stem in c("revision-plan.md", "analysis-summary.tex")) {
  text <- readLines(paste0("manuscript/", stem, ".in"))
  for (key in names(strings)) text <- gsub(paste0("{{", key, "}}"), strings[key], text, fixed=TRUE)
  stopifnot(!any(grepl("{{", text, fixed=TRUE)))
  writeLines(text, paste0("manuscript/", stem))
}
