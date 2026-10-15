# Run from the repository root: Rscript scripts/pipeline.R [--refresh]
options(timeout = 180)
required <- c("sf", "MASS", "jsonlite", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
source("scripts/netRainfall.R")
source("scripts/droughtIndex.R")
for (p in c("Data/raw", "Data/processed", "outputs")) dir.create(p, recursive = TRUE, showWarnings = FALSE)
refresh <- "--refresh" %in% commandArgs(TRUE)
# Cached snapshots imported from the ANF refresh on 2026-10-15 UTC.
retrieval_path <- "Data/raw/retrieval.json"
service <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceCurrentEdition_01/MapServer/0"
weather_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/all/USW00093805.dly"
get_file <- function(url, path) {
  tmp <- paste0(path, ".download")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  if (!file.rename(tmp, path)) stop("Cannot save ", path)
}
get_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  download.file(url, tmp, quiet = TRUE, mode = "wb")
  x <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  if (!is.null(x$error)) stop(jsonlite::toJSON(x$error, auto_unbox = TRUE))
  x
}
query_url <- function(base, args) paste0(base, "/query?", paste(paste0(names(args), "=",
  vapply(args, URLencode, character(1), reserved = TRUE)), collapse = "&"))
# Keep the original study boundary fixed so historical comparisons retain their footprint.
boundary_path <- "Data/raw/anf-boundary.gpkg"
if (!file.exists(boundary_path)) {
  b <- sf::st_read("archive/reference/S_USA.NFSLandUnit", quiet = TRUE)
  b <- b[b$NFSLANDU_2 == "Apalachicola National Forest", c("NFSLANDU_2", "geometry")]
  stopifnot(nrow(b) == 1)
  sf::st_write(b, boundary_path, quiet = TRUE)
}
boundary <- sf::st_make_valid(sf::st_read(boundary_path, quiet = TRUE))
fire_path <- "Data/raw/fires-bbox.json"
if (refresh || !file.exists(fire_path)) {
  get_file(paste0(service, "?f=pjson"), "Data/raw/fire-service.json")
  bbox <- sf::st_bbox(sf::st_transform(boundary, 4326))
  args <- list(f = "json", where = "state='FL'", geometry = paste(bbox, collapse = ","),
               geometryType = "esriGeometryEnvelope", inSR = "4326", spatialRel = "esriSpatialRelIntersects",
               returnIdsOnly = "true")
  ids <- unlist(get_json(query_url(service, args))$objectIds)
  if (!length(ids)) stop("No fire IDs returned.")
  features <- list()
  for (batch in split(ids, ceiling(seq_along(ids) / 500))) {
    page <- get_json(query_url(service, list(f = "json", objectIds = paste(batch, collapse = ","),
       outFields = "fod_id,fire_year,discovery_date,fire_size,fire_size_class,nwcg_cause_classification,nwcg_general_cause,latitude,longitude",
       returnGeometry = "false")))
    if (isTRUE(page$exceededTransferLimit)) stop("Server truncated fire query.")
    features <- c(features, page$features)
  }
  stopifnot(length(features) == length(ids))
  jsonlite::write_json(list(features = features), fire_path, auto_unbox = TRUE)
}
metadata <- jsonlite::fromJSON("Data/raw/fire-service.json")
coverage <- as.integer(format(as.POSIXct(unlist(metadata$timeInfo$timeExtent) / 1000,
                                       origin = "1970-01-01", tz = "UTC"), "%Y"))
fires <- jsonlite::fromJSON(fire_path)$features$attributes
stopifnot(!anyDuplicated(fires$fod_id), all(is.finite(fires$fire_size)), all(fires$fire_size >= 0))
points <- sf::st_as_sf(fires, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
points <- sf::st_transform(points, sf::st_crs(boundary))
fires <- fires[lengths(sf::st_intersects(points, sf::st_union(boundary))) > 0, ]
fires$Date <- as.Date(as.POSIXct(fires$discovery_date / 1000, origin = "1970-01-01", tz = "UTC"))
fires$Year <- as.integer(format(fires$Date, "%Y"))
fires$Month <- as.integer(format(fires$Date, "%m"))
stopifnot(all(fires$Year == fires$fire_year))
write.csv(fires, "Data/processed/anf-fires.csv", row.names = FALSE)
if (refresh || !file.exists("Data/raw/USW00093805.dly")) get_file(weather_url, "Data/raw/USW00093805.dly")
# NOAA fixed-width GHCN: reject every nonblank quality flag; convert tenths SI.
lines <- readLines("Data/raw/USW00093805.dly")
lines <- lines[substr(lines, 18, 21) %in% c("PRCP", "TMAX", "TMIN")]
weather <- do.call(rbind, lapply(seq_len(31), function(day) {
  start <- 22 + (day - 1) * 8
  value <- as.numeric(substr(lines, start, start + 4))
  quality <- substr(lines, start + 6, start + 6)
  value[value == -9999 | quality != " "] <- NA_real_
  # Invalid end-of-month slots carry -9999. Date parsing excludes them.
  date_text <- sprintf("%s-%s-%02d", substr(lines, 12, 15), substr(lines, 16, 17), day)
  dates <- as.Date(date_text, format = "%Y-%m-%d")
  data.frame(Date = dates, Element = substr(lines, 18, 21), Value = value / 10)
}))
weather <- weather[!is.na(weather$Date), ]
stopifnot(!anyDuplicated(weather[c("Date", "Element")]))
w <- reshape(weather, idvar = "Date", timevar = "Element", direction = "wide")
names(w) <- sub("Value\\.", "", names(w))
# Start with a full calendar year, decades before fitting; exclude partial current data.
w <- merge(data.frame(Date = seq(as.Date("1944-01-01"), max(weather$Date[is.finite(weather$Value)]), by = "day")), w, all.x = TRUE)
w <- w[order(w$Date), ]
w$Year <- as.integer(format(w$Date, "%Y")); w$Month <- as.integer(format(w$Date, "%m"))
# No unrecorded rainfall imputation. Flag gaps and require a full year of complete
# daily weather before using drought again, reinitializing Q at 400 after a gap.
w$complete <- is.finite(w$PRCP) & is.finite(w$TMAX)
w$KBDI <- NA_real_; w$days_since_gap <- 0L
runs <- split(seq_len(nrow(w)), cumsum(c(TRUE, diff(w$complete) != 0)))
for (idx in runs) if (w$complete[idx[1]]) {
  w$KBDI[idx] <- droughtIndex(400, 60, w$TMAX[idx] * 1.8 + 32, netRainfall(w$PRCP[idx] / 25.4))$Ql
  w$days_since_gap[idx] <- seq_along(idx)
}
w$Qlm <- w$KBDI * .254
write.csv(w, "Data/processed/daily-weather.csv", row.names = FALSE)
write.csv(w[!w$complete, c("Date", "PRCP", "TMAX")], "outputs/weather-gaps.csv", row.names = FALSE)
seasonal <- expand.grid(Year = seq(coverage[1], coverage[2]), Cause = c("All", "Human", "Natural"), stringsAsFactors = FALSE)
seasonal$nFires <- 0L; seasonal$TAB <- 0
for (i in seq_len(nrow(seasonal))) {
  keep <- fires$Year == seasonal$Year[i] & fires$Month %in% 5:7
  if (seasonal$Cause[i] != "All") keep <- keep & !is.na(fires$nwcg_cause_classification) &
    fires$nwcg_cause_classification == seasonal$Cause[i]
  seasonal$nFires[i] <- sum(keep)
  seasonal$TAB[i] <- sum(fires$fire_size[keep])
}
apr <- w[format(w$Date, "%m-%d") == "04-30", c("Year", "Qlm", "days_since_gap")]
names(apr)[2] <- "QlmLastDay"
seasonal <- merge(seasonal, apr, by = "Year", all.x = TRUE)
seasonal$eligible <- !is.na(seasonal$days_since_gap) & seasonal$days_since_gap >= 365
seasonal$DrynessCm <- seasonal$QlmLastDay / 10
write.csv(seasonal, "Data/processed/seasonal-fires.csv", row.names = FALSE)
# Count forecasts by cause, trained only on preceding eligible years.
fit_nb <- function(data, predictor) {
  m <- MASS::glm.nb(if (predictor) nFires ~ DrynessCm else nFires ~ 1, data = data,
                    control = glm.control(maxit = 100))
  if (!isTRUE(m$converged) || !is.null(m$th.warn)) stop("Negative binomial fit failed convergence.")
  m
}
models <- list(); predictions <- list(); coefficients <- list()
for (cause in c("All", "Human", "Natural")) {
  d <- seasonal[seasonal$Cause == cause & seasonal$eligible, ]; d <- d[order(d$Year), ]
  if (nrow(d) < 16) stop("Insufficient eligible seasons for ", cause)
  for (predictor in c(FALSE, TRUE)) {
    label <- if (predictor) "Drought" else "Climatology"
    key <- paste(cause, label, sep = "_")
    models[[key]] <- fit_nb(d, predictor)
    co <- as.data.frame(summary(models[[key]])$coefficients)
    coefficients[[key]] <- data.frame(Cause = cause, Model = label, Term = rownames(co), co, row.names = NULL)
    for (i in seq.int(16, nrow(d))) {
      m <- fit_nb(d[seq_len(i - 1), ], predictor)
      mu <- as.numeric(predict(m, newdata = d[i, ], type = "response"))
      predictions[[length(predictions) + 1L]] <- data.frame(Cause = cause, Model = label,
        Year = d$Year[i], Observed = d$nFires[i], Expected = mu,
        Lower90 = qnbinom(.05, mu = mu, size = m$theta), Upper90 = qnbinom(.95, mu = mu, size = m$theta),
        LogScore = -dnbinom(d$nFires[i], mu = mu, size = m$theta, log = TRUE))
    }
  }
}
predictions <- do.call(rbind, predictions)
metrics <- do.call(rbind, lapply(split(predictions, interaction(predictions$Cause, predictions$Model)), function(p)
  data.frame(Cause = p$Cause[1], Model = p$Model[1], N = nrow(p),
    MAE = mean(abs(p$Observed - p$Expected)), RMSE = sqrt(mean((p$Observed - p$Expected)^2)),
    MeanNegLogScore = mean(p$LogScore), Coverage90 = mean(p$Observed >= p$Lower90 & p$Observed <= p$Upper90))))
write.csv(predictions, "outputs/hindcasts.csv", row.names = FALSE)
write.csv(metrics, "outputs/metrics.csv", row.names = FALSE)
write.csv(do.call(rbind, coefficients), "outputs/coefficients.csv", row.names = FALSE)
saveRDS(models, "outputs/count-models.rds")
# Expected means and conditional NB predictive intervals, not parameter uncertainty.
forecast <- list()
for (cause in c("All", "Human", "Natural")) {
  future <- apr[apr$Year > coverage[2] & apr$days_since_gap >= 365 & !is.na(apr$days_since_gap), ]
  if (nrow(future)) {
    future$DrynessCm <- future$QlmLastDay / 10
    m <- models[[paste0(cause, "_Drought")]]
    mu <- as.numeric(predict(m, future, type = "response"))
    forecast[[cause]] <- data.frame(Cause = cause, Year = future$Year, Expected = mu,
      Lower90 = qnbinom(.05, mu = mu, size = m$theta), Upper90 = qnbinom(.95, mu = mu, size = m$theta))
  }
}
forecast_table <- if (length(forecast)) do.call(rbind, forecast) else
  data.frame(Cause=character(), Year=integer(), Expected=numeric(), Lower90=numeric(), Upper90=numeric())
write.csv(forecast_table, "outputs/forecasts.csv", row.names = FALSE)
library(ggplot2)
p <- ggplot(seasonal, aes(Year, nFires)) + geom_line() + geom_point() + facet_wrap(~Cause) +
  labs(x = NULL, y = "May–July wildfire count") + theme_minimal()
ggsave("outputs/seasonal-counts.png", p, width = 10, height = 4, dpi = 160, bg = "white")
p <- ggplot(predictions, aes(Year, Expected, color = Model)) + geom_line() +
  geom_point(aes(y = Observed), color = "black") + facet_wrap(~Cause, scales = "free_y") +
  labs(x = NULL, y = "Forecast count; black dots = observed") + theme_minimal()
ggsave("outputs/hindcasts.png", p, width = 10, height = 4, dpi = 160, bg = "white")
if (refresh || !file.exists(retrieval_path)) jsonlite::write_json(
  list(retrieved_utc = if (refresh) format(Sys.time(), tz="UTC", usetz=TRUE) else "2026-10-15 UTC",
       imported_from = if (refresh) "Public source endpoints" else "pred-wildfires verified source snapshots"),
  retrieval_path, auto_unbox=TRUE, pretty=TRUE)
manifest <- list(run_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), fire_service = service,
  fire_coverage = coverage, weather_url = weather_url, weather_last_day = as.character(max(w$Date)),
  boundary = "Original NFSLandUnit study footprint; not refreshed", initial_kbdi = 400, annual_rain_inches = 60,
  missing_policy = "No imputation; 365 complete days required since initialization or latest gap",
  retrieval = jsonlite::fromJSON(retrieval_path),
  raw_md5 = as.list(tools::md5sum(list.files("Data/raw", full.names = TRUE))))
jsonlite::write_json(manifest, "outputs/manifest.json", pretty = TRUE, auto_unbox = TRUE)
writeLines(trimws(capture.output(sessionInfo()),which="right"), "outputs/session-info.txt")
print(metrics)
cat("Fire coverage:", coverage, "; ANF records:", nrow(fires), "; eligible seasons per cause:", sum(seasonal$eligible) / 3, "\n")
