# Historical spatial representativeness check using the original cached PRISM archive.
# This script does not download continental daily grids. Use --reextract to rebuild.
# Rscript scripts/prism-check.R [--prism-dir=/absolute/path/to/PRISM] [--reextract]
for (p in c("terra", "sf", "jsonlite", "ggplot2")) stopifnot(requireNamespace(p, quietly = TRUE))
source("scripts/netRainfall.R"); source("scripts/droughtIndex.R")
args <- commandArgs(TRUE)
arg <- args[grepl("^--prism-dir=", args)]
root <- if (length(arg)) sub("^--prism-dir=", "", arg[1]) else "Data/PRISM"
cache <- "Data/raw/prism-anf-extracted.rds"
if (!file.exists(cache) || "--reextract" %in% args) {
  if (!dir.exists(root)) stop("PRISM archive missing. Supply --prism-dir or restore the extracted cache.")
  dates <- seq(as.Date("2009-01-01"), as.Date("2018-12-31"), by = "day")
  paths <- function(variable) {
    stem <- paste0("PRISM_", variable, "_stable_4kmD2_", format(dates, "%Y%m%d"), "_bil")
    file.path(root, stem, paste0(stem, ".bil"))
  }
  tp <- paths("tmax"); pp <- paths("ppt")
  stopifnot(all(file.exists(tp)), all(file.exists(pp)))
  boundary <- sf::st_read("Data/raw/anf-boundary.gpkg", quiet = TRUE)
  r <- terra::rast(tp[1])
  polygon <- terra::vect(sf::st_transform(boundary, terra::crs(r)))
  template <- terra::crop(r, polygon)
  extracted <- terra::extract(template, polygon, cells = TRUE, exact = TRUE)
  cells <- extracted$cell
  xy <- terra::xyFromCell(template, cells)
  areas <- terra::values(terra::cellSize(template, unit = "km"))[cells]
  weights <- extracted$fraction * areas
  weights <- weights / sum(weights)
  tmax <- ppt <- matrix(NA_real_, nrow = length(dates), ncol = length(cells))
  for (i in seq_along(dates)) {
    rt <- terra::crop(terra::rast(tp[i]), polygon)
    rp <- terra::crop(terra::rast(pp[i]), polygon)
    terra::compareGeom(rt, template, stopOnError = TRUE)
    terra::compareGeom(rp, template, stopOnError = TRUE)
    tmax[i, ] <- terra::values(rt)[cells]
    ppt[i, ] <- terra::values(rp)[cells]
    if (i %% 365 == 0) message("Extracted PRISM through ", dates[i])
  }
  source_files <- c(tp, pp)
  info <- file.info(source_files)
  inventory <- data.frame(File = basename(source_files), Bytes = info$size,
                          ModifiedUTC = format(info$mtime, tz = "UTC", usetz = TRUE))
  write.csv(inventory, "Data/raw/prism-source-inventory.csv", row.names = FALSE)
  saveRDS(list(dates = dates, xy = xy, weights = weights, tmax = tmax, ppt = ppt,
    crs = terra::crs(template), source = "Local original PRISM stable 4kmD2 2009–2018 archive",
    extracted_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)), cache)
}
x <- readRDS(cache)
stopifnot(nrow(x$tmax) == length(x$dates), identical(dim(x$tmax), dim(x$ppt)),
          all(diff(x$dates) == 1), abs(sum(x$weights) - 1) < 1e-8)
kbdi <- matrix(NA_real_, nrow(x$tmax), ncol(x$tmax))
valid_days <- matrix(FALSE, nrow(x$tmax), ncol(x$tmax))
for (j in seq_len(ncol(x$tmax))) {
  good <- is.finite(x$tmax[, j]) & is.finite(x$ppt[, j]) & x$ppt[, j] >= 0
  runs <- split(seq_along(good), cumsum(c(TRUE, diff(good) != 0)))
  for (idx in runs) if (good[idx[1]]) {
    kbdi[idx, j] <- droughtIndex(400, 60, x$tmax[idx, j] * 1.8 + 32,
                                netRainfall(x$ppt[idx, j] / 25.4))$Ql * .254
    valid_days[idx, j] <- seq_along(idx) >= 365
  }
}
w <- read.csv("Data/processed/daily-weather.csv")
w <- w[match(as.character(x$dates), w$Date), ]
stopifnot(!anyNA(w$Date))
key <- format(x$dates, "%m-%d")
rows <- list()
for (j in seq_len(ncol(kbdi))) {
  keep <- valid_days[, j] & w$days_since_gap >= 365 & is.finite(w$Qlm) & is.finite(kbdi[, j])
  a <- kbdi[keep, j]; b <- w$Qlm[keep]; k <- key[keep]
  # Remove the common seasonal cycle before assessing anomaly correspondence.
  anomaly_a <- a - ave(a, k, FUN = mean)
  anomaly_b <- b - ave(b, k, FUN = mean)
  april <- keep & key == "04-30"
  rows[[j]] <- data.frame(Cell = j, Longitude = x$xy[j, 1], Latitude = x$xy[j, 2],
    AreaWeight = x$weights[j], N = sum(keep), RawR2 = cor(a, b)^2,
    AnomalyR2 = cor(anomaly_a, anomaly_b)^2, MeanBiasMm = mean(a - b),
    RMSEmm = sqrt(mean((a - b)^2)), April30N = sum(april),
    April30R = if (sum(april) >= 4) cor(kbdi[april, j], w$Qlm[april]) else NA_real_)
}
summary <- do.call(rbind, rows)
write.csv(summary, "outputs/prism-station-check.csv", row.names = FALSE)
# Require all cells to be eligible; never let missing area silently change the footprint.
good <- apply(valid_days & is.finite(kbdi), 1, all)
forest <- rep(NA_real_, nrow(kbdi)); forest[good] <- as.vector(kbdi[good, , drop = FALSE] %*% x$weights)
write.csv(data.frame(Date = x$dates, ForestDeficitMm = forest, StationDeficitMm = w$Qlm),
          "Data/processed/prism-forest-deficit.csv", row.names = FALSE)
library(ggplot2)
boundary <- sf::st_read("Data/raw/anf-boundary.gpkg", quiet = TRUE)
p <- ggplot(summary, aes(Longitude, Latitude, fill = AnomalyR2)) + geom_tile() +
  geom_sf(data = boundary, inherit.aes = FALSE, fill = NA, linewidth = .35) +
  scale_fill_viridis_c(limits = c(0, 1), name = "Daily anomaly\nR²") +
  coord_sf() + theme_minimal() + labs(x = NULL, y = NULL,
    title = "Airport–forest drought correspondence beyond seasonality",
    subtitle = "Historical PRISM check: 2009–2018 archive, with one-year spin-up")
ggsave("outputs/prism-anomaly-correspondence.png", p, width = 8, height = 5, dpi = 180, bg = "white")
jsonlite::write_json(list(period = range(as.character(x$dates)), source = x$source,
  extracted_cache_md5 = unname(tools::md5sum(cache)), cells = nrow(summary),
  raw_r2_area_weighted = weighted.mean(summary$RawR2, summary$AreaWeight),
  anomaly_r2_area_weighted = weighted.mean(summary$AnomalyR2, summary$AreaWeight),
  status = "Recomputed from historical cached grids; PRISM observations have not been extended beyond 2018"),
  "outputs/prism-manifest.json", pretty = TRUE, auto_unbox = TRUE)
print(summary(summary$AnomalyR2))
